import Base.push!  # for DisjointMinSets
using Base.Cartesian # TODO: delete this

include("DataStructures.jl")

"""
```
label = label_components(tf, [connectivity])
label = label_components(tf, [region])
```

Find the connected components in a binary array `tf`. There are two forms that
`connectivity` can take:

- It can be a boolean array of the same dimensionality as `tf`, of size 1 or 3
  along each dimension. Each entry in the array determines whether a given
  neighbor is used for connectivity analyses. For example, `connectivity = trues(3,3)`
  would use 8-connectivity and test all pixels that touch the
  current one, even the corners.

- You can provide a list indicating which dimensions are used to determine
  connectivity. For example, `region = [1,3]` would not test neighbors along
  dimension 2 for connectivity. This corresponds to just the nearest neighbors,
  i.e., 4-connectivity in 2d and 6-connectivity in 3d.

The default is `region = 1:ndims(A)`.

The output `label` is an integer array, where 0 is used for background
pixels, and each connected region gets a different integer index.
"""
label_components(A, connectivity = 1:ndims(A), bkg = 0) = label_components!(zeros(Int, size(A)), A, connectivity, bkg)

#### 4-connectivity in 2d, 6-connectivity in 3d, etc.
# But in fact you can choose which dimensions are connected
let _label_components_cache = Dict{Tuple{Int, Vector{Int}}, Function}()
global label_components!
function label_components!(Albl::AbstractArray{Int}, A::Array, region::Union{Dims, AbstractVector{Int}}, bkg = 0)
    uregion = unique(region)
    if isempty(uregion)
        # Each pixel is its own component
        k = 0
        for i = 1:length(A)
            if A[i] != bkg
                k += 1
                Albl[i] = k
            end
        end
        return Albl
    end
    # We're going to compile a version specifically for the chosen region. This should make it very fast.
    key = (ndims(A), uregion)
    if !haskey(_label_components_cache, key)
        # Need to generate the function
        N = length(uregion)
        ND = ndims(A)
        iregion = [Symbol("i_", d) for d in uregion]
        f! = eval(quote
            local lc!
            function lc!(Albl::AbstractArray{Int}, sets, A::Array, bkg)
                offsets = strides(A)[$uregion]
                @nexprs $N d->(offsets_d = offsets[d])
                k = 0
                @nloops $ND i A begin
                    k += 1
                    val = A[k]
                    label = typemax(Int)
                    if val != bkg
                        @nexprs $N d->begin
                            if $iregion[d] > 1  # is this neighbor in-bounds?
                                if A[k-offsets_d] == val  # if the two have the same value...
                                    newlabel = Albl[k-offsets_d]
                                    if label != typemax(Int) && label != newlabel
                                        label = union!(sets, label, newlabel)  # ...merge labels...
                                    else
                                        label = newlabel  # ...and assign the smaller to current pixel
                                    end
                                end
                            end
                        end
                        if label == typemax(Int)
                            label = push!(sets)   # there were no neighbors, create a new label
                        end
                        Albl[k] = label
                    end
                end
                Albl
            end
        end)
        _label_components_cache[key] = f!
    else
        f! = _label_components_cache[key]
    end
    sets = DisjointMinSets()
    eval(:($f!($Albl, $sets, $A, $bkg)))
    # Now parse sets to find the labels
    newlabel = minlabel(sets)
    for i = 1:length(A)
        if A[i] != bkg
            Albl[i] = newlabel[find_root!(sets, Albl[i])]
        end
    end
    Albl
end
end # let
label_components!(Albl::AbstractArray{Int}, A::BitArray, region::Union{Dims, AbstractVector{Int}}, bkg = 0) = label_components!(Albl, convert(Array{Bool}, A), region, bkg)

#### Arbitrary connectivity
for N = 1:4
    @eval begin
        function label_components!(Albl::AbstractArray{Int,$N}, A::AbstractArray, connectivity::Array{Bool,$N}, bkg = 0)
            if isempty(connectivity) || !any(connectivity)
                # Each pixel is its own component
                k = 0
                for i = 1:length(A)
                    if A[i] != bkg
                        k += 1
                        Albl[i] = k
                    end
                end
                return Albl
            end
            for d = 1:ndims(connectivity)
                (isodd(size(connectivity, d)) && connectivity == reverse(connectivity, dims=d)) || error("connectivity must be symmetric")
            end
            size(Albl) == size(A) || throw(DimensionMismatch("size(Albl) must be equal to size(A)"))
            @nexprs $N d->(halfwidth_d = size(connectivity,d)>>1)
            @nexprs $N d->(offset_d = 1+halfwidth_d)
            cstride_1 = 1
            @nexprs $N d->(cstride_{d+1} = cstride_d * size(connectivity,d))
            @nexprs $N d->(k_d = 0)
            sets = DisjointMinSets()
            @nloops $N i A begin
                val = @nref($N, A, i)
                label = typemax(Int)
                if val != bkg
                    @nloops($N, j,
                            d->(max(-halfwidth_d, 1-i_d):min(halfwidth_d, size(A,d)-i_d)),  # loop range
                            d->(k_{d-1} = k_d + j_d*cstride_d; # break if linear index is >= 0
                                d > 1 ? (if k_{d-1} > 0 break end) : (if k_0 >= 0 break end);
                                jj_d = j_d+offset_d; ii_d = i_d+j_d),    # pre-expression
                            begin
                        if @nref($N, connectivity, jj)
                            if @nref($N, A, ii) == val
                                newlabel = @nref($N, Albl, ii)
                                if label != typemax(Int) && label != newlabel
                                    label = union!(sets, label, newlabel)
                                else
                                    label = newlabel
                                end
                            end
                        end
                    end)
                    if label == typemax(Int)
                        label = push!(sets)
                    end
                    @nref($N, Albl, i) = label
                end
            end
            # Now parse sets to find the labels
            newlabel = minlabel(sets)
            for i = 1:length(A)
                if A[i]!=bkg
                    Albl[i] = newlabel[find_root!(sets, Albl[i])]
                end
            end
            Albl
        end
        label_components!(Albl::AbstractArray{Int,$N}, A::AbstractArray, connectivity::BitArray{$N}, bkg = 0) =
            label_components!(Albl, A, convert(Array{Bool}, connectivity), bkg)
    end
end


function minlabel(sets::DisjointMinSets)
    out = Vector{Int}(undef, length(sets.parents))
    k = 0
    for i = 1:length(sets.parents)
        if sets.parents[i] == i
            k += 1
        end
        out[i] = k
    end
    out
end

"`component_boxes(labeled_array)` -> an array of bounding boxes for each label, including the background label 0"
function component_boxes(img::AbstractArray{Int})
    nd = ndims(img)
    n = [Vector{Int64}[ fill(typemax(Int64),nd), fill(typemin(Int64),nd) ]
            for i=0:maximum(img)]
    s = CartesianIndices(size(img))
    for i=1:length(img)
        vcur = s[i]
        vmin = n[img[i]+1][1]
        vmax = n[img[i]+1][2]
        for d=1:nd
            vmin[d] = min(vmin[d], vcur[d])
            vmax[d] = max(vmax[d], vcur[d])
        end
    end
    map(x->map(y->tuple(y...),x),n)
end

"`component_lengths(labeled_array)` -> an array of areas (2D), volumes (3D), etc. for each label, including the background label 0"
function component_lengths(img::AbstractArray{Int})
    n = zeros(Int64,maximum(img)+1)
    for i=1:length(img)
        n[img[i]+1]+=1
    end
    n
end

"`component_indices(labeled_array)` -> an array of pixels for each label, including the background label 0"
function component_indices(img::AbstractArray{Int})
    n = [Int64[] for i=0:maximum(img)]
    for i=1:length(img)
      push!(n[img[i]+1],i)
    end
    n
end

"`component_subscripts(labeled_array)` -> an array of pixels for each label, including the background label 0"
function component_subscripts(img::AbstractArray{Int})
    n = [Tuple[] for i=0:maximum(img)]
    s = CartesianIndices(size(img))
    for i=1:length(img)
      push!(n[img[i]+1],s[i])
    end
    n
end

"`component_centroids(labeled_array)` -> an array of centroids for each label, including the background label 0"
function component_centroids(img::AbstractArray{Int,N}) where N
    len = length(0:maximum(img))
    n = fill(zero(CartesianIndex{N}), len)
    counts = fill(0, len)
    @inbounds for I in CartesianIndices(size(img))
        v = img[I] + 1
        n[v] += I
        counts[v] += 1
    end
    map(v -> n[v].I ./ counts[v], 1:len)
end

"""
Highly efficient julia `connect_spatial`

```
bwlabel(mat, diag)
```
"""
function connect_spatial_tree(mat::AbstractArray{Bool, 2}; diag::Bool = false)
    # wrapper for Images.label_components
    #define pixel connectivity matrices for appropriate inputs
    if diag
        connectivityMatrix = Bool.([1 1 1;1 1 1; 1 1 1]);
    else
        connectivityMatrix = Bool.([0 1 0;1 1 1; 0 1 0])
    end
    # if connectivity == 4
    #     connectivityMatrix = Bool.([0 1 0;1 1 1; 0 1 0])
    # elseif connectivity == 8
    #     connectivityMatrix = Bool.([1 1 1;1 1 1; 1 1 1]);
    # else
    #     error("Invalid pixel connectivity. Pixel connectivity can be either 4 or 8");
    # end
    label_components(mat, connectivityMatrix)
end


export connect_spatial_tree
