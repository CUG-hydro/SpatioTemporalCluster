function connect_spatial(mat_bl::AbstractArray{Bool, 3}; 
    method = "tree", 
    ncell_connect::Int = 25, factor::Int = 1000000, diag::Bool = false)

    dim = size(mat_bl)
    clusterIds = ones(Int, dim) .* -999
    ntime = dim[3]
    nC = zeros(Int, ntime)
    cno_list = []

    FUNs = Dict(
        "tree" => connect_spatial_tree,
        "recursive" => connect_spatial_recursive,
        "low" => connect_spatial_low)
    FUN = FUNs[method]
    
    for t in 1:ntime
        # println("t = $t")
        # clusterId = @view(clusterIds[:,:,t])
        clusterId = FUN(@view(mat_bl[:,:,t]), diag = diag) 
        clusterId[clusterId .> 0] .+= factor * t
        # println("ok")

        ids_long = mask_SmallCluster!(clusterId, ncell_connect)
        nC[t] = length(ids_long)
        push!(cno_list, ids_long)

        clusterIds[:,:,t] = clusterId
    end
    cno = list2mat(cno_list)
    nC, cno, clusterIds
end


# @return 
# + `ids_long`: ids with enough `ncell_connect`
# + `clusterId` with be modified directly by physical address
#   - `-1`: small cluster
#   - ` 0`: non cluster
function mask_SmallCluster!(clusterId::AbstractArray{Int,2}, ncell_connect::Int)
    # count how many clusters and filter small clusters
    info = countmap(clusterId[:]) # filter 
    ids_short = filter(x -> ( x[2] < ncell_connect ), info) |> keys # 1: key, 2: val

    # save("debug.jld", "ids_short", ids_short, "cluster", cluster)
    # println("ids_short: ", length(ids_short))
    ind = findall(clusterId .> 0)
    @inbounds for i in ind
        if clusterId[i] in ids_short; clusterId[i] = -1; end
    end

    ids_long = filter(x -> ( x[2] > ncell_connect ), info) |> keys
    ids_long = setdiff(ids_long, 0)
    ids_long
end


function list2mat(list)
    nc = []
    for x in list; push!(nc, length(x)); end

    cmax = maximum(nc)
    ntime = length(list)
    mat = zeros(Int, cmax, ntime)
    # println(size(mat))
    for i = 1:ntime
        x = list[i]
        mat[1:length(x), i] = collect(x)
    end
    mat
end


export connect_spatial
