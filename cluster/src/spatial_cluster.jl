using Printf

mutable struct clusterStatus
    status_prev::Bool # previous status, default false
    ID ::Int
    nrow::Int
    ncol::Int
end
# set_option(opt) = opt.num += 1

# find Cluster for each given point
# mat_bool: continuous TRUE will be grouped together
# # return
# - `0`: not TRUE in `mat_bool`
# - `1`: cells less than minCells
# - `NO`: cluster NO, which is great 
function find_clutser(mat_bool::AbstractArray{Bool,2}, IdCluster, i::Int, j::Int, opt; 
    miss_val::Int = -999) 

    if (IdCluster[i, j] != miss_val); return; end    
    if !mat_bool[i, j]  # not urban
        IdCluster[i, j] = 0 # 
        return
    else
        if (!opt.status_prev) 
            opt.ID += 1
            opt.status_prev = true
        end
        IdCluster[i, j] = opt.ID

        # search for children
        pos = [0 -1; 0 1; -1 0; 1 0]
        # pos = [0 -1; 0 1; 1 0]
        for k = 1:size(pos)[1]
            i2 = pos[k, 1] + i
            j2 = pos[k, 2] + j

            if (i2 > opt.nrow || i2 <= 0 || j2 > opt.ncol || j2 <= 0); continue; end
            find_clutser(mat_bool, IdCluster, i2, j2, opt)
        end
    end
end

# only TRUE value will be accounted.
function spatial_cluster!(mat_bl::AbstractArray{Bool, 2}, IdCluster::AbstractArray{Int, 2};
    ID0::Int = 0)

    nrow, ncol = size(mat_bl)
    # IdCluster = ones(Int, nrow, ncol) .* -999
    opt = clusterStatus(false, ID0, nrow, ncol)
    inds = CartesianIndices(mat_bl)
    # inds = findall(mat_bl)
    iter = 0
    @inbounds for ind = inds
        iter += 1
        i = ind[1]
        j = ind[2]
        find_clutser(mat_bl, IdCluster, i, j, opt)
        # if mod(iter, 10000) == 0; println("($i, $j): $opt"); end
        opt.status_prev = false
    end
    IdCluster
end


function spatial_cluster(mat_bl::AbstractArray{Bool, 3}; 
    time_factor::Int = 1000000, 
    minCells::Int = 25)

    dim = size(mat_bl)
    IdClusters = ones(Int, dim) .* -999
    ntime = dim[3]
    nC = zeros(Int, ntime)
    cno_list = []

    for t in 1:ntime
        println("t = $t")
        cluster = @view(IdClusters[:,:,t])
        spatial_cluster!(@view(mat_bl[:,:,t]), cluster, ID0 = time_factor*t)
        
        # count how many clusters and filter small clusters
        info = countmap(cluster[:]) # filter 
        ids_short = filter(x -> ( x[2] < minCells ), info) |> keys # 1: key, 2: val

        # save("debug.jld", "ids_short", ids_short, "cluster", cluster)
        # println("ids_short: ", length(ids_short))
        @inbounds for i in eachindex(cluster)
            if cluster[i] in ids_short; cluster[i] = -1; end
        end

        ids_long = filter(x -> ( x[2] > minCells ), info) |> keys
        ids_long = setdiff(ids_long, 0)
        
        nC[t] = length(ids_long)
        # get cno info 
        push!(cno_list, ids_long)
        # cluster = spatial_cluster!(mat_bl[:,:,t], IdCluster[:,:,t])
        # IdCluster[:,:,t] = cluster
    end
    cno = list2mat(cno_list)
    nC, cno, IdClusters
end


function list2mat(list)
    nc = []
    for x in list; push!(nc, length(x)); end

    cmax = maximum(nc)
    ntime = length(list)
    mat = zeros(Int, cmax, ntime)
    println(size(mat))

    for i = 1:ntime
        x = list[i]
        mat[1:length(x), i] = collect(x)
    end
    mat
end


export spatial_cluster

