using Printf

mutable struct clusterStatus
    status_prev::Bool # previous status, default false
    ID ::Int
    nrow::Int
    ncol::Int
    diag::Bool
end


# find Cluster for each given point
# mat_bool: continuous TRUE will be grouped together
# # return
# - `0`: not TRUE in `mat_bool`
# - `1`: cells less than ncell_connect
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
        if opt.diag 
            pos = [-1 -1; -1 1; 1 -1; 1 1; 0 -1; 0 1; -1 0; 1 0]
        else
            pos = [0 -1; 0 1; -1 0; 1 0]
        end

        for k = 1:size(pos)[1]
            i2 = pos[k, 1] + i
            j2 = pos[k, 2] + j

            if (i2 > opt.nrow || i2 <= 0 || j2 > opt.ncol || j2 <= 0); continue; end
            find_clutser(mat_bool, IdCluster, i2, j2, opt)
        end
    end
end

# only TRUE value will be accounted.
function connect_spatial_recursive!(mat_bl::AbstractArray{Bool, 2}, IdCluster::AbstractArray{Int, 2};
    diag::Bool = false, ID0::Int = 0)

    nrow, ncol = size(mat_bl)
    # IdCluster = ones(Int, nrow, ncol) .* -999
    # println("hello")
    opt = clusterStatus(false, ID0, nrow, ncol, diag)
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

# This function only for R
# only TRUE value will be accounted.
function connect_spatial_recursive(mat_bl::AbstractArray{Bool, 2};
    diag::Bool = false, ID0::Int = 0)
    nrow, ncol = size(mat_bl)
    IdCluster = ones(Int, nrow, ncol) .* -999
    
    connect_spatial_recursive!(mat_bl, IdCluster; diag = diag, ID0 = ID0)
    IdCluster
end


export connect_spatial_recursive
