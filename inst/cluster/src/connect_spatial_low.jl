
mutable struct clusterStatus2
    ID
    pos
    nrow
    ncol
end


function find_cluster_forward(matrix::AbstractArray{Bool, 2}, clusterID::AbstractArray{Int, 2}, 
    i::Int, j::Int, opt; miss_val::Int = -999)

    if matrix[i, j] && (clusterID[i, j] == miss_val)
        opt.ID += 1
        clusterID[i, j] = opt.ID
    end

    @inbounds for k = 1:size(opt.pos)[1]
         i2 = opt.pos[k, 1] + i
         j2 = opt.pos[k, 2] + j
         if (i2 > opt.nrow || i2 <= 0 || j2 > opt.ncol || j2 <= 0); continue; end
         
         if (matrix[i2, j2]) && (clusterID[i, j] == miss_val)
            clusterID[i2, j2] == clusterID[i, j]
         end
    end
    clusterID
end

# # set_option(opt) = opt.num += 1
function find_cluster_backward(clusterID::AbstractArray{Int, 2}, inds::Array{CartesianIndex{2},1}, 
    i, j, opt; miss_val::Int = -999)

    for k = 1:size(opt.pos)[1]
        i2 = opt.pos[k, 1] + i
        j2 = opt.pos[k, 2] + j
        if (i2 > opt.nrow || i2 <= 0 || j2 > opt.ncol || j2 <= 0); continue; end
        
        if (clusterID[i, j] != miss_val) && (clusterID[i2, j2] != miss_val) && 
                (clusterID[i, j] != clusterID[i2, j2])
            opt.ID = opt.ID - 1
            # replace value
            id_org = clusterID[i2, j2]
            id_new = clusterID[i, j]
            replace_mat!(clusterID, inds, id_org, id_new)
        end
    end
end



# ncell_connect
function connect_spatial_low!(matrix::AbstractArray{Bool,2}, clusterID::AbstractArray{Int,2}; 
    diag = false, verbose = true, miss_val::Int = -999)
    
    if diag 
        pos = [-1 -1; -1 1; 1 -1; 1 1; 0 -1; 0 1; -1 0; 1 0]
    else
        pos = [0 -1; 0 1; -1 0; 1 0]
    end

    nrow, ncol = size(matrix)
    opt = clusterStatus2(0, pos, nrow, ncol)
    # clusterID = ones(Int, nrow, ncol) .* miss_val

    inds = findall(matrix) # 
    
    for k in 1:length(inds)
        if verbose && mod(k, 10000) == 0; println("forward: k = $k"); end
        i = inds[k][1]
        j = inds[k][2]
        find_cluster_forward(matrix, clusterID, i, j, opt; miss_val = miss_val)
    end

    # 2. backward
    for k in length(inds):-1:1
        if verbose && mod(k, 10000) == 0; println("forward: k = $k"); end

        i = inds[k][1]
        j = inds[k][2]
        find_cluster_backward(clusterID, inds, i, j, opt; miss_val = miss_val)
    end
    # clusterID
end


function connect_spatial_low(matrix::AbstractArray{Bool,2}; diag = false, verbose = true, miss_val::Int = -999)
    nrow, ncol = size(matrix)
    clusterID = ones(Int, nrow, ncol) .* miss_val
    connect_spatial_low!(matrix, clusterID; diag = diag, verbose = verbose, miss_val)
    clusterID
end


export connect_spatial_low
