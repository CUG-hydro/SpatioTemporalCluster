function replace_mat!(mat::AbstractArray{T,2}, mask_vec::Array{CartesianIndex{2},1}, 
    val_org::T, val_new::T) where {T <: Real}

    # inds = CartesianIndices(mat)
    # nrow, ncol, ntime = size(mat)
    @inbounds for loc in mask_vec
        if mat[loc[1], loc[2]] == val_org; mat[loc[1], loc[2]] = val_new; end
    end
end


function replace_clusterId!(mat::AbstractArray{T,3}, mask_vec::Array{CartesianIndex{2},1}, 
    t::Int, val_org::T, val_new::T) where {T <: Real}

    # inds = CartesianIndices(mat)
    # nrow, ncol, ntime = size(mat)
    @inbounds for k = 1:t
        for loc in mask_vec
            # for i = 1:nrow, j = 1:ncol, k = 1:t
            # for ind = inds
            if mat[loc[1], loc[2], k] == val_org; mat[loc[1], loc[2], k] = val_new; end
        end
    end
end


function replace_cno!(cdo::AbstractArray{T,2}, t::Int, val_org::T, val_new::T) where {T <: Real}
    # inds = CartesianIndices(mat)
    cmax, ntime = size(cdo)
    @inbounds for i = 1:cmax, k = 1:t
        if cdo[i, k] == val_org; cdo[i, k] = val_new; end
    end
end
