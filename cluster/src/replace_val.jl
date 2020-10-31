function replace_val!(mat::AbstractArray{T,3}, t::Int, val_org::T, val_new::T) where {T <: Real}
    # inds = CartesianIndices(mat)
    nrow, ncol, ntime = size(mat)
    @inbounds for i = 1:nrow, j = 1:ncol, k = 1:t
        # for ind = inds
        if mat[i, j, k] == val_org; mat[i, j, k] = val_new; end
    end
end

function replace_val!(mat::AbstractArray{T,2}, t::Int, val_org::T, val_new::T) where {T <: Real}
    # inds = CartesianIndices(mat)
    nrow, ntime = size(mat)
    @inbounds for i = 1:nrow, k = 1:t
        if mat[i, k] == val_org; mat[i, k] = val_new; end
    end
end
