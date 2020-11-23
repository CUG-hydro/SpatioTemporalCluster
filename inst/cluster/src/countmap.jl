using StatsBase

# function countmap(vec::AbstractArray{T, 1}) where {T <: Int}
#     keys = unique(vec)
#     counts = zeros(Int, length(keys))
    
#     @inbounds for i in eachindex(vec)
#         for k in eachindex(keys)
#             if vec[i] == keys[k]
#                 counts[k] += 1
#                 break
#             end
#         end
#     end
#     Dict(keys[i] => counts[i] for i in eachindex(keys))
# end

# export countmap
