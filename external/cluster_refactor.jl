# 重新编号，考虑时间
function cluster_refactor!(IdClusters::AbstractArray{Int, 3}; 
    factor = 1e6, miss_val::Int = 0)

    nlat, nlon, ntime = size(IdClusters)
    # 1. nC: [ntime]
    nC = zeros(Int, ntime)
    for t = 1:ntime
        nC[t] = maximum(@view(IdClusters[:,:,t]))
    end

    maxNc = maximum(nC)
    cno = ones(Int, ntime, maxNc) .* 0
    # nC = zeros(Int, ntime)
    # 2. assign value to cno
    @inbounds for t = 1:ntime
        println("t = $t")
        if nC[t] == 0; continue; end
        cluster = @view(IdClusters[:, :, t])
        # cluster = IdClusters[:, :, t]
        ind_valid = cluster .!= miss_val
        # # high efficient way
        cluster[ind_valid] .= cluster[ind_valid] .+ (t * factor)
        
        cno_t = unique(cluster[ind_valid]) |> sort
        cno[t, 1:length(cno_t)] .= cno_t
        # cluster[ind_nan] .= miss_val
        ## second
    end
    nC, cno
end
