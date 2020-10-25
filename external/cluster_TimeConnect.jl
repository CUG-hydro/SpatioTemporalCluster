# IdCluster: [nlat, nlon, ntime]
# cno: [ntime, ncluster]
function cluster_TimeConnect(
    IdClusters::AbstractArray{Int, 3}, 
    cno::AbstractArray{Int, 2}; 
    minOverlapCells::Int = 25, factor::Int = 1000, miss_val::Int = 0) 

    nlat, nlon, ntime = size(IdClusters)
    ntime, cmax  = size(cno)

    for t = 2:ntime
        # for i = 1:nC[t]
        cluster_now = @view(IdClusters[:, :, t])
        cluster_pre = @view(IdClusters[:, :, t-1])
        
        ind_overlap = (cluster_now .!= miss_val) .& (cluster_pre .!= miss_val) 
        NOs_raw = cluster_now[ind_overlap] |> countmap
        NOs_long = filter(x -> ( x[2] >= minOverlapCells ), NOs_raw)
        NOs_no = keys(NOs_long)
        # NOs_no = cluster_now[ind_overlap] |> unique |> sort
        println("clusters: ", length(NOs_no))

        i = 0
        for cno_now in NOs_no
            i += 1
            if (mod(i, 1000) == 0); println("(t = $t, i = $i)"); end

            # foreach c, only loop the overlaped NOs
            ind_now = cluster_now .== cno_now
            NOs_pre = unique(cluster_pre[ind_now]) |> sort
            # print(length(cnos_pre), ", ")
            ## how to speed up the overlap operation?
            for cno_pre in NOs_pre
                # cno_pre = cno[t-1, j]
                if cno_pre == miss_val; continue; end
                ## improve
                # ncInter = overlap_area(IdClusters, t, cno_pre, cno_now)
                ncInter = sum( ind_now .& (cluster_pre .== cno_pre) )
                
                if ncInter >= minOverlapCells
                    replace_val!(IdClusters, t, cno_pre, cno_now)
                    replace_val!(cno, t, cno_pre, cno_now)
                end
            end
        end
    end
    # IdClusters
end

# temp = @view IdClusters[:,:,1:t]
# temp[temp .== cno[t-1, j]] .= cno[t, i] # 赋值操作，不需要指定给idCluster
# cno_temp = cno[1:t, :]
# cno_temp[cno_temp .== cno[t-1, j]] .= cno[t, i]
# cno[1:t, :] = cno_temp

# get the overlap grids at time(t)
function overlap_area(IdClusters::AbstractArray{T,3}, t::Int, cno_pre::Int, cno_now::Int) where {T <: Real}
    nrow, ncol, ntime = size(IdClusters)
    grids = 0
    # sum(IdClusters[:, :, t] .== cno_now) .& (IdClusters[:, :, t-1] .== cno_pre)
    @inbounds for i = 1:nrow, j = 1:ncol
        if (IdClusters[i, j, t] == cno_now) && (IdClusters[i, j, t-1] == cno_pre)
            grids += 1
        end
    end
    grids
end
