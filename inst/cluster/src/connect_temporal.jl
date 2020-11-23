# clusterIds: [nlat, nlon, ntime]
function connect_temporal(clusterIds::AbstractArray{Int, 3}; 
    ncell_overlap::Int = 25) 

    ID_min = 0
    nlat, nlon, ntime = size(clusterIds)
    # mask to speed up replace_val
    mask = sum(clusterIds .> 0, dims = 3)[:, :, 1] .> 0 # 
    mask_ind = findall(mask)

    # cmax  = size(cno, 1)
    for t = 2:ntime
        cluster_now = @view(clusterIds[:, :, t])
        cluster_pre = @view(clusterIds[:, :, t-1])
        # The overlaped grids
        ind_overlap = (cluster_now .> ID_min) .& (cluster_pre .> ID_min) 
        ids_raw = cluster_now[ind_overlap] 
        if isempty(ids_raw); continue; end
        
        ids_long = filter(x -> ( x[2] >= ncell_overlap ), countmap(ids_raw))
        ids_now = keys(ids_long) |> collect |> sort
        # NOs_no = cluster_now[ind_overlap] |> unique |> sort
        println("t = $t, clusters: ", length(ids_now), ids_now)

        i = 0
        for id_now in ids_now
            i += 1
            if (mod(i, 1000) == 0); println("(t = $t, i = $i)"); end

            ind_now = findall(cluster_now .== id_now) #|> collect            
            ids_pre = cluster_pre[ind_now]
            ids_pre = unique(ids_pre[ids_pre .> ID_min]) |> sort
            # println(ids_pre)
            # add a while loop, in case of 周边cluster被同化
            while true
                stats_pre = zeros(Bool, length(ids_pre))
                j = 0;
                for id_pre in ids_pre
                    j += 1;
                    ncInter = 0
                    for k in ind_now
                        if (cluster_pre[k] == id_pre); ncInter += 1; end
                    end

                    if ncInter >= ncell_overlap
                        replace_clusterId!(clusterIds, mask_ind, t, id_pre, id_now)
                        # replace_cno!(cno, t, id_pre, id_now)
                        ind_now = findall(cluster_now .== id_now) #|> collect
                        stats_pre[j] = true
                    end
                end
                if (all(stats_pre) || (all(.!(stats_pre)))); break; end
                ids_pre = ids_pre[.!stats_pre]
            end
        end
    end
end

export connect_temporal
