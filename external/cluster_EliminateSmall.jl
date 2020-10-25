function cluster_EliminateSmall!(IdClusters::AbstractArray{Int, 3}, minCells::Int = 25)
    dim = size(IdClusters)

    for t in 1:dim[3]
        cluster = @view(IdClusters[:,:,t]) |> countmap
        info = countmap(cluster[:]) 
        ids = filter(x -> ( x[2] < minCells), info) |> keys
        
        for id in ids
            cluster[cluster .== id] .= -1
        end
        # spatial_cluster!(@view(IdClusters[:,:,t]), )
        # cluster = spatial_cluster!(mat_bl[:,:,t], idCluster[:,:,t])
        # idCluster[:,:,t] = cluster
    end
end
