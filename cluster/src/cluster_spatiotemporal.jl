function cluster_spatiotemporal(arr_3d::AbstractArray{Bool, 3}; 
    time_factor::Int = 1000000, 
    minCells::Int = 25, 
    minOverlapCells = 25)

    nC, cno, IdClusters = spatial_cluster(arr_3d; time_factor = time_factor, minCells = minCells);
    TimeConnect(IdClusters, cno; minOverlapCells = minOverlapCells);
    IdClusters
end


export cluster_spatiotemporal
