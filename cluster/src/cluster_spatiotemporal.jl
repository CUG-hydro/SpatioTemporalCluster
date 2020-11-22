function cluster_SpatioTemporal(arr_3d::AbstractArray{Bool, 3}; 
    factor::Int = 1000000, 
    minCells::Int = 25, 
    minOverlapCells = 25, diag = false)

    nC, cno, IdClusters = connect_spatial(arr_3d; factor = factor, minCells = minCells, diag = diag);
    connect_temporal(IdClusters; minOverlapCells = minOverlapCells);
    IdClusters
end


export cluster_SpatioTemporal
