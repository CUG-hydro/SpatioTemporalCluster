function cluster_SpatioTemporal(arr_3d::AbstractArray{Bool, 3}; 
    method = "tree", 
    ncell_connect::Int = 25, ncell_overlap::Int = 25, 
    factor::Int = 1000000, 
    diag = false)

    nC, cno, clusterIds = connect_spatial(arr_3d; method = method, 
        factor = factor, ncell_connect = ncell_connect, diag = diag);
    connect_temporal(clusterIds; ncell_overlap = ncell_overlap);
    clusterIds
end


export cluster_SpatioTemporal
