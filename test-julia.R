

{
    overlap = 5
    ## action now
    # arr = arr[,,1:3]
    verbose = FALSE
    diag = FALSE
    # diag = TRUE
    
    # clusterId = r_cluster$clusterID
    # system.time()
    
    factor = 1e3
    system.time(clusterId_jl <- cluster_SpatioTemporal_julia(arr, ncell_overlap = overlap, 
                                                             diag = diag, factor = factor))
    system.time(clusterId_f90 <- cluster_SpatioTemporal(arr, ncell_overlap = overlap, factor = factor)$clusterId)
    
    # microbenchmark::microbenchmark(
    rbenchmark::benchmark(
        clusterId_jl <- cluster_SpatioTemporal_julia(arr, ncell_overlap = overlap, 
                                                     diag = diag, factor = 1000),
        clusterId_f90 <- cluster_SpatioTemporal(arr, ncell_overlap = overlap, factor = 1000)$clusterId,
        replications = 10
    )
    
    # clusterId_rf90 = connect_temporal_Rfortran(r_cluster, overlap = overlap, verbose = verbose)
    # clusterId_f90 = cluster_SpatioTemporal(arr[,,1:3], ncell_overlap = overlap, factor = 100)$clusterId
    
    # all.equal(cluster_grids(clusterId_rf90), cluster_grids(clusterId_f90))
    # all.equal(cluster_grids(clusterId_rf90), cluster_grids(clusterId_rjl))
    # all.equal(cluster_grids(clusterId_f90), cluster_grids(clusterId_rjl))
    all.equal(cluster_grids(clusterId_jl), cluster_grids(clusterId_f90))
}

plot.cluster(clusterId_rjl, main = "R")
plot.cluster(clusterId_jl, main = "julia")

x = connect_spatial_julia(arr[,,], factor = 100, diag = TRUE)
plot.cluster(x[[3]], main = "connect_spatial_julia")
plot.cluster(clusterId, main = "clusterId")
