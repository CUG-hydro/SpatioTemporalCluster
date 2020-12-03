test_that("cluster_SpatioTemporal in julia,R,Fortran works",
{
    set.seed(100)
    n <- 10
    nrow <- n
    ncol <- n
    ngrid <- nrow * ncol
    ntime <- 4
    arr <- (array(rnorm(nrow * ncol * ntime), dim = c(nrow, ncol, ntime))) > 0.5

    ## action now
    diag = FALSE
    overlap = 5
    factor = n^2
    ncell_connect = 5
    ncell_overlap = 5
    
    
    # write_fig(plot.cluster(clusterId_jl, 1:3, main = "Julia"), "julia.pdf")
    system.time(clusterId_f90 <- cluster_SpatioTemporal(arr, NULL, ncell_connect, ncell_overlap, factor, diag))
    
    # system.time(r_cluster <- connect_spatial(arr, diag = diag, factor = factor))
    # system.time(clusterId_rjl <- connect_temporal_Rjulia(r_cluster, ncell_overlap = overlap, verbose = TRUE))
    # clusterId_rjl <- connect_temporal_Rfortran(r_cluster, ncell_overlap = overlap, verbose = TRUE)
    # rbenchmark::benchmark(
    #     clusterId_rjl <- connect_temporal_Rjulia(r_cluster, ncell_overlap = overlap, verbose = FALSE), 
    #     clusterId_jl <- cluster_SpatioTemporal_julia(arr, ncell_overlap = overlap, factor = factor, diag = diag), 
    #    replications = 10
    # )
    system.time(clusterId_rjl  <- cluster_SpatioTemporal_R(arr, ncell_connect, ncell_overlap, factor, diag, version = "julia"))
    system.time(clusterId_rf90 <- cluster_SpatioTemporal_R(arr, ncell_connect, ncell_overlap, factor, diag, version = "fortran"))
    system.time(clusterId_jl <- cluster_SpatioTemporal_julia(arr, method = "tree",
                                                             ncell_connect, ncell_overlap, factor, diag))
    
    v_jl   = cluster_grids(clusterId_jl)
    v_f90  = cluster_grids(clusterId_f90)
    v_rjl  = cluster_grids(clusterId_rjl)
    v_rf90 = cluster_grids(clusterId_rf90)
    
    expect_equal(v_jl, v_f90)
    expect_equal(v_rjl, v_rf90)
    expect_equal(v_jl, v_rjl)
})

# write_fig(plot.cluster(clusterId_jl, 1:3, main = "Julia"), "julia.pdf")
# write_fig(plot.cluster(clusterId_rjl, 1:3, main = "R Julia"), "R julia.pdf")
# 
# save(arr, file = "debug-cluster_spatial.rda")
# r_cluster <- cluster_spatial(arr, diag = diag, factor = 100)
# write_fig(plot.cluster(clusterId_f90, 1:3, main = "cluster_spatial"), "cluster_spatial.pdf")
