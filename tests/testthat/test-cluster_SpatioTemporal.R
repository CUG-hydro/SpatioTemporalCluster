

test_that("julia,R,Fortran: cluster_SpatioTemporal works",
{
    set.seed(100)
    n <- 1000
    nrow <- n
    ncol <- n
    ngrid <- nrow * ncol
    ntime <- 12
    arr <- (array(rnorm(nrow * ncol * ntime), dim = c(nrow, ncol, ntime))) > 0.2

    ## action now
    diag = FALSE
    overlap = 5
    factor = 1e4
    
    system.time(clusterId_jl <- cluster_SpatioTemporal_julia(arr,
        ncell_overlap = overlap, factor = factor, diag = diag))
    system.time(clusterId_f90 <- cluster_SpatioTemporal(arr, ncell_overlap = overlap, factor = factor, diag = diag))
    
    system.time(r_cluster <- connect_spatial(arr, diag = diag, factor = factor))
    system.time(clusterId_rjl <- connect_temporal_Rjulia(r_cluster, ncell_overlap = overlap, verbose = TRUE))
    clusterId_rjl <- connect_temporal_Rfortran(r_cluster, ncell_overlap = overlap, verbose = TRUE)
    
    
    rbenchmark::benchmark(
        clusterId_rjl <- connect_temporal_Rjulia(r_cluster, ncell_overlap = overlap, verbose = FALSE), 
        clusterId_jl <- cluster_SpatioTemporal_julia(arr, ncell_overlap = overlap, factor = factor, diag = diag), 
       replications = 10
    )
    
    
    system.time(clusterId_rjl  <- cluster_SpatioTemporal_R(arr, ncell_overlap = overlap, factor = factor, diag = diag, version = "julia"))
    system.time(clusterId_rf90 <- cluster_SpatioTemporal_R(arr, ncell_overlap = overlap, factor = factor, diag = diag, version = "fortran"))
    
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
write_fig(plot.cluster(clusterId_f90, 1:3, main = "cluster_spatial"), "cluster_spatial.pdf")
