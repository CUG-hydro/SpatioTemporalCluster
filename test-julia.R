#! /usr/bin/Rscript

# library(heatwave)
{
    set.seed(1)
    n <- 1000
    nrow <- n
    ncol <- n
    ngrid <- nrow * ncol
    ntime <- 12

    mat <- matrix(rnorm(nrow * ncol * ntime), ngrid, ntime)
    SMI_thld <- 0.5
    mask <- matrix(mat[, 1], nrow, ncol) > SMI_thld
    arr <- array(mat, dim = c(nrow, ncol, ntime)) > SMI_thld
}

{
    library(JuliaCall)
    # julia_setup(rebuild = TRUE)
    julia_source("cluster/src/cluster.jl")
    # system.time(res <- julia_call("cluster.spatial_cluster", arr))
    # names(res) <- c("nC", "cno", "IdClusters")
    system.time({
        IdClusters = julia_call("cluster.cluster_spatiotemporal", arr, 
                                time_factor = as.integer(10000), # max clusters for each time
                                minOverlapCells = as.integer(25),
                                minCells = as.integer(25))    
    })
}

{
    p <- plot.cluster(IdClusters, 1:9)
    ratio <- 0.8
    write_fig(p, "r_julia_final.pdf", 12 * ratio, 8 * ratio)
}
