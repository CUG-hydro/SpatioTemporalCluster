{
    library(JuliaCall)
    library(plyr)
    julia_source("cluster/src/cluster.jl")
}

{
    set.seed(1)
    n <- 100
    nrow <- n
    ncol <- n
    ngrid <- nrow * ncol
    ntime <- 5

    mat <- matrix(rnorm(nrow * ncol * ntime), ngrid, ntime)
    SMI_thld <- 0.5
    # mask <- matrix(mat[, 1], nrow, ncol) > SMI_thld
    arr <- array(mat, dim = c(nrow, ncol, ntime)) > SMI_thld

    t_gu <- system.time({
        IdClusters_sp <- llply(1:dim(arr)[3], function(ind) spatial_cluster_Fortran(arr[,,ind], thres = 0, diag = FALSE)) %>% 
            abind::abind(along = 3)
        IdClusters_gu = ClusterEvolution_Fortran(IdClusters_sp, overlap = 1)
    })
    
    # t_kong <- system.time({
    #     r = julia_call("cluster.spatial_cluster", arr, 
    #                    time_factor = as.integer(0), # max clusters for each time
    #                    # minOverlapCells = as.integer(25),
    #                    minCells = as.integer(1)) %>% as.array()  
    # })
    system.time({
        IdClusters_kong = julia_call("cluster.cluster_spatiotemporal", arr, 
                                time_factor = as.integer(1e4), # max clusters for each time
                                minOverlapCells = as.integer(1),
                                minCells = as.integer(1))
        # class(IdClusters) <- "array"
        # IdClusters <- r[[3]]
        IdClusters_kong[IdClusters_kong <= 0] <- NA
    })
    # n = table(IdClusters[,,1]) %>% sort() %>% as.numeric()
    # n_gu = table(IdClusters_gu) %>% sort() %>% as.numeric()
    # all.equal(n, n_gu)
}

x = table(IdClusters_kong) %>% as.numeric() %>% sort()
y = table(IdClusters_gu) %>% as.numeric() %>% sort()
all.equal(x, y)

## visual
{
    # IdClusters = IdClusters_gu
    # IdClusters[IdClusters_gu <= 0] <- NA
    p <- plot.cluster(IdClusters_gu, 1:9)
    ratio <- 0.8
    write_fig(p, "r_gu.pdf", 12 * ratio, 8 * ratio)
}

{
    # IdClusters = IdClusters_gu
    # IdClusters[IdClusters <= 0] <- NA
    p <- plot.cluster(IdClusters_kong, 1:9)
    ratio <- 0.8
    write_fig(p, "r_kong.pdf", 12 * ratio, 8 * ratio)
}
