# #! /usr/bin/Rscript
# 
# # library(heatwave)

library(abind)
arr <- abind(matrix, along = 3)

{
    library(JuliaCall)
    # julia_setup(rebuild = TRUE)
    julia_source("cluster/src/cluster.jl")
    # system.time(res <- julia_call("cluster.spatial_cluster", arr))
    # names(res) <- c("nC", "cno", "IdClusters")
    system.time({
        IdClusters = julia_call("cluster.cluster_spatiotemporal", arr, 
                                time_factor = as.integer(10000), # max clusters for each time
                                minOverlapCells = as.integer(5),
                                minCells = as.integer(1))    
    })
}

{
    IdClusters[IdClusters <= 0] <- NA
    p <- plot.cluster(IdClusters, 1:9)
    ratio <- 0.8
    write_fig(p, "r_julia_final.pdf", 12 * ratio, 8 * ratio)
}

# 



