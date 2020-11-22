library(plyr)
library(doParallel)
library(magrittr)
library(parallel)
library(JuliaCall)

set.seed(888)
ntime = 10
n = 20
list.mat <- llply(1:ntime, function(ind){
    array(rnorm(n*n), dim = c(n, n)) >= 0.3
	# matrix(ifelse(sample(1:100, 400, T) > 50, T, F), 20)
})
arr <- abind::abind(list.mat, along = 3)
# library(raster)
# writeRaster(brick(arr), "arr.tif")
# profvis::profvis(
#     {
#     dim = c(n, n, ntime)
#     r_f90 <- ClusterEvolution_Fortran2(list.mat, 0, 5) %>% array(dim = dim)
#     # p <- plot.cluster(r_gu, 1:ntime)
#     # write_fig(p, "r_gu.pdf", 10, 5)
#     # table(r_gu)
# }
# )

clusters = cluster_spatial(list.mat, 0, diag = FALSE)
refactor_clusterID
clusterID = array_3dTo2d(clusters$clusterID) 


# refactor_clusterID(clusterID, factor = 1e4)
# profvis::profvis
system.time({
    dim = c(n, n, ntime)
    r_jl <- ClusterEvolution_julia(clusterID, overlap = 5) %>% array(dim = dim)
    # p <- plot.cluster(r_gu, 1:ntime)
    # write_fig(p, "r_gu.pdf", 10, 5)
    # table(r_gu)
    })


system.time({
    r_julia = julia_call("cluster.cluster_spatiotemporal", arr,
                         time_factor = as.integer(1e4), # max clusters for each time
                         minOverlapCells = as.integer(5),
                         minCells = as.integer(0))
    r_julia[r_julia <= 0] = NA
})

table(r_jl) %>% as.numeric() %>% sort() %>% tail()

summary <- function(x) {
    table(x) %>% as.numeric() %>% sort(decreasing = TRUE) %>% .[1:20]
}

y_jl    <- summary(r_jl)
y_f90   <- summary(r_f90)
y_julia <- summary(r_julia)
all.equal(y_f90, y_julia)
all.equal(y_f90, y_jl)

all.equal(y_julia, y_jl)

# )

{
    clusterIDs = foreach(t = 1:ntime, i = icount()) %do% {
        clusterID = spatial_cluster_Fortran(arr[,,t], 0)
    } %>% abind::abind(along = 3)
       
    r_v010 <- ClusterEvolution_Fortran(clusterIDs, 5)
    
}
# arr2 <- arr
# arr2[arr2 == 0] <- NA
# levelplot(arr2, at = c(0, 1, 2), as.table = TRUE)

{
    p <- plot.cluster(r_julia, 1:ntime)
    write_fig(p, "r_julia.pdf", 10, 5)
    table(r_julia)
}

{
    p <- plot.cluster(r_v010, 1:ntime)
    write_fig(p, "r_010.pdf", 10, 5)
    # table(r_gu)
}

y010 = table(r_v010) %>% as.numeric() %>% sort() 
y011 = table(r_gu) %>% as.numeric() %>% sort() 
y = table(r_julia) %>% as.numeric() %>% sort() 

all.equal(y010, y011)
all.equal(y[-length(y)], y011)
