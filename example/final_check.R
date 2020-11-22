
for (i in 1:1000) {
    runningId(i, 10)
    set.seed(i)
    # i = 164
    n = 20
    nrow <- n
    ncol <- n
    ngrid <- nrow*ncol
    # ntime <- 12
    ntime = 10
    arr <- (array(rnorm(nrow*ncol*ntime), dim = c(nrow, ncol, ntime))) > 0.2
    # load("D:/Documents/WeChat Files/wxid_udml2ofatec521/FileStorage/File/2020-11/example(1).RData")
    overlap = 5
    
    r_cluster = cluster_spatial(arr[,,1:3], diag = TRUE, factor = 100)
    clusterId = r_cluster$clusterID
    # r_cluster$clusterID %<>% .[, , 1:ntime]
    # microbenchmark::microbenchmark(
    #     clusterId_rf90 = connect_temporal_Rfortran(r_cluster, overlap = overlap, verbose = TRUE),
    #     clusterId_rjl = connect_temporal_Rjulia(r_cluster, overlap = overlap, verbose = TRUE)
    # )
    verbose = FALSE
    clusterId_rf90 = connect_temporal_Rfortran(r_cluster, overlap = overlap, verbose = verbose)
    clusterId_rjl = connect_temporal_Rjulia(r_cluster, overlap = overlap, verbose = verbose)
    # all.equal(clusterId_rjl, clusterId_rf90)
    clusterId_f90 = cluster_SpatioTemporal(arr[,,1:3], ncell_overlap = overlap, factor = 100)$clusterId
    # all.equal(clusterId_rf90, clusterId_f90)
    all.equal(cluster_grids(clusterId_rf90), cluster_grids(clusterId_f90))
    all.equal(cluster_grids(clusterId_rf90), cluster_grids(clusterId_rjl))
    all.equal(cluster_grids(clusterId_f90), cluster_grids(clusterId_rjl))
    
    system.time(clusterId_jl <- cluster_SpatioTemporal_julia(arr[,,1:3], ncell_overlap = overlap, 
                                                diag = TRUE, factor = 100))
    all.equal(cluster_grids(clusterId_jl), cluster_grids(clusterId_rjl))
    
    if (!all.equal(cluster_grids(clusterId_rjl), cluster_grids(clusterId_rf90)) ) {
        break
    }
    
    save(arr, file = "debug.rda")
}

plot.cluster(clusterId_rjl)
plot.cluster(clusterId_jl)

all.equal(clusterId, clusterId_f90)


table(clusterId_rjl) %>% sort()


x = clusterId_rjl[,,2] == 214
y = clusterId_rjl[,,3] %in% c(303, 307)
sum(x & y, na.rm = TRUE)

write_fig(plot.cluster(r_cluster$clusterID), "a.pdf", 15, 5)
plot.cluster(r_cluster$clusterID)
plot.cluster(clusterId_rf90)


levelplot(clusterId_rf90)
levelplot(clusterId_rjl)

all.equal(r_cluster$clusterID, clusterId_rjl)
all.equal(r_cluster$clusterID, clusterId_rf90)
# profvis::profvis(

# write_arry into excel
write_array <- function(arr, outfile = "input.xlsx") {
    ntime = dim(arr) %>% last()
    foreach(i = 1:ntime) %do% {
        a[,,i] %>% as.data.table()
    } %>% set_names(1:ntime) %>% write_list2xlsx(outfile)
}
write_array(clusterId_rf90, "clusterId_rf90")
write_array(clusterId_rjl, "clusterId_rjl")
