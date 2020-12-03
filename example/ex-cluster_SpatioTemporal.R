set.seed(1)

nlon <- 10
nlat <- 10
ngrid <- nlon*nlat
ntime <- 12
dim = c(nlon, nlat, ntime)
threshold = 0.2
 
arr <- array(rnorm(nlon*nlat*ntime), dim = c(nlon, nlat, ntime)) > 0.2 # boolean arr
# mask = apply_3d(arr >= 0.2, FUN = rowSums2) > 0
# r  <- droughtIndicator( arr, mask, threshold = 0.2)
r_cluster <- cluster_SpatioTemporal(arr, ncell_connect = 1, ncell_overlap = 1, factor = 1000)
# plot.cluster(r_cluster$clusterId)
# r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$clusterId, r_cluster$shortCnoList)
