library(SpatioTemporalCluster)

ndim <- 10
ntime = 4
arr <- array(data = ifelse(sample(1:100, 20^2*20, T) > 50, T, F), c(ndim, ndim, ntime))
z1 <- cluster_SpatioTemporal(arr, ncell_connect = 5, ncell_overlap = 5, factor = 100)

z2 <- cluster_SpatioTemporal_julia(arr, method = 'tree', ncell_connect = 5, ncell_overlap = 5)
z3 <- cluster_SpatioTemporal_julia(arr, method = 'recursive', ncell_connect = 5, ncell_overlap = 5)
z4 <- cluster_SpatioTemporal_julia(arr, method = 'low', ncell_connect = 5, ncell_overlap = 5)

z5 <- cluster_SpatioTemporal_R(arr, version = 'julia', ncell_connect = 5, ncell_overlap = 5, factor = 100)
z6 <- cluster_SpatioTemporal_R(arr, version = 'fortran', ncell_connect = 5, ncell_overlap = 5, factor = 100)

# cluster_grids(z6)
all.equal(cluster_grids(z2), cluster_grids(z3))
all.equal(cluster_grids(z2), cluster_grids(z4))
all.equal(cluster_grids(z2), cluster_grids(z1))
all.equal(cluster_grids(z5), cluster_grids(z6))
all.equal(cluster_grids(z1), cluster_grids(z5))
# write_fig(plot.cluster(z1, main = "Fortran"), "Fortran.pdf")
# write_fig(plot.cluster(z5, main = "Rjulia"), "RJulia.pdf")
