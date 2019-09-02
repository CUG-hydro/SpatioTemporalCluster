# source("debug.R")
# devtools::load_all()
# make_clean()
devtools::load_all()
# library(SMI)

set.seed(1)

nrow <- 10
ncol <- 10
ngrid <- nrow*ncol

SMI_thld <- 0.2

## memory leak test
ntime <- 11
mat <- matrix(rnorm(nrow*ncol*ntime), ngrid, ntime)
mask <- matrix(mat[,1], nrow, ncol) > 0.5

r         <- droughtIndicator( mat, mask, SMI_thld)    
r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor)
r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)

## memory leak test
ntime <- 12
mat <- matrix(rnorm(nrow*ncol*ntime), ngrid, ntime)
mask <- matrix(mat[,1], nrow, ncol) > 0.5

r         <- droughtIndicator( mat, mask, SMI_thld)    
r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor)
r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)
