# source("debug.R")
# devtools::load_all()
# make_clean()
devtools::load_all()
# library(SMI)

nrow <- 10
ncol <- 10
ngrid <- nrow*ncol

SMI_thld <- -0.2

## memory leak test
set.seed(1)
ntime <- 11
mat <- array(rnorm(nrow*ncol*ntime), c(nrow, ncol, ntime))
mask <- matrix(mat[,,1], nrow, ncol) < 0.8

r         <- droughtIndicator( mat, mask, SMI_thld)    
r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor, thCellClus = 4, nCellInter = 2)
(r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)) %>% as.tibble()
plot.cluster(r_cluster$idCluster)

# {mask & (matrix(mat < 0.2, nrow, ncol))}*1
# 
# mat2 <- matrix(mat, 10, 10) %>% round(2)
# mat2[!mask] <- NA
# mask*1

## memory leak test
ntime <- 12
# set.seed(100)
mat <- matrix(rnorm(nrow*ncol*ntime), ngrid, ntime)
mask <- matrix(mat[,1], nrow, ncol) > 0.5

r         <- droughtIndicator( mat, mask, SMI_thld)    
r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor)
r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)

mat2 <- `dim<-`(mat, c(nrow, ncol, ntime))
plot.cluster(r_cluster$idCluster)
r_status
