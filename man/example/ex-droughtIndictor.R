# devtools::load_all()
library(heatwave)

set.seed(1)

nrow <- 10
ncol <- 10
ngrid <- nrow*ncol
ntime <- 12

mat <- matrix(rnorm(nrow*ncol*ntime), ngrid, ntime)
mask <- matrix(mat[,1], nrow, ncol) > 0.5
SMI_thld <- 0.2

r  <- droughtIndicator( mat, mask, SMI_thld)    
r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor)
r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)

## 2. visualization ------------------------------------------------------------
# devtools::install_github("kongdd/Ipaper")
# p <- plot.cluster(r2)
# ratio = 0.8
# Ipaper::write_fig(p, "a.pdf", 12*ratio, 8*ratio)
