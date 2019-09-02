devtools::load_all()
library(SMI)

set.seed(1)

nrow <- 10
ncol <- 10
ngrid <- nrow*ncol
ntime <- 12

mat <- matrix(rnorm(nrow*ncol*ntime), ngrid, ntime)
mask <- matrix(mat[,1], nrow, ncol) > 0.5
SMI_thld <- 0.2

# system.time(
# )
r  <- droughtIndicatorR( mat, mask, SMI_thld)    
r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor)
r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)


mat <- r2$idCluster
microbenchmark::microbenchmark(
    alply(mat, 3, function(x) unique(as.numeric(x))), 
    unique(as.numeric(mat))
)


p <- plot.cluster(r2)
ratio = 0.8
# devtools::install_github("kongdd/Ipaper")
Ipaper::write_fig(p, "a.pdf", 12*ratio, 8*ratio)
## 2. visualization ------------------------------------------------------------
