library(SMI)

set.seed(1)

nrow <- 10
ncol <- 10
ngrid <- nrow*ncol
ntime <- 1

mat <- matrix(rnorm(nrow*ncol), ngrid, ntime)
mask <- matrix(mat[,1], nrow, ncol) > 0.5
SMI_thld <- 0.2

mode(mat) <- "single"
# cellCoor <- matrix(0L, nrow, ncol)
# SMIc <- array(0, dim = c(nrow, ncol, ntime))

r <- droughtIndicatorR( mat, mask, SMI_thld)

r2 <- ClusterEvolution(r[[8]], r[[7]])

# r <- .Fortran("__mo_drought_evaluation_MOD_droughtindicator", mat, mask, SMI_thld, cellCoor, SMIc, PACKAGE = "SMI2")
# 
# library(foreach)
# foreach(pkg = packages)  %do% {
#     cmd <- sprintf("git clone https://github.com/cran/%s github/%s", pkg, pkg)
#     shell(cmd)
# }
