{
    set.seed(1)
    
    n = 1000
    nrow <- n
    ncol <- n
    ngrid <- nrow*ncol
    ntime <- 12
    
    mat <- matrix(rnorm(nrow*ncol*ntime), ngrid, ntime)
    SMI_thld <- 0.5
    mask <- matrix(mat[,1], nrow, ncol) > SMI_thld
    
    arr <- array(mat, dim = c(nrow, ncol, ntime)) > SMI_thld
}

spatial_cluster(arr[,,1])
