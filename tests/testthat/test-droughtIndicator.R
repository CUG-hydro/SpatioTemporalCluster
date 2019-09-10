nrow <- 10
ncol <- 10
ngrid <- nrow*ncol

SMI_thld <- -0.2

test_that("droughtIndicator works", {
    ## memory leak test
    set.seed(1)
    ntime <- 1
    mat  <- array(rnorm(nrow*ncol*ntime), c(nrow, ncol, ntime))
    
    ## 1. droughtIndicator
    # no cluster
    mask <- matrix(mat[,,1], nrow, ncol) > 0.8
    r <- droughtIndicator( mat, mask, SMI_thld = 0.7, exceeding = FALSE)  
    # r$SMIc
    # ans <- (mat >= SMI_thld) * 1; ans[!mask] <- NA; ans
    r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor, thCellClus = 4, nCellInter = 2)
    expect_true(all(r$SMIc < 1))

    # have cluster
    mask <- matrix(mat[,,1], nrow, ncol) < 0.5
    r <- droughtIndicator( mat, mask, SMI_thld = 0.2, exceeding = FALSE)  
    expect_true(any(r$SMIc == 1))
    
    # mask works
    expect_true(all(r$SMIc[,,1][!mask] == -9999L))
    
    ## plot.cluster works
    expect_silent({
        r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor, thCellClus = 4, nCellInter = 2)
        plot.cluster(r_cluster$idCluster)
    })
})

test_that("memory leak", {
    expect_true({
        ntime <- 11
        set.seed(1)
        mat  <- array(rnorm(nrow*ncol*ntime), c(nrow, ncol, ntime))
        mask <- matrix(mat[,,1], nrow, ncol) > 0.8
        
        r <- droughtIndicator( mat, mask, SMI_thld = 0.7 )  
        r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor, thCellClus = 4, nCellInter = 2)
        r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)
        
        ntime <- 12
        set.seed(1)
        mat  <- array(rnorm(nrow*ncol*ntime), c(nrow, ncol, ntime))
        mask <- matrix(mat[,,1], nrow, ncol) > 0.8
        r <- droughtIndicator( mat, mask, SMI_thld = 0.7 )  
        r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor, thCellClus = 4, nCellInter = 2)
        r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)
        
        TRUE
    })
})
