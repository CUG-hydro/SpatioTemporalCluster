
#' Calculate drought status
#' 
#' @export
ClusterStats <- function(mat, mask, SMI_thld, 
    idCluster, shortCnoList, 
    mGridArea = NULL)
{
    if (is.null(mGridArea)) mGridArea <- 1.0 * mask
    
    dim1 <- dim(mat)
    dim2 <- dim(mask)
    
    nrows   <- dim2[1]
    ncols   <- dim2[2]
    nMonths <- last(dim1)
    nCells  <- sum(mask)
    cellCoor <- matrix(-9999L, nCells, 2)

    if (is.null(mask)) {
        mask <- matrix(TRUE, nrows, ncols)
    }

    dim(mat) <- c(nrows*ncols, nMonths)
    if (!all(mask)){
        I <- which(mask)
        mat <- mat[I, ]
    }
    
    mode(mat)          <- "single"
    mode(mask)         <- "logical"
    mode(mGridArea)    <- "single"

    # SMIc <- array(-9999L, dim = c(nrows, ncols, nMonths))
    nClusters  = length(shortCnoList)
    DAreaEvol2 = array(-9999, dim = c(nMonths, nClusters))
    DTMagEvol2 = array(-9999, dim = c(nMonths, nClusters))

    r <- .Fortran("ClusterStats2", 
                  mat, mask, SMI_thld, 
                  nrows, ncols, nMonths, nCells, 
                  mGridArea, 
                  # as.integer(nCellInter), as.integer(thCellClus), 
                  idCluster, nClusters, shortCnoList,
                  DAreaEvol2, DTMagEvol2)
    r <- last(r, 2) %>% set_names(c("DAreaEvol", "DTMagEvol"))
    r
}
