
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
    nMonths <- dim1[2]
    nCells  <- sum(mask)
    cellCoor <- matrix(-9999L, nCells, 2)

    SMIc <- array(-9999L, dim = c(nrows, ncols, nMonths))
    
    mode(mat)          <- "single"
    mode(mask)         <- "logical"
    mode(mGridArea)    <- "single"

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
