#' Calculate drought status
#' 
#' @inheritParams droughtIndicator
#' @inheritParams ClusterEvolution
#' @param idCluster array of cluster num_id, with the dim `[nrow, ncol, ntime]`.
#' @param shortCnoList vector, cluster num_id
#' @param mGridArea matrix of grid area
#' 
#' @example man/example/ex-droughtIndictor.R
#' 
#' @export
ClusterStats <- function(mat, mask, SMI_thld, 
    idCluster, shortCnoList, exceeding = TRUE, 
    mGridArea = NULL, masked = FALSE)
{
    if (is.null(mGridArea)) mGridArea <- 1.0 * mask
    
    nrows   <- nrow(mask)
    ncols   <- ncol(mask)
    nMonths <- last(dim(mat))
    nCells  <- sum(mask)
    cellCoor <- matrix(-9999L, nCells, 2)

    # if (is.null(mask)) {
    #     mask <- matrix(TRUE, nrows, ncols)
    # }

    if (!masked) {
        dim(mat) <- c(nrows*ncols, nMonths)
        if (!all(mask)){
            I <- which(mask)
            mat <- mat[I, ]
        }    
    }
    
    if (!is.matrix(SMI_thld)) {
        SMI_thld <- mask*1*SMI_thld
    } 
    
    mat[is.na(mat)] <- -9999
    SMI_thld[is.na(SMI_thld)] <- -9999

    mode(mat)          <- "single"
    mode(mask)         <- "logical"
    mode(SMI_thld)     <- "single"
    mode(mGridArea)    <- "single"

    # SMIc <- array(-9999L, dim = c(nrows, ncols, nMonths))
    nClusters  = length(shortCnoList)
    DAreaEvol2 = array(-9999, dim = c(nMonths, nClusters))
    DTMagEvol2 = array(-9999, dim = c(nMonths, nClusters))

    if (!is.matrix(SMI_thld)) {
        SMI_thld <- mask*1*SMI_thld
    } 
    mode(SMI_thld) <- "single"

    r <- .Fortran("ClusterStats2", 
                  mat, mask, SMI_thld, 
                  nrows, ncols, nMonths, nCells, 
                  mGridArea, 
                  # as.integer(nCellInter), as.integer(thCellClus), 
                  idCluster, nClusters, shortCnoList,
                  DAreaEvol2, DTMagEvol2)
    r <- last(r, 2) %>% set_names(c("DAreaEvol", "DTMagEvol"))
    if (exceeding) {
        r$DTMagEvol %<>% multiply_by(-1)
    }
    r
}
