#' Calculate drought status
#' 
#' @inheritParams eventIndicator
#' @inheritParams cluster_SpatioTemporal
#' @param clusterId array of cluster num_id, with the dim `[nrow, ncol, ntime]`.
#' @param shortCnoList vector, cluster num_id
#' @param mGridArea matrix of grid area
#' 
#' @example example/ex-cluster_SpatioTemporal.R
#'
#' @export
cluster_stats <- function(mat, mask, threshold, 
    clusterId, shortCnoList, exceeding = TRUE, 
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
    
    if (!is.matrix(threshold)) {
        threshold <- mask*1*threshold
    } 
    
    mat[is.na(mat)] <- -9999
    threshold[is.na(threshold)] <- -9999

    mode(mat)          <- "single"
    mode(mask)         <- "logical"
    mode(threshold)     <- "single"
    mode(mGridArea)    <- "single"

    # SMIc <- array(-9999L, dim = c(nrows, ncols, nMonths))
    nClusters  = length(shortCnoList)
    DAreaEvol2 = array(-9999, dim = c(nMonths, nClusters))
    DTMagEvol2 = array(-9999, dim = c(nMonths, nClusters))

    if (!is.matrix(threshold)) {
        threshold <- mask*1*threshold
    } 
    mode(threshold) <- "single"

    r <- .Fortran("ClusterStats2", 
                  mat, mask, threshold, 
                  nrows, ncols, nMonths, nCells, 
                  mGridArea, 
                  # as.integer(nCellInter), as.integer(thCellClus), 
                  clusterId, nClusters, shortCnoList,
                  DAreaEvol2, DTMagEvol2)
    r <- last(r, 2) %>% set_names(c("DAreaEvol", "DTMagEvol"))
    if (exceeding) {
        r$DTMagEvol %<>% multiply_by(-1)
    }
    r
}
