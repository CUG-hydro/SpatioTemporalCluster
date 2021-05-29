#' cluster_SpatioTemporal
#' 
#' cluster drought clusters in space and time
#' 
#' @param arr integer array, `[nrow, ncol, ntime]`, returned by [eventIndicator()]
#' @param cellCoor integer matrix, `[nCells, 2]`
#' @param ncell_connect treshold  for cluster formation in space
#' @param ncell_overlap number cells for joining clusters in time
#' @param factor max number of clusters per time
#' 
#' @author
#' Dongdong Kong, 2021-05-29
#' 
#' @example R/example/ex-cluster_SpatioTemporal.R
#' 
#' @return
#' - clusterId: with the same dimension as arr
#' 
#' @import magrittr
#' @export
cluster_SpatioTemporal <- function(
    arr, cellCoor = NULL, 
    ncell_connect = 1L, ncell_overlap = 1L, factor = 1e4, diag = FALSE)
{
    dim   <- dim(arr) 
    nrows <- dim[1]
    ncols <- dim[2]
    ntime <- dim[3]
    
    if (is.null(cellCoor)) cellCoor <- expand.grid(x = 1:nrows, y = 1:ncols) %>% as.matrix()
    nCells  <- dim(cellCoor)[1]    
    
    mode(arr) <- "integer"
    mode(cellCoor) <- "integer"

    clusterId = array(-9999L, dim = c(nrows, ncols, ntime))
    nCluster  = 0L
    r <- .Fortran("ClusterEvolution", 
                  arr, nrows, ncols, ntime, nCells, cellCoor, 
                  as.integer(ncell_overlap), as.integer(ncell_connect), as.integer(factor), diag,
                  clusterId, nCluster)
    r <- set_names(last(r, 2), c("clusterId", "nCluster"))
    r$clusterId[r$clusterId == -9999L] = NA_integer_
    shortCnoList   <- unique(as.numeric(r$clusterId)) %>% sort() %>% as.integer()
    r$shortCnoList <- shortCnoList[shortCnoList != -9999L]

    if (length(r$shortCnoList) != r$nCluster) {
        stop("[e]: shortCnoList not equal to nCluster!")
    }
    r$clusterId
}

# tools::package_native_routine_registration_skeleton(".")

#' eventIndicator
#' 
#' Return a drought status array with 0 (non-drought), 1 (drought), -9999 (NA).
#'  
#' @param arr double 2d array or 3d array.
#' If `mask` is not presented, arr should be a 3d array, with the dim of `[nlon, nlat, ntime]`.
#' 
#' @param mask logical matrix, `[nrow, ncol]`, only TRUE will be proceed.
#' @param threshold double number
#' @param exceeding Boolean. How to define a event? Exceed the threshold or less than?
#' For examples, for heatwaves, `exceeding` should be true; for drought, cold waves 
#' `exceeding` should be false.
#' @param masked whether mat has been masked? In the Fortran version SMI, nrow(mat)
#' should be equal to sum(mask).   
#' - If true, no further process; 
#' - If false, `mat` will be masked by `mask`. 
#' @return 
#' - `array`:
#' 
#'   * `1`     : extreme event
#'   * `0`     : not
#'   * `-9999` : masked regions
#' 
#' - `cellCoor`: index of selected region.
#'  
#' @example R/example/ex-cluster_SpatioTemporal.R
#' @keywords internal
#' @export
eventIndicator <- function(arr, threshold, mask = NULL, exceeding = TRUE, masked = FALSE){
    dim = dim(mat)
    if (is.null(mask)) {
        # in this situation, mat should be a 3d array
        nrows <- dim[1]
        ncols <- dim[2]
        ntime <- dim[3]    
        mask  <- matrix(TRUE, nrows, ncols)
    } else {
        nrows <- nrow(mask)
        ncols <- ncol(mask)
        ntime <- last(dim(mat))
    }
    
    cellCoor <- matrix(-9999L, sum(mask), 2)
    SMIc <- array(-9999L, dim = c(nrows, ncols, ntime))
    
    # mat is a matrix, not 3d array
    if (!masked) {
        dim(mat) <- c(nrows*ncols, ntime)
        # if some pixels masked
        if (!all(mask)){
            I <- which(mask)
            mat <- mat[I, ]
        }
    }
    
    if (!is.matrix(threshold)) threshold <- mask*1*threshold
    
    mat[is.na(mat)] <- -9999
    threshold[is.na(threshold)] <- -9999

    # browser()
    mode(mat)      <- "single"
    mode(mask)     <- "logical"
    mode(threshold) <- "single"
    mode(cellCoor) <- "integer"
    mode(SMIc)     <- "integer"

    # mask = t(mask)
    r <- .Fortran("droughtIndicator", mat, mask, threshold, 
                  nrows, ncols, ntime, exceeding,
                  cellCoor, SMIc)
    # names(r) <- c("mat", "mask", "thld", "cellCoor", "SMIc")
    # .Fortran("eventIndicator", SMI, mask, threshold, )
    set_names(last(r, 2)[2:1], c("array", "cellCoor"))    
}
