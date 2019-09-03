# tools::package_native_routine_registration_skeleton(".")

#' droughtIndicator
#' 
#' Return a drought status array with 0 (non-drought), 1 (drought), -9999 (NA).
#'  
#' @param mat double matrix, with the dim of `[ngrid, nmonth]`, (`ngrid = nrow*ncol`). 
#' SMI drought index.
#' @param mask logical matrix, `[nrow, ncol]`.
#' @param SMI_thld double number, 
#' 
#' @return 
#' * SMIc a drought status array with 0 (non-drought), 1 (drought), -9999 (NA).
#' * cellCoor index of masked region.
#' 
#' @example man/example/ex-droughtIndictor.R
#' @export
droughtIndicator <- function(mat, mask, SMI_thld){
    dim1 <- dim(mat)
    dim2 <- dim(mask)
    
    nrows <- as.integer(dim2[1])
    ncols <- as.integer(dim2[2])
    nMonths <- last(dim1)
    
    cellCoor <- matrix(-9999L, sum(mask), 2)
    SMIc <- array(-9999L, dim = c(nrows, ncols, nMonths))
    
    if (is.null(mask)) {
        mask <- matrix(TRUE, nrows, ncols)
    } 

    dim(mat) <- c(nrows*ncols, nMonths)
    if (!all(mask)){
        I   <- which(mask)
        mat <- mat[I, ]
    }

    # browser()
    mode(mat)      <- "single"
    mode(mask)     <- "logical"
    mode(cellCoor) <- "integer"
    mode(SMIc)     <- "integer"
    
    # mask = t(mask)
    r <- .Fortran("droughtindicator", mat, mask, as.single(SMI_thld), 
                  nrows, ncols, nMonths,
                  cellCoor, SMIc)
    # names(r) <- c("mat", "mask", "thld", "cellCoor", "SMIc")
    # .Fortran("droughtIndicator", SMI, mask, SMI_thld, )
    set_names(last(r, 2)[2:1], c("SMIc", "cellCoor"))    
}

last <- function(x, len = 1){
    n <- length(x)
    if (len == 1) {
        x[[n]]
    } else {
        x[seq(n - len + 1, n)]
    }
}
