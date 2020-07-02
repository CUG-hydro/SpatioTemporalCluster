# tools::package_native_routine_registration_skeleton(".")

#' droughtIndicator
#' 
#' Return a drought status array with 0 (non-drought), 1 (drought), -9999 (NA).
#'  
#' @param mat double matrix, with the dim of `[ngrid, nmonth]`, (`nmask = nrow*ncol`). 
#' SMI drought index.
#' @param mask logical matrix, `[nrow, ncol]`.
#' @param SMI_thld double number, 
#' @param exceeding Boolean. How to define a event? Exceed the threshold or less than?
#' For examples, for heatwaves, `exceeding` should be true; for drought, cold waves 
#' `exceeding` should be false.
#' @param masked If true, mat will be a 2d matrix, with the dimension of `[nCells, ngrid]`. 
#' If not, 3d mat will be masked by `mask` and transfer to 2d matrix.
#' 
#' @return 
#' * SMIc a drought status array with 0 (non-drought), 1 (drought), -9999 (NA).
#' * cellCoor index of masked region.
#' 
#' @example man/example/ex-droughtIndictor.R

#' @export
droughtIndicator <- function(mat, mask, SMI_thld, exceeding = TRUE, masked = FALSE){
    nrows   <- nrow(mask)
    ncols   <- ncol(mask)
    nMonths <- last(dim(mat))

    cellCoor <- matrix(-9999L, sum(mask), 2)
    SMIc <- array(-9999L, dim = c(nrows, ncols, nMonths))
    
    # if (is.null(mask)) {
    #     mask <- matrix(TRUE, nrows, ncols)
    # }

    # mat is a matrix, not 3d array
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

    # browser()
    mode(mat)      <- "single"
    mode(mask)     <- "logical"
    mode(SMI_thld) <- "single"
    mode(cellCoor) <- "integer"
    mode(SMIc)     <- "integer"

    # mask = t(mask)
    r <- .Fortran("droughtindicator", mat, mask, SMI_thld, 
                  nrows, ncols, nMonths, exceeding,
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
