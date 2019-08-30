# tools::package_native_routine_registration_skeleton(".")

#' droughtIndicator
#' 
#' @param mat double matrix, with the dim of `[ngrid, nmonth]`, (`ngrid = nrow*ncol`). 
#' SMI drought index.
#' @param mask logical matrix, `[nrow, ncol]`.
#' @param SMI_thld double number, 
#' 
#' @example man/example/ex-droughtIndictor.R
#' @export
droughtIndicatorR <- function(mat, mask, SMI_thld){
    dim1 <- dim(mat)
    dim2 <- dim(mask)

    cellCoor <- matrix(-999L, sum(mask), 2)
    SMIc <- array(-999L, dim = c(dim2, dim1[2]))

    r <- .Fortran("droughtindicator", mat, mask, as.double(SMI_thld), cellCoor, SMIc)
    # .Fortran("droughtIndicator", SMI, mask, SMI_thld, )
    r
}

# #' @export
# Fpi <- function(DARTS, ROUNDS) {
#   retvals <- .Fortran("pi", avepi = as.numeric(1), DARTS =  as.integer(DARTS), ROUNDS =  as.integer(ROUNDS))
#   return(retvals$avepi)
# }

#' @export
make_clean <- function(){
    shell("cd src && make clean")
}

# bsamgp
