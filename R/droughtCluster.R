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
droughtIndicatorR <- function(mat, mask, SMI_thld){
    dim1 <- dim(mat)
    dim2 <- dim(mask)
    
    nrows <- as.integer(dim2[1])
    ncols <- as.integer(dim2[2])
    nMonths <- as.integer(dim1[2])
    
    cellCoor <- matrix(-9999L, sum(mask), 2)
    SMIc <- array(-9999L, dim = c(nrows, ncols, nMonths))
    
    mode(mat)  <- "single"
    mode(mask) <- "logical"
    mode(cellCoor) <- "integer"
    mode(SMIc)     <- "integer"
    
    r <- .Fortran("droughtindicator", mat, mask, as.single(SMI_thld), 
                  nrows, ncols, nMonths,
                  cellCoor, SMIc)

    n <- length(r)
    ans <- r[c(n, n-1)] 
    # names(r) <- c("mat", "mask", "thld", "cellCoor", "SMIc")
    # .Fortran("droughtIndicator", SMI, mask, SMI_thld, )
    setNames(ans, c("SMIc", "cellCoor"))    
}


#' ClusterEvolution
#' 
#' cluster drought clusters in space and time
#' 
#' @param SMIc integer array, `[nrow, ncol, ntime]`
#' @param cellCoor integer matrix, `[nCells, 2]`
#' @param thCellClus treshold  for cluster formation in space
#' @param nCellInter number cells for joining clusters in time
#' 
#' @author
#' Budapest, 10-11.03.2011
#' 
#' @export
ClusterEvolution <- function(SMIc, cellCoor, thCellClus = 1L, nCellInter = 1L){
    dim     <- dim(SMIc) 
    nrows   <- dim[1]
    ncols   <- dim[2]
    nMonths <- dim[3]
    nCells  <- dim(cellCoor)[1]    
    
    mode(SMIc)     <- "integer"
    mode(cellCoor) <- "integer"
    
    r <- .Fortran("ClusterEvolution", 
                  SMIc, nrows, ncols, nMonths, nCells, cellCoor, 
                  as.integer(nCellInter), as.integer(thCellClus))
    r
}



findClusters <- function(cellCoor, thCellClus, t,iC,nCluster, nrows, ncols, nCells, SMIc){
}



# #' @export
# Fpi <- function(DARTS, ROUNDS) {
#   retvals <- .Fortran("pi", avepi = as.numeric(1), DARTS =  as.integer(DARTS), ROUNDS =  as.integer(ROUNDS))
#   return(retvals$avepi)
# }

#' @export
make_clean <- function(){
    if (.Platform$OS.type == "unix"){
        system("cd src && rm *.dll *.so *.mod *.o")
    } else {
        shell("cd src && rm *.dll *.so *.mod *.o")
    }
}

# bsamgp
