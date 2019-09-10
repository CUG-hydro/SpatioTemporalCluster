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
#' @example man/example/ex-droughtIndictor.R
#' 
#' @import magrittr
#' @export
ClusterEvolution <- function(SMIc, cellCoor, thCellClus = 1L, nCellInter = 1L){
    dim     <- dim(SMIc) 
    nrows   <- dim[1]
    ncols   <- dim[2]
    nMonths <- dim[3]
    nCells  <- dim(cellCoor)[1]    
    
    mode(SMIc)     <- "integer"
    mode(cellCoor) <- "integer"

    idCluster = array(-9999L, dim = c(nrows, ncols, nMonths))
    nCluster  = 0L
    r <- .Fortran("clusterevolution", 
                  SMIc, nrows, ncols, nMonths, nCells, cellCoor, 
                  as.integer(nCellInter), as.integer(thCellClus), 
                  idCluster, nCluster)
    r <- set_names(last(r, 2), c("idCluster", "nCluster"))
    # idCluster2 <- last(r)
    # r$idCluster[r$idCluster == -9999L] = NA_integer_
    shortCnoList   <- unique(as.numeric(r$idCluster)) %>% sort() %>% as.integer()
    r$shortCnoList <- shortCnoList[shortCnoList != -9999L]

    if (length(r$shortCnoList) != r$nCluster) {
        stop("[e]: shortCnoList not equal to nCluster!")
    }
    # idCluster2
    r
}
