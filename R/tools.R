#' make_clean
#' 
#' @param load_all boolean
#' 
#' @keywords internal
#' 
#' @export
make_clean <- function(load_all = FALSE){
    if (.Platform$OS.type == "unix"){
        system("cd src && rm *.dll *.so *.mod *.o")
    } else {
        shell("cd src && rm *.dll *.so *.mod *.o")
    }
    # if (load_all) devtools::load_all()
}

last <- function(x, len = 1) {
    n <- length(x)
    if (len == 1) {
        x[[n]]
    } else {
        x[seq(n - len + 1, n)]
    }
}

#' count grids for each cluster
#' @keywords internal
#' @export
cluster_grids <- function(clutserId) {
    sort(table(clutserId), decreasing = TRUE) %>% as.numeric()
}


listk <- function (...) 
{
    cols <- as.list(substitute(list(...)))[-1]
    vars <- names(cols)
    Id_noname <- if (is.null(vars)) 
        seq_along(cols)
    else which(vars == "")
    if (length(Id_noname) > 0) 
        vars[Id_noname] <- sapply(cols[Id_noname], deparse)
    x <- setNames(list(...), vars)
    return(x)
}

array_3dTo2d <- function(array, I_grid){
    # array <- fliplr.3d(array)
    dim <- dim(array)
    if (length(dim) >= 3) {
        dim(array) <- c(prod(dim[1:2]), dim[3])
    }
    
    if (!missing(I_grid)) {
        array <-  array[I_grid, ]    
    }
    return(array)
}

#' @title Open multi-clusters for parallel calculation
#' @description Open multi-clusters for parallel calculation and be suitable for unix and windows
#' @param ncluster the number of cluster will be opened for parallel calculation
#' @param outfile the output file for open cluster information
#' @param kill if TRUE the opened clusters will be stopped
#' @importFrom parallel makeCluster
#' @importFrom doParallel registerDoParallel
#' @export
InitCluster <- function(ncluster = 16, outfile = "log.txt", kill = TRUE) {
    killCluster <- function() {
        os <- .Platform$OS.type
        if (os == "windows") {
            system("taskkill /IM Rscript.exe -f")
        }
        else if (os == "unix") {
            system("pkill -f R")
        }
    }

    if (kill) {
        killCluster()
    }

    if (file.exists(outfile)) {
        file.remove(outfile)
    }

    if (.Platform$OS.type == "unix") {
        doMC::registerDoMC(ncluster)
    }
    else {
        cl <<- makeCluster(ncluster, outfile = outfile)
        registerDoParallel(cl)
    }
}

 #' @title Is object an empty vector or NULL?
 #' @description Is object an empty vector or NULL?
 #' @param x a vector
 #' @export
 is_empty <- function(x) {
     length(x) == 0
 }
