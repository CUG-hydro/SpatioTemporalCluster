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
