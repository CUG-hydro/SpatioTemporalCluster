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
# bsamgp
