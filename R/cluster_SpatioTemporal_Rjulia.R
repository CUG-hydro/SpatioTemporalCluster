env <- list2env(list(.julia = FALSE))

julia_setup <- function() {
    JuliaCall::julia_setup()
    # infile <- system.file("cluster/src/cluster.jl", package = "SpatioTemporal.cluster")
    # print(infile)
    JuliaCall::julia_library("SpatioTemporalCluster")
    # JuliaCall::julia_source(infile)
}

#' @importFrom JuliaCall julia_setup julia_source julia_call
julia_init <- function() {
    if (!env$.julia) {
        env$.julia = TRUE
        julia_setup()
    }
}

connect_spatial_julia <- function(arr, ncell_connect = 1L, factor = 1e4, diag = FALSE) {
    ans = julia_call("connect_spatial", arr,
        factor = as.integer(factor), # max clusters for each time
        minCells = as.integer(ncell_connect),
        diag = diag
    )   
    ans %<>% set_names(c("ncluster", "cno", "clusterID"))
    ans$clusterID[ans$clusterID <= 0] = NA_integer_
    ans
}

#' @param method one of "tree", "recursive" and "low". "tree" is the default and
#' the fastest. The second fastest is "recursive".
#' 
#' @rdname cluster_SpatioTemporal
#' @export
cluster_SpatioTemporal_julia <- function(arr, method = "tree",
    ncell_connect = 1L, ncell_overlap = 5L, factor = 1e4, diag = FALSE, ...) 
{
    julia_init()
    clusterId = julia_call("cluster_SpatioTemporal", arr,
        method = "tree",
        ncell_connect = as.integer(ncell_connect),
        ncell_overlap = as.integer(ncell_overlap),
        factor = as.integer(factor), # max clusters for each time
        diag = diag)
    clusterId[clusterId <= 0] <- NA_integer_
    clusterId
}

#' @rdname cluster_SpatioTemporal
#' @param version `connect_temporal` version in R language, one of "julia" or fortran
#' @export
cluster_SpatioTemporal_R <- function(
    arr, ncell_connect = 1L, ncell_overlap = 5L, factor = 1e4, diag = FALSE, 
    verbose = FALSE, version = c("julia", "fortran"), ...) 
{
    r_cluster <- connect_spatial(arr, ncell_connect = ncell_connect, factor = factor, diag = diag)
    FUN <- switch(version[1],
        "julia" = connect_temporal_Rjulia,
        "fortran" = connect_temporal_Rfortran
    )
    FUN(r_cluster, ncell_overlap = ncell_overlap, verbose = verbose)
}
