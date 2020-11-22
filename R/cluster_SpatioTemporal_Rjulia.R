env <- list2env(list(.julia = FALSE))

julia_setup <- function() {
    JuliaCall::julia_setup()
    JuliaCall::julia_source("cluster/src/cluster.jl")
}

#' @importFrom JuliaCall julia_setup julia_source julia_call
julia_init <- function() {
    if (!env$.julia) {
        env$.julia = TRUE
        julia_setup()
    }
}

connect_spatial_julia <- function(arr, ncell_connected = 1L, factor = 1e4, diag = FALSE) {
    ans = julia_call("cluster.connect_spatial", arr,
        factor = as.integer(factor), # max clusters for each time
        minCells = as.integer(ncell_connected),
        diag = diag
    )   
    ans %<>% set_names(c("ncluster", "cno", "clusterID"))
    ans$clusterID[ans$clusterID <= 0] = NA_integer_
    ans
}

#' @rdname cluster_SpatioTemporal
#' @export
cluster_SpatioTemporal_julia <- function(
    arr, ncell_connected = 1L, ncell_overlap = 5L, factor = 1e4, diag = FALSE, ...) 
{
    julia_init()
    clusterId = julia_call("cluster.cluster_SpatioTemporal", arr,
        factor = as.integer(factor), # max clusters for each time
        minCells = as.integer(ncell_connected),
        minOverlapCells = as.integer(ncell_overlap),
        diag = diag)
    clusterId[clusterId <= 0] <- NA_integer_
    clusterId
}

#' @rdname cluster_SpatioTemporal
#' @param version character, "julia" or fortran
#' @export
cluster_SpatioTemporal_R <- function(
    arr, ncell_connected = 1L, ncell_overlap = 5L, factor = 1e4, diag = FALSE, 
    verbose = FALSE, version = c("julia", "fortran"), ...) 
{
    r_cluster <- connect_spatial(arr, diag = diag, factor = factor)
    FUN <- switch(version[1],
        "julia" = connect_temporal_Rjulia,
        "fortran" = connect_temporal_Rfortran
    )
    FUN(r_cluster, ncell_overlap = ncell_overlap, verbose = verbose)
}
