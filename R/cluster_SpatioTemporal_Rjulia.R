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

#' @title Find spatiotemporal connected clusters in a three-dimensional array
#' @description Connect clusters in space and time
#' @param arr a three-dimensional array including only TRUE and FALSE.
#' @param ncell_connect a integer. If a cluster with number of grids is no more
#' than this threshold, this cluster will be excluded.
#' @param ncell_overlap a integer. If the share grids of the two clusters in two
#' consecutive layers are no less than the overlap, the two clusters will be
#' taken as the same one.
#' @param factor a integer that is used to recode cluster encoding.
#' @param diag a logical value. If TRUE, the diagonal grids are taken as
#' the adjacent congeneric grids.
#' @param nodes a integer. The number of cluster will be opened for parallel
#' calculation.
#' @param verbose a logical value. If TRUE, the cluster translation process will
#' be printed.
#' @importFrom plyr llply
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @import foreach
cluster_SpatioTemporal_R <- function(arr, ncell_connect = 1, ncell_overlap = 5,
                                     factor = 10000, diag = FALSE, nodes = 16,
                                     verbose = TRUE) {
    dims <- dim(arr)
    arr <- llply(1:dims[3], function(ind) arr[, , ind])
    initCluster(ncluster = nodes)
    clusterID <- foreach(
        matrix = arr, .combine = cbind,
        .packages = c("progress"),
        .export = c(
            "ncell_connect", "diag",
            "connect_spatial_forward",
            "connect_spatial_backward",
            "connect_spatial_Fortran"
        )
    ) %dopar% {
        c(connect_spatial_Fortran(
            matrix = matrix,
            ncell_connect = ncell_connect,
            diag = diag
        ))
    }

    stopCluster(cl)
    rm(arr)
    gc()
    r.cluster <- clusterID_recode(clusterID = clusterID, factor = factor)
    clusterID <- connect_temporal(
        r.cluster = r.cluster, verbose = verbose,
        ncell_overlap = ncell_overlap
    )
    clusterID <- array(clusterID, dim = dims)
    return(clusterID)
}
