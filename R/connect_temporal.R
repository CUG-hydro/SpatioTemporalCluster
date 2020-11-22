rev_rows <- function(mat) {
    for (i in 1:nrow(mat)) {
        x = mat[i, ] %>% sort(decreasing = TRUE)
        mat[i, 1:length(x)] <- x
    }
    mat
}
  
#' @title Find spatio-temporal clusters in a array
#' @description Identify the evolution of regional event
#' @param clusterID a three-dimensional array. This array is formed layer by
#' layer. Each layer is a matrix that is the clusters identified by the function
#' spatial_cluster_Fortran.
#' @param ncell_overlap a integer. If the share grids of the two clusters in two
#' consecutive layers are no less than the ncell_overlap, the two clusters will be
#' taken as the same one.
#' @param factor a integer which is used to compile a series number for each
#' cluster.
#' @return A array. The clusters in param clusterID are connected by temporal
#' overlaps and renamed by a new series number.
#' @details The function is used to connect the temporal links of spatial
#' clusters identified by the function spatial_cluster_Fortran.
#' The original codes are sourced from Fortran language and written by
#' Samaniego et al., nature climate change, 2018.
#' @export
connect_temporal_Rfortran <- function(r_cluster, ncell_overlap = 5, factor = 10000, 
    verbose = FALSE)
{
    clusterID = r_cluster$clusterID
    cno       = r_cluster$cno
    ncluster  = r_cluster$ncluster

    k = 0
    for (t in 2:dim(clusterID)[3]) {
        for (i in 1:ncluster[t]) {
            if (is.na(cno[t, i])) next
            for (j in 1:ncluster[t-1]) {
                if (is.na(cno[t-1, j])) next
                ncInter <- sum(clusterID[,,t] == cno[t, i] & clusterID[,,t-1] == cno[t-1, j],
                               na.rm = TRUE)
                if (ncInter >= ncell_overlap){
                    id.now = cno[t, i]
                    id.pre = cno[t - 1, j]
                    if (id.now == id.pre) next()

                    clusterID[,,1:t][clusterID[,,1:t] == cno[t-1, j]] <- cno[t, i]
                    cno[1:t, ][cno[1:t, ] == cno[t-1, j]] <- cno[t, i] 

                    k = k + 1
                    if (verbose) fprintf("[k = %d, t=%d] %d -> %d\n", k, t, id.pre, id.now)
                    # print(check(clusterID) %>% tail())
                }
            }
        }
    }
    return(clusterID)
}


#' @rdname connect_temporal_Rfortran
connect_temporal_Rjulia <- function(r_cluster, ncell_overlap = 5, factor = 10000, 
    verbose = FALSE)
{
    dim = dim(r_cluster$clusterID)
    clusterID = r_cluster$clusterID %>% array_3dTo2d()
    ncluster  = r_cluster$ncluster

    ntime <- dim(clusterID) %>% last()
    mask  <- apply(clusterID, 1, function(x) sum(!is.na(x)))
    mask  <- which(mask > 0)

    k = 0
    for (t in 2:ntime){
        cluster.now <- clusterID[,t]
        cluster.pre <- clusterID[,t-1]
        ind.ncell_overlap <- !is.na(cluster.now) & !is.na(cluster.pre)
        ids.raw <- cluster.now[ind.ncell_overlap] %>% table
        ids.now <- names(ids.raw)[ids.raw >= ncell_overlap] %>% as.numeric()

        # if (verbose) fprintf("t = %d, cluster: %d: ", t, length(ids.now))
        for (id.now in ids.now){
            ind.now <- which(cluster.now == id.now)
            ids.pre <- cluster.pre[ind.now]
            ids.pre <- ids.pre[!is.na(ids.pre)] %>% unique() # %>% sort

            iter = 0
            while (TRUE) {
                iter = iter + 1; j = 0
                stats.pre <- rep(FALSE, length(ids.pre))
                
                for (id.pre in ids.pre) {
                    j = j + 1
                    ncInter <- sum(cluster.pre[ind.now] == id.pre, na.rm = TRUE)
                    if (ncInter >= ncell_overlap) {
                        clusterID[mask, 1:t][clusterID[mask, 1:t] == id.pre] <- id.now

                        k = k + 1
                        if (verbose) fprintf("[k = %d, t=%d] %d -> %d\n", k, t, id.pre, id.now)
                        
                        cluster.now <- clusterID[, t]
                        cluster.pre <- clusterID[, t - 1]
                        ind.now <- which(cluster.now == id.now)
                        stats.pre[j] <- TRUE
                    }
                }
                if (all(stats.pre) || all(!stats.pre)) break()
                ids.pre <- ids.pre[!stats.pre] # loop for the rest
            }
        }
    }
    dim(clusterID) <- dim
    clusterID
}
