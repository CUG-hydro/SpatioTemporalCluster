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
                if (id.now == id.pre) next()
                
                ncInter <- sum(clusterID[,,t] == cno[t, i] & clusterID[,,t-1] == cno[t-1, j],
                               na.rm = TRUE)
                if (ncInter >= ncell_overlap){
                    id.now = cno[t, i]
                    id.pre = cno[t - 1, j]
                    
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
