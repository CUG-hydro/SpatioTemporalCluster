#' @title Find spatio-temporal clusters in a array
#' @description Identify the evolution of regional event
#' @param clusterID a three-dimensional array. This array is formed layer by
#' layer. Each layer is a matrix that is the clusters identified by the function
#' spatial_cluster_Fortran.
#' @param overlap a integer. If the share grids of the two clusters in two
#' consecutive layers are no less than the overlap, the two clusters will be
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
ClusterEvolution_Fortran <- function(clusterID, overlap = 1, factor = 10000){
    ######reorder the serial number by a factor#################################
    cno <- matrix(NA, nrow = dim(clusterID)[3], ncol = factor)
    ncluster <- vector()
    for (i in 1:dim(clusterID)[3]){
        if (sum(!is.na(clusterID[,,i])) == 0){
            ncluster[i] <- 0
            next
        } else {
            initID <- names(table(clusterID[,,i]))
            ncluster[i] <- length(initID)
            if (ncluster[i] > factor){
                stop('The number of clusters is more than the factor')
            }
            for (j in 1:ncluster[i]){
                cno[i, j] <- i * factor + j
                clusterID[,,i][clusterID[,,i] == initID[j]] <- cno[i, j]
            }
        }
    }
    ######Find clusters in time by intersection sets############################
    for (t in 2:dim(clusterID)[3]){
        for (i in 1:ncluster[t]){
            if (is.na(cno[t, i])) next
            for (j in 1:ncluster[t-1]){
                if (is.na(cno[t-1, j])) next
                ncInter <- sum(clusterID[,,t] == cno[t, i] & clusterID[,,t-1] == cno[t-1, j],
                               na.rm = TRUE)
                if (ncInter >= overlap){
                    clusterID[,,1:t][clusterID[,,1:t] == cno[t-1, j]] <- cno[t, i]
                    cno[1:t, ][cno[1:t, ] == cno[t-1, j]] <- cno[t, i]
                }
            }
        }
    }
    return(clusterID)
}
