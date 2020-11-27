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
#' @importFrom Ipaper fprintf
#' @export
# connect_temporal_Rfortran <- function(r_cluster, ncell_overlap = 5, factor = 10000,
#     verbose = FALSE)
# {
#     clusterID <- r_cluster$clusterID
#     cno       <- r_cluster$cno
#     ncluster  <- r_cluster$ncluster
# #########From first time to end time and from top to bottom#########
#     k <- 0
#     for (t in 2:dim(clusterID)[3]) {
#       if (ncluster[t] < 1) next
#       for (i in 1:ncluster[t]) {
#             if (is.na(cno[t, i])) next
#             if (ncluster[t-1] < 1) next
#             for (j in 1:ncluster[t-1]) {
#               if (is.na(cno[t-1, j])) next
#               id.now = cno[t, i]
#               id.pre = cno[t - 1, j]
#               if (id.now == id.pre) next
#               ncInter <- sum(clusterID[,,t] == id.now & clusterID[,,t-1] == id.pre,
#                                na.rm = TRUE)
#               if (ncInter >= ncell_overlap){
#                 clusterID[,,1:t][clusterID[,,1:t] == id.pre] <- id.now
#                 cno[1:t, ][cno[1:t, ] == id.pre] <- id.now
#                 k = k + 1
#                 if (verbose) fprintf("First:[k = %d, t=%d] %d -> %d\n", k, t, id.pre, id.now)
#                 # print(check(clusterID) %>% tail())
#               }
# 
#             }
#         }
#     }
#     #########From end time to first time and from bottom to top#########
#     k <- 0
#     for (t in dim(clusterID)[3]:2){
#       if (ncluster[t-1] < 1) next
#       for (i in ncluster[t-1]:1){
#         if (is.na(cno[t-1, i])) next
#         if (ncluster[t] < 1) next
#         for (j in ncluster[t]:1){
#           if (is.na(cno[t, j])) next
#           id.now <- cno[t, j]
#           id.pre <- cno[t-1, i]
#           if (id.pre == id.now) next
#           ncInter <- sum(clusterID[,,t] == id.now & clusterID[,,t-1] == id.pre,
#                          na.rm = TRUE)
#           if (ncInter >= ncell_overlap){
#             clusterID[clusterID == id.now] <- id.pre
#             cno[cno == id.now] <- id.pre
#             k = k + 1
#             if (verbose) fprintf("Second:[k = %d, t=%d] %d -> %d\n", k, t, id.now, id.pre)
#           }
#         }
#       }
#     }
#     return(clusterID)
# }



connect_temporal_Rfortran <- function(r_cluster, ncell_overlap = 5, factor = 10000,
                                      verbose = FALSE)
{
  clusterID <- r_cluster$clusterID
  cno       <- r_cluster$cno
  ncluster  <- r_cluster$ncluster
  #########From first time to end time and from top to bottom#########
  k <- 0
  for (t in 2:dim(clusterID)[3]) {
    while(t >= 2){
      if (ncluster[t] < 1) next
      for (i in 1:ncluster[t]) {
        if (is.na(cno[t, i])) next
        if (ncluster[t-1] < 1) next
        for (j in 1:ncluster[t-1]) {
          if (is.na(cno[t-1, j])) next
          id.now = cno[t, i]
          id.pre = cno[t - 1, j]
          if (id.now == id.pre) next
          ncInter <- sum(clusterID[,,t] == id.now & clusterID[,,t-1] == id.pre,
                         na.rm = TRUE)
          if (ncInter >= ncell_overlap){
            clusterID[clusterID == id.pre] <- id.now
            cno[cno == id.pre] <- id.now
            k = k + 1
            if (verbose) fprintf("First:[k = %d, t=%d] %d -> %d\n", k, t, id.pre, id.now)
            # print(check(clusterID) %>% tail())
          }
        }
      }
      t <- t - 1
    }
  }
  # #########From end time to first time and from bottom to top#########
  # k <- 0
  # for (t in dim(clusterID)[3]:2){
  #   if (ncluster[t-1] < 1) next
  #   for (i in ncluster[t-1]:1){
  #     if (is.na(cno[t-1, i])) next
  #     if (ncluster[t] < 1) next
  #     for (j in ncluster[t]:1){
  #       if (is.na(cno[t, j])) next
  #       id.now <- cno[t, j]
  #       id.pre <- cno[t-1, i]
  #       if (id.pre == id.now) next
  #       ncInter <- sum(clusterID[,,t] == id.now & clusterID[,,t-1] == id.pre,
  #                      na.rm = TRUE)
  #       if (ncInter >= ncell_overlap){
  #         clusterID[clusterID == id.now] <- id.pre
  #         cno[cno == id.now] <- id.pre
  #         k = k + 1
  #         if (verbose) fprintf("Second:[k = %d, t=%d] %d -> %d\n", k, t, id.now, id.pre)
  #       }
  #     }
  #   }
  # }
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
