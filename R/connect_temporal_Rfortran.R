rev_rows <- function(mat) {
    for (i in 1:nrow(mat)) {
        x = mat[i, ] %>% sort(decreasing = TRUE)
        mat[i, 1:length(x)] <- x
    }
    mat
}

#' @title Connect clusters in two neighbouring times
#' @description Find same clusters in two neighbouring times
#' @param r_cluster a list which is the output of the function clusterID_recode
#' @param ntime a integer indicates clusters in this time and before are connected
#' @param mask a vector to show which grids have clusters
#' @param ncluster a vector with the number of clusters in each time
#' @param ncell_overlap a integer. If the share grids of the two clusters in two
#' consecutive layers are no less than the overlap, the two clusters will be
#' taken as the same one.
#' @param verbose a logical value. If TRUE, the cluster translation process will
#' be printed.
#' @return a list including clusterID and cno
#' \describe{
#' \item{clusterID}{the same as para clusterID but recoded}
#' \item{cno}{a ntime x ncluster matrix with cluster encodings}
#' }
#' @export
connect_neighbor <- function(r_cluster, ntime, ncluster, mask, ncell_overlap = 5, verbose = FALSE){
    clusterID <- r_cluster$clusterID
    cno       <- r_cluster$cno
    while(olp$overlap2){
        olp$overlap2 <- FALSE
        for (i in 1:ncluster[ntime]) {
            if (is.na(cno[ntime, i]) || is_empty(cno[ntime, i])) next
            for (j in 1:ncluster[ntime-1]){
                if (is.na(cno[ntime-1, j]) || is_empty(cno[ntime-1, j])) next
                id.now <- cno[ntime, i]
                id.pre <- cno[ntime-1, j]
                if (id.now == id.pre) next
                ncInter <- sum(clusterID[mask,ntime] == id.now & clusterID[mask,ntime-1] == id.pre,
                               na.rm = TRUE)
                if (ncInter >= ncell_overlap){
                    clusterID[mask, ][clusterID[mask, ] == id.pre] <- id.now
                    cno[cno == id.pre] <- id.now
                    olp$overlap1 <- TRUE
                    olp$overlap2 <- TRUE
                    olp$k2 = olp$k2 + 1
                    if (verbose) {
                        cat(sprintf("[%dst][k = %d, t=%d] %d -> %d\n", olp$k1, olp$k2, ntime, id.pre, id.now))
                    }
                }
            }
        }
    }
    listk(clusterID, cno)
}

#' @title Connect clusters in all times
#' @description Find the same clusters in the whole period
#' @param r_cluster a list which is the output of the function clusterID_recode
#' @param ncell_overlap a integer. If the share grids of the two clusters in two
#' consecutive layers are no less than the overlap, the two clusters will be
#' taken as the same one.
#' @param verbose a logical value. If TRUE, the cluster translation process will
#' be printed.
#' @return a a ngrid x ntime matrix in which all clusters are connected in space
#' and time.
#' @importFrom progress progress_bar
#' @export
connect_temporal_Rfortran <- function(r_cluster, ncell_overlap = 5, verbose = FALSE){
    olp <- list2env(x = list(k1 = 0, k2 = 0, overlap1 = TRUE, overlap2 = TRUE))
    #Store to a physical address
    assign("olp", olp, envir = .GlobalEnv) #Assign to the global environment
    ts <- ncol(r_cluster$clusterID)
    ncluster  <- r_cluster$ncluster
    mask      <- r_cluster$mask
    while (olp$overlap1){
        olp$overlap1 <- FALSE
        olp$k1 <- olp$k1 + 1
        pb <- progress_bar$new(format = 'Complete[:bar]:percent in :elapsed',
                               total = ts-1, clear = FALSE)
        for (t in 2:ts){
            olp$overlap2 <- TRUE
            r_cluster <- connect_neighbor(r_cluster = r_cluster, ntime = t,
                                          ncluster = ncluster, mask = mask,
                                          ncell_overlap = ncell_overlap,
                                          verbose = verbose)
            pb$tick()
        }
    }
    return(r_cluster$clusterID)
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
