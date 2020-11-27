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
    for (t in 2:ntime) {
        cluster.now <- clusterID[,t]
        cluster.pre <- clusterID[,t-1]
        ind.ncell_overlap <- !is.na(cluster.now) & !is.na(cluster.pre)
        ids.raw <- cluster.now[ind.ncell_overlap] %>% table
        ids.now <- names(ids.raw)[ids.raw >= ncell_overlap] %>% as.numeric()
        # if (verbose) fprintf("t = %d, cluster: %d: ", t, length(ids.now))
        # 循环需要进行两次以消灭错误
        for (id.now in c(ids.now, ids.now)){
            ind.now <- which(cluster.now == id.now)
            ids.pre <- cluster.pre[ind.now]
            ids.pre <- ids.pre[!is.na(ids.pre)] %>% unique() # %>% sort

            iter = 0
            while (TRUE) {
                iter = iter + 1; j = 0; n_matched = 0
                stats.pre <- rep(FALSE, length(ids.pre))
        
                for (id.pre in ids.pre) {
                    j = j + 1
                    if (id.now == id.pre) next()

                    ncInter <- sum(cluster.pre[ind.now] == id.pre, na.rm = TRUE)
                    if (ncInter >= ncell_overlap) {
                        clusterID[mask, 1:t][clusterID[mask, 1:t] == id.pre] <- id.now

                        # n_matched = n_matched + 1; ids.pre_new[n_matched] = id.now
                        k = k + 1
                        if (verbose) fprintf("[k = %d, t=%d] %d -> %d\n", k, t, id.pre, id.now)
                        
                        cluster.now <- clusterID[, t]
                        cluster.pre <- clusterID[, t - 1]
                        ind.now <- which(cluster.now == id.now)
                        stats.pre[j] <- TRUE
                    }
                }

                if (all(stats.pre) || all(!stats.pre)) break()
                ids.pre <- ids.pre[!stats.pre]
                # ids.pre <- setdiff(ids.pre_new, ids.pre)) %>% unique()  # loop for the rest
            }
        }
    }
    dim(clusterID) <- dim
    clusterID
}
