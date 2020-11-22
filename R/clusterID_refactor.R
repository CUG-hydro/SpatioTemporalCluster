#' clusterID_refactor
#' 
#' @param clusterId 3d array
#' @keywords internal
#' @export 
clusterID_refactor <- function(clusterID, factor = 10000) {
    # clusterID <- sapply(clusterID, c)
    
    dim = dim(clusterID)
    ntime <- last(dim)
    dim2 = c(prod(dim[1:2]), dim[3])
    dim(clusterID) <- dim2

    initID <- plyr::alply(clusterID, 2, function(x) names(table(x)))
    ncluster <- sapply(initID, length)

    mask <- apply(clusterID, 1, function(x) sum(!is.na(x)))
    mask <- which(mask > 0)

    cno <- matrix(NA, nrow = ntime, ncol = max(ncluster, na.rm = T))
    # browser()
    for (i in 1:ntime) {
        for (j in 1:ncluster[i]) {
            id = i * factor + j
            cno[i, j] <- id
            clusterID[mask, i][clusterID[mask, i] == initID[[i]][j]] <- id
        }
    }
    dim(clusterID) <- dim
    listk(clusterID, cno, ncluster)
}
