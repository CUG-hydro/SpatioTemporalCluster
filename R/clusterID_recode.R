#' @title Recode encodings of clusters
#' @description Recode encodings of clusters by a factor
#' @param clusterID a ngrid x ntime matrix. The clusters in this matrix have
#' been found by the function connect_spatial_Fortran.
#' @param factor a integer that is used to recode cluster encoding.
#' @return a list including clusterID, cno, ncluster, and mask:
#' \describe{
#' \item{clusterID}{the same as para clusterID but recoded}
#' \item{cno}{a ntime x ncluster matrix with cluster encodings}
#' \item{ncluster}{a vector with the number of clusters in each time}
#' \item{mask}{a vector to show which grids have clusters}
#' }
#' @keywords internal
#' @export
clusterID_recode <- function(clusterID, factor = 10000) {
    dims <- dim(clusterID)
    mask <- apply(clusterID, 1, function(x) sum(!is.na(x)))
    mask <- which(mask > 0)
    initID <- apply(clusterID, 2, function(x) names(table(x)))
    ncluster <- sapply(initID, length)
    cno <- matrix(NA, nrow = dims[2], ncol = max(ncluster, na.rm = T))
    if (max(ncluster, na.rm = T) > factor) {
        stop("The number of clusters is more than the factor")
    }
    for (i in 1:dims[2]) {
        if (ncluster[i] < 1) next
        for (j in 1:ncluster[i]) {
            cno[i, j] <- i * factor + j
            clusterID[mask, i][clusterID[mask, i] == initID[[i]][j]] <- cno[i, j]
        }
    }
    listk(clusterID, cno, ncluster, mask)
}

# clusterID_recode <- function(clusterID, factor = 10000) {
#     # clusterID <- sapply(clusterID, c)
#     dim = dim(clusterID)
#     ntime <- last(dim)
#     dim2 = c(prod(dim[1:2]), dim[3])
#     dim(clusterID) <- dim2

#     initID <- plyr::alply(clusterID, 2, function(x) names(table(x)))
#     ncluster <- sapply(initID, length)

#     mask <- apply(clusterID, 1, function(x) sum(!is.na(x)))
#     mask <- which(mask > 0)

#     cno <- matrix(NA, nrow = ntime, ncol = max(ncluster, na.rm = T))
#     for (i in 1:ntime) {
#         if (ncluster[i] < 1) next
#         for (j in 1:ncluster[i]) {
#             id = i * factor + j
#             cno[i, j] <- id
#             clusterID[mask, i][clusterID[mask, i] == initID[[i]][j]] <- id
#         }
#     }
#     dim(clusterID) <- dim
#     listk(clusterID, cno, ncluster)
# }
