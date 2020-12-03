#' @title Forward direction to search cluster
#' @description Search neighbouring grids forward
#' @param matrix a nxm matrix. Only TRUE and FALSE are included in this matrix.
#' TRUE indicates this grid is a congeneric grid, while FALSE indicates not.
#' @param clusterID a nxm matrix. All the initial values in this matrix are NA.
#' @param i an integer which indicates the ith row.
#' @param j an integer which indicates the jth column.
#' @return A matrix with the serial number for the identified cluster.
#' @details This function is used to search clusters from top-left to bottom-right
#' corner. The original codes are sourced from Fortran language and written by
#' Samaniego et al., nature climate change, 2018.
#' @keywords internal
connect_spatial_forward <- function(matrix, clusterID, i, j) {
    if (isTRUE(matrix[i, j]) & is.na(clusterID[i, j])) {
        opt$id <- opt$id + 1
        clusterID[i, j] <- opt$id
    }

    for (k in 1:nrow(opt$pos)) {
        i2 <- opt$pos[k, 1] + i
        j2 <- opt$pos[k, 2] + j
        if (i2 > nrow(matrix) | i2 <= 0 | j2 > ncol(matrix) | j2 <= 0) {
            next
        }
        if (is.na(clusterID[i2, j2]) & isTRUE(matrix[i2, j2])) {
            clusterID[i2, j2] <- clusterID[i, j]
        }
    }
    return(clusterID)
}
#' @title  Backward direction to search cluster
#' @description Search neighbouring grids backward
#' @param clusterID a nxm matrix indicating cluster serial number. It is the
#' output of the forward search.
#' @param i an integer which indicates the ith row.
#' @param j an integer which indicates the jth column.
#' @return A matrix with the serial number for the identified cluster.
#' @details This function is used to search clusters from bottom-right to top-left
#' corner. This search is completed after the forward search. The original codes
#' are sourced from Fortran language and written by
#' Samaniego et al., nature climate change, 2018.
#' @keywords internal
connect_spatial_backward <- function(clusterID, i, j) {
    for (k in 1:nrow(opt$pos)) {
        i2 <- opt$pos[k, 1] + i
        j2 <- opt$pos[k, 2] + j
        if (i2 > nrow(clusterID) | i2 <= 0 | j2 > ncol(clusterID) | j2 <= 0) {
            next
        }
        if (!is.na(clusterID[i, j]) & !is.na(clusterID[i2, j2]) &
            clusterID[i, j] != clusterID[i2, j2]) {
            opt$id <- opt$id - 1
            clusterID[clusterID == clusterID[i2, j2]] <- clusterID[i, j]
        }
    }
    return(clusterID)
}

#' @title Find all clusters in a matrix
#' @description Find all clusters in space
#' @param matrix a nxm matrix. Only TRUE and FALSE are included in this matrix.
#' TRUE indicates this grid is a congeneric grid, while FALSE indicates not.
#' @param ncell_connect a integer. If a cluster with number of grids is no more
#' than this threshold, this cluster will be excluded.
#' @param diag a logical value. If TRUE, the diagonal grids are taken as
#' the adjacent congeneric grids.
#' @return A matrix with the serial number for the identified cluster.
#' @details This function searches each grid in the matrix and identifies all
#' clusters and assigns a serial number for each cluster. The original codes
#' are sourced from Fortran language and written by
#' Samaniego et al., nature climate change, 2018.
#' @importFrom progress progress_bar
#' @export
connect_spatial_Rfortran <- function(matrix, ncell_connect, diag = FALSE) {
    ####### Searching directions of the neighbouring grids#######################
    if (isTRUE(diag)) {
        pos <- matrix(c(1, 0, 0, 1, -1, 0, 0, -1, 1, 1, 1, -1, -1, -1, -1, 1),
            ncol = 2, byrow = T
        )
        # The diagonal grids are taken as the adjacent congeneric grids
    } else {
        pos <- matrix(c(1, 0, 0, 1, -1, 0, 0, -1), ncol = 2, byrow = T)
        # The diagonal grids are not taken as the adjacent congeneric grids
    }
    ######## assign cluster number to physical address###########################
    opt <- list2env(x = list(id = 0, pos = pos)) # Store to a physical address
    assign("opt", opt, envir = .GlobalEnv) # Assign to the global environment
    clusterID <- matrix(NA, nrow = nrow(matrix), ncol = ncol(matrix))
    ######## search clusters from the matrix#####################################
    if (sum(matrix) > 0) {
        ####### Find the locations of grids with TRUE############################
        index <- list()
        ngrid <- 0
        for (i in 1:nrow(matrix)) {
            for (j in 1:ncol(matrix)) {
                if (isTRUE(matrix[i, j])) {
                    ngrid <- ngrid + 1
                    index[[ngrid]] <- c(i, j)
                }
            }
        }
        ################## searching from top-left to bottom-right corner########
        pb <- progress_bar$new(
            format = "Complete[:bar]:percent in :elapsed",
            total = ngrid, clear = FALSE
        )
        for (k in 1:ngrid) {
            clusterID <- connect_spatial_forward(
                matrix = matrix, clusterID = clusterID,
                i = index[[k]][1],
                j = index[[k]][2]
            )
            pb$tick()
        }
        ################## searching from bottm-right to top-left corner#########
        pb <- progress_bar$new(
            format = "Complete[:bar]:percent in :elapsed",
            total = ngrid, clear = FALSE
        )
        for (k in ngrid:1) {
            clusterID <- connect_spatial_backward(
                clusterID = clusterID,
                i = index[[k]][1],
                j = index[[k]][2]
            )
            pb$tick()
        }
        ################## exclude clusters with girds <= ncell_connect##################
        areas <- table(c(clusterID))
        names <- names(areas)
        clusterID[clusterID %in% names[areas < ncell_connect]] <- NA
    }
    return(clusterID)
}
