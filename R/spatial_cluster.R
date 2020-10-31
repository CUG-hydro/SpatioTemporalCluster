#' @title Search adjacent congeneric grids
#' @description The adjacent congeneric grids are identified by recursion algorithm
#' @param matrix a nxm matrix. Only TRUE and FALSE are included in this matrix.
#' TRUE indicates this grid is a congeneric grid, while FALSE indicates not.
#' @param clusterID a nxm matrix. All the vaues in this matrix are the same, i.e. miss.val.
#' @param i an integer which indicates the ith row.
#' @param j an integer which indicates the jth column.
#' @param miss.val a numeric value which is the initial value of the clusterID.
#' @param diag a logical value. If TRUE, the diagonal grids are taken as
#' the adjacent congeneric grids.
#' @return A matrix with the serial number for the identified cluster.
#' @details For the [i, j] grid, this function is used to find all the adjacent
#' congeneric grids of this [i, j] grid. Then, all the identified grids are taken
#' as a cluster. For this cluster, a serial number is named in the grids of this cluster.
#' @export
find_cluster <- function(matrix, clusterID, i, j, miss.val = -999, diag = FALSE){
    if (clusterID[i, j] != miss.val){
        return(clusterID)
        break
    }
    #If this [i, j] grid was searched, it will be skipped
    if (isFALSE(matrix[i, j])){
        clusterID[i, j] <- 0
        return(clusterID)
        #If this [i, j] grid is no congeneric grid, it will be returned as zero
    } else {

        if (isFALSE(opt$id.status)){
            opt$id <- opt$id + 1
            opt$id.status <- TRUE
            #for the current identified cluster, its serial number is opt$id
            #the id.status controls the current adjacent congeneric grids with the same serial number
        }
        clusterID[i, j] <- opt$id

        if (isTRUE(diag)){
            pos <- matrix(c(1,0, 0,1, -1,0, 0,-1, 1,1, 1,-1, -1,-1, -1,1),
                          ncol = 2, byrow = T)
            #The diagonal grids are taken as the adjacent congeneric grids
        } else {
            pos <- matrix(c(1,0, 0,1, -1,0, 0,-1), ncol = 2, byrow = T)
            #The diagonal grids are not taken as the adjacent congeneric grids
        }

        for (k in 1:nrow(pos)){
            i2 <- pos[k, 1] + i
            j2 <- pos[k, 2] + j
            if (i2 > nrow(matrix) | i2 <= 0 | j2 > ncol(matrix) | j2 <= 0){
                next
            }
            clusterID <- find_cluster(matrix, clusterID, i2, j2)
        }
        return(clusterID)
    }
}

#' @title Find all clusters in a matrix
#' @description Find all clusters in space with the function find_cluster
#' @param matrix a nxm matrix. Only TRUE and FALSE are included in this matrix.
#' TRUE indicates this grid is a congeneric grid, while FALSE indicates not.
#' @param miss.val a numeric value which is the initial value of the clusterID in
#' the function find_cluster.
#' @param diag a logical value. If TRUE, the diagonal grids are taken as
#' the adjacent congeneric grids.
#' @return A matrix with serial numbers for all identified clusters.
#' @details This function searches each grid in the matrix and identifies all
#' clusters and assigns a serial number for each cluster. This function is built
#' based on the find_cluster.
#' @export
spatial_cluster <- function(matrix, miss.val = -999, diag = FALSE){
    opt <- list2env(x = list(id = 0, id.status = FALSE)) #Store to a physical address
    assign("opt", opt, envir = .GlobalEnv) #Assign to the global environment
    clusterID <- matrix(miss.val, nrow = nrow(matrix), ncol = ncol(matrix))

    for (i in 1:nrow(matrix)){
        for (j in 1:ncol(matrix)){

            clusterID <- find_cluster(matrix = matrix, clusterID = clusterID,
                                      i = i, j = j, miss.val = miss.val,
                                      diag = diag)
            opt$id.status <- FALSE
        }
        cat(sprintf('=================[%.2f%s]', i/nrow(matrix)*100, '%'), '\n')
    }
    return(clusterID)
}
