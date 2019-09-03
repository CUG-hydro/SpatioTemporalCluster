#' Plot drought cluster result
#' 
#' @param idClusters obj returned by [ClusterEvolution()]
#' @param x numeric vector, latitudes
#' @param y numeric vector, longitudes
#' 
#' @importFrom scales alpha
#' @import data.table
#' @import lattice
#' 
#' @export plot.cluster
plot.cluster <- function(idClusters, x = NULL, y = NULL){
    dim   <- dim(idClusters)
    nrow  <- dim[1]
    ncol  <- dim[2]
    ntime <- dim[3]

    if (is.null(x)) x <- 1:nrow
    if (is.null(y)) y <- 1:ncol
    grid <- expand.grid(x=x, y=y)
    
    mat <- idClusters
    mat[mat == -9999L] = NA_integer_
    
    dim(mat) <- c(nrow*ncol, ntime)
    df <- melt(cbind(grid, mat), c("x", "y"), variable.name = "time")
    
    pos_grid <- c(2, 4, 6, 8)+0.5
    scale <- list(at = pos_grid, labels = pos_grid - 0.5)
    p <- levelplot(value ~ x*y | time, df, as.table = TRUE, 
            scales = list(x = scale, y = scale), 
            layout = c(4, 3),
            panel = function(x, y, z, subscripts, ...){
                lty = 1
                lwd = 0.4
                color = alpha("black", 0.1)
                panel.abline(h = pos_grid, col = color, lty = lty, lwd = lwd)
                panel.abline(v = pos_grid, col = color, lty = lty, lwd = lwd)

                panel.levelplot.raster(x, y, z, subscripts, ...)

                I <- subscripts[which(!is.na(z[subscripts]))]

                d_lab <- data.table(x = x[I], y = y[I], labels = z[I])
                # d_lab <- d_lab[, .(x = mid(x), y = mid(y)), labels]
                # labels <- as.character(z[subscripts])
                panel.text(d_lab$x, d_lab$y, labels = d_lab$labels, ..., cex = 0.5)
            })
    p
}

mid <- function(x){
    n <- length(x)
    mid <- floor(n/2)
    sort(x, na.last = TRUE)[mid]
}
