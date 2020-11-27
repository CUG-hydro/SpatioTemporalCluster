#' Plot drought cluster result
#' 
#' @param idClusters obj returned by [cluster_SpatioTemporal()]
#' @param times integer vector, which time to plot?
#' @param x numeric vector, latitudes
#' @param y numeric vector, longitudes
#' @param ... other parameters to [lattice::levelplot()]
#' 
#' @importFrom scales alpha
#' @import data.table
#' @import lattice sp
#' 
#' @rdname plot
#' @export 
plot.cluster <- function(idClusters, times = 1:4, range = NULL, origin = "1961-01-01", 
    ..., sp.layout = NULL)
{
    
    dim   <- dim(idClusters)
    nrow  <- dim[1]
    ncol  <- dim[2]
    ntime <- dim[3]

    if (is.null(times)) {
        times <- 1:ntime
    } else if (class(times) == "date") {
        times <- difftime(times, origin, units = "days") %>% as.numeric() %>% add(1)
    }

    if (is.null(range)) {
        x <- 1:nrow
        y <- 1:ncol
    } else {
        #' @param range 2 numeric vector
        get_center <- function(range, length) {
            from <- range[1]
            to   <- range[2]
            values   <- seq(from, to, length.out = length + 1)
            cellsize <- diff(values[1:2])
            values[-1] - cellsize/2
        }
        # 1. define dimensions -----------------------------------------------------
        x <- get_center(range[1:2], nrow)
        y <- get_center(range[3:4], ncol)
    }

    grid <- expand.grid(x=x, y=y) %>% data.table()
    
    times <- times[times <= ntime]
    mat   <- idClusters[,,times]

    origin <- as.Date(origin)
    date   <- seq(origin, origin + length(times) - 1, by = "day")

    mat[mat %in% c(-9999L, 0L, -1L)] = NA_integer_
    
    dim(mat) <- c(nrow*ncol, length(times))
    I_good <- colSums2(!is.na(mat), na.rm = TRUE) %>% {which(. > 0)}
    
    if (length(I_good) == 0){
        message("No Cluster detected!")
        return()
    }
    I_start <- first(I_good)
    I_end   <- last(I_good)
    
    date <- date[I_start:I_end]
    mat  <- mat[, I_start:I_end, drop = FALSE]
    
    # mat <- mat - min(as.numeric(mat), na.rm = TRUE) + 1
    df  <- melt(cbind(grid, mat), c("x", "y"), variable.name = "time")
    df$value %<>% factor()
    
    pos_grid <- seq(2, max(df$y), 2)+0.5

    # panel <- function(x, y, z, subscripts, ..., sp.layout) {
    #     # panel.levelplot(x, y, z, subscripts, ...)
    #     sppanel(list(sp.layout), panel.number(), first = TRUE)
    #     panel.levelplot.raster(x, y, z, subscripts, ...)
    #     sppanel(list(sp.layout), panel.number(), first = FALSE)
    # }

    # scale <- list(at = pos_grid, labels = pos_grid - 0.5)
    p <- levelplot(value ~ x*y | time, df, as.table = TRUE, 
            # scales = list(x = scale, y = scale), 
            # at = c(0, 1),
            ...,
            xlab = NULL, ylab = NULL,
            strip=strip.custom(factor.levels=format(date)),
            # names.attr = format(date),
            panel = function(x, y, z, subscripts, ...){
                sppanel(list(sp.layout), panel.number(), first = TRUE)

                lty = 1
                lwd = 0.4
                color = alpha("black", 0.1)
                panel.abline(h = pos_grid, col = color, lty = lty, lwd = lwd)
                panel.abline(v = pos_grid, col = color, lty = lty, lwd = lwd)

                panel.levelplot.raster(x, y, z, subscripts, ...)
                # panel.grid()
                
                I <- subscripts[which(!is.na(z[subscripts]))]
                d_lab <- data.table(x = x[I], y = y[I], labels = z[I])
                d_lab <- d_lab[, .(x = mid(x), y = mid(y)), labels]
                labels <- as.character(z[subscripts])
                # panel.text(d_lab$x, d_lab$y, labels = d_lab$labels, ..., cex = 0.5)
                labels = as.character(z)
                labels[is.na(z)] = ""
                panel.text(x[subscripts], y[subscripts], labels[subscripts], cex = 0.7)
                sppanel(list(sp.layout), panel.number(), first = FALSE)
            })
    p
}

# #' @importFrom methods isGeneric
# #' @export
# plot <- function(x, y, ...) if (!isGeneric("plot")) UseMethod("plot")


mid <- function(x){
    n <- length(x)
    mid <- floor(n/2)
    sort(x, na.last = TRUE)[mid]
}
