% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_SpatioTemporal.R
\name{eventIndicator}
\alias{eventIndicator}
\title{eventIndicator}
\usage{
eventIndicator(arr, threshold, mask = NULL, exceeding = TRUE, masked = FALSE)
}
\arguments{
\item{arr}{double 2d array or 3d array.
If \code{mask} is not presented, arr should be a 3d array, with the dim of \verb{[nlon, nlat, ntime]}.}

\item{threshold}{double number}

\item{mask}{logical matrix, \verb{[nrow, ncol]}, only TRUE will be proceed.}

\item{exceeding}{Boolean. How to define a event? Exceed the threshold or less than?
For examples, for heatwaves, \code{exceeding} should be true; for drought, cold waves
\code{exceeding} should be false.}

\item{masked}{whether mat has been masked? In the Fortran version SMI, nrow(mat)
should be equal to sum(mask).
\itemize{
\item If true, no further process;
\item If false, \code{mat} will be masked by \code{mask}.
}}
}
\value{
\itemize{
\item \code{array}:
\itemize{
\item \code{1}     : extreme event
\item \code{0}     : not
\item \code{-9999} : masked regions
}
\item \code{cellCoor}: index of selected region.
}
}
\description{
Return a drought status array with 0 (non-drought), 1 (drought), -9999 (NA).
}
\examples{
set.seed(1)

nlon <- 10
nlat <- 10
ngrid <- nlon*nlat
ntime <- 12
dim = c(nlon, nlat, ntime)
threshold = 0.2
 
arr <- array(rnorm(nlon*nlat*ntime), dim = c(nlon, nlat, ntime)) > 0.2 # boolean arr
# mask = apply_3d(arr >= 0.2, FUN = rowSums2) > 0
# r  <- droughtIndicator( arr, mask, threshold = 0.2)
r_cluster <- cluster_SpatioTemporal(arr, ncell_connect = 1, ncell_overlap = 1, factor = 1000)
plot.cluster(r_cluster)
# r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$clusterId, r_cluster$shortCnoList)

ndim <- 10
ntime = 4
arr <- array(data = ifelse(sample(1:100, 20^2*20, T) > 50, T, F), c(ndim, ndim, ntime))
z1 <- cluster_SpatioTemporal(arr, ncell_connect = 5, ncell_overlap = 5, factor = 100)

z2 <- cluster_SpatioTemporal_julia(arr, method = 'tree', ncell_connect = 5, ncell_overlap = 5)
z3 <- cluster_SpatioTemporal_julia(arr, method = 'recursive', ncell_connect = 5, ncell_overlap = 5)
z4 <- cluster_SpatioTemporal_julia(arr, method = 'low', ncell_connect = 5, ncell_overlap = 5)

z5 <- cluster_SpatioTemporal_R(arr, version = 'julia', ncell_connect = 5, ncell_overlap = 5, factor = 100)
z6 <- cluster_SpatioTemporal_R(arr, version = 'fortran', ncell_connect = 5, ncell_overlap = 5, factor = 100)

# cluster_grids(z6)
all.equal(cluster_grids(z2), cluster_grids(z3))
all.equal(cluster_grids(z2), cluster_grids(z4))
all.equal(cluster_grids(z2), cluster_grids(z1))
all.equal(cluster_grids(z5), cluster_grids(z6))
all.equal(cluster_grids(z1), cluster_grids(z5))
}
\keyword{internal}
