library(raster)
library(rgdal)
library(sp2)

cellsize <- 0.5
range    <- c(72, 136, 18, 54)
grid     <- get_grid(range, cellsize)

file <- "N:/DATA/Population/China/pop2015_wgs84.tif"
x <- readGDAL(file)

plot(x)

# need to sum, not simply mean or nearest
y <- resample(raster(x), raster(grid))
