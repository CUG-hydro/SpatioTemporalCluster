library(maptools)
library(sp2)
source("test/main_pkgs.R")
## check the spatial distribution of urban
b    <- brick("cluster/OUTPUT/urban_IdClusters.tif")
vals <- values(b)
r    <- b[[1]]
coor <- coordinates(r)

years = 1980:2015 %>% set_names(., .)
lst <- foreach(year = years, i = icount()) %do% {
    runningId(i)
    # vals <- values(r)
    vals_i = vals[, i]
    ind <- vals_i > 0
    d = cbind(coor[ind, ], id = vals_i[ind]) %>% as.data.table()        
}
fwrite(df, "urban_IdClusters.csv")

# x <- as(s, "SpatialPolygons")
# r$lon <- coor[, 1]
# r$lat <- coor[, 2]
# construct a data.table with the columns of  [lon, lat, id]

r[r <= 0] = NA
# arr <- as.array(r[[1]])
system.time(polys <- rasterToPolygons(r))

# system.time(x <- as_SpatialPolygonsDataFrame(r))
lst <- split(polys, polys$urban_IdClusters.1)

ids = unique(r)
# system.time({
#     r.to.poly <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(r), 
#                                              as_points = FALSE, merge = TRUE)
#     ) # requires the sf, sp, raster and stars packages
#     polys2 <- unionSpatialPolygons(r.to.poly, IDs = r.to.poly$urban_IdClusters.1)
# })

sf::as_Spatial(sf::st_as_sf(d, 
                            as_points = FALSE, merge = TRUE))
# sf::st_as_sf(d, as_points = FALSE, merge = TRUE)


system.time(x <- polygonize(r))
l <- lst$`1006669`
do.call(union, l)
dim = dim(arr)
grid <- array(1:prod(dim[1:2]), dim = dim[1:2])

ids <- unique(as.numeric(arr[,,1]))

r_year <- raster("E:/Research/cmip5/SpatioTemporal.cluster/cluster/urban_DevelopYear.tif")
r_year[r_year == 0] = NA
plot(r_year)


library(sp2)
poly2 <- rasterToPolygons(r_year)
sp <- as_SpatialPixelsDataFrame(r_year)

shp <- rgdal::readOGR(path.mnt("E:/WSL/r_library/ChinaHW/extdata/shp/bou1_4l_south_sml.shp"))
poly <- list("sp.polygons", shp)

{
    brks = c(-Inf, seq(1985, 2010, 1), Inf)
    p <- spplot(sp, sp.layout = poly, 
            at = brks, 
                aspect = 0.75, 
                xlim = c(73, 136), ylim = c(17.3, 53.8))
    write_fig(p, "urban_DevelopYear.pdf", 8)
}
