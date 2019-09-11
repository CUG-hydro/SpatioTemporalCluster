# source('test/00_basement.R')
source('test/main_pkgs.R')
library(maptools)
# load("data/basement_5deg.rda")

# polyline
sp_arc_world <- read_polyline("G:/ArcGIS/continent.shp") # sp_arc_world

file_arc     <- "D:/Documents/ArcGIS/china_arc_sml_01deg.shp"
# file_arc     <- "../Rcmip5/inst/extdata/shp/bou1_4l_sml.shp"
sp_arc_CH    <- read_polyline(file_arc)
