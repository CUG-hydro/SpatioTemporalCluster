library(maptools)
library(sp2)
source("test/main_pkgs.R")

# list of SpatialPolygons into SpatialPolygonsDataFrame
merge_SpatialPolygons <- function(x) {
    joined = SpatialPolygons(lapply(x, function(x){x@polygons[[1]]}))
    p <- SpatialPolygonsDataFrame(Sr=joined, data=data.frame(year=as.numeric(names(x))),FALSE)
    p
}

# 少于5年的urban不考虑
df <- fread("urban_IdClusters.csv")
info = df[, .N, .(id, year)][, .N, .(id)][N >= 5]
lst_poly <- df[id %in% info$id] %>% {split(.[, 1:3], .$id)}

outdir = "cluster/OUTPUT/shp"
# d <- lst_poly$`1006669`
InitCluster(12)
lst2 <- foreach(d = lst_poly, name = names(lst_poly), i = icount()) %dopar% {
    runningId(i, 10)
    tryCatch({
        shps <- dlply(d, .(year), function(x) {
            suppressWarnings({
                coordinates(x) <- ~ x + y
                # s <- as_SpatialPixelsDataFrame(x)
                p <- as_SpatialPolygonsDataFrame(x)
                p <- unionSpatialPolygons(p, p$year)
                p
            })
        })
        
        p <- merge_SpatialPolygons(shps)
        
        outfile = sprintf("%s/%s.shp", outdir, name)
        write_shp(p, outfile)
    }, error = function(e) {
        message(sprintf('%s', e$message))
    })
}

files <- dir(outdir, "*.shp$") %>% gsub(".shp", "", .)



