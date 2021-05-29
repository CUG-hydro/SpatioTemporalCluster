# devtools::load_all()
library(heatwave)
library(raster)
library(glue)
library(foreach)
library(iterators)
library(abind)

lst_urban = foreach(year = 1980:2015, i = icount()) %do% {
    runningId(i)
    r1 <- raster(glue("F:/SciData/LULC/LULC_China_landsat_1km/{year}/CLUDA_{year}_88_1km_prop.tif"))
    r2 <- raster(glue("F:/SciData/LULC/LULC_China_landsat_1km/{year}/CLUDA_{year}_89_1km_prop.tif"))
    r <- r1 + r2
    arr <- as.array(r)
}
arr_urban_ratio <- abind(lst_urban, along = 3)
arr_urban <- arr_urban >= 0.5

{
    
    # system.time(res <- julia_call("cluster.spatial_cluster", arr >= 0.2))
    # names(res) <- c("nC", "cno", "IdClusters")
    system.time({
        IdClusters = julia_call("cluster.cluster_spatiotemporal", arr_urban[,,], 
                                time_factor = as.integer(1e6), # max clusters for each time
                                minOverlapCells = as.integer(25),
                                minCells = as.integer(25))    
    })
}

# idC2 = julia_call("time_connect", idC, factor = 1e3)
mat = as.matrix(r)
dim = dim(mat)
nrow <- dim[1]
ncol <- dim[2]
ngrid <- nrow*ncol
ntime <- 1

mat2 = matrix(mat, ngrid, 1)
# mat <- matrix(rnorm(nrow*ncol*ntime), ngrid, ntime)
# mask <- matrix(TRUE, nrow, ncol)
SMI_thld <- 0.5
mask <- mat > SMI_thld

r  <- droughtIndicator( mat2, mask, SMI_thld)
# write_fig({
#     image(r$SMIc[,,1])    
# }, "a.tif")
# r_ans = r
# values(r_ans) <- r$SMIc
# writeRaster(r_ans, "urban_No.tif")
# values()
system.time({
    r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor)    
})
# r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList)


## 2. visualization ------------------------------------------------------------
# devtools::install_github("kongdd/Ipaper")
# p <- plot.cluster(r2)
# ratio = 0.8
# Ipaper::write_fig(p, "a.pdf", 12*ratio, 8*ratio)
