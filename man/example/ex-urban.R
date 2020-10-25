# devtools::load_all()
library(heatwave)
library(raster)

set.seed(1)

r1 <- raster("F:/urban/CLUDA_2015_88_1km_prop.tif")
r2 <- raster("F:/urban/CLUDA_2015_89_1km_prop.tif")
r = r1 + r2

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
