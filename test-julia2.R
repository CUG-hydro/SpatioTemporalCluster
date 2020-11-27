# rm(list = ls())
library(SpatioTemporal.cluster)
library(lattice)
for (i in 1:10000){
    runningId(i, 100)
    i = 146
    set.seed(i)
    #i = 146 is the bug
    ndim <- 20
    arr <- array(data = ifelse(sample(1:100, ndim^2*20, T) > 50, T, F), c(ndim, ndim, 20))[,,8:10]
    # x <- ncdim_def(name = 'x', units = '', vals = 1:100)
    # y <- ncdim_def(name = 'y', units = '', vals = 1:100)
    # z <- ncdim_def(name = 'z', units = '', vals = 1:20)
    # var3d <- ncvar_def(name = 'arr', units = '', dim = list(x, y, z))
    # nc <- nc_create(filename = 'C:\\Users\\Lenovo\\Desktop\\test\\example.nc', vars = var3d)
    # ncvar_put(nc, varid = var3d, arr)
    # nc_close(nc)
    
    
    # rbenchmark::benchmark(
    #     z1 <- cluster_SpatioTemporal_julia(arr = arr, method = 'tree', ncell_connect = 5, ncell_overlap = 5),
    #     z2 <- cluster_SpatioTemporal_julia(arr = arr, method = 'recursive', ncell_connect = 5, ncell_overlap = 5),
    #     z3 <- cluster_SpatioTemporal_julia(arr = arr, method = 'low', ncell_connect = 5, ncell_overlap = 5),
    #     replications = 10
    # )
    factor = 100
    z1 <- cluster_SpatioTemporal(arr = arr, ncell_connect = 5, ncell_overlap = 5, factor = factor)
    # z2 <- cluster_SpatioTemporal_julia(arr = arr, method = 'tree', ncell_connect = 1, ncell_overlap = 1, 
    #                                    factor = factor, verbose = TRUE)
    
    # z3 <- cluster_SpatioTemporal_julia(arr = arr, method = 'recursive', ncell_connect = 5, ncell_overlap = 5)
    # z4 <- cluster_SpatioTemporal_julia(arr = arr, method = 'low', ncell_connect = 5, ncell_overlap = 5)
    z2 <- cluster_SpatioTemporal_R(arr = arr, version = 'julia', ncell_connect = 5, ncell_overlap = 5, 
                                   verbose = F, factor = factor)
    # z6 <- cluster_SpatioTemporal_R(arr = arr, version = 'fortran', ncell_connect = 5, ncell_overlap = 5, verbose = T, factor = 100)
    if (length(cluster_grids(z1)) > length(cluster_grids(z2))){
        print("bug")
        break
    }
    # print(all.equal(cluster_grids(z1), cluster_grids(z2), cluster_grids(z3),
    # cluster_grids(z4), cluster_grids(z5), cluster_grids(z6)))
}

write_fig(plot.cluster(z1), "f90.pdf", 15, 5)
write_fig(plot.cluster(z2), "jl_5.pdf", 15, 5)

cluster_grids(z1)
cluster_grids(z2)

cluster_grids(z1) %>% length()
cluster_grids(z2) %>% length()

table(z1) %>% sort(decreasing = TRUE)
table(z2) %>% sort(decreasing = TRUE) 


cluster1 <- table(z1) %>% sort()
names1 <- names(cluster1[c(which.max(cluster1))]) %>% as.numeric()

cluster2 <- table(z2) %>% sort()
names2 <- names(cluster2[c(which.max(cluster2), which(cluster2 == 302))]) %>% as.numeric()

z1[!c(z1 %in% names1)] <- NA
z2[!c(z2 %in% names2)] <- NA
levelplot(z1, as.table = T)
levelplot(z2, as.table = T)

dat <- data.frame(t9 = c(z2[,,9]), t10 = c(z2[,,10]))

dat[which(dat$t9 == names2[2] & dat$t10 == names2[1]), ]

SpatioTemporal.cluster:::plot.cluster(z6, 9:10)

loc11 <- mapply(ind = 1:19, jnd = 2:20, function(ind, jnd){
    which(z2[,,ind] == names2[1] & z2[,,jnd] == names2[2]) %>% length
})

loc22 <- mapply(ind = 1:19, jnd = 2:20, function(ind, jnd){
    which(z2[,,ind] == names2[2] & z2[,,jnd] == names2[1]) %>% length
})


# ndim <- 50
# arr <- array(data = ifelse(sample(1:100, ndim^2*20, T) > 50, T, F), c(ndim, ndim, 20))
# x <- ncdim_def(name = 'x', units = '', vals = 1:100)
# y <- ncdim_def(name = 'y', units = '', vals = 1:100)
# z <- ncdim_def(name = 'z', units = '', vals = 1:20)
# var3d <- ncvar_def(name = 'arr', units = '', dim = list(x, y, z))
# nc <- nc_create(filename = 'C:\\Users\\Lenovo\\Desktop\\test\\example.nc', vars = var3d)
# ncvar_put(nc, varid = var3d, arr)
# nc_close(nc)


# rbenchmark::benchmark(
#     z1 <- cluster_SpatioTemporal_julia(arr = arr, method = 'tree', ncell_connect = 5, ncell_overlap = 5),
#     z2 <- cluster_SpatioTemporal_julia(arr = arr, method = 'recursive', ncell_connect = 5, ncell_overlap = 5),
#     z3 <- cluster_SpatioTemporal_julia(arr = arr, method = 'low', ncell_connect = 5, ncell_overlap = 5),
#     replications = 10
# )


# z1 <- cluster_SpatioTemporal(arr = arr, ncell_connect = 5, ncell_overlap = 5)
# z2 <- cluster_SpatioTemporal_julia(arr = arr, method = 'tree', ncell_connect = 5, ncell_overlap = 5)
# z3 <- cluster_SpatioTemporal_julia(arr = arr, method = 'recursive', ncell_connect = 5, ncell_overlap = 5)
# z4 <- cluster_SpatioTemporal_julia(arr = arr, method = 'low', ncell_connect = 5, ncell_overlap = 5)
# z5 <- cluster_SpatioTemporal_R(arr = arr, version = 'julia', ncell_connect = 5, ncell_overlap = 5)
# z6 <- cluster_SpatioTemporal_R(arr = arr, version = 'fortran', ncell_connect = 5, ncell_overlap = 5)
# 
# all.equal(cluster_grids(z1), cluster_grids(z2), cluster_grids(z3),
#           cluster_grids(z4), cluster_grids(z5), cluster_grids(z6))
# 
# z1[!(z1 %in% c(10002, 180008, 40002))] <- NA
# z2[!(z2 %in% c(40004, 180020))] <- NA
# 
# levelplot(z1, as.table = T)
# levelplot(z2, as.table = T)
# 
# levelplot(z1, as.table = T)
# levelplot(z2, as.table = T)
# 
# cluster1 <- table(z1) %>% sort()
# names1 <- names(cluster1[c(which.max(cluster1))]) %>% as.numeric()
# 
# cluster2 <- table(z2) %>% sort()
# names2 <- names(cluster2[c(which.max(cluster2), cluster2 == 302)]) %>% as.numeric()
# 
# z1[!c(z1 %in% names1)] <- NA
# z2[!c(z2 %in% names2)] <- NA
