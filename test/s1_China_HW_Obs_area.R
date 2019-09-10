source('test/main_pkgs.R', encoding = "utf-8")

grid <- get_grid(range, cellsize = 0.5)
mGridArea <- raster(grid) %>% area() %>% {.@data@values}
# mGridArea

duration <- function(x){
    # I <- which(x != 0)
    I <- which.notna(x)
    start <- I[1]
    end <- I[length(I)]
    duration <- end -  start + 1
    listk(start, end, duration)
}

HW_characteristics <- function(r_cluster, r_status, origin = "1961-01-01"){
    info <- r_cluster$shortCnoList %>% data.table(id = ., date = as.Date(floor(./100) - 1, origin))

    x <- r_status$DAreaEvol
    x[x == 0] <- NA

    area_avg <- colMeans2(x, na.rm = TRUE)# %>% plot()
    area_sum <- colSums2(x, na.rm = TRUE)# %>% plot()

    res <- alply(x, 2, duration)
    info_time <- purrr::transpose(res) %>% map(unlist) 
    info_time[1:2] %<>% map(~as.Date(., as.Date(origin) - 1))
    info_time %<>% as.data.table()

    df <- cbind(info, area_avg, area_sum, info_time)
    df
}

dates <- seq.Date(ymd('1961-01-01'), ymd('2019-08-31'), by = "day")
years <- year(dates)

# ------------------------------------------------------------------------------
files_nc <- dir("e:/SciData/China_daily_temp/", full.names = TRUE) %>% 
    set_names(c("Tmax", "Tmean", "Tmin"))
# files_nc <- dir("/mnt/e/SciData/China_daily_temp/", full.names = TRUE)

# file <- files_nc[1]
# lst  <- map(files_nc[1], read_stars)


## 1. 测试干旱面积 -------------------------------------------------------------
file <- files_nc[1]
r <- ncread_cmip5(file, convertTo2d = FALSE)

arr  <- r$data
mask <- !is.na(arr[,,1])
I_grid <- which.notna(mask)

mat  <- array_3dTo2d(arr, I_grid = which(mask))

## 采取其中一年的数据做测试 


# 1. get threshold -------------------------------------------------------------
TRS  <- apply_3d(arr, 3, rowQuantiles, probs = 0.99) # 相当于 3 times/year
# TRS2 <- apply_3d(arr_year, 3, rowQuantiles, probs = 0.99) # 相当于 3 times/year

lst <- foreach(year = 1961:2019 %>% set_names(., .), i = icount()) %do% {
    runningId(year)
    
    I_year   <- which(years == year)
    arr_year <- arr[,, I_year]

    mat  <- array_3dTo2d(arr_year, I_grid = which(mask))

    # 添加中国底图
    r <- droughtIndicator( mat, mask, SMI_thld = TRS, masked = TRUE)
    r_cluster <- ClusterEvolution(r$SMIc, r$cellCoor, thCellClus = 16, nCellInter = 8)
    r_status  <- ClusterStats(mat, mask, SMI_thld, r_cluster$idCluster, r_cluster$shortCnoList, 
        mGridArea = mGridArea, masked = TRUE)
    
    origin <- glue("{year}-01-01")
    d <- HW_characteristics(r_cluster, r_status, origin = origin)
}

df <- melt_list(lst, "year")
df$year %<>% as.integer()

{
    d <- df[order(-duration)][duration > 10]
    p <- ggplot(d, aes(duration, area_avg, color = year)) + 
        geom_point() + 
        geom_text_repel(data = d[duration > 20 | area_avg > 300], aes(label = year))
    p
    # ggplotly(p)
}

d <- df[, .(area = sum(area_sum)), .(year)][, smooth := movmean(area, halfwin = 2)]
d
    ggplot(d, aes(year, area)) + geom_point() + 
    geom_line(aes(year, smooth), size = 0.4) + 
    geom_text_repel(data = d[area > 2.5e4], aes(label = year))

df %>% {hist(.$area_avg, breaks = 30)}
    
p <- plot.cluster(r_cluster$idCluster, times = NULL, range = range, sp.layout = sp_arc_CH)
write_fig(p, "a.pdf", 16, 16)


## 2. HW characteristics -------------------------------------------------------

df[order(-area_sum), ][1:20, ]
df[order(-area_avg), ][1:20, ]


## 2. 干旱指标
# .nchunk <- 8
# InitCluster(.nchunk)

# res <- foreach(file = files_nc, i = icount(1)) %do% {
#     runningId(i)  
#     r <- ncread_cmip5(file)
#     # 1. get threshold
#     mat <- r$data
#     if (i == 1){
#         I   <- which.notna(mat[,1])    
#     }
#     # quantile()
#     # temp <- HW_index(mat[I, ], TRS = NULL, probs, date,
#     #                 period.ref = c(1961, 1990), simplify = FALSE, 
#     #                 .parallel = 2,
#     #                 .nchunk = .nchunk)
# }
# saveRDS(res, file = "China_HW_Obs_196101-201908")

# nrow = 10000
# ncol = 365

# mat <- matrix(rnorm(nrow*ncol), nrow, ncol)
# probs = c(0.9, 0.95, 0.975, 0.99, 0.995, 0.9975, 0.999, 0.9995, 0.99975, 0.9999)

# system.time(y2 <- apply(mat, 1, quantile, probs)) # 
# system.time(y <- rowQuantiles(mat, probs = probs))

# microbenchmark::microbenchmark(
#     res = rowQuantiles(mat, probs = probs), 
#     y2 <- apply(mat, 1, quantile, probs),
#     times = 10)
