
duration <- function(x){
    # I <- which(x != 0)
    I <- which.notna(x)
    start <- I[1]
    end <- I[length(I)]
    duration <- end -  start + 1
    listk(start, end, duration)
}

#' @import matrixStats
HW_characteristics <- function(r_cluster, r_status, origin = "1961-01-01"){
    info <- r_cluster$shortCnoList %>% data.table(id = ., date = as.Date(floor(./100) - 1, origin))

    x <- r_status$DAreaEvol
    x[x == 0] <- NA

    area_mean <- colMeans2(x, na.rm = TRUE)# %>% plot()
    area_max  <- colMaxs(x, na.rm = TRUE)# %>% plot()
    area_sum  <- colSums2(x, na.rm = TRUE)# %>% plot()

    res <- alply(x, 2, duration)
    info_time <- purrr::transpose(res) %>% map(unlist) 
    info_time[1:2] %<>% map(~as.Date(., as.Date(origin) - 1))
    info_time %<>% as.data.table()

    # global_area_max
    idc <- (r_cluster$idCluster != -9999) * 1L
    area_daily <- idc * mGridArea
    
    dim <- dim(area_daily)
    dim(area_daily) <- c(prod(dim[1:2]), dim[3])
    area_daily2     <- colSums2(area_daily) 
    global_areaMX   <- area_daily2 %>% max()
    global_areaMX_date   <- area_daily2 %>% which.max() %>% add(as.Date(origin)-1)
    
    # REsult
    df <- cbind(info, area_mean, area_max, area_sum, info_time, 
        global_areaMX, global_areaMX_date)
    df
}

# HW_cluster <- function(arr, mGridArea, prob, nCellInter){
#     TRS  <- apply_3d(arr, 3, rowQuantiles, probs = prob) # 相当于 3 times/year

#     lst <- foreach(year = 1961:2019 %>% set_names(., .), i = icount()) %do% {
#         runningId(year)
        
#         I_year   <- which(years == year)
#         arr_year <- arr[,, I_year]

#         mat  <- array_3dTo2d(arr_year, I_grid = which(mask))

#         # TRS = 35
#         # 添加中国底图
#         ## bugs at here
#         # r <- eventIndicator( mat, mask, SMI_thld = TRS, masked = TRUE)
#         # r_cluster <- cluster_SpatioTemporal(r$SMIc, r$cellCoor, thCellClus = 16, nCellInter = nCellInter)
#         # r_status  <- ClusterStats(mat, mask, SMI_thld = TRS, r_cluster$idCluster, r_cluster$shortCnoList, 
#         #     mGridArea = mGridArea, masked = TRUE)
        
#         origin <- glue("{year}-01-01")

#         d <- HW_characteristics(r_cluster, r_status, origin = origin)
#         # p <- plot.cluster(r_cluster$idCluster, times = NULL, range = range, origin = origin, sp.layout = sp_arc_CH) # , sp.layout = sp_arc_CH
#         # write_fig(p, "a.pdf", 16, 16)
#         # region <- arr_year[,,yday("2010-06-23")] >= 35 #%>% image()
#         # image(region)
#     }
#     df <- melt_list(lst, "year")
#     df$year %<>% as.numeric()
#     df
# }

check_outlier <- function(x){
    x2 <- x[is.finite(x)]
    mean <- mean(x2)    
    sd   <- sd(x2)
    
    times <- 1.5
    upper <- mean + times*sd
    lower <- mean - times*sd
    I_bad <- which(x >= upper)
    
    # glue("lower = {lower}, upper = {upper}\n") %>% print()
    flag <- rep(NA, length(x))
    flag[I_bad] <- 1
    flag
}
