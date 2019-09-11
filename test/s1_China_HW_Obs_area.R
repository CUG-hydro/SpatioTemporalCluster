source('test/main_pkgs.R', encoding = "utf-8")

grid <- get_grid(range, cellsize = 0.5)
mGridArea <- raster(grid) %>% area() %>% {.@data@values}
# mGridArea

dates <- seq.Date(ymd('1961-01-01'), ymd('2019-08-31'), by = "day")
years <- year(dates)

# ------------------------------------------------------------------------------

indir <- "e:/SciData/China_daily_temp/"
files_nc <- dir2(indir, full.names = TRUE) %>% set_names(c("Tmax", "Tmean", "Tmin"))
# files_nc <- dir(indir, full.names = TRUE)

# file <- files_nc[1]
# lst  <- map(files_nc[1], read_stars)

## 1. 测试干旱面积 -------------------------------------------------------------
file <- files_nc[1]
r <- ncread_cmip5(file, convertTo2d = FALSE)

arr    <- r$data
mask   <- !is.na(arr[,,1])
I_grid <- which.notna(mask)

# mat  <- array_3dTo2d(arr, I_grid = which(mask))

# Tmax <- colMeans2(mat, na.rm = TRUE) #%>% plot()
# data.table(temp = Tmax, year = years)[, mean(temp), .(year)] %>% plot(V1~year, .)

## 采取其中一年的数据做测试 

# 1. get threshold -------------------------------------------------------------
# TRS2 <- apply_3d(arr_year, 3, rowQuantiles, probs = 0.99) # 相当于 3 times/year

## 1. 敏感性分析
nCellInters = 4^c(1.5, 2, 2.5, 3, 3.5) %>% set_names(., .)
probs = c(0.95, 0.99, 0.995) %>% set_names(., .)

InitCluster(8)

d_param <- expand.grid(prob = probs, nCellInter = nCellInters)

lst <- foreach(prob = d_param$prob, nCellInter = d_param$nCellInter) %dopar% {
    df <- HW_cluster(arr, mGridArea, prob, nCellInter)
}

dim(lst)      <- c(length(probs), length(nCellInters))
dimnames(lst) <- list(probs, nCellInters)

df <- map(seq_along(nCellInters) %>% set_names(nCellInters), 
    ~lst[, .x] %>% set_names(probs) %>% melt_list("prob")) %>% melt_list("nCellInter")
file_sensitive <- "cluster_sensitity.rda"

df$nCellInter %<>% factor(nCellInters, sprintf("nCellInter = %s", nCellInters))

save(df, lst, file = file_sensitive)
# ------------------------------------------------------------------------------

check_temp_dist = FALSE
if (check_temp_dist) {
    # check temp spatial dist
    brks   <- c(-Inf, seq(15, 30, 3), 33, seq(35, 40), Inf)
    ncolor <- length(brks) - 1
    cols   <- colorRampPalette(.colors$Tavg)(ncolor*4)
    
    nrow   <- nrow(arr_year)
    ncol   <- ncol(arr_year)
    
    dates  <- seq(make_date(year, 1, 1), (make_date(year+1,1,1)-1), "day")
    I_sel  <- c("2010-06-01", "2010-09-30") %>% ymd() %>% yday() %>% {seq(.[1], .[2])}
    
    yaxis  <- list(at = pretty(1:ncol))
    xaxis  <- list(at = pretty(1:nrow))
    
    p_temp <- levelplot(arr_year[,,I_sel], col.regions = cols, 
                        at = brks, 
                        strip=strip.custom(factor.levels=format(dates[I_sel])), 
                        scale = list(x = xaxis, y = yaxis),
                        as.table = TRUE,
                        xlab = NULL, ylab = NULL)
    write_fig(p_temp, "2010 temp.pdf", 16, 16)
}


fig.no <- 2
prefix <- "[TRS=quantile]"

# 1.1 duration
{
    # df <- melt_list(lst, "year")
    d_dur <- df[duration >= 3, .(year, duration, nCellInter, prob, 
        grp = factor((year >= 1991)*1, labels = c("before 1990", "after 1991")))]

    ylab = expression("Average affect area of each HW event (10"^3 * " " * km^2 * ")")
    p1 <- ggplot(d_dur, aes(duration, color = grp, fill = grp)) + geom_histogram() + 
        labs(x = "Duration (days)", y = ylab) 
        
    # ALL HW events
    # d  <- df[order(-duration)][duration > 10]
    # p1 <- ggplot(d, aes(duration, area_mean, color = year)) + 
    #     geom_point()
    #     geom_text_repel(data = d[duration > 20 | area_mean > 800], aes(label = year)) + 
       
    outfile <- glue("Figure{fig.no}.1 {prefix} HW duration frequency.svg")
    write_fig(p1, outfile, 10, 4.5)
    # ggplotly(p)
}

# d <- df[, .(area = sum(area_sum)), .(year)][, smooth := ]
# ggplot(d, aes(year, area)) + geom_point() + 
#     geom_line(aes(year, smooth), size = 0.4) + 
#     geom_text_repel(data = d[area > 5e7], aes(label = year)) + 
#     labs(y = ylab)
# 
# df %>% {hist(.$area_mean, breaks = 30)}
#     

# ## 2. HW characteristics -------------------------------------------------------
# 
# df[order(-area_sum), ][1:20, ]
# df[order(-area_mean), ][1:20, ]
# 

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
