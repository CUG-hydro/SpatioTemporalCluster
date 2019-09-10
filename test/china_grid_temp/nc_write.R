source('test/main_pkgs.R', encoding = "utf-8")
source("test/main_gaps.R")

# Try to fill gaps in 2019

missInfo <- function(dates, date_begin = NULL, date_end = NULL){
    if (is.null(date_begin)) date_begin = dates[1]
    if (is.null(date_end))  date_end = dates[length(dates)]

    dates_left <- dates[ dates >= date_begin & dates <= date_end]
    dates_all  <- seq(date_begin, date_end)

    info <- zip_dates(dates_left)
    info
    # d <- data.table(date = dates_left) %>% mutate(ym = format(date, "%Y%m")) 
    # d
}

# dates_all <- seq(ymd("19610101"), ymd("20181231"), by = "day")
date_begin2<- ymd("20181221")
date_begin <- ymd("19610101")
date_end   <- ymd("20190831")
dates_all  <- seq(date_begin, date_end, by = "day")
dates_2019 <- seq(date_begin2, date_end, by = "day")

# 1. CMA files
files_fix <- dir("output/", "*.txt|*.TXT", full.names = TRUE)

indir <- "/mnt/e/Research/cmip5/DATA/ChinaData/temp_daily_0.5deg"
files <- dir(indir, "*.txt|*.TXT", full.names = TRUE)

files_final <- c(files, files_fix)
## check_miss

lst_files <- split_vars(files_final) %>% set_names(c("Tmax", "Tmean", "Tmin"))

d_miss <- get_miss(files_final, dates_all)

InitCluster(10)
lst <- foreach(files = lst_files, var = names(lst_files)) %do% {
    # ans <- mclapply(files, read_asc) # mclapply 
    ans <- llply(files, read_asc, .parallel = TRUE)
    # map_int(ans, nrow) %>% {files[which(. == 2)]} %>% basename()
    arr <- abind(ans, along = 3)

    dates <- get_date(files)
    
    file <- glue("{outdir}/CHN_{var}_DAY_GRID_0.5_196101-201908.nc")
    # short: -32768~32767
    lst_nc <- list(arr[,,1:21427]) %>% set_names(var)
    ncwrite_cmip5(lst_nc, file, "degC", range = range, 
                  prec = "short", scale = 0.01, 
                  # offset = 10, 
                  dates = dates)
    # fid <- nc_open(file)
    # ncwrite_cmip5(list(Tmin = arr), "Tmin.nc", "degC", range = range)
}

files_nc <- dir("e:/SciData/China_daily_temp/", full.names = TRUE) %>% 
    set_names(c("Tmax", "Tmean", "Tmin"))
# files_nc <- dir("/mnt/e/SciData/China_daily_temp/", full.names = TRUE)
file <- files_nc[1]
lst  <- map(files_nc, read_stars)

# {
    # devtools::load_all("../Rcmip5")
    # x <- ncvar_get(fid, "Tmin")
    # summary(as.numeric(x))
    # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    # -42.300 -22.600 -17.390 -14.924  -7.025  18.100   10054  
    # nc_close(fid)
# }
