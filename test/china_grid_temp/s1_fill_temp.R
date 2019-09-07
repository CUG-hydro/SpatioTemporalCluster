source('test/main_pkgs.R', encoding = "utf-8")
source("test/main_gaps.R")

# FILL MISSING DATA OF SURF_CLI_CHN_TEM_DAY_GRID_0.5
# 
# @references
# 1. http://data.cma.cn/data/detail/dataCode/SURF_CLI_CHN_TEM_DAY_GRID_0.5.html

# 1. CMA files
indir <- "/mnt/e/Research/cmip5/DATA/ChinaData/temp_daily_0.5deg"
files <- dir(indir, "*.txt|*.TXT", full.names = TRUE)

files2 <- dir("output", "*.txt|*.TXT", full.names = TRUE)
files %<>% c(files2) %>% unique()

# 2. ITPCAS
indir2 <- "/mnt/n/DATA/3h metrology data/3h temperature"
files_nc <- dir(indir2, "*.nc$", full.names = TRUE)

dates_all <- seq(ymd("19610101"), ymd("20190831"), by = "day")

range_nc <- c(70, 140, 15, 55)
range    <- c(72, 136, 18, 54)

ilon <- match(seq(72+cellsize/2, 136, cellsize), seq(70+cellsize/2, 140, cellsize))
ilat <- match(seq(18+cellsize/2, 54, cellsize) , seq(15+cellsize/2, 55, cellsize))

# need to fill gaps in 2019

# ------------------------------------------------------------------------------

# files2 <- dir("/mnt/e/github/Research/cmip5/DATA/ChinaData/temp_daily_0.5deg2/", "*.txt|*.TXT", full.names = TRUE)
# info <- match2(basename(files2), basename(files))
# files2[-info$I_x] %>% {file.rename(.,  paste0(indir, "/", basename(.)))}
# info <- split(files, vars) %>% map(get_dates) %>% map(zip_dates) %>% melt_list("variable")

vars <- basename(files) %>% str_extract("(?<=-).*(?=-)")
info_miss <- split(files, vars) %>% map(get_dates) %>% melt_list("variable") %>% 
    .[order(ym), ] %>% 
    mutate(outfile = sprintf("output/SURF_CLI_CHN_TEM_DAY_GRID_0.5-%s-%s_fillgap.txt", variable, format(date, "%Y%m%d")))

#%>% map(zip_dates) %>% melt_list("variable")
# I_fix <- {!file.exists(info_miss$outfile)} %>% which()
# info_miss %<>% .[I_fix, ]

yms  <- unique(info_miss$ym)
temp <- foreach(YM = yms) %do% {
    dj <- info_miss[ym == YM]
    inds <- mday(dj$date)
    
    infile <- glue("{indir2}/temp_ITPCAS-CMFD_V0106_B-01_03hr_010deg_{YM}.nc")
    r <- hour3ToDaily(infile)
    
    tempI <- foreach(I = inds, outfile = dj$outfile, i = icount()) %do% {
        runningId(i)

        mat <- round(r[[var]][,,I] - 273.15, 2)
        val <- mat[ilon, ilat]
        write_asc(val, outfile)
        # x <- readGDAL(outfile)
        # plot(x)
    }
}

# image(mat[ilon, ilat])

# grid <- get_grid(range, 0.5)
# dim <- grid@grid@cells.dim

# grid@data <- data.frame(x = as.numeric(mat[ilon, ilat]))
# writeGDAL(grid, "a.asc", drivername = "AAIGrid", mvFlag = -99.0, type = "Float32")

# # # bilinear lead to wired result
# # x_bil <- read_stars(file, RasterIO = list(nXOff = 1, nYOff = 1, nXsize = 700, nYSize = 400, 
# #                                     nBufXSize = 140, nBufYSize = 80, resample = "nearest_neighbour"))
# # plot(x_avg[,,,1])
# outfile2 <- "/mnt/e/Research/cmip5/DATA/ChinaData/temp_daily_0.5deg2/SURF_CLI_CHN_TEM_DAY_GRID_0.5-MAX-20100103.txt"
# a <- readGDAL(outfile)
# b <- readGDAL(outfile2)
# plot(a)
# plot(b)
#     # .[seq(1, length(.), 8)] %>% 
#     # c(last(.) + ddays(1))#%>% format("%Y%m%d") %>% ymd() #  %>% unique()
# # x <- aggregate(x_avg, date, mean, na.rm = TRUE)
# # image(arr[,end:1,1])
# # check missing info
# # file <- files_nc[1]

# ## read into nc
# I   <- matrix(1:prod(dim), dim[1], dim[2])
# I_fix <- I[, seq(dim[2], 1)]

# # 
# x <- readGDAL(file) # asc file
# grid@data <- x@data[I_fix, , drop = FALSE]

# x$band1 <- t(as.matrix(y)) %>% as.numeric()
# plot(x)

# # microbenchmark::microbenchmark(
# #     fread(file, skip = 6, sep = "\t"), 
# #     read.table(file, skip = 6, sep = "\t"), 
# #     x <- readGDAL(file),
# #     times =10
# # )
