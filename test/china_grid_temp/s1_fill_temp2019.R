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
date_end   <- ymd("20190630")
dates_all  <- seq(date_begin, date_end, by = "day")
dates_2019 <- seq(date_begin2, date_end, by = "day")

# 1. CMA files
files_fix <- dir("output/", "*.txt|*.TXT", full.names = TRUE)

indir <- "/mnt/e/Research/cmip5/DATA/ChinaData/temp_daily_0.5deg"
files <- dir(indir, "*.txt|*.TXT", full.names = TRUE)

files_final <- c(files, files_fix)
## check_miss

d_miss <- get_miss(files_final, dates_all) %>% 
    .[date <= ymd("20181231")]
# d_miss$date %>% unique() %>% sort() %>% zip_dates() %>% show_miss()

indir_3h <- "/mnt/n/DATA/3h metrology data/3h temperature"
fill_miss(d_miss, indir_3h)


## check about bad files -------------------------------------------------------
InitCluster(10)

means <- laply(files_final, function(file){
    mat <- read_asc(file)
    mean(as.numeric(mat), na.rm = TRUE)
}, .progress = "text", .parallel = TRUE)

I_bad <- means %>% {which(is.na(.) | . == 0)}
file.move(files[I_bad], "temp")

dates_bad <- dir("temp") %>% get_date() %>% unique() %>% sort() 
d_miss <- zip_dates(dates_bad)

# missing in 2019: 20190101, 20190117-20190118, 20190319-20190320, 20190414, 20190624
files_2019 <- filter_file(files_final, date_begin2, date_end) %>% split_vars()
fill_2019(files_2019, dates_2019)


# indir2 <- "/mnt/e/Research/cmip5/DATA/ChinaData/temp_daily_0.5deg2"
# files2 <- dir(indir2, "*.txt|*.TXT", full.names = TRUE)

# I_fix <- match(dir("temp"), basename(files2))

# means2 <- laply(files2[I_fix], function(file){
#     mat <- read_asc(file)
#     mean(as.numeric(mat), na.rm = TRUE)
# }, .progress = "text", .parallel = TRUE)

# vals <- means2
# I_good <- which(!(is.na(vals) | vals == 0))

# file.move(files2[I_good], indir)

# # files2 <- dir("output", "*.txt|*.TXT", full.names = TRUE)
# # files %<>% c(files2) %>% unique()

# # 2. ITPCAS
# # indir2 <- "/mnt/n/DATA/3h metrology data/3h temperature"
# # files_nc <- dir(indir2, "*.nc$", full.names = TRUE)
