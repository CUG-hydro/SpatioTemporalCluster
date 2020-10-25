library(plyr)
library(glue)
library(foreach)
library(iterators)
library(lubridate)

# dirs = list.dirs("N:/DATA/China/2400climate data")
dir_root = "N:/DATA/China/2400climate data"
varnames = c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")

# [826] 69473, 59034 2429 10839   1851 2019 10 235000002     64      5    106      5 9 9 9 9 9
#   win_avg: 5000002 -> 20
# [827] 17129: 53955 3531 11028   4616 2019 11 292500012     33      3     64      3 9 9 9 9 9
#   win_avg: 500012 -> 12
# [829]  3792: 51238 4454  8204   5322 2020  1  72500001     21     13     32     13 9 9 9 9 9
#   win_avg: 2500001 -> 10
foreach(varname = varnames[8], i = icount()) %do% {
    outfile = glue("{dir_root}/SURF_CLI_CHN_MUL_DAY_{varname} (195101-202003).csv")
    # if (file.exists(outfile)) return()

    indir = glue("{dir_root}/{varname}")
    files = dir(indir, "*.TXT", full.names = TRUE)
    
    lst <- foreach(file = files[1:length(files)], i = icount()) %do% {
        runningId(i, 10)
        tryCatch({
            fread(file)
        }, warning = function(e) {
            message(sprintf('[i] %s: %s', i, basename(file), e$message))
        })
    }
    # lst <- llply(files, fread, .progress = "text")
    df = do.call(rbind, lst)
    invisible()
    fwrite(df, outfile)
}

library(purrr)
files = dir(dir_root, "*.csv", full.names = TRUE) %>% set_names(varnames) %>% .[7:8]
lst <- map(files, ~fread(.x, select = 1:4) %>% set_colnames(c("site", "lat", "lon", "alt")))
# dir(dir_root, "*.csv")

# d = fread(files[1])
## 获取台站变迁记录
st = df[, 1:7] %>% set_colnames(c("site", "lat", "lon", "alt", "year", "month", "day"))
st_moveInfo = dlply(st, .(site), function(d) {
    d <- data.table(d)
    d$tag = d[,1:4] %>% {!duplicated(.)} %>% cumsum()
    d$date = d[, make_date(year, month, day)]
    date_begin = min(d$date)
    date_end   = max(d$date)

    d[, .(period_date_begin = min(date), period_date_end = max(date), 
        date_begin, date_end), 
        .(site, tag, lon, lat, alt)]
}, .progress = "text")

st_moveInfo %<>% do.call(rbind, .)
st_moveInfo[, moveTimes := max(tag), .(site)]
st_moveInfo %<>% reorder_name(c("site", "moveTimes", "tag"))
st_moveInfo[, alt := get_alt(alt)]
fwrite(st_moveInfo, "mete2481_站点变迁记录-(195101-202003).csv")
