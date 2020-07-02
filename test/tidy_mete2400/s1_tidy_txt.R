library(plyr)
library(glue)
library(foreach)
library(iterators)
library(lubridate)

# dirs = list.dirs("N:/DATA/China/2400climate data")
dir_root = "N:/DATA/China/2400climate data"
varnames = c("EVP", "GST", "PRE", "PRS", "RHU", "SSD", "TEM", "WIN")

foreach(varname = varnames, i = icount(1)) %do% {
    outfile = glue("{dir_root}/SURF_CLI_CHN_MUL_DAY_{varname} (1951-2018).csv")
    # if (file.exists(outfile)) return()

    indir = glue("{dir_root}/{varname}")
    files = dir(indir, "*.TXT", full.names = TRUE)
    
    lst <- llply(files, fread, .progress = "text")
    df = do.call(rbind, lst)
    invisible()
    # fwrite(df, outfile)
}

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
fwrite(st_moveInfo, "mete2481_站点变迁记录-(1951-2018).csv")
