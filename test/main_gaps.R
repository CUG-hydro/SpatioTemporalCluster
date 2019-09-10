# source("test/main_gaps.R")
library(stars)

ilon <- match(seq(72+cellsize/2, 136, cellsize), seq(70+cellsize/2, 140, cellsize))
ilat <- match(seq(18+cellsize/2, 54, cellsize) , seq(15+cellsize/2, 55, cellsize))

rm_dup_file <- function(files){
    I <- grep("\\(|\\.\\d\\.txt", basename(files))
    files[I] %>% file.remove()
    invisible()
}

filter_file <- function(files, date_begin = NULL, date_end = NULL){
    dates <- str_extract(basename(files), "\\d{8}") %>% ymd()
    
    if (is.null(date_begin)) date_begin <- min(dates)
    if (is.null(date_end))   date_end   <- max(dates)
    
    I <- which(dates >= date_begin & dates <= date_end)
    files[I]
}

split_vars <- function(files){
    vars <- basename(files) %>% str_extract("(?<=-).*(?=-)")
    split(files, vars) %>% map(function(files){
        files[order(basename(files))]
    })
}

get_date <- function(files){
    str_extract(basename(files), "\\d{8}") %>% ymd()
}

get_miss <- function(files, dates_all){
    lst_dates <- files %>% split_vars() %>% map(get_date)
    lst_miss <- map(lst_dates, ~ setdiff(dates_all, .) %>% as.Date("1970-01-01"))
    d_miss <- map(lst_miss, ~ data.table(date = ., year = year(.), ym = format(., "%Y%m"))) %>% 
        melt_list("variable") %>% 
        .[order(ym), ] %>% 
        mutate(outfile = sprintf("output/SURF_CLI_CHN_TEM_DAY_GRID_0.5-%s-%s_fillgap.txt", variable, format(date, "%Y%m%d")))
    d_miss
}

show_miss <- function(info){
    # , var = info$variable
    temp <- foreach(str = info$str_miss) %do% {
        # fprintf(glue("{var} --------------\n\n"))
        str_new <- str %>% gsub(", ", "\n", .)
        cat(str_new)
        NULL
    }
    # browser()
    # cat(temp)
    invisible()
}

# fill missing val by other dataset
fill_miss <- function(d_miss, indir_3h) {
    yms  <- unique(d_miss$ym)
    temp <- foreach(YM = yms) %do% {
        dj <- d_miss[ym == YM]
        inds <- mday(dj$date)
        
        infile <- glue("{indir_3h}/temp_ITPCAS-CMFD_V0106_B-01_03hr_010deg_{YM}.nc")
        r <- hour3ToDaily(infile)
        
        tempI <- foreach(I = inds, var = dj$variable, outfile = dj$outfile, i = icount()) %do% {
            runningId(i)

            mat <- round(r[[var]][,,I] - 273.15, 2)
            val <- mat[ilon, ilat]
            write_asc(val, outfile)
            # x <- readGDAL(outfile)
            # plot(x)
        }
    }
}

# fill 2019 missing val by `na.approx`
fill_2019 <- function(files_2019, dates_2019) {  
  lst <- map(files_2019, function(files){
      ans <- llply(files, read_asc, .progress = "text")
      ans
  })
  
  ntime <- length(dates_2019)
  vars  <- c("MEAN", "MAX", "MIN") %>% set_names(., .)
  
  dim <- c(128, 72, 192)
  dim2 <- c(prod(dim[1:2]), dim[3])

  temp <- foreach(var = vars, k = icount()) %do% {
      runningId(k, prefix = var)
      
      # 1. prepare 3d input matrix
      x <- lst[[var]] %>% abind(along = 3)    
      I <- match(files_2019[[var]] %>% get_date(), dates_2019)
      I_miss   <- seq_along(dates_2019)[-I]
      outfiles <- sprintf("output/SURF_CLI_CHN_TEM_DAY_GRID_0.5-%s-%s_fillgap.txt", var, format(dates_2019[I_miss], "%Y%m%d"))
  
      res <- array(NA_real_, dim = c(dim(x)[1:2], ntime))
      res[,,I] <- x 
      dim(res) <- dim2
  
      # 2. fill gaps
      mask <- rowSums(is.na(res)) #%>% image()
      mask[mask >= mask[1]*0.66] = NA
      # image(mask)
      I_mask <- which(!is.na(as.numeric(mask)))
      mat <- res[I_mask, ]
      # na values all filled
      mat_fix <- aaply(mat, 1, na.approx, na.rm = FALSE, .progress = "text")
      
      # 3. return result
      ans <- array(NA_real_, dim = dim2)
      ans[I_mask, ] <- mat_fix
      dim(ans) <- dim
      
      temp <- foreach(outfile = outfiles, i = I_miss) %do% {
          runningId(i)
          val <- ans[,,i] %>% round(2)
          write_asc(val, outfile)
      }
  }
}

#' @importFrom stars read_stars
hour3ToDaily <- function(file) {
    x_avg <- read_stars(file, RasterIO = list(nXOff = 1, nYOff = 1, nXsize = 700, nYSize = 400, 
                                              nBufXSize = 140, nBufYSize = 80, resample = "average"))
    arr  <- structure(x_avg[[1]], dimensions = NULL, class = NULL) %>% {.[, ncol(.):1, ] }
    date <- st_dimensions(x_avg)$time %>% seq() %>% as.Date()
    
    Tmean <- Ipaper::apply_3d(arr, 3, rowMeans2, by = date)
    Tmax  <- Ipaper::apply_3d(arr, 3, rowMaxs, by = date)
    Tmin  <- Ipaper::apply_3d(arr, 3, rowMins, by = date)
    
    Tmax[is.infinite(Tmax)] <- NA
    Tmin[is.infinite(Tmin)] <- NA
    
    listk(MEAN = Tmean, MAX = Tmax, MIN = Tmin)    
}


write_asc <- function(val, outfile){
    dim <- dim(val)
    head <- sprintf("NCOLS      %d
NROWS       %d
XLLCORNER    72.000000000000    
YLLCORNER    18.000000000000    
CELLSIZE    0.50000000000000    
NODATA_VALUE   -99.00000", dim[1], dim[2])
    
    val[is.na(val)] <- -99
    val %<>% .[, ncol(.):1] %>% t()

    writeLines(head, outfile)
    write.table(val, outfile, sep = "\t", append = TRUE, row.names = FALSE, col.names = FALSE)
}

read_asc <- function(file, na = -99){
    tryCatch({
        mat <- fread(file, skip=6) %>% as.matrix() %>% t() %>% flipud() 
        mat[mat == na] = NA
        mat    
    }, error = function(e){
        message(sprintf("[e]: %s, %s", basename(file), e$message))
        NA
    })
}

