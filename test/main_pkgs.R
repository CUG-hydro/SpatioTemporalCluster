# source('test/main_pkgs.R', encoding = "utf-8")
suppressMessages({
    library(ncdf4)
    library(magrittr)
    library(dplyr)
    library(plyr)
    
    library(data.table)
    library(abind)
    library(lubridate)
    library(readxl)
    
    # library(pryr)
    library(bigmemory)
    library(biganalytics)
    library(doParallel)
    
    library(sp)
    library(lattice)
    library(latticeExtra)
    library(grid)
    library(gridExtra)
    library(scales)
    library(plotrix)
    library(ggrepel)
    
    library(tidyverse)
    library(pracma)
    library(matrixStats)
    library(rlang)
    library(glue)
    # library(drake)
    # library(RcppMovStat)
    
    library(Ipaper)
    library(sp2)
    library(phenofit)
    library(Rcmip5)
    library(missInfo)
    library(ECOF)

    library(purrr)
    library(stars)
    # devtools::install_github("peleonard/RcppMovStat")
})


cellsize <- 0.5
range    <- c(72, 136, 18, 54)
range_nc <- c(70, 140, 15, 55)

flipud <- function(x){
    x %>% .[, ncol(.):1]
}


file.move <- function(file, target){
    file_new <- paste0(target, "/", basename(file))
    file.rename(file, file_new)
}
