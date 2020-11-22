library(plyr)
library(doParallel)
library(magrittr)
library(parallel)
library(JuliaCall)
# julia_init()

for (seed in 1:100){
    set.seed(seed)
    list.mat <- llply(1:10, function(ind){
        matrix(ifelse(sample(1:100, 20^2, T) > 40, T, F), 20)
    })

    mask <- sapply(list.mat, c) %>% apply(., 1, sum)
    mask <- which(mask > 0)

    arr.julia <- abind::abind(list.mat, along = 3)

    system.time({
        z1 = julia_call("cluster.cluster_spatiotemporal", arr.julia,
                        time_factor = as.integer(10000), # max clusters for each time
                        minOverlapCells = as.integer(5),
                        minCells = as.integer(0))
    })
    z1[z1 <= 0] <- NA


    system.time({
        z2 <- ClusterEvolution_Fortran(list.mat = list.mat, thres = 0, overlap = 5,
                                       factor = 10000)
    })

    # system.time({
    #     z3 <- ClusterEvolution_julia(list.mat = list.mat, thres = 0, overlap = 5,
    #                                  factor = 10000)
    # })

    z1 <- table(z1) %>% sort() %>% as.numeric()
    z2 <- table(z2) %>% sort() %>% as.numeric()
    # z3 <- table(z3) %>% sort() %>% as.numeric()
    print(seed)
    if (!isTRUE(all.equal(z1, z2))){
        break
    }

}
# set.seed(100000)


# z <- data.frame(z1 = z1[1:length(z2)], z2 = z2)
