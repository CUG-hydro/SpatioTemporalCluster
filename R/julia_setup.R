
julia_init <- function() {
    JuliaCall::julia_setup()
    JuliaCall::julia_source("cluster/src/cluster.jl")
}
