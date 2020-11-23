# include("main_pkgs.jl")
# using StatsBase
using cluster
# using RCall
# R"load('../debug.rda')"
# arr = randn(10, 10)
using Random
Random.seed!(123);
n = Int(1000)
arr = rand(n, n, 4) .> 0.2;
"ok"

# using JLD2
# arr_raw = GeoArrays.read("../arr.tif").A .> 0;
# IdClusters = GeoArrays.read("../arr.tif").A;
@time begin
    factor = Int(n*n)
    # arr = @rget arr;
    # # IdClusters0 = trunc.(Int, IdClusters);
    # TimeConnect(
    #     arr; 
    #     minOverlapCells = 5, ID_min = 0) 
    # r = cluster_SpatioTemporal(arr; 
    #     factor = 100000, 
    #     minCells = 1, 
    #     minOverlapCells = 5, 
    #     diag = true);
    "ok"    
end

# counts = values(countmap(r[:])) |> collect |> sort

# using Plots
# pyplot()
# gr()
# heatmap(clutserId[:,:,1])
