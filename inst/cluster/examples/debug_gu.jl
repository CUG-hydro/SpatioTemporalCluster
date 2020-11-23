include("main_pkgs.jl")
# using StatsBase

# using RCall
R"load('../debug.rda')"
# arr_raw = GeoArrays.read("../arr.tif").A .> 0;
# IdClusters = GeoArrays.read("../arr.tif").A;
begin
    arr = @rget arr;
    # IdClusters0 = trunc.(Int, IdClusters);
    nC, cno, clutserId = connect_spatial(arr, minCells = 1, diag = true, factor = Int(1000)) 
    # TimeConnect(
    #     arr; 
    #     minOverlapCells = 5, ID_min = 0) 
    # r = cluster_SpatioTemporal(arr; 
    #     time_factor = 100, 
    #     minCells = 0, 
    #     minOverlapCells = 5, 
    #     diag = true);
    # "ok"    
end

counts = values(countmap(r[:])) |> collect |> sort

using Plots
pyplot()
gr()
heatmap(clutserId[:,:,1])
