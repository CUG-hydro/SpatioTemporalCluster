# include("main_pkgs.jl")
# using StatsBase
using cluster
using RCall
using Random

function plot_cluster(clusterId, outfile)
    R"""
    x = $clusterId
    x[x == -999] = NA
    if (length(dim(x)) <= 2) {
        # this step make sure this function works for 2d and 3d array
        dim(x) <- c(dim(x), 1)
    }
    write_fig(SpatioTemporal.cluster:::plot.cluster(x), $outfile, 8, 8)
    """
end

# using JLD2
# arr_raw = GeoArrays.read("../arr.tif").A .> 0;
# IdClusters = GeoArrays.read("../arr.tif").A;
begin
    # Random.seed!(123);
    n = Int(10)
    arr = rand(n, n, 4) .> 0.7;
    "ok"

    factor = Int(n*n)
    ncell_connect = 1
    diag = true
    # arr = @rget arr;
    # # IdClusters0 = trunc.(Int, IdClusters);
    @time nc, cno, clusterId_tree = connect_spatial(arr[:,:,:], method = "tree", ncell_connect = ncell_connect, factor = factor, diag = diag)
    @time nc, cno, clusterId_recursive = connect_spatial(arr[:,:,:], method = "recursive", ncell_connect = ncell_connect, factor = factor, diag = diag)
    @time nc, cno, clusterId_low = connect_spatial(arr[:,:,:], method = "low", ncell_connect = ncell_connect, factor = factor, diag = diag)
    # nC, cno, clusterId = connect_spatial(arr; minCells = 1, factor = factor, diag = false) 
    # TimeConnect(
    #     arr; 
    #     minOverlapCells = 5, ID_min = 0) 
    # r = cluster_SpatioTemporal(arr; 
    #     factor = 100000, 
    #     minCells = 1, 
    #     minOverlapCells = 5, 
    #     diag = true);
    # clusterId_tree
    "ok"
end

using ProfileView
@time r = connect_spatial_tree(arr[:,:,1], diag = true)
@time r = connect_spatial_recursive(arr[:,:,1], diag = true)
"ok"

plot_cluster(clusterId_tree, "clusterId_tree.pdf")
plot_cluster(clusterId_recursive, "clusterId_recursive.pdf")
# plot_cluster(clusterId_low, "clusterId_low.pdf")
