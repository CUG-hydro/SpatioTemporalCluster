
begin
    # using Plots
    using cluster
    using Random
    # using JLD
    using StatsBase

    Random.seed!(123);
    n = Int(1e3)
    data = rand(n, n, 12)
    arr_3d = data .> 0.5
    # heatmap(arr_3d[:,:,1])
    size(arr_3d)
    ## in action
    # include("../cluster.jl")
    # IdClusters = spatial_cluster(arr_3d[:,:,:]);
    # idClusters2 = idClusters
end

# print(arr_3d)
# savefig("a.pdf")
# include("find_neighbors.jl")
# ProfileView.@profview begin
@time begin
    IdClusters = cluster_spatiotemporal(arr_3d; time_factor = Int(1e3), minCells = 2, minOverlapCells = 2)
    # nC, cno, IdClusters = spatial_cluster(arr_3d; time_factor = Int(1e4), minCells = 2);
    # TimeConnect(IdClusters, cno; minOverlapCells = 2);
    # IdClusters
    "ok"
end

# iC[iC .< 0] .= 0
# heatmap(iC)
# using ProfileView
# ProfileView.@profview begin
@time begin
    # z = x .+ y;
    "ok"
end
# 60s
# 161.129874
# 1329.030200

# using Profile
# using Traceur
# # Profile.@profile 
# @trace time_connect(IdClusters, cno);
# n = Int(1e4)
# x = rand(n, n)
# y = rand(n, n)

using BenchmarkTools

x = Int.(round.(rand(Int(1e6))*1000))
@benchmark countmap(x)

@benchmark unique(x)

using StatsBase
@benchmark StatsBase.countmap(x)
