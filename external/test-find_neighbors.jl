
begin
    # using Plots
    using Random
    using StatsBase

    Random.seed!(123);
    n = Int(1e3)
    data = rand(n, n, 12)
    arr_3d = data .> 0.5
    # heatmap(arr_3d[:,:,1])
    size(arr_3d)

    ## in action
    include("cluster.jl")
    # IdClusters = spatial_cluster(arr_3d[:,:,:]);
    # idClusters2 = idClusters
end

# arr_3d .= 1

# arr_3d
# print(arr_3d)
# savefig("a.pdf")
# include("find_neighbors.jl")
# ProfileView.@profview begin
@time begin
    # @run 
    nC, cno_list, IdClusters = spatial_cluster(arr_3d[:,:,:]);
    # nC, cno = cluster_refactor!(IdClusters)
    # res = time_connect(idClusters2)
    # res[:,:,1]
end
# iC[iC .< 0] .= 0
# heatmap(iC)

# using ProfileView
# ProfileView.@profview begin
# @time begin
# using ProfileView
# @profile begin
# ProfileView.@profview begin

@time begin
    # z = x .+ y;
    time_connect(IdClusters, cno);
    "ok"
end
# using Profile
# using Traceur
# # Profile.@profile 
# @trace time_connect(IdClusters, cno);
# n = Int(1e4)
# x = rand(n, n)
# y = rand(n, n)
