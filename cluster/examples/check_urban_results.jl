include("main_pkgs.jl")
using StatsBase


function urban_DevelopYear(arr, outfile = "urban_DevelopYear.tif"; year_begin = 1980)
    ntime = size(arr,3)
    ## 制作城市化演进进程，判断哪一年变成城市化
    mat_UrbanDevelopYear = zeros(Int, size(arr)[1:2])
    # years = 1980:2015
    year_begin = 1980

    for i in 1:ntime
        println(i)
        year = year_begin + i - 1    
        ind = (@view(arr[:, :, i]) .> 0) .& (mat_UrbanDevelopYear .== 0)
        mat_UrbanDevelopYear[ind] .= year
    end

    r = as_raster(mat_UrbanDevelopYear, range)
    GeoArrays.write!(outfile, r)
end


## scripts ---------------------------------------------------------------------
file = "./OUTPUT/urban_IdClusters.tif"
@time arr = GeoArrays.read(file).A
# urban_DevelopYear(arr, "urban_DevelopYear2.tif")


## get coordinates
# GeoArrays.




## 指导思想
# 1. 每个id单独拎出来，制作shapefile和tif
# 2. 取每个urban最大的范围作为shapefile边界

# ids取交集
# ntime = size(arr, 3)
# lst_ids = []
# for i in 1:ntime
#     println(i)
#     mat = @view(arr[:, :, i])
#     x = mat[mat .> 0]
#     ids = StatsBase.countmap(x)
#     push!(lst_ids, ids)
# end
# map(length, lst_ids)

# using Base
cno = load("data/urban_cluster_final_(50%).jld", "cno")[:]
cno = cno[cno .> 0]
ids_full = StatsBase.countmap(cno[:])
ids_full = Base.keys(ids_full) |> collect |> sort 


id = ids_full[1]
# this extremely low efficiency
urban_max = GeoArrays.read("urban_DevelopYear.tif").A
mask = findall(urban_max .> 0)

dim = size(arr)
ntime = dim[3]

begin
    k = 0
    for id in ids_full
        k+=1;
        if (mod(k, 10) == 0); println(k); end
        ind = []
        for i in 1:ntime
            # println(i)
            mat = @view(arr[:,:,i])
            # for 
            # temp = []
            # for index in mask
            #     if (mat[index] == id); push!(temp, index); end
            # end
            vec = mat[mask]
            push!(ind, mask[vec .== id])
        end
    end
end


i = 1
# for i in 
using Plots
gr()

heatmap(urban_max.A)

# @time mask = sum(arr .> 0, dims = 3)[:, :, 1] .> 0 # 
# mask_ind = findall(mask)
temp = arr[arr .== id]

# find the max extent first

## 对于每个城市群生成一个shapefile
using RCall
R"1"

using Plots
gr()
urban = urban_DevelopYear
# urban[urban .== 0] .= NaN
heatmap(urban_DevelopYear)
