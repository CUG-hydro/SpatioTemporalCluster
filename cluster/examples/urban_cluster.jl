year  = 2015
years = 1980:2015
nyear = length(years)
arr_urban_perc = zeros(7392, 5967, length(years))

for i = 1:nyear
  println(i)
  year = years[i]
  file1 = @sprintf("F:/SciData/LULC/LULC_China_landsat_1km/%d/CLUDA_%d_88_1km_prop.tif", year, year)
  file2 = @sprintf("F:/SciData/LULC/LULC_China_landsat_1km/%d/CLUDA_%d_89_1km_prop.tif", year, year)

  file1 = @sprintf("/mnt/f/SciData/LULC/LULC_China_landsat_1km/%d/CLUDA_%d_88_1km_prop.tif", year, year)
  file2 = @sprintf("/mnt/f/SciData/LULC/LULC_China_landsat_1km/%d/CLUDA_%d_89_1km_prop.tif", year, year)

  r1 = GeoArrays.read(file1)
  r2 = GeoArrays.read(file1)
  r = r1+r2
  arr_urban_perc[:, :, i] = r.A
end

arr_urban = arr_urban_perc .>= 0.5
@time nC, cno, IdClusters = spatial_cluster(arr_urban; time_factor = Int(1e6), minCells = 25);

save("data/urban_cluster.jld", "IdClusters", IdClusters, "nC", nC, "cno", cno)

l = load("data/urban_cluster.jld")
IdClusters = l["IdClusters"]
cno = l["cno"]

@time begin
    # IdClusters = cluster_spatiotemporal(arr_urban; time_factor = Int(1e6), 
    #     minCells = 25, minOverlapCells = 25)
    TimeConnect(IdClusters, cno; minOverlapCells = 25);
    # IdClusters
    "ok"
end

save("data/urban_cluster_final.jld", "IdClusters", IdClusters, "cno", cno)

l = load("data/urban_cluster_final_(50%).jld")
IdClusters = l["IdClusters"]
cno = l["cno"]

for year in years
  write_tif(IdClusters, year)  
end
writedlm("cno_(50%).txt",  cno, ',')

## save into tif file directly -------------------------------------------------
ga = as_raster(IdClusters, range)
outfile = "OUTPUT/urban_IdClusters.tif"
GeoArrays.write!(outfile, ga)  
# @time x = GeoArrays.read(outfile)
