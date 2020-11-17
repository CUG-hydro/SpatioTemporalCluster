using GeoArrays
using Printf
using cluster
using JLD
using NetCDF
using DelimitedFiles
# using Plots
# gr()

range = [73.48769838361044, 135.08769813715907, 3.8363272523055656, 53.561327053364096]

# convert array into raster by `GeoArrays`
function as_raster(arr, range) 
  ga = GeoArray(arr)
  bbox!(ga, (min_x = range[1], min_y = range[3], max_x = range[2], max_y = range[4]))
  epsg!(ga, 4326)  # in WGS84
  ga  
end





## FUNCTIONS for writing raster ------------------------------------------------
function write_nc(IdClusters, outfile = "urban_cluster_1980-2015.nc")
  cellsize = 1/120
  lon = collect(73.48769838361044:cellsize:135.08769813715907+cellsize/2)
  lat = collect(3.8363272523055656:cellsize:53.561327053364096+cellsize/2)
  time = collect(0:35)
  # Define some attributes of the variable (optionlal)
  varatts = Dict("longname" => "Urban Cluster ID")
  lonatts = Dict("longname" => "Longitude", "units" => "degrees east")
  latatts = Dict("longname" => "Latitude", "units" => "degrees north")
  timatts = Dict("longname" => "Time", "units" => "years since 01-01-1980 00:00:00")

  isfile(outfile) && rm(outfile)
  nccreate(outfile, "IdClusters", 
      "lon", lon, lonatts,
      "lat", lat, latatts,
      "time", time, timatts,
      atts = varatts)
  @time ncwrite(IdClusters, outfile, "IdClusters")
end

function write_tif(IdClusters, year)
  i = year - 1980 + 1
  ga = GeoArray(IdClusters[:,:,i])
  bbox!(ga, (min_x = 73.48769838361044, min_y = 3.8363272523055656, max_x = 135.08769813715907, max_y = 53.561327053364096))
  epsg!(ga, 4326)  # in WGS84

  year = year_begin + i - 1
  outfile = "OUTPUT/urban_IdClusters_$year.tif"
  GeoArrays.write!(outfile, ga)    
end
