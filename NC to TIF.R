library(terra)
library(ncdf4)

nc_file <- nc_open("tmp_2020.nc")
var_name <- "tmp"
data <- ncvar_get(nc_file, var_name)
lon <- ncvar_get(nc_file, "lon")
lat <- ncvar_get(nc_file, "lat")

nt <- dim(data)[3]

for (i in 1:nt) {
  data_slice <- data[, , i]
  r <- rast(t(data_slice))
  ext(r) <- c(min(lon), max(lon), min(lat), max(lat))
  crs(r) <- "epsg:4326"
  writeRaster(r, paste0("output_", i, ".tif"), filetype="GTiff")
}

nc_close(nc_file)