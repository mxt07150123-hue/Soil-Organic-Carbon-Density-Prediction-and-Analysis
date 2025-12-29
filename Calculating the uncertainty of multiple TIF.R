library(terra)


soc_files <- list.files("K:/Uncertainty/1990s/Mapping", 
                        pattern = "SOC_Result.*\\.tif$", 
                        full.names = TRUE)

r_stack <- rast(soc_files)  


r_sd <- app(r_stack, fun = sd, na.rm = TRUE)


writeRaster(r_sd, "SOC_Uncertainty_1990s.tif", 
            overwrite = TRUE, filetype = "GTiff")