library(raster)
library(dplyr)

soc_raster <- raster("SOC_Content_2020s_GD.tif")

soc_points <- rasterToPoints(soc_raster, spatial = FALSE)
colnames(soc_points) <- c("lon", "lat", "SOC")

soc_df <- as.data.frame(soc_points)
soc_df$lat_abs_band <- floor(abs(soc_df$lat))  

soc_summary <- soc_df %>%
  group_by(lat_abs_band) %>%
  summarise(mean_SOC = mean(SOC, na.rm = TRUE)) %>%
  arrange(lat_abs_band) %>%
  ungroup()

# 5. 输出CSV文件
write.csv(soc_summary, "SOC_Latitude_AbsoluteBand_Mean.csv", row.names = FALSE)
cat("SOC_Latitude_AbsoluteBand_Mean.csv\n")