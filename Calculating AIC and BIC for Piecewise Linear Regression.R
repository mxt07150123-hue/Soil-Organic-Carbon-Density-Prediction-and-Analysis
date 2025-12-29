
library(readr)


Input_data <- read.csv("Data3.csv")
data <- as.data.frame(Input_data)

global_model <- lm(SOC ~ Tavg, data = data)


global_aic <- AIC(global_model)
global_bic <- BIC(global_model)


cat("Global Linear Model AIC:", global_aic, "\n")
cat("Global Linear Model BIC:", global_bic, "\n")



library(segmented)


grouped_models <- list()


for (group in unique(data$group)) {
  group_data <- subset(data, group == group)
  model <- lm(SOC ~ Tavg, data = group_data)
  segmented_model <- segmented(model, seg.Z = ~Tavg, psi = list(Tavg = mean(group_data$Tavg)))  
  
 
  grouped_models[[group]] <- segmented_model
}


for (group in names(grouped_models)) {
  model <- grouped_models[[group]]
  segment_aic <- AIC(model)
  segment_bic <- BIC(model)
  
  cat(paste("Segmented Linear Model (", group, ") AIC:", segment_aic, "\n", sep = ""))
  cat(paste("Segmented Linear Model (", group, ") BIC:", segment_bic, "\n", sep = ""))
}


combined_model <- lm(SOC ~ Tavg * group, data = data)  
segmented_combined_model <- segmented(combined_model, seg.Z = ~Tavg, psi = list(Tavg = mean(data$Tavg)))  


combined_aic <- AIC(segmented_combined_model)
combined_bic <- BIC(segmented_combined_model)

cat("Combined Segmented Linear Model AIC:", combined_aic, "\n")
cat("Combined Segmented Linear Model BIC:", combined_bic, "\n")
