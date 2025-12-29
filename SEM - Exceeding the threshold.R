library(piecewiseSEM)
library(readxl)
library(lavaan)
df<-read.csv("Data.csv", header = T)
df=scale(df[,1:13],center=TRUE,scale=TRUE)
df <- as.data.frame(df)
mymod <- psem(
  lm(SOC ~ Nrate + Yield + GPP + Tavg + SLO, data = df),
  lm(Nrate ~ GPP + SLO, data = df),
  lm(Yield ~ Nrate + GPP + Tavg + SLO, data = df),
  lm(GPP ~  SLO + Tavg, data = df),
  lm(Tavg ~ SLO, data = df),
  data = df)


summary(mymod, .progressBar = F)


library(lavaan)
library(haven)
library(Hmisc)
library(semPlot)
library(readxl)

df<-read.csv("Data.csv", header = T)
df=scale(df[,1:13],center=TRUE,scale=TRUE)
model <- '
      SOC ~ Nrate + Yield + GPP + Tavg
      Nrate ~ GPP + SLO
      Yield ~ Nrate + GPP + Tavg + SLO
      GPP ~  SLO + Tavg
      Tavg ~ SLO
'
fit_model <- sem(model, data = df)

summary(fit_model, standardized = TRUE, fit.measures = TRUE)
fitmeasures(fit_model, c("chisq", "df", "gfi", "pvalue", "rmse", "srmr", "AIC", "TLI"))
semPaths(fit_model, "std", edge.label.cex = 0.8, 
         fade=FALSE, layout = "spring",
         optimizeLatRes = FALSE, residuals = FALSE)


library(dplyr)
kk1 <- summary(mymod, .progressBar = F)
kk2 <- kk1$coefficients[,-(3:6)]
kk3 <- as.data.frame(kk2)
kk4 <- kk3[!grepl("~~", kk3$Predictor), ]
result1 <- kk4 %>%  
  dplyr::relocate(    
    from   = Predictor,    
    to     = Response,    
    weight = Std.Estimate,    
    p   = P.Value  
  )
data <-result1 


calculateDirectEffects <- function(data, factors) {
  direct_effects <- numeric(nrow(data))    
  for (i in 1:nrow(data)) {    
    if (paste(data$from[i], data$to[i], sep = "_") %in% factors) {      
      direct_effects[i] <- data$weight[i]    
    } else {      
      direct_effects[i] <- 0    
    }  
  }      
  non_zero_rows <- direct_effects != 0  
  direct_effects <- direct_effects[non_zero_rows]  
  from_to <- paste(data$from[non_zero_rows], "→", data$to[non_zero_rows])    
  direct_effects_df <- data.frame(    
    from_to = from_to,    
    Direct_Effect = direct_effects  
  )    
  return(direct_effects_df)
}


generateDirectEffectFactors <- function(data) {
  unique_direct_combinations <- unique(paste(data$from, data$to, sep = "_"))
  from_factors <- unique(data$from)
  to_factors <- unique(data$to)
  direct_factors <- paste(rep(from_factors, each = length(to_factors)), rep(to_factors, length(from_factors)), sep = "_")  
  return(direct_factors)
}
direct_factors <- generateDirectEffectFactors(data)
direct_effects_result <- calculateDirectEffects(data, direct_factors)
direct_effects_result




unique_from <- unique(data$from)
unique_to <- unique(data$to)
paths <- expand.grid(from = unique_from, to = unique_to)
paths <- split(paths, seq(nrow(paths)))
paths <- lapply(paths, function(x) c(as.character(x$from), as.character(x$to), "SOC"))


calculateIndirectEffects <- function(data, from_factor, through_factor, to_factor) {
  through_weight <- data$weight[data$from == from_factor & data$to == through_factor]
  to_weight <- data$weight[data$from == through_factor & data$to == to_factor]
  if (length(through_weight) == 0 | length(to_weight) == 0) {
    message(paste("Skipping invalid path:", from_factor, "→", through_factor, "→", to_factor))
    return(NULL)
  }
  indirect_effect <- through_weight * to_weight
  return(indirect_effect)
}
indirect_effects_df <- data.frame(from_to = character(), Indirect_Effect = numeric())
indirect_effects_df


for (path in paths) {
  from_factor <- path[1]
  through_factor <- path[2]
  to_factor <- path[3]
  indirect_effect <- calculateIndirectEffects(data, from_factor, through_factor, to_factor)
  if (!is.null(indirect_effect)) {
    from_to <- paste(from_factor, through_factor, to_factor, sep = " → ")
    indirect_effects_df <- rbind(indirect_effects_df, data.frame(from_to = from_to, Indirect_Effect = indirect_effect))
  }
}


calculate_total_indirect_effect <- function(data) {
  
  
  total_indirect_effect <- data %>%
    mutate(
      start_pattern = sub(" → .*", "", from_to),
      end_pattern = sub(".* → ", "", from_to)
    ) %>%
    group_by(start_pattern, end_pattern) %>%
    summarise(total_indirect_effect = sum(Indirect_Effect), .groups = "drop") %>%
    ungroup() %>%
    arrange(start_pattern, end_pattern) %>%
    mutate(from_to = paste0(start_pattern, " → ", end_pattern)) %>%
    select(from_to, total_indirect_effect)
  return(total_indirect_effect)
}
result_total_indirect_effect <- calculate_total_indirect_effect(indirect_effects_df)
total_indirect_effect_df <- as.data.frame(result_total_indirect_effect)
total_effects_df <- bind_rows(direct_effects_result, indirect_effects_df, total_indirect_effect_df)
total_effects_df

data <- total_effects_df
data_processed <- data %>%
  group_by(from_to) %>%
  summarise(Direct_Effect = sum(Direct_Effect, na.rm = TRUE),
            Indirect_Effect = sum(Indirect_Effect, na.rm = TRUE),
            total_indirect_effect = sum(total_indirect_effect, na.rm = TRUE))
data_processed2 <- data_processed %>%
  mutate(Total_Effect = Direct_Effect + total_indirect_effect)
df_filtered1 <- data_processed2 %>%
  filter(grepl("SOC$", from_to))
data_frame <- as.data.frame(df_filtered1)
filtered_data1 <- data_frame %>%
  filter(Indirect_Effect == 0)
filtered_data2 <- filtered_data1[,-(3)]
data_frame <- as.data.frame(filtered_data2)
data_frame$from_to_clean <- sub(" .*", "", data_frame$from_to)
kk4 <- print(data_frame[, c("from_to_clean", "Direct_Effect", "total_indirect_effect", "Total_Effect")])



library(ggplot2)
library(tidyr)
data_frame <- as.data.frame(kk4 )
data_long <- pivot_longer(data_frame, 
                          cols = c(Direct_Effect, total_indirect_effect, Total_Effect),
                          names_to = "Effect_Type", 
                          values_to = "Effect_Value")
data_long$from_to_clean <- factor(data_long$from_to_clean,
                                  levels = c("CI","SI","EVI","PR","Tavg","pet","DEM"))
data_long$Effect_Type <- factor(data_long$Effect_Type,
                                levels = c("Direct_Effect", "total_indirect_effect", "Total_Effect"))
p <- ggplot(data_long, aes(x=from_to_clean, y=Effect_Value)) +
  geom_bar(aes(fill = Effect_Type), stat = "identity", color = "black", size = 0.8,
           position = position_dodge(0.9), width = 0.7) +
  scale_y_continuous(limits = c(-1, 1)) + 
  
  scale_fill_manual(values = c("#C77CFF", "#00BFC4", "#F8766D"),
                    breaks = c("Direct_Effect", "total_indirect_effect", "Total_Effect")) +
  theme_bw()+ 
  
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 1) 
print(p)

