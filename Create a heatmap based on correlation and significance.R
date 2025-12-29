
library(ggplot2)    
library(reshape2)   
library(readxl)     
library(dplyr)      


corr_data <- read_excel(
  path = "r.xlsx",  
  sheet = 1,                     
  .name_repair = "unique"         
)


p_data <- read_excel(
  path = "p.xlsx",  
  sheet = 1,
  .name_repair = "unique"
)


row_names <- corr_data[[1]]


corr_matrix <- as.matrix(corr_data[, -1])
p_matrix <- as.matrix(p_data[, -1])
rownames(corr_matrix) <- row_names
rownames(p_matrix) <- row_names

# 转为长格式
corr_long <- melt(corr_matrix, 
                  varnames = c("Var2", "index"), 
                  value.name = "corr")
p_long <- melt(p_matrix, 
               varnames = c("Var2", "index"), 
               value.name = "p_value") %>%
  mutate(sig = p_value < 0.001)  


df_combined <- merge(corr_long, p_long[, c("Var2", "index", "sig")], 
                     by = c("Var2", "index"))



custom_order <- c("GM", "NA", "SA", "EU", "SI", "AS")  # 替换为你的完整行名顺序


df_combined$Var2 <- factor(df_combined$Var2, levels = custom_order)


df_combined$corr_clamped <- pmin(pmax(df_combined$corr, -0.6), 0.6)


ggplot(df_combined, aes(x = index, y = Var2)) +
  
  geom_tile(aes(fill = corr_clamped), color = "white", linewidth = 0.3) +
  
  geom_point(data = subset(df_combined, sig == TRUE),
             color = "black", shape = 16, size = 2) +
  
  scale_fill_gradient2(
    low = rgb(0, 51, 102, maxColorValue = 255),      
    mid = rgb(255, 255, 255, maxColorValue = 255),  
    high = rgb(153, 0, 0, maxColorValue = 255),      
    midpoint = 0,
    limit = c(-0.6, 0.6),  #
    name = "Correlation Coefficient"
  ) +
 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
    axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, margin = margin(r = 10)),
    legend.position = "right",
    legend.key.height = unit(2, "cm"),
    panel.grid = element_blank()  
  ) +
  labs(
    x = "factor",  
    y = "type"   
  )