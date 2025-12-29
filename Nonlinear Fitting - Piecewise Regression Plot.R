library(ggplot2)
library(dplyr)
library(patchwork)
library(mgcv)  
library(ggExtra) 

set.seed(123)

data <- read.csv("Data1.csv")


threshold_ci_lower <- 0.0080
threshold_ci_upper <- 0.0106


p1 <- ggplot(data, aes(x = Tavg, y = SOC)) +
  
  geom_point(size = 4, alpha = 0.7, color = "black") +
  
  
  geom_smooth(method = "gam", formula = y ~ s(x), 
              se = TRUE, level = 0.95, 
              color = "blue", fill = "blue", alpha = 0.3, linetype = "dashed") +
  
  
  annotate("rect", xmin = threshold_ci_lower, xmax = threshold_ci_upper,
           ymin = -Inf, ymax = Inf, 
           alpha = 0.2, fill = "red") +
  
  
  geom_vline(xintercept = 0.0087, color = "red", linetype = "dashed", size = 1) +
  
  
  geom_vline(xintercept = threshold_ci_lower, color = "red", linetype = "dotted", size = 0.7) +
  geom_vline(xintercept = threshold_ci_upper, color = "red", linetype = "dotted", size = 0.7) +
  
 
  
  labs(
    title = "Tavg of SOC - GAM fit",
    x = expression("Tavg (g kg"^{-1} * ")"),
    y = "SOC"
  ) +
  
 
  annotate("text", x = max(data$Tavg) * 0.85, y = max(data$SOC) * 0.95,
           label = " ", size = 5, fontface = "bold") +
  
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) 
  

p1_with_marginals <- ggMarginal(
  p1,
  type = "density",           
  color = "grey",             
  fill = "grey",              
  alpha = 0.7,               
  size = 10                   
)
print(p1_with_marginals)



p2 <- ggplot(data, aes(x = Tavg, y = SOC, color = group, fill = group)) +
  
  geom_point(size = 4, alpha = 0.6) +
  
  
  geom_smooth(method = "lm", formula = y ~ x,
              se = TRUE, level = 0.95, 
              alpha = 0.2, size = 1.2, color = "#11469B", aes(linetype = group)) +
  
  
  geom_vline(xintercept = 0.0087, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = 0.0087, y = max(data$SOC), 
           label = " ", hjust = -0.1, color = "red", size = 4) +
  
  
  scale_color_manual(values = c("Group A" = "#8B5A00", "Group B" = "#458B75")) +
  scale_fill_manual(values = c("Group A" = "#262626", "Group B" = "#262626")) + 
  scale_linetype_manual(values = c("Group A" = "dashed", "Group B" = "dashed")) +
  
  
  labs(
    title = "Tavg of SOC - fit",
    x = expression("Tavg (g kg"^{-1} * ")"),
    y = "SOC",
    color = "Changing rate of SOC",
    fill = "Changing rate of SOC"
  ) +
  
  
  annotate("text", x = max(data$Tavg) * 0.85, y = max(data$SOC) * 0.95,
           label = " ", size = 5, fontface = "bold") +
  
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = c(0.18, 0.98),
    legend.background = element_rect(fill = "black", color = "black"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) 
 


p2_with_marginals <- ggMarginal(
  p2,
  type = "density",           
  color = "grey",            
  fill = "grey",             
  alpha = 0.7,               
  size = 10                   
)
print(p2_with_marginals)


custom_width <- 20   
custom_height <- 10  
custom_dpi <- 1000    

png("custom_output.png", 
    width = custom_width, 
    height = custom_height, 
    units = "in", 
    res = custom_dpi)
gridExtra::grid.arrange(p1_with_marginals, p2_with_marginals, ncol = 2)
dev.off()