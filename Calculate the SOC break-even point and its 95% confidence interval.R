
data <- read.csv("Data.csv")


head(data)
str(data)

model <- lm(SOCC ~ TavgC.yr, data = data)


summary_model <- summary(model)
print(summary_model)

coefficients <- coef(model)
b0 <- coefficients[1]  
b1 <- coefficients[2]  


tavg_at_socc_zero <- -b0 / b1
cat(":", tavg_at_socc_zero, "\n")

if (!require(msm)) {
  install.packages("msm")
  library(msm)
}


se_ratio <- deltamethod(~ -x1/x2, coef(model), vcov(model))


alpha <- 0.05
z_value <- qnorm(1 - alpha/2)
ci_lower <- tavg_at_socc_zero - z_value * se_ratio
ci_upper <- tavg_at_socc_zero + z_value * se_ratio

cat(" [", ci_lower, ",", ci_upper, "]\n")


t_stat <- tavg_at_socc_zero / se_ratio
p_value <- 2 * (1 - pt(abs(t_stat), df = summary_model$df[2]))

cat("t", t_stat, "\n")
cat("P:", p_value, "\n")
cat("p<0.05:", ifelse(p_value < 0.05, "是", "否"), "\n")


plot(data$TavgC.yr, data$SOCC, 
     xlab = "TavgC/yr", ylab = "SOCC",
     main = "SOCC vs TavgC/yr ",
     pch = 16, col = "blue")

abline(model, col = "red", lwd = 2)


abline(h = 0, lty = 2, col = "gray")
abline(v = tavg_at_socc_zero, lty = 2, col = "red", lwd = 2)


abline(v = ci_lower, lty = 3, col = "red")
abline(v = ci_upper, lty = 3, col = "red")

points(tavg_at_socc_zero, 0, col = "red", pch = 16, cex = 2)

legend("topright", 
             col = c("blue", "red", "red", "red"),
       pch = c(16, NA, 16, NA),
       lty = c(NA, 1, NA, 3))