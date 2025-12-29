setwd("D:\\Desktop")
library(SiZer)
library(ggplot2)
dev.new(title="Partial RDA", width=10, height=8,
        noRStudioGD = TRUE)
data <- read.csv("Data.csv")


ggplot(data, aes(Tavg, SOC)) + 
  geom_point() + 
  geom_smooth()



plot(model, xlab = "Tavg", ylab = "SOC")


model <- piecewise.linear(x = data$Tavg, y = data$SOC, CI = TRUE,
                          bootstrap = 1000, sig.level = 0.05)
model


plot(model, xlab = "Tavg", ylab = "SOC")

fit1 <- predict(model, data$Tavg)
SSre <- sum((data$SOC-fit1)^2)
SStot <- sum((data$SOC-mean(data$SOC))^2)
R2 <- round(1 - SSre/SStot, 3)
R2


library(segmented)

fit_lm <- lm(SOC~Tavg, data = data)
summary(fit_lm)


lm_seg1 <- segmented(fit_lm, seg.Z = ~Tavg, npsi = 1)
summary(lm_seg1)
plot(lm_seg1, xlab = "Tavg", ylab = "SOC")
points(SOC~Tavg, data = data)


lm_seg2 <- segmented(fit_lm, seg.Z = ~Tavg, psi = 0.008)
summary(lm_seg2)
plot(lm_seg2, xlab = "Tavg", ylab = "SOC")
points(SOC~Tavg, data = data)

