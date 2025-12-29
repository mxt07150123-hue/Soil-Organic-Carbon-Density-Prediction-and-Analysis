library(readr)
library(dplyr)
library(olsrr)
library(car)
df <- read_csv("Data1.csv")

response <- "SOC"
all_cols <- colnames(df)
predictors <- setdiff(all_cols, response)

# 去缺失
dat <- df %>% select(all_of(c(response, predictors))) %>% na.omit()


X <- scale(dat[, predictors], center = TRUE, scale = TRUE)
y <- dat[[response]]


cor_mat <- cor(dat[, predictors], use = "pairwise.complete.obs", method = "pearson")
write.csv(cor_mat, "correlation_matrix_predictors.csv", row.names = TRUE)


mod_all <- lm(SOC ~ ., data = data.frame(SOC = y, X))
vif_tbl <- data.frame(predictor = colnames(X), VIF = vif(mod_all))
vif_tbl <- vif_tbl[order(-vif_tbl$VIF), ]
write.csv(vif_tbl, "VIF_table.csv", row.names = FALSE)


ci_tbl <- ols_eigen_cindex(mod_all) 

write.csv(ci_tbl[, c("Condition Index")],
          "ConditionIndex_table.csv", row.names = FALSE)


summary(mod_all)


if ("Tavg" %in% colnames(X)) {
  X_df <- as.data.frame(X)
  X_T <- X_df %>% select(-Tavg)

  res_T <- lm(Tavg ~ ., data = cbind(Tavg = X_df$Tavg, X_T))
  Tavg_orth <- resid(res_T)
  m_orth <- lm(SOC ~ Tavg_orth + ., data = cbind(SOC = y, X_T))
  summary(m_orth)
  

  m_y <- lm(SOC ~ ., data = cbind(SOC = y, X_T))
  y_res <- resid(m_y)
  x_res <- Tavg_orth
  png("partial_regression_Tavg.png", width = 1200, height = 1000, res = 150)
  plot(x_res, y_res, pch = 16, cex = 0.6,
       xlab = "Tavg residual (orthogonal to others)",
       ylab = "SOC residual (after removing others)",
       main = "Partial regression: SOC ~ Tavg | others")
  abline(lm(y_res ~ x_res), lwd = 2)
  dev.off()
}