library(mgcv)

Input_data <- read.csv("Data3.csv")
dat <- as.data.frame(Input_data)

m_lin  <- lm(SOC ~ Tavg, data = dat)
m_gam2 <- gam(SOC ~ Tavg + s(Tavg, bs = "tp", k = 20), data = dat, method = "REML")
summary(m_gam2)$s.table   
anova(m_lin, m_gam2, test = "F")  


library(readr)
library(mgcv)

dat <- read_csv("Data3.csv", show_col_types = FALSE)


x_col <- "Tavg"
y_col <- "SOC"

d <- dat[, c(x_col, y_col)]
names(d) <- c("x","y")
d$x <- as.numeric(d$x)
d$y <- as.numeric(d$y)
d <- d[is.finite(d$x) & is.finite(d$y), ]


m_gam <- gam(y ~ s(x, bs = "tp", k = 20), data = d, method = "REML")
s_tab <- summary(m_gam)$s.table
EDF   <- s_tab[1, "edf"]
Fval  <- s_tab[1, "F"]
pval  <- s_tab[1, "p-value"]
cat(sprintf("EDF = %.3f, F = %.3f, p = %.3g\n", EDF, Fval, pval))