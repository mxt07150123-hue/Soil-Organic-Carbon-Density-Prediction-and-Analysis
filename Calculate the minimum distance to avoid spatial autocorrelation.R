
library(terra)
library(sf)
library(spdep)
library(ggplot2)
library(dplyr)

raster_path <- "CDD_2016-2020_Final.tif"   
sample_n    <- 1200         
alpha       <- 0.05         
nsim_perm   <- 999          
n_steps     <- 12          
max_steps_if_all_sig <- 60  

r <- rast(raster_path)

set.seed(42)
pts <- spatSample(r, size = min(sample_n, ncell(r)),
                  method = "random", na.rm = TRUE,
                  as.points = TRUE, values = TRUE)


val_name <- names(pts)[ncol(as.data.frame(pts))]

if (is.lonlat(r)) {
  pts_m <- project(pts, "EPSG:3857")
} else {
  pts_m <- pts
}

coords <- crds(pts_m, df = TRUE)
z      <- as.data.frame(pts_m)[[val_name]]


good <- is.finite(z)
coords <- coords[good, , drop = FALSE]
z      <- z[good]
if (nrow(coords) < 20) {
  stop(" ")
}


knn <- knearneigh(as.matrix(coords), k = 2)
nb_knn <- knn2nb(knn)

dlist <- nbdists(nb_knn, coords)

nn_min <- vapply(dlist, min, numeric(1))
base_dist <- median(nn_min, na.rm = TRUE)


compute_moran_band <- function(d_thr, coords, z, nsim_perm, zero.policy = TRUE) {
  nb <- dnearneigh(as.matrix(coords), d1 = 0, d2 = d_thr)
  
  lw <- nb2listw(nb, style = "W", zero.policy = zero.policy)
 
  degs <- sapply(nb, length)
  if (all(degs == 0)) {
    return(list(I = NA_real_, p = 1.0, mean_deg = 0, links = 0))
  }
  
  mc <- moran.mc(z, lw, nsim = nsim_perm, alternative = "two.sided",
                 zero.policy = zero.policy)
  list(I = as.numeric(mc$statistic),
       p = as.numeric(mc$p.value),
       mean_deg = mean(degs),
       links = sum(degs))
}

thresholds <- seq(base_dist, base_dist * n_steps, length.out = n_steps)

results <- lapply(thresholds, function(dthr) {
  out <- compute_moran_band(dthr, coords, z, nsim_perm)
  c(threshold_m = dthr,
    moran_I = out$I,
    p_value = out$p,
    mean_neighbors = out$mean_deg,
    links = out$links)
})
df <- as.data.frame(do.call(rbind, results))

if (all(df$p_value < alpha)) {
  thresholds2 <- seq(max(thresholds) + base_dist,
                     base_dist * max_steps_if_all_sig,
                     length.out = max(0, max_steps_if_all_sig - n_steps))
  if (length(thresholds2) > 0) {
    results2 <- lapply(thresholds2, function(dthr) {
      out <- compute_moran_band(dthr, coords, z, nsim_perm)
      c(threshold_m = dthr,
        moran_I = out$I,
        p_value = out$p,
        mean_neighbors = out$mean_deg,
        links = out$links)
    })
    df2 <- as.data.frame(do.call(rbind, results2))
    df  <- bind_rows(df, df2)
  }
}


idx_nonsig <- which(df$p_value >= alpha)[1]
if (!is.na(idx_nonsig)) {
  min_spacing <- df$threshold_m[idx_nonsig]
  note <- " p ≥ α （α=0.05）。"
} else {
  min_spacing <- tail(df$threshold_m, 1)
  note <- "  "
}

cat(sprintf("%d\n\n",
            min_spacing, note, nrow(coords)))


out_csv <- "moransI_distance_band_results.csv"
write.csv(df, out_csv, row.names = FALSE)

p <- ggplot(df, aes(threshold_m, moran_I)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "", y = "Moran's I",
       title = "") +
  theme_bw()
ggsave("moransI_distance_band_plot.png", p, width = 7, height = 5, dpi = 160)


p2 <- ggplot(df, aes(threshold_m, p_value)) +
  geom_point() + geom_line() +
  geom_hline(yintercept = alpha, linetype = "dashed") +
  labs(x = "distance", y = "p 值",
       title = "p ") +
  theme_bw()
ggsave("moransI_pvalue_vs_distance.png", p2, width = 7, height = 5, dpi = 160)


print(head(df, 10))
print(tail(df, 10))
cat(sprintf("n- moransI_pvalue_vs_distance.png\n",
            min_spacing, note, out_csv))