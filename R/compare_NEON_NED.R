# compare NEON DTMs to NED
library(raster)

# 1. loop over rasters
source('R/check_ground_measurements.R')

# load NED
ned <- raster('data/GRSM_DEM_USGS_UTM.tif')

dtm_dir <- get_dtm_dir()
dtm_files <- find_dtm_files(dtm_dir)
full_dtm_paths <- file.path(dtm_dir, dtm_files)

# subset the dtms
n_to_use <- 50
keep <- sample(length(full_dtm_paths), size = n_to_use)

full_dtm_paths <- full_dtm_paths[keep]

NAvec <- rep(NA, length(dtm_files))
res <- data.frame(intercept = NAvec,
                  slope = NAvec,
                  residual_sd = NAvec)

pb <- txtProgressBar(max = length(full_dtm_paths))
for (i in seq_along(full_dtm_paths)) {
  r <- raster(full_dtm_paths[i])
  all_na <- all(is.na(values(r)))
  if (all_na) {
    next
  } else {
    pts <- rasterToPoints(r, spatial = TRUE)
    ned_vals <- extract(ned, pts, sp = TRUE)
    model <- lm(ned_vals@data[, 1] ~ ned_vals@data[, 2])
    res[i, c('intercept', 'slope')] <- coef(model)
    res$residual_sd[i] <- sigma(model)
  }
  setTxtProgressBar(pb, i)
}
close(pb)

complete_res <- res[complete.cases(res), ]
saveRDS(complete_res, 'res.rds')
