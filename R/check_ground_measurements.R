library(raster)
library(magrittr)
library(ggplot2)

get_dtm_dir <- function() {
  user <- system('echo $USER', intern = TRUE)
  dtm_dir <- paste0('/media/', user,
                    '/AOP-NEON1-4/D07/GRSM/2015/GRSM_L3/GRSM_Lidar/DTM')
  dtm_dir
}
dtm_dir <- get_dtm_dir()
dtm_files <- list.files(dtm_dir)
extensions <- gsub("^.*\\.","", dtm_files)
dtm_files <- dtm_files[extensions == 'tif']

# find extent for each raster
get_extent <- function(file) {
  r <- raster(file)
  extent(r)
}

points_in_extent <- function(extent, x, y) {
  in_x <- x > extent@xmin & x < extent@xmax
  in_y <- y > extent@ymin & y < extent@ymax
  any(in_x & in_y)
}

full_dtm_paths <- file.path(dtm_dir, dtm_files)
extents <- lapply(full_dtm_paths, get_extent)

truth <- read.csv('data/GRSM_LiDAR_Val_ITRF00_Geoid12A_UTM17N.csv')
coords <- cbind(truth$Easting, truth$Northing)
sp <- SpatialPoints(coords,
                    proj4string = raster(full_dtm_paths[1]) %>% crs)
spdf <- SpatialPointsDataFrame(sp, truth)


relevant <- lapply(extents,
                   FUN = points_in_extent,
                   x = truth$Easting,
                   y = truth$Northing) %>%
  unlist()

relevant_dtms <- full_dtm_paths[relevant]

# cycle through relevant rasters and try to fill in value from DTM
truth$dtm_val <- NA
for (i in seq_along(relevant_dtms)) {
  r <- raster(relevant_dtms[i])
  extracted <- extract(r, spdf)
  non_na <- !is.na(extracted)
  truth$dtm_val[non_na] <- extracted[non_na]
}

# compare values
truth %>%
  ggplot(aes(x = Elevation, y = dtm_val)) +
  geom_point(size = 3) +
  geom_abline(intercept = 0, slope = 1) +
  ylab('DTM value')
