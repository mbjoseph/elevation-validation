
get_dtm_dir <- function() {
  user <- system('echo $USER', intern = TRUE)
  dtm_dir <- paste0('/media/', user,
                    '/AOP-NEON1-4/D07/GRSM/2015/GRSM_L3/GRSM_Lidar/DTM')
  dtm_dir
}

find_dtm_files <- function(dtm_dir) {
  dtm_files <- list.files(dtm_dir)
  extensions <- gsub("^.*\\.","", dtm_files)
  dtm_files <- dtm_files[extensions == 'tif']
  dtm_files
}

get_extent <- function(file) {
  r <- raster(file)
  extent(r)
}

points_in_extent <- function(extent, x, y) {
  in_x <- x > extent@xmin & x < extent@xmax
  in_y <- y > extent@ymin & y < extent@ymax
  any(in_x & in_y)
}

# cycle through relevant rasters and try to fill in value from DTM
get_dtm_values <- function(dtms, spdf) {
  dtm_vals <- NA
  for (i in seq_along(dtms)) {
    r <- raster(dtms[i])
    extracted <- extract(r, spdf)
    missing <- is.na(extracted)
    dtm_vals[!missing] <- extracted[!missing]
  }
  dtm_vals
}
