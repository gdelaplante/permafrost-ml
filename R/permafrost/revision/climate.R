compare_climate <- function(cfg) {
  stations <- utils::read.csv(cfg$climate_stations, colClasses = c(climate_id = "character"))
  normals <- utils::read.csv(cfg$climate_normals, colClasses = c(climate_id = "character"))
  ids <- c("2100LRP", "2100301", "2100701")
  stations <- stations[match(ids, stations$climate_id), ]
  if (anyNA(stations$climate_id) || anyDuplicated(stations$climate_id)) {
    stop("Climate stations must contain one coordinate record for Dawson, Carmacks, and Mayo.")
  }
  points <- terra::vect(stations, geom = c("lon", "lat"), crs = "EPSG:4326")
  r <- terra::rast(cfg$climate_raster)
  points <- terra::project(points, r)
  result <- stations
  result$ERA5_Land_C <- terra::extract(r, points, ID = FALSE)[[1L]]
  result$ECCC_annual_C <- vapply(ids, function(id) {
    x <- normals[normals$climate_id == id & normals$period_of_record == "Normal" &
                   normals$period == "Year", "daily_average_c"]
    if (length(x) != 1L || !is.finite(x)) stop("Missing unique annual normal for ", id)
    x
  }, numeric(1))
  # Preserve the old calculation as an audit column: it averaged Jan-Dec AND Year.
  result$ECCC_legacy_mean_C <- vapply(ids, function(id) {
    mean(normals[normals$climate_id == id & normals$period_of_record == "Normal",
                 "daily_average_c"], na.rm = TRUE)
  }, numeric(1))
  result$Difference_annual_C <- result$ERA5_Land_C - result$ECCC_annual_C
  result$Difference_legacy_C <- result$ERA5_Land_C - result$ECCC_legacy_mean_C
  utils::write.csv(result, output_file(cfg, "climate_comparison.csv"), row.names = FALSE)
  print(result)
  invisible(result)
}
