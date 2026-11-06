input_paths <- function(cfg, stages) {
  paths <- character()
  if (any(stages %in% c("model", "predict", "importance", "pdp", "mess"))) {
    paths <- c(paths, cfg$predictors, cfg$training)
  }
  if ("validation" %in% stages) paths <- c(paths, cfg$training, cfg$thermistors, cfg$casino)
  if ("maps" %in% stages) paths <- c(paths, cfg$casino, cfg$water, cfg$streams,
                                   cfg$glacial, cfg$boundary, cfg$predictors[10L], cfg$thermistors,
                                   cfg$training, cfg$external_probability, cfg$external_extent)
  if ("climate" %in% stages) paths <- c(paths, cfg$climate_raster,
                                      cfg$climate_normals, cfg$climate_stations)
  unique(paths)
}

check_predictor_metadata <- function(cfg) {
  rs <- lapply(cfg$predictors, terra::rast)
  names(rs) <- cfg$predictor_names
  if (!cfg$hypso_layer %in% names(rs$hypso)) stop("Required hypsometric layer is absent.")
  rs$hypso <- rs$hypso[[cfg$hypso_layer]]
  for (v in setdiff(names(rs), "snow")) {
    if (terra::nlyr(rs[[v]]) != 1L ||
        !terra::compareGeom(rs[[v]], rs$DEM, stopOnError = FALSE)) {
      stop("Predictor '", v, "' must match the DEM grid. Prepare it before running.")
    }
  }
  if (!nzchar(terra::crs(rs$snow))) stop("Snow raster is missing its CRS.")
  outcome <- terra::vect(cfg$training)
  if (!"Type" %in% names(outcome)) stop("Training polygons need a Type column.")
  if (anyNA(outcome$Type) || !setequal(unique(as.character(outcome$Type)), cfg$class_levels)) {
    stop("Training Type labels do not match the configured four classes.")
  }
  invisible(TRUE)
}

load_predictors <- function(cfg) {
  check_predictor_metadata(cfg)
  features <- lapply(cfg$predictors, terra::rast)
  names(features) <- cfg$predictor_names
  features$hypso <- features$hypso[[cfg$hypso_layer]]
  # project() with a raster template sets CRS, resolution, extent, and alignment.
  # A second resampling of this result is unnecessary.
  if (!terra::compareGeom(features$snow, features$DEM, stopOnError = FALSE)) {
    features$snow <- terra::project(features$snow, features$DEM, method = "bilinear",
      filename = output_file(cfg, "snow_dem_grid.tif"), overwrite = TRUE)
  }
  features$snow <- terra::mask(features$snow, features$DEM,
    filename = output_file(cfg, "snow_masked.tif"), overwrite = TRUE)
  for (v in names(features)) names(features[[v]]) <- v
  features <- terra::rast(features)
  names(features) <- cfg$predictor_names
  outcome <- terra::vect(cfg$training)[, "Type"]
  outcome$Type <- factor(outcome$Type, levels = cfg$class_levels)
  outcome <- terra::project(outcome, features)
  list(features = features, outcome = outcome)
}
