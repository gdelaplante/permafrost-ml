# All analysis choices belong here. Paths are relative to the project root.
permafrost_config <- function(root, profile = c("publication", "quick"),
                              output_dir = NULL, raster_dir = NULL) {
  profile <- match.arg(profile)
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  settings <- if (profile == "publication") {
    list(samples_per_class = 80000L, folds = 10L, repeats = 10L)
  } else {
    list(samples_per_class = 10000L, folds = 2L, repeats = 2L)
  }
  p <- function(...) file.path(root, ...)
  if (is.null(output_dir)) output_dir <- p("data/permafrost/outputs", profile)
  if (is.null(raster_dir)) raster_dir <- output_dir
  list(
    root = root, profile = profile, model = settings,
    output_dir = normalizePath(output_dir, winslash = "/", mustWork = FALSE),
    raster_dir = normalizePath(raster_dir, winslash = "/", mustWork = FALSE),
    seed = 20261105L, workers = 1L, ranger_threads = 1L,
    importance_seed = 123L, reference_seed = 42L, probability_seed = 123L,
    permutations = 5L, importance_per_class = 2000L,
    pdp_per_class = 1000L, pdp_grid = 35L,
    pdp_vars = c("aspect", "DEM", "difrad", "dirrad", "profcurv",
                 "tancurv", "slope", "veg", "snow", "hypso"),
    # Fixed class order also defines the integer codes in prediction.tif.
    class_levels = c("Ice-poor", "Ice-rich", "Unfrozen", "Very Ice-rich"),
    casino_radius_km = 15, north_row_ids = NULL,
    predictors = p("data/permafrost/predvars", c(
      aspect = "Aspect.tif", DEM = "DEMClipped.tif", difrad = "DiffuseRad.tif",
      dirrad = "DirectRad.tif", profcurv = "FocalStatProfileCurv.tif",
      tancurv = "FocalStatTangentCurv.tif", slope = "SlopeDeg.tif",
      veg = "vegetation_resampled.tif",
      snow = "HLS_Fmask_v1_1_snow_lengthT_winterYear1819_2425_Canada_yt_clipped_median.tif",
      hypso = "hypsometric_predictors_50km_MRDEM.tif")),
    predictor_names = c("aspect", "DEM", "difrad", "dirrad", "profcurv",
                        "tancurv", "slope", "veg", "snow", "hypso"),
    hypso_layer = "hypsometric_range50_m",
    training = p("data/permafrost/training/MineSitePermafrost_TetraTech_20170224.shp"),
    thermistors = p("data/permafrost/testing/YGS_PFT_LOCATIONS.CSV"),
    casino = p("data/permafrost/testing/casino_rast.tif"),
    # Compact GeoPackages can be built with prepare_permafrost.R.
    water = p("data/permafrost/context/waterbodies.gpkg"),
    streams = p("data/permafrost/context/watercourses.gpkg"),
    glacial = p("data/permafrost/context/glacial_limits.gpkg"),
    boundary = p("data/permafrost/context/boundary.gpkg"),
    climate_raster = p("data/permafrost/climate/ERALClim_bio1_1991-2020.tif"),
    climate_normals = p("data/permafrost/climate/eccc_normals_1991_2020.csv"),
    climate_stations = p("data/permafrost/climate/eccc_stations.csv")
  )
}
