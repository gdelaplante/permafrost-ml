calculate_mess <- function(features, outcome, cfg) {
  # rast(existing_spatRaster) creates an empty raster template and drops values.
  if (!inherits(features, "SpatRaster")) features <- terra::rast(features)

  # Extract the predictor values at the locations of outcome
  outcome <- terra::project(outcome, features) # Project outcome to match features
  predvals_coffee <- terra::crop(features, outcome)
  predvals_coffee <- terra::mask(predvals_coffee, outcome)
  predvals_coffee <- terra::as.data.frame(predvals_coffee, xy = TRUE, na.rm = TRUE)


  # install.packages("dismo")
  # With dismo, first argument is a raster, second is a matrix/data.frame
  # Convert predvals_coffee to a raster::stack for input to dismo::mess
  # Drop vegetation, dismo::mess doesn't play well with categorical variables
  features_stack <- raster::stack(terra::subset(features, c("DEM", "slope", "aspect", "tancurv", "profcurv", "difrad", "dirrad", "snow", "hypso")))
  predvals_coffee <- predvals_coffee[, c("x", "y", "DEM", "slope", "aspect", "tancurv", "profcurv", "difrad", "dirrad", "snow", "hypso")]
  # Expect warnings about 'no non-missing arguments' because of NA values
  dismo <- dismo::mess(features_stack, predvals_coffee[, c(3:11)],
    filename = output_file(cfg, "MESS_snow_hypso.tif"), overwrite = TRUE) # Leave out x and y columns

  # Save dismo as a raster
  # dismo <- raster::raster(output_file(cfg, "MESS_snow_hypso.tif")) # reload to make sure it's a raster object

  # Turn it back to a terra object for easier plotting
  dismo <- terra::rast(dismo)


  # define colour-blind friendly palettes
  mess_cols <- grDevices::hcl.colors(100, "Viridis")
  neg_cols  <- grDevices::hcl.colors(100, "Inferno")

  # negative raster
  neg <- dismo
  neg[neg >= 0] <- NA

  # proportion negative
  prop_neg <- terra::global(dismo < 0, "sum", na.rm = TRUE)[1, 1] /
    terra::global(!is.na(dismo), "sum", na.rm = TRUE)[1, 1]

  # publication figure for MESS values
  png(
    output_file(cfg, "mess_publication_figure.png"),
    width = 3000,
    height = 1000,
    res = 300
  )

  par(
    mfrow = c(1, 3), # 1 row, 3 columns
    oma = c(0, 0, 2, 0) # outer margins (bottom, left, top, right)
  )

  # 1. Full MESS raster (left panel)
  terra::plot(
    dismo,
    col = mess_cols,
    axes = FALSE,
    box = FALSE,
    main = "",
  )
  mtext(
    "A: MESS values\n",
    side = 3,
    line = 2,
    cex = 1
  )

  # 2. Histogram (middle panel)
  set.seed(cfg$seed)
  terra::hist(
    dismo, maxcell = 1000000,
    breaks = 50,
    col = "grey75",
    border = "grey35",
    main = "",
    xlab = "MESS values",
    ylab = "Frequency (sampled cells)"
  )
  # Add line at 0 to show the threshold for extrapolation
  abline(v = 0, lty = 2, lwd = 2)
  mtext(
    "B: Distribution\n",
    side = 3,
    line = 2,
    cex = 1
  )

  # 3. Negative MESS values only (right panel)
  terra::plot(
    neg,
    col = neg_cols,
    axes = FALSE,
    box = FALSE,
    main = ""
  )

  mtext(
    paste0("C: Negative MESS values\n", round(prop_neg * 100, 1), "% of cells"),
    side = 3,
    line = 2,
    cex = 1
  )

  dev.off()

  write.csv(data.frame(proportion_negative = prop_neg),
            output_file(cfg, "MESS_summary.csv"), row.names = FALSE)
  invisible(dismo)
}
