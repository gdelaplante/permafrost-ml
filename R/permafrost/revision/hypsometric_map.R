# Source this file to define the standalone SI map function; no map runs on source.
plot_hypsometric_predictor <- function(root = getwd(), output_dir = NULL,
                                     maxcell = 2000000L) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("The terra package is required.")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (is.null(output_dir)) output_dir <- file.path(root, "data/permafrost/outputs/SI")
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(root, "data/permafrost/predvars/hypsometric_predictors_50km_MRDEM.tif")
  band <- "hypsometric_range50_m" # Same band selected in revision/config.R.
  h <- terra::rast(path)[[band]]
  projects <- terra::vect(data.frame(
    name = c("Coffee", "Casino"), lon = c(-139.34044, -138.83065),
    lat = c(62.8893, 62.73887)), geom = c("lon", "lat"), crs = "EPSG:4326")
  projects <- terra::project(projects, h)
  draw <- function() {
    previous <- options(scipen = 999)
    on.exit(options(previous), add = TRUE)
    terra::plot(h, col = grDevices::hcl.colors(100, "Viridis"), maxcell = maxcell,
      main = "Hypsometric predictor", xlab = "Easting (m; EPSG:3579)",
      ylab = "Northing (m)", plg = list(title = "m"), mar = c(4, 5, 3, 5))
    terra::points(projects, pch = 21, bg = "white", col = "black", cex = 1.1)
    terra::text(projects, labels = projects$name, halo = TRUE, pos = 4, cex = 0.8)
  }
  png_path <- file.path(output_dir, "FigureS3_hypsometric_predictor.png")
  pdf_path <- file.path(output_dir, "FigureS3_hypsometric_predictor.pdf")
  grDevices::png(png_path, width = 2400, height = 2800, res = 300)
  tryCatch(draw(), finally = grDevices::dev.off())
  grDevices::pdf(pdf_path, width = 8, height = 9.33)
  tryCatch(draw(), finally = grDevices::dev.off())
  message("Hypsometric map: ", png_path)
  invisible(list(png = png_path, pdf = pdf_path, band = band))
}
