plot_prediction_map <- function(cfg) {
  prediction <- terra::rast(raster_file(cfg, "prediction_w_snow_hypso.tif"))

  # Make NA values in 'prediction' where mess is negative
  mess <- terra::rast(raster_file(cfg, "MESS_snow_hypso.tif"))
  prediction <- mask_mess(prediction, mess)

  # Load the waterbodies vector file
  waterbodies <- terra::vect(cfg$water)
  # Clip the waterbodies to the extent of the prediction raster
  # Project waterbodies to the same CRS as prediction
  waterbodies <- terra::project(waterbodies, prediction)
  waterbodies_clipped <- terra::crop(waterbodies, prediction)

  # Make prediction NA where waterbodies are present
  prediction_clipped <- terra::mask(prediction, waterbodies_clipped, updatevalue = NA, inverse = TRUE)

  # prediction_clipped  lost its categorical levels, so reassign them
  prediction_clipped <- terra::as.factor(prediction_clipped)
  cls <- data.frame(id = c(1, 2, 3, 4),
                    category = c("Ice-poor", "Ice-rich", "Unfrozen", "Very ice rich"))
  levels(prediction_clipped) <- cls

  # Now let's add other things: Glacial limits, major projects, settlements.
  glacial_limits <- terra::vect(cfg$glacial)
  glacial_limits <- terra::project(glacial_limits, prediction_clipped)
  glacial_limits_clipped <- terra::crop(glacial_limits, prediction_clipped)
  # Drop the'GLACIAL_DESCRIPTION' = 'Unglaciated area' category, since it just adds noise to the map and isn't relevant to our study area
  glacial_limits_clipped <- glacial_limits_clipped[glacial_limits_clipped$GLACIAL_DESCRIPTION != "Unglaciated area",]


  # ---- Colours -------------------------------------------------
  pf_cols <- c(
    "#BCE7FA",  # Ice-poor
    "#00BFFF",  # Ice-rich
    "#FAFAF5",  # Permafrost-free
    "#084B9B"   # Very ice-rich
  )

  # Map directly from the ORIGINAL attribute values.
  # Approximate colours from your original figure.
  glacial_cols <- c(
    "Cordilleran and montane glacial features (ca. 200 ka)" = "#EB65E4",
    "Cordilleran and montane glacial features (ca. 22 ka)"  = "#65CEAD",
    "Cordilleran and montane glacial features (ca. 3 Ma)"   = "#FF3737"
  )

  gcol <- unname(glacial_cols[
    as.character(glacial_limits_clipped$GLACIAL_DESCRIPTION)
  ])

  if (anyNA(gcol)) {
    stop("Unmapped glacial categories: ",
         paste(unique(glacial_limits_clipped$GLACIAL_DESCRIPTION[
           is.na(gcol)]), collapse = "; "))
  }

  # ---- Other spatial layers ------------------------------------

  yt <- terra::vect(cfg$boundary)
  yt <- terra::project(yt, prediction_clipped)

  communities_df <- data.frame(
    name = c("Dawson", "Forty Mile", "Fort Selkirk"),
    lon = c(-139.4333, -140.5332, -137.3975),
    lat = c(64.0601, 64.4239, 62.7719)
  )

  mines_df <- data.frame(
    name = c("Coffee Gold Project", "Casino Project"),
    lon = c(-139.34044, -138.83065),
    lat = c(62.8893, 62.73887)
  )

  communities <- terra::project(
    terra::vect(communities_df, geom = c("lon", "lat"),
                crs = "EPSG:4326"),
    prediction_clipped
  )

  mines <- terra::project(
    terra::vect(mines_df, geom = c("lon", "lat"),
                crs = "EPSG:4326"),
    prediction_clipped
  )

  # ---- Background ----------------------------------------------

  bg_rast <- terra::rast(
    terra::ext(prediction_clipped), res = 1000, crs = terra::crs(prediction_clipped)
  )
  bg_rast[] <- 1

  # ---- Output --------------------------------------------------

  png(
    output_file(cfg, "prediction_map.png"),
    width = 2400,
    height = 3200,
    res = 300
  )

  par(mar = c(1.4, 1.2, 0.6, 1.4), family = "sans")

  # Background
  terra::plot(
    bg_rast,
    col = "grey98",
    legend = FALSE,
    axes = FALSE,
    box = TRUE,
    mar = c(2.2, 1.2, 0.6, 1.8)
  )

  # Waterbodies
  terra::plot(
    waterbodies_clipped,
    col = "grey78",
    border = NA,
    legend = FALSE,
    add = TRUE
  )

  # Permafrost predictions
  terra::plot(
    prediction_clipped,
    col = pf_cols,
    colNA = NA,
    type = "classes",
    maxcell = 5000000,
    legend = FALSE,
    axes = FALSE,
    add = TRUE
  )

  # Glacial hatching -- thin coloured lines
  terra::plot(
    glacial_limits_clipped,
    add = TRUE,
    col = gcol,
    border = NA,
    density = 15,
    angle = 45,
    lwd = 0.35,
    legend = FALSE
  )

  # Glacial boundaries -- slightly thicker
  terra::plot(
    glacial_limits_clipped,
    add = TRUE,
    col = NA,
    border = gcol,
    lwd = 0.9,
    legend = FALSE
  )

  # Yukon boundary
  terra::plot(
    yt,
    add = TRUE,
    col = NA,
    border = "black",
    lwd = 0.8
  )

  # ---- Graticule -----------------------------------------------

  grat <- terra::graticule(
    lon = -141:-137,
    lat = 62:65,
    crs = terra::crs(prediction_clipped)
  )

  grat <- terra::crop(grat, prediction_clipped)

  terra::plot(
    grat,
    add = TRUE,
    col = "grey50",
    lwd = 0.6,
    labels = TRUE,
    retro = TRUE,
    lab.loc = c(1, 2),
    # terra 1.8-54 reverses right-edge coordinates; an explicit label selection
    # makes its plotting method reverse the matching latitude labels as well.
    lab.lat = seq_len(sum(terra::vect(grat)$h)),
    lab.cex = 0.7,
    off.lon = 0.15,
    off.lat = 0.15
  )

  # ---- Settlements and projects --------------------------------

  comm_xy <- terra::crds(communities)
  mines_xy <- terra::crds(mines)

  points(comm_xy, pch = 21, bg = "firebrick2",
         col = "black", cex = 0.9)

  points(mines_xy, pch = 21, bg = "deepskyblue1",
         col = "black", cex = 0.9)

  text(comm_xy, labels = communities_df$name,
       pos = 1, offset = 0.35, cex = 0.95)

  text(mines_xy, labels = mines_df$name,
       pos = 1, offset = 0.35, cex = 0.95)

  # ---- Custom two-column legend -------------------------------

  # Normalized coordinates within the plotting region
  u <- par("usr")
  xx <- function(p) u[1] + p * (u[2] - u[1])
  yy <- function(p) u[3] + (p - 0.02) * (u[4] - u[3])

  lbl <- function(x, y, s, cex = 0.75, font = 1) {
    text(xx(x), yy(y), labels = s,
         adj = c(0, 0.5), cex = cex,
         font = font, xpd = NA)
  }

  swatch <- function(x, y, colour, hatch = FALSE) {
    rect(
      xx(x), yy(y - 0.007),
      xx(x + 0.022), yy(y + 0.007),
      col = colour,
      border = if (hatch) colour else "grey60",
      density = if (hatch) 22 else NULL,
      angle = 45,
      lwd = 0.7,
      xpd = NA
    )
  }

  # White legend panel
  rect(xx(0.53), yy(0.828),
       xx(0.995), yy(0.995),
       col = "white", border = "grey30",
       lwd = 0.8, xpd = NA)

  # Left column: locations
  points(xx(0.555), yy(0.976), pch = 21,
         bg = "firebrick2", col = "black",
         cex = 0.9, xpd = NA)
  lbl(0.577, 0.976, "Settlements")

  points(xx(0.555), yy(0.957), pch = 21,
         bg = "deepskyblue1", col = "black",
         cex = 0.9, xpd = NA)
  lbl(0.577, 0.957, "Major projects")

  # Left column: permafrost
  lbl(0.542, 0.931, "Permafrost Type",
      cex = 0.95)

  class_y <- c(0.907, 0.888, 0.869, 0.850)
  class_labels <- c(
    "Permafrost-free",
    "Ice-poor",
    "Ice-rich",
    "Very ice-rich"
  )
  class_cols <- pf_cols[c(3, 1, 2, 4)]

  for (i in seq_along(class_y)) {
    swatch(0.545, class_y[i], class_cols[i])
    lbl(0.578, class_y[i], class_labels[i],
        cex = 0.74)
  }

  # Right column: glacial limits
  lbl(0.778, 0.978, "Glacial Limits",
      cex = 0.98)

  glacial_y <- c(0.941, 0.900, 0.859)
  glacial_labels <- c(
    "Cordilleran and\nmontane (ca. 200 ka)",
    "Cordilleran and\nmontane (ca. 22 ka)",
    "Cordilleran and\nmontane (ca. 3 Ma)"
  )

  for (i in seq_along(glacial_y)) {
    swatch(0.780, glacial_y[i],
           unname(glacial_cols[i]), hatch = TRUE)
    lbl(0.812, glacial_y[i], glacial_labels[i],
        cex = 0.67)
  }

  dev.off()
  invisible(NULL)
}

plot_casino_map <- function(cfg) {
  # -------------------- Inputs and options ---------------------------
  model_path   <- raster_file(cfg, "prediction_w_snow_hypso.tif")
  mess_path    <- raster_file(cfg, "MESS_snow_hypso.tif")
  casino_path  <- cfg$casino
  water_path   <- cfg$water
  streams_path <- cfg$streams
  output_path  <- output_file(cfg, "casino_comparison_map_revised.png")

  # Use TRUE for the present manuscript's non-extrapolated predictions.
  # Set FALSE only as a diagnostic to see whether MESS explains grey areas.
  apply_mess <- TRUE

  map_width_m  <- 13800
  map_height_m <- 9560
  north_of_project_m <- 1450
  project_lon <- -138.83065
  project_lat <- 62.73887

  # Raster-based patterns are independent of base graphics polygon hatching.
  hatch_spacing_m <- 230  # Period of each diagonal-line family
  hatch_width_m   <- 20   # Line thickness on the ground
  stippling_spacing_m <- 90

  # -------------------- Map geometry -------------------------------
  model_all <- terra::rast(model_path)
  if (terra::is.lonlat(model_all)) {
    stop("Model CRS must be projected in metres, not longitude/latitude.")
  }

  project_pt <- terra::vect(
    data.frame(lon = project_lon, lat = project_lat),
    geom = c("lon", "lat"), crs = "EPSG:4326"
  )
  project_pt <- terra::project(project_pt, model_all)
  site_xy <- terra::crds(project_pt)[1, ]

  # Matches the Casino site placement in the reference figure:
  # about 47% across, 15% down from top.
  plot_extent <- terra::ext(
    site_xy[1] - 0.47 * map_width_m,
    site_xy[1] + 0.53 * map_width_m,
    site_xy[2] + north_of_project_m - map_height_m,
    site_xy[2] + north_of_project_m
  )

  # -------------------- Model binary classes -----------------------
  model <- terra::crop(model_all, plot_extent)
  if (!terra::hasValues(model)) stop("No model values in Casino map extent.")

  before_mess <- terra::global(!is.na(model), "sum", na.rm = TRUE)[1, 1]
  if (apply_mess) {
    mess <- terra::crop(terra::rast(mess_path), plot_extent)
    if (!terra::compareGeom(model, mess, stopOnError = FALSE)) {
      stop("MESS and model rasters differ in grid geometry. Align them first.")
    }
    model <- mask_mess(model, mess)
  }
  after_mess <- terra::global(!is.na(model), "sum", na.rm = TRUE)[1, 1]
  message(sprintf("Model valid cells before/after MESS: %s / %s (%.1f%% excluded)",
                  before_mess, after_mess,
                  if (before_mess > 0) 100 * (before_mess - after_mess) / before_mess else NA_real_))
  if (after_mess == 0) stop("No valid model predictions remain in map extent.")

  # Numeric model codes: 1 Ice-poor, 2 Ice-rich, 3 Permafrost-free,
  # 4 Very ice-rich. Class 0 = Permafrost-free; 1 = Frozen.
  model_bin <- terra::classify(
    model,
    rcl = matrix(c(1, 1, 2, 1, 3, 0, 4, 1),
                 ncol = 2, byrow = TRUE),
    others = NA
  )
  model_bin <- terra::as.factor(model_bin)
  levels(model_bin) <- data.frame(
    id = c(0L, 1L), category = c("Permafrost-free", "Frozen")
  )

  # -------------------- Casino reference classes -------------------

  # Load Casino raster and convert to polygons
  casino <- casino_polygons(casino_path)

  # Verify the existing binary classification
  stopifnot("pmfst_int" %in% names(casino))
  stopifnot(all(na.omit(casino$pmfst_int) %in% c(0, 1)))

  # Project and crop
  casino <- terra::project(casino, terra::crs(model_all))
  casino <- terra::crop(casino, plot_extent)

  # Rasterize onto the EXACT grid of the predicted model.
  # This is the key to plotting the independent reference as a visible overlay.
  casino_ref <- terra::rasterize(casino, model_bin, field = "pmfst_int")
  # For visual comparisons, do not hatch areas with no valid model prediction.
  casino_ref <- terra::mask(casino_ref, model_bin)

  # -------------------- Raster-aligned crosshatch -------------------
  # Construct diagonal strips from projected x/y coordinates, then
  # restrict strips to the Casino frozen reference class (1).
  # This is more deterministic than 'density'/'angle' on a SpatVector.
  rx <- terra::init(casino_ref, "x")
  ry <- terra::init(casino_ref, "y")

  stripes <- ((rx + ry) %% hatch_spacing_m < hatch_width_m) |
             ((rx - ry) %% hatch_spacing_m < hatch_width_m)
  hatch <- terra::ifel((casino_ref == 1) & stripes, 1L, NA)
  hatch <- terra::as.factor(hatch)
  levels(hatch) <- data.frame(id = 1L, category = "Casino frozen hatch")

  n_hatch <- terra::global(!is.na(hatch), "sum", na.rm = TRUE)[1, 1]

  message("Casino frozen hatch pixels: ", n_hatch)
  if (is.na(n_hatch) || n_hatch == 0) {
    stop("No Casino frozen hatch pixels were generated.")
  }

  # Stippling at regularly spaced 90-m points on Casino permafrost-free (0).
  dot_grid <- terra::rast(
    plot_extent, res = stippling_spacing_m, crs = terra::crs(model_all)
  )
  dot_ref <- terra::resample(casino_ref, dot_grid, method = "near")
  dot_mask <- terra::ifel(dot_ref == 0, 1L, NA)
  dot_pts <- terra::as.points(dot_mask, na.rm = TRUE)
  message("Casino permafrost-free stipple points: ", nrow(dot_pts))
  if (nrow(dot_pts) == 0) stop("No Casino permafrost-free stipple points generated.")

  # -------------------- Vector context ----------------------------
  water <- terra::vect(water_path)
  water <- terra::crop(terra::project(water, model_all), plot_extent)

  assert_files(streams_path)
  has_streams <- TRUE
  if (has_streams) {
    streams <- terra::vect(streams_path)
    streams <- terra::crop(terra::project(streams, model_all), plot_extent)
    has_streams <- nrow(streams) > 0
  }

  # -------------------- Figure appearance -------------------------
  bg_col          <- "#CCCCC3"  # Missing / outside valid model
  model_free_col  <- "#FFFFFF"
  model_ice_col   <- "#00A9E6"
  casino_ice_col  <- "#454545"
  casino_free_col <- "#DCA564"
  stream_col      <- "#B40000"

  bg <- terra::rast(plot_extent, nrows = 96, ncols = 138,
                    crs = terra::crs(model_all))
  bg[] <- 1

  png(output_path, width = 3000, height = 2400, res = 300)
  layout(matrix(c(1, 2), nrow = 2), heights = c(0.865, 0.135))

  # -------------------- Main map panel ----------------------------
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  terra::plot(bg, col = bg_col, axes = FALSE,
              legend = FALSE, box = FALSE, mar = c(0, 0, 0, 0))

  terra::plot(model_bin, add = TRUE, type = "classes",
              col = c(model_free_col, model_ice_col),
              colNA = NA, legend = FALSE, axes = FALSE, maxcell = 10000000)

  # Crosshatching is a dark, one-class raster with transparent NA cells.
  terra::plot(hatch, add = TRUE, type = "classes", col = casino_ice_col,
              colNA = NA, legend = FALSE, axes = FALSE, maxcell = 10000000)

  # Overlay reference permafrost-free dots.
  terra::points(dot_pts, pch = 20, col = casino_free_col, cex = 0.36)

  # Draw water after patterns so hatching does not cross waterbody polygons.
  if (nrow(water) > 0) {
    terra::plot(water, add = TRUE, col = "#999999", border = NA)
  }
  if (has_streams) {
    terra::plot(streams, add = TRUE, col = stream_col, lwd = 1.3)
  }

  points(site_xy[1], site_xy[2], pch = 21,
         bg = model_ice_col, col = "black", cex = 1.0)
  text(site_xy[1] + 125, site_xy[2] + 590,
       "Casino\nProject", adj = c(0, 0.5), cex = 1.25)
  box(lwd = 0.8)

  # -------------------- Legend panel ------------------------------
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  plot.new()
  plot.window(xlim = c(0, 100), ylim = c(0, 10), asp = NA)
  segments(0, 9.92, 100, 9.92, lwd = 0.7)

  text(0.2, 7.9, "Binary Model Output", adj = c(0, 0.5), cex = 0.96)
  text(0.2, 4.6, "Permafrost-free", adj = c(0, 0.5), cex = 0.77)
  text(0.2, 1.7, "Frozen", adj = c(0, 0.5), cex = 0.77)
  rect(18.9, 3.6, 22.4, 5.5, col = model_free_col, border = "grey70")
  rect(18.9, 0.7, 22.4, 2.6, col = model_ice_col, border = NA)

  text(27, 7.9, "Casino Permafrost Map", adj = c(0, 0.5), cex = 0.96)
  text(27, 4.6, "Permafrost-free", adj = c(0, 0.5), cex = 0.77)
  text(27, 1.7, "Frozen", adj = c(0, 0.5), cex = 0.77)
  rect(43, 3.6, 46.5, 5.5, col = "white", border = "grey75")
  for (a in seq(43.3, 46.2, by = 0.55)) {
    for (b in seq(3.9, 5.2, by = 0.43)) {
      points(a, b, pch = 20, col = casino_free_col, cex = 0.25)
    }
  }
  rect(43, 0.7, 46.5, 2.6, col = "white", border = "grey75")
  rect(43, 0.7, 46.5, 2.6, col = casino_ice_col,
       border = NA, density = 24, angle = 45, lwd = 0.5)
  rect(43, 0.7, 46.5, 2.6, col = casino_ice_col,
       border = NA, density = 24, angle = 135, lwd = 0.5)

  if (has_streams) {
    text(54.6, 3.6, "Streams", adj = c(0, 0.5), cex = 0.86)
    segments(62.8, 3.6, 67, 3.6, col = stream_col, lwd = 2.3)
  }

  text(74.7, 8.4, "N", cex = 0.95)
  polygon(c(74.7, 73.6, 74.7), c(7.0, 1.0, 2.6),
          col = "black", border = "black")
  polygon(c(74.7, 75.8, 74.7), c(7.0, 1.0, 2.6),
          col = "white", border = "black")

  scale_x <- 80.1
  scale_y <- 1.6
  scale_width <- 100 * (2000 / map_width_m)
  segments(scale_x, scale_y, scale_x + scale_width, scale_y,
           lwd = 2.8, lend = "butt")
  for (i in 0:2) {
    xpos <- scale_x + i * scale_width / 2
    segments(xpos, scale_y, xpos, scale_y + 1.8, lwd = 2.5)
    text(xpos, scale_y + 3.1, as.character(i), cex = 0.92)
  }
  text(scale_x + scale_width + 1.4, scale_y + 3.1,
       "Km", adj = c(0, 0.5), cex = 0.92)

  dev.off()
  message("Wrote: ", output_path)
  invisible(NULL)
}
