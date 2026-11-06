validate_predictions <- function(cfg, features = NULL) {
  output_dir <- cfg$output_dir
  prediction_rast <- terra::rast(raster_file(cfg, "prediction_w_snow_hypso.tif"))
  mess_rast <- terra::rast(raster_file(cfg, "MESS_snow_hypso.tif"))
  if (terra::is.lonlat(prediction_rast)) {
    stop("The prediction raster must use a projected CRS in metres.")
  }

  # ---- 1. Read and georeference the observations ---------------
  ygs_pts <- read.csv(cfg$thermistors,
                      skip = 1, stringsAsFactors = FALSE)
  stopifnot(all(c("LATITUDE", "LONGITUDE", "PERMAFROST") %in% names(ygs_pts)))
  ygs_pts$ROW_ID <- seq_len(nrow(ygs_pts))
  n_input <- nrow(ygs_pts)
  ygs_pts <- ygs_pts[, c("ROW_ID", "LATITUDE", "LONGITUDE", "PERMAFROST")]
  ygs_pts$LATITUDE <- suppressWarnings(as.numeric(ygs_pts$LATITUDE))
  ygs_pts$LONGITUDE <- suppressWarnings(as.numeric(ygs_pts$LONGITUDE))
  valid_coord <- complete.cases(ygs_pts[, c("LATITUDE", "LONGITUDE")]) &
    ygs_pts$LATITUDE >= -90 & ygs_pts$LATITUDE <= 90 &
    ygs_pts$LONGITUDE >= -180 & ygs_pts$LONGITUDE <= 180
  message(sprintf("Thermistor records: %d input; %d valid coordinates; %d removed.",
                  n_input, sum(valid_coord), sum(!valid_coord)))
  ygs_pts <- ygs_pts[valid_coord, , drop = FALSE]
  if (nrow(ygs_pts) == 0L) stop("No thermistors have valid coordinates.")

  # Yes = 1, No = 0; Undetermined (and any other status) = NA.
  status <- tolower(trimws(ygs_pts$PERMAFROST))
  ygs_pts$PERMAFROST_BIN <- match(status, c("no", "yes")) - 1L

  ygs_vect <- terra::vect(ygs_pts, geom = c("LONGITUDE", "LATITUDE"),
                          crs = "EPSG:4326", keepgeom = TRUE)
  ygs_vect <- terra::project(ygs_vect, prediction_rast)

  # ---- 2. Audit availability BEFORE any regional filtering -----
  # A raster extent is rectangular. Being inside it does not imply that
  # the cell has a prediction; and a valid raw prediction can be MESS-excluded.
  xy <- terra::crds(ygs_vect)
  ygs_vect$INSIDE_RASTER_EXTENT <- !is.na(
    terra::cellFromXY(prediction_rast, xy)
  )

  # Raw class codes: 1 ice-poor, 2 ice-rich, 3 permafrost-free,
  #                  4 very ice-rich.
  model_codes <- as.integer(terra::extract(prediction_rast, ygs_vect,
                                           ID = FALSE, raw = TRUE)[, 1])
  if (any(!is.na(model_codes) & !(model_codes %in% 1:4))) {
    stop("Unexpected model class codes (expected 1, 2, 3, 4).")
  }
  ygs_vect$MODEL_CLASS <- model_codes
  ygs_vect$MODEL_RAW <- ifelse(is.na(model_codes), NA_integer_,
                                as.integer(model_codes != 3L))

  # Extract MESS in its own CRS if necessary. This is a point-level test;
  # raster grid alignment is not needed for extraction at points.
  mess_points <- if (terra::same.crs(prediction_rast, mess_rast)) {
    ygs_vect
  } else {
    terra::project(ygs_vect, mess_rast)
  }
  ygs_vect$MESS <- as.numeric(terra::extract(mess_rast, mess_points,
                                           ID = FALSE)[[1]])
  ygs_vect$MODEL_PRED <- ifelse(!is.na(ygs_vect$MODEL_RAW) &
                                !is.na(ygs_vect$MESS) & ygs_vect$MESS >= 0,
                                ygs_vect$MODEL_RAW, NA_integer_)

  # Mutually exclusive status, independent of whether YGS is classified.
  coverage_status <- rep("Valid after MESS", nrow(ygs_vect))
  coverage_status[!is.na(ygs_vect$MODEL_RAW) & is.na(ygs_vect$MESS)] <-
    "MESS missing"
  coverage_status[!is.na(ygs_vect$MODEL_RAW) &
                    !is.na(ygs_vect$MESS) & ygs_vect$MESS < 0] <-
    "Excluded by MESS"
  coverage_status[is.na(ygs_vect$MODEL_RAW) &
                    ygs_vect$INSIDE_RASTER_EXTENT] <-
    "No prediction inside extent"
  coverage_status[!ygs_vect$INSIDE_RASTER_EXTENT] <-
    "Outside raster extent"
  ygs_vect$COVERAGE_STATUS <- coverage_status

  # Optional diagnostic if 'features' is still in memory from the full script:
  # report exactly which input layers are NA at each thermistor. The original
  # terra::predict(..., na.rm=TRUE) excludes cells missing ANY input layer.
  expected_features <- c("aspect", "DEM", "difrad", "dirrad", "profcurv",
                         "tancurv", "slope", "veg", "snow", "hypso")
  if (!is.null(features) &&
      inherits(features, "SpatRaster") &&
      all(expected_features %in% names(features))) {
    features_pts <- if (terra::same.crs(features, ygs_vect)) {
      ygs_vect
    } else {
      terra::project(ygs_vect, features)
    }
    fvals <- terra::extract(features[[expected_features]], features_pts,
                            ID = FALSE, raw = TRUE)
    missing_matrix <- is.na(fvals)
    colnames(missing_matrix) <- expected_features
    ygs_vect$N_MISSING_FEATURES <- rowSums(missing_matrix)
    ygs_vect$MISSING_FEATURES <- apply(missing_matrix, 1, function(z) {
      paste(expected_features[z], collapse = ";")
    })
  } else {
    message("'features' not available: skipping optional missing-predictor audit.")
  }

  # Assign all observations by the actual training and testing map footprints.
  # Region assignment never depends on whether a prediction is available.
  training_map <- terra::project(terra::vect(cfg$training), prediction_rast)
  casino <- terra::project(casino_polygons(cfg$casino), prediction_rast)
  in_training <- terra::is.related(ygs_vect, training_map, "intersects")
  in_casino <- terra::is.related(ygs_vect, casino, "intersects") & !in_training
  in_outside <- !in_training & !in_casino
  ygs_vect$REGION <- "Outside Coffee and Casino"
  ygs_vect$REGION[in_training] <- "Training footprint"
  ygs_vect$REGION[in_casino] <- "Casino footprint"
  regions <- list(
    "Training footprint" = ygs_vect[in_training, ],
    "Casino footprint" = ygs_vect[in_casino, ],
    "Outside Coffee and Casino" = ygs_vect[in_outside, ]
  )

  # ---- 4. Coverage report --------------------------------------
  # All coverage counts below refer to classified (Yes/No) observations;
  # these counts therefore reconcile to N_classified for each region.
  coverage_region <- function(v, region) {
    classified <- !is.na(v$PERMAFROST_BIN)
    n_classified <- sum(classified)
    statuses <- v$COVERAGE_STATUS
    count_status <- function(s) sum(classified & statuses == s)
    n_valid <- sum(classified & !is.na(v$MODEL_PRED))
    data.frame(
      Region = region,
      N_sites = nrow(v),
      N_classified = n_classified,
      Outside_raster_extent = count_status("Outside raster extent"),
      Missing_inside_extent = count_status("No prediction inside extent"),
      Excluded_by_MESS = count_status("Excluded by MESS"),
      MESS_missing = count_status("MESS missing"),
      N_unmasked_prediction = sum(classified & !is.na(v$MODEL_RAW)),
      N_valid_after_MESS = n_valid,
      Coverage_pct = if (n_classified) round(100 * n_valid / n_classified, 1)
                     else NA_real_
    )
  }

  coverage <- do.call(rbind, c(
    lapply(names(regions), function(region) {
      coverage_region(regions[[region]], region)
    }),
    list(coverage_region(ygs_vect, "All thermistors"))
  ))
  message("Coverage audit (counts refer to classified thermistors):")
  print(coverage, row.names = FALSE)
  write.csv(coverage, file.path(output_dir, "thermistors_coverage_by_region.csv"),
            row.names = FALSE)

  # ---- 5. Performance only on points with predictions ----------
  pct <- function(numerator, denominator) {
    if (denominator == 0) NA_real_ else round(100 * numerator / denominator, 1)
  }
  score_region <- function(v, region, field, domain) {
    obs <- v$PERMAFROST_BIN
    pred <- as.data.frame(v)[[field]]
    valid <- !is.na(obs) & !is.na(pred)
    obs <- obs[valid]
    pred <- pred[valid]

    tp <- sum(obs == 1L & pred == 1L)
    tn <- sum(obs == 0L & pred == 0L)
    fp <- sum(obs == 0L & pred == 1L)
    fn <- sum(obs == 1L & pred == 0L)
    sens <- pct(tp, tp + fn)
    spec <- pct(tn, tn + fp)
    metrics <- data.frame(
      Region = region,
      Domain = domain,
      N_classified = sum(!is.na(v$PERMAFROST_BIN)),
      N_compared = sum(valid),
      N_not_compared = sum(!is.na(v$PERMAFROST_BIN)) - sum(valid),
      Accuracy_pct = pct(tp + tn, sum(valid)),
      Sensitivity_pct = sens,
      Specificity_pct = spec,
      TP = tp, TN = tn, FP = fp, FN = fn
    )
    cm <- table(
      YGS = factor(obs, levels = 0:1, labels = c("No", "Yes")),
      Model = factor(pred, levels = 0:1, labels = c("No", "Yes"))
    )
    list(metrics = metrics, confusion = cm)
  }

  scores <- list()
  confusion_rows <- list()
  k <- 1L
  for (region in names(regions)) {
    for (domain in c("MESS-filtered", "Unmasked diagnostic")) {
      field <- if (domain == "MESS-filtered") "MODEL_PRED" else "MODEL_RAW"
      s <- score_region(regions[[region]], region, field, domain)
      scores[[k]] <- s$metrics
      cm_df <- as.data.frame(s$confusion, responseName = "N")
      cm_df$Region <- region
      cm_df$Domain <- domain
      confusion_rows[[k]] <- cm_df
      message(sprintf("%s | %s | n = %d", region, domain,
                      s$metrics$N_compared))
      print(s$confusion)
      k <- k + 1L
    }
  }
  performance <- do.call(rbind, scores)
  print(performance, row.names = FALSE)
  write.csv(performance, file.path(output_dir, "thermistors_performance_by_region.csv"),
            row.names = FALSE)
  write.csv(do.call(rbind, confusion_rows),
            file.path(output_dir, "thermistors_confusion_by_region.csv"),
            row.names = FALSE)
  write.csv(as.data.frame(ygs_vect),
            file.path(output_dir, "thermistors_point_comparison.csv"),
            row.names = FALSE)


            # ---- 6. Diagnostic comparison maps ---------------------------
  # Plot MESS-valid cells only. Distinguish true disagreement from
  # absent predictions, MESS exclusion, and unclassified observations.
  plot_comparison <- function(v, panel_title) {
    if (nrow(v) == 0L) {
      plot.new()
      title(main = paste(panel_title, "(no sites)"))
      return(invisible(NULL))
    }

    coords <- terra::crds(v)
    pad_m <- 7000
    map_ext <- terra::ext(
      min(coords[, 1]) - pad_m, max(coords[, 1]) + pad_m,
      min(coords[, 2]) - pad_m, max(coords[, 2]) + pad_m
    )
    plot(NA, xlim = as.vector(map_ext)[1:2],
         ylim = as.vector(map_ext)[3:4],
         asp = 1, axes = FALSE, xlab = "", ylab = "", main = panel_title)

    local_pred <- tryCatch(terra::crop(prediction_rast, map_ext),
                           error = function(e) NULL)
    if (!is.null(local_pred)) {
      local_bin <- terra::classify(
        local_pred,
        rcl = matrix(c(1, 1, 2, 1, 3, 0, 4, 1), ncol = 2, byrow = TRUE),
        others = NA
      )
      local_mess <- tryCatch(terra::crop(mess_rast, terra::ext(local_bin)),
                             error = function(e) NULL)
      if (!is.null(local_mess)) {
        if (!terra::compareGeom(local_bin, local_mess,
                                stopOnError = FALSE)) {
          local_mess <- terra::project(local_mess, local_bin, method = "near")
        }
        local_bin <- terra::ifel(!is.na(local_mess) & local_mess >= 0,
                                 local_bin, NA)
        n_cells <- terra::global(!is.na(local_bin), "sum", na.rm = TRUE)[1, 1]
        if (!is.na(n_cells) && n_cells > 0) {
          local_bin <- terra::as.factor(local_bin)
          levels(local_bin) <- data.frame(
            id = c(0L, 1L), category = c("Permafrost-free", "Frozen")
          )
          terra::plot(local_bin, add = TRUE, type = "classes",
                      col = c("#FFFFFF", "#BCE7FA"), colNA = NA,
                      legend = FALSE, axes = FALSE, maxcell = 1500000)
        }
      }
    }

    classified <- !is.na(v$PERMAFROST_BIN)
    comparable <- classified & !is.na(v$MODEL_PRED)
    agree <- which(comparable & v$PERMAFROST_BIN == v$MODEL_PRED)
    disagree <- which(comparable & v$PERMAFROST_BIN != v$MODEL_PRED)
    mess_excluded <- which(classified & !is.na(v$MODEL_RAW) &
                           is.na(v$MODEL_PRED))
    no_raster <- which(classified & is.na(v$MODEL_RAW))
    undetermined <- which(!classified)

      if (length(agree)) terra::points(v[agree, ], pch = 19,
                                     col = "#009E73", cex = 0.9)
    if (length(disagree)) terra::points(v[disagree, ], pch = 19,
                                        col = "#D55E00", cex = 0.9)
    if (length(mess_excluded)) terra::points(v[mess_excluded, ],
                                             pch = 17, col = "#B8860B", cex = 0.9)
    if (length(no_raster)) terra::points(v[no_raster, ],
                                         pch = 4, col = "#555555", cex = 0.9)
    if (length(undetermined)) terra::points(v[undetermined, ],
                                            pch = 1, col = "#999999", cex = 0.9)
    box()
    legend("bottomleft",
           legend = c("Agree", "Disagree", "MESS excluded/missing",
                      "No raster prediction", "Unclassified"),
           pch = c(19, 19, 17, 4, 1),
           col = c("#009E73", "#D55E00", "#B8860B", "#555555", "#999999"),
           bty = "n", cex = 0.68)
  }

  png(file.path(output_dir, "thermistors_regional_comparison.png"),
      width = 3300, height = 1500, res = 300)
  par(mfrow = c(1, 3), mar = c(1, 1, 2.3, 0.5))
  for (region in names(regions)) {
    plot_comparison(regions[[region]], region)
  }

  dev.off()

  local_prediction <- terra::crop(prediction_rast, terra::ext(casino))
  local_mess <- terra::crop(mess_rast, terra::ext(local_prediction))
  model <- mask_mess(local_prediction, local_mess)
  casino_rast <- terra::rasterize(casino, model, field = "pmfst_int")
  agreement <- binary_model(model) == casino_rast
  n_compared <- terra::global(!is.na(agreement), "sum", na.rm = TRUE)[1, 1]
  n_agree <- terra::global(agreement, "sum", na.rm = TRUE)[1, 1]
  casino_summary <- data.frame(N_compared = n_compared, N_agree = n_agree,
    Agreement_pct = if (n_compared > 0) 100 * n_agree / n_compared else NA_real_)
  write.csv(casino_summary, output_file(cfg, "casino_agreement.csv"), row.names = FALSE)
  invisible(list(coverage = coverage, performance = performance, casino = casino_summary))
}
