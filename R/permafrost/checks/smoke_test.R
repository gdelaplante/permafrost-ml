# Small integration check of the live spatPredict -> raster -> interpretation path.
# This uses synthetic data and does not estimate publication performance.
# Run from the project root with Rscript --vanilla R/permafrost/checks/smoke_test.R
source("R/permafrost/permafrost_model_revision.R")
m <- environment(run_permafrost)$modules
checks_dir <- file.path(getwd(), "data/permafrost/outputs/checks")
dir.create(checks_dir, recursive = TRUE, showWarnings = FALSE)
cfg <- m$permafrost_config(getwd(), "quick",
  output_dir = tempfile("smoke_", tmpdir = checks_dir))
dir.create(cfg$output_dir)
cfg$model <- list(samples_per_class = 50L, folds = 2L, repeats = 1L)
cfg$importance_per_class <- 20L
cfg$permutations <- 2L
cfg$pdp_per_class <- 10L
cfg$pdp_grid <- 3L
options(ranger.num.threads = 1L)
Sys.setenv(R_RANGER_NUM_THREADS = "1")
m$require_packages(c("terra", "SAiVE", "caret", "ranger", "ggplot2", "raster", "dismo"))

# Single-member classes must never select an unrelated row through sample().
set.seed(42)
stopifnot(setequal(m$sample_by_class(c("a", "a", "b"), 10L), 1:3))
base <- terra::rast(nrows = 40, ncols = 40, xmin = 0, xmax = 4000,
                    ymin = 0, ymax = 4000, crs = "EPSG:3579")
xy <- terra::crds(base)
set.seed(cfg$seed)
features <- lapply(seq_along(cfg$predictor_names), function(i) {
  r <- base
  terra::values(r) <- if (cfg$predictor_names[i] == "veg") {
    rep(1:4, length.out = terra::ncell(r))
  } else xy[, 1] / (i + 1) + xy[, 2] / (11 - i) + stats::rnorm(nrow(xy))
  names(r) <- cfg$predictor_names[i]
  r
})
features <- terra::rast(features)
names(features) <- cfg$predictor_names
classes <- base
terra::values(classes) <- 1L + as.integer(xy[, 1] > 2000) + 2L * as.integer(xy[, 2] > 2000)
outcome <- terra::as.polygons(classes, aggregate = TRUE)
outcome$Type <- factor(cfg$class_levels[as.data.frame(outcome)[[1L]]], levels = cfg$class_levels)
outcome <- outcome[, "Type"]
# Exercise the disk-backed input loader as well as the model stages.
fixture_dir <- file.path(cfg$output_dir, "fixture")
dir.create(fixture_dir)
fixture_cfg <- cfg
fixture_cfg$predictors <- file.path(fixture_dir, paste0(cfg$predictor_names, ".tif"))
for (i in seq_along(cfg$predictor_names)) {
  r <- features[[i]]
  if (cfg$predictor_names[i] == "hypso") names(r) <- cfg$hypso_layer
  terra::writeRaster(r, fixture_cfg$predictors[i], datatype = "FLT8S")
}
fixture_cfg$training <- file.path(fixture_dir, "training.gpkg")
terra::writeVector(outcome, fixture_cfg$training)
loaded <- m$load_predictors(fixture_cfg)
stopifnot(identical(names(loaded$features), cfg$predictor_names),
          # The masked snow intermediate uses terra's default float32 storage.
          isTRUE(all.equal(terra::values(loaded$features), terra::values(features), tolerance = 1e-6)))
features <- loaded$features
outcome <- loaded$outcome
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
res <- m$run_stage("model", cfg, m$fit_permafrost(features, outcome, cfg))
stopifnot(inherits(res$training, "SpatVector"), nrow(as.data.frame(res$testing)) > 0L)
prediction <- m$run_stage("predict", cfg, m$predict_permafrost(features, res, cfg))
stopifnot(all(terra::freq(prediction)$value %in% 1:4))
portable <- readRDS(m$output_file(cfg, "fitted_model.rds"))
stopifnot(identical(as.character(predict(portable, as.data.frame(res$testing))),
                    as.character(predict(res$best_model, as.data.frame(res$testing)))))
repeat_cfg <- cfg
repeat_cfg$output_dir <- file.path(cfg$output_dir, "repeat")
dir.create(repeat_cfg$output_dir)
res_again <- m$run_stage("repeat_model", repeat_cfg, m$fit_permafrost(features, outcome, repeat_cfg))
stopifnot(identical(as.data.frame(res$training), as.data.frame(res_again$training)),
          identical(terra::crds(res$training), terra::crds(res_again$training)),
          identical(res$best_model$bestTune, res_again$best_model$bestTune),
          identical(res$selected_model_performance$table, res_again$selected_model_performance$table))
m$run_stage("importance", cfg, m$plot_importance(res, cfg))
m$run_stage("pdp", cfg, m$plot_partial_dependence(res, cfg))
mess <- m$run_stage("mess", cfg, m$calculate_mess(features, outcome, cfg))
stopifnot(terra::compareGeom(prediction, mess, stopOnError = FALSE))
stopifnot(is.null(grDevices::dev.list()))
stopifnot(!file.exists(m$output_file(cfg, "res_w_snow_hypso.rds")))
cat("SMOKE TEST PASSED. Synthetic outputs:", cfg$output_dir, "\n")
