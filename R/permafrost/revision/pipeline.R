run_pipeline <- function(cfg, stages, check_only = FALSE, checksums = TRUE) {
  allowed <- c("model", "predict", "importance", "pdp", "mess", "validation", "maps", "climate")
  if (!length(stages) || any(!stages %in% allowed)) stop("Unknown or empty stage selection.")
  stages <- allowed[allowed %in% stages]
  needs_model <- any(stages %in% c("predict", "importance", "pdp"))
  if (needs_model && !"model" %in% stages) {
    stop("predict, importance and pdp require model in the same run; res cannot be reloaded.")
  }
  packages <- "terra"
  if (any(stages %in% c("model", "predict", "importance", "pdp"))) {
    packages <- c(packages, "SAiVE", "caret", "ranger", "foreach", "doParallel")
  }
  if (any(stages %in% c("importance", "pdp"))) packages <- c(packages, "ggplot2")
  if ("mess" %in% stages) packages <- c(packages, "raster", "dismo")
  require_packages(unique(packages))
  paths <- input_paths(cfg, stages)
  shapes <- paths[grepl("\\.shp$", paths, ignore.case = TRUE)]
  for (p in shapes) paths <- c(paths, paste0(tools::file_path_sans_ext(p), c(".dbf", ".shx", ".prj")))
  assert_files(paths)
  if (any(stages %in% c("model", "predict", "importance", "pdp", "mess"))) {
    check_predictor_metadata(cfg)
  }
  # Standalone map/validation runs must explicitly point at a prior run directory.
  if (any(stages %in% c("validation", "maps"))) {
    if (!"predict" %in% stages) paths <- c(paths, raster_file(cfg, "prediction_w_snow_hypso.tif"))
    if (!"mess" %in% stages) paths <- c(paths, raster_file(cfg, "MESS_snow_hypso.tif"))
    assert_files(paths)
  }
  if (check_only) {
    message("Preflight passed for ", cfg$profile, ": ", paste(stages, collapse = ", "),
            ". No model was fitted and no raster cells were processed.")
    return(invisible(cfg))
  }
  if (dir.exists(cfg$output_dir) && length(list.files(cfg$output_dir, all.files = TRUE, no.. = TRUE))) {
    stop("The output directory is not empty. Choose a new output_dir to preserve existing files and provenance.")
  }
  dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
  old_options <- options(ranger.num.threads = cfg$ranger_threads)
  old_threads <- Sys.getenv("R_RANGER_NUM_THREADS", unset = NA_character_)
  Sys.setenv(R_RANGER_NUM_THREADS = as.character(cfg$ranger_threads))
  old_rng <- RNGkind()
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    options(old_options)
    if (is.na(old_threads)) Sys.unsetenv("R_RANGER_NUM_THREADS")
    else Sys.setenv(R_RANGER_NUM_THREADS = old_threads)
    do.call(RNGkind, as.list(old_rng))
    if (had_seed) assign(".Random.seed", old_seed, envir = .GlobalEnv)
    else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(cfg$seed)
  cfg$stages <- stages
  # Per-run metadata is written even when a later stage fails.
  dput(cfg, file = output_file(cfg, "run_config.R"))
  saveRDS(cfg, output_file(cfg, "run_config.rds"))
  utils::write.csv(file_manifest(paths, cfg$root, checksums),
                   output_file(cfg, "input_manifest.csv"), row.names = FALSE)
  code <- list.files(file.path(cfg$root, "R/permafrost/revision"), "\\.R$", full.names = TRUE)
  code <- c(code, file.path(cfg$root, "R/permafrost/permafrost_model_revision.R"))
  utils::write.csv(file_manifest(code, cfg$root), output_file(cfg, "code_manifest.csv"), row.names = FALSE)
  writeLines("running", output_file(cfg, "run_status.txt"))
  success <- FALSE
  on.exit({
    writeLines(if (success) "complete" else "failed", output_file(cfg, "run_status.txt"))
    writeLines(capture.output(sessionInfo()), output_file(cfg, "sessionInfo.txt"))
  }, add = TRUE)
  record_environment(cfg)
  result <- list(config = cfg)
  if (any(stages %in% c("model", "predict", "importance", "pdp", "mess"))) {
    inputs <- run_stage("inputs", cfg, load_predictors(cfg))
    result$features <- inputs$features
    result$outcome <- inputs$outcome
  }
  if ("model" %in% stages) result$res <- run_stage("model", cfg,
    fit_permafrost(result$features, result$outcome, cfg))
  if ("predict" %in% stages) result$prediction <- run_stage("predict", cfg,
    predict_permafrost(result$features, result$res, cfg))
  if ("importance" %in% stages) result$importance <- run_stage("importance", cfg,
    plot_importance(result$res, cfg))
  if ("pdp" %in% stages) result$pdp <- run_stage("pdp", cfg,
    plot_partial_dependence(result$res, cfg))
  if ("mess" %in% stages) result$mess <- run_stage("mess", cfg,
    calculate_mess(result$features, result$outcome, cfg))
  if ("validation" %in% stages) result$validation <- run_stage("validation", cfg,
    validate_predictions(cfg, result$features))
  if ("maps" %in% stages) run_stage("maps", cfg, {
    plot_prediction_map(cfg)
    plot_casino_map(cfg)
  })
  if ("climate" %in% stages) result$climate <- run_stage("climate", cfg, compare_climate(cfg))
  success <- TRUE
  message("Finished. Outputs: ", cfg$output_dir)
  invisible(result)
}
