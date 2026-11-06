fit_permafrost <- function(features, outcome, cfg) {
  set.seed(cfg$seed)
  # Sequential caret resampling avoids dependence on a user's parallel backend.
  control <- caret::trainControl(method = "repeatedcv", number = cfg$model$folds,
    repeats = cfg$model$repeats, verboseIter = FALSE, returnResamp = "final",
    # 100 resamples x several tuning candidates would retain hundreds of
    # millions of CV prediction rows. They are not used by downstream stages.
    savePredictions = FALSE, allowParallel = FALSE)
  on.exit(foreach::registerDoSEQ(), add = TRUE)
  res <- SAiVE::spatPredict(features = features, outcome = outcome,
    poly_sample = cfg$model$samples_per_class, trainControl = control,
    methods = "ranger", predict = FALSE, fastCompare = FALSE,
    thinFeatures = FALSE, n.cores = cfg$workers)
  assert_class_codes(res$best_model, cfg)
  # Never serialize res: training/testing contain live terra external pointers.
  # Export the non-spatial forest and ordinary tables as separate artifacts.
  saveRDS(res$best_model, output_file(cfg, "fitted_model.rds"))
  for (part in c("training", "testing")) {
    d <- as.data.frame(res[[part]])
    xy <- as.data.frame(terra::crds(res[[part]]))
    names(xy) <- c("x", "y")
    utils::write.csv(cbind(d, xy), output_file(cfg, paste0(part, "_samples.csv")),
                     row.names = FALSE)
  }
  writeLines(terra::crs(res$training), output_file(cfg, "sample_crs.wkt"))
  perf <- res$selected_model_performance
  utils::write.csv(as.data.frame(perf$table), output_file(cfg, "holdout_confusion.csv"),
                   row.names = FALSE)
  utils::write.csv(data.frame(statistic = names(perf$overall), value = unname(perf$overall)),
                   output_file(cfg, "holdout_overall.csv"), row.names = FALSE)
  utils::write.csv(data.frame(class = rownames(perf$byClass), perf$byClass, check.names = FALSE),
                   output_file(cfg, "holdout_by_class.csv"), row.names = FALSE)
  utils::write.csv(data.frame(
    metric = "Overall accuracy", value = unname(perf$overall["Accuracy"])),
    output_file(cfg, "holdout_accuracy_definitions.csv"), row.names = FALSE)
  writeLines(capture.output(print(res$best_model), print(perf)),
             output_file(cfg, "model_report.txt"))
  res
}

predict_permafrost <- function(features, res, cfg) {
  assert_class_codes(res$best_model, cfg)
  # Return explicit integer codes instead of relying on raster category offsets.
  predict_codes <- function(model, data, ...) {
    as.integer(factor(as.character(stats::predict(model, newdata = data)),
                      levels = model$levels))
  }
  prediction <- terra::predict(features, res$best_model, fun = predict_codes,
    na.rm = TRUE, cores = 1L, filename = output_file(cfg, "prediction_w_snow_hypso.tif"),
    overwrite = TRUE, wopt = list(datatype = "INT1U"))
  utils::write.csv(data.frame(code = seq_along(cfg$class_levels), class = cfg$class_levels,
    permafrost = as.integer(cfg$class_levels != "Unfrozen")),
    output_file(cfg, "prediction_classes.csv"), row.names = FALSE)
  prediction
}
