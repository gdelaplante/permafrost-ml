plot_partial_dependence <- function(res, cfg) {
  set.seed(cfg$probability_seed)

  # 1. Extract model and training data --------------------------

  model <- res$best_model
  train <- as.data.frame(res$training)

  response <- names(train)[1]
  predictors <- model$finalModel$forest$independent.variable.names

  stopifnot(all(predictors %in% names(train)))

  train <- train[
    complete.cases(train[, c(response, predictors)]),
    ,
    drop = FALSE
  ]

  train_x <- train[, predictors, drop = FALSE]

  # 2. Obtain a probability forest ------------------------------

  if (identical(model$finalModel$treetype,
                "Probability estimation")) {

    pdp_model <- model

  } else {

    # Refit once with probabilities enabled, using the
    # same predictors, observations, and tuned parameters.
    # No cross-validation or regional prediction.

    train_y <- factor(
      make.names(as.character(train[[response]]))
    )

    pdp_model <- caret::train(
      x = train_x,
      y = train_y,
      method = "ranger",
      trControl = caret::trainControl(
        method = "none",
        classProbs = TRUE
      ),
      tuneGrid = model$bestTune,
      num.trees = model$finalModel$num.trees,
      num.threads = cfg$ranger_threads
    )

  }

  # 3. Select predictors ----------------------------------------

  pdp_vars <- cfg$pdp_vars

  missing_vars <- setdiff(pdp_vars, names(train_x))
  if (length(missing_vars)) {
    stop(
      "Check predictor names: ",
      paste(missing_vars, collapse = ", "),
      "\nAvailable: ",
      paste(names(train_x), collapse = ", ")
    )
  }

  # 4. Sample reference observations ----------------------------
  # Balanced sample of up to 1,000 observations per class.
  # This reduces the cost of marginal predictions.

  set.seed(cfg$reference_seed)

  idx <- sample_by_class(train[[response]], cfg$pdp_per_class)
  reference <- train_x[idx, , drop = FALSE]

  # 5. Calculate partial dependence -----------------------------

  n_grid <- cfg$pdp_grid
  pdp_list <- list()
  k <- 1L

  for (v in pdp_vars) {

    # Use central 96% of the predictor distribution
    grid <- if (v == "veg") sort(unique(train_x[[v]])) else seq(
      quantile(train_x[[v]], 0.02, na.rm = TRUE),
      quantile(train_x[[v]], 0.98, na.rm = TRUE),
      length.out = n_grid
    )

    for (value in grid) {

      newdata <- reference
      newdata[[v]] <- value

      probs <- predict(
        pdp_model,
        newdata = newdata,
        type = "prob"
      )

      mean_probs <- colMeans(probs, na.rm = TRUE)

      # Convert radiation to kWh/m\u00b2 for display
      plot_value <- if (v %in% c("dirrad", "difrad")) value / 1000 else value

      pdp_list[[k]] <- data.frame(
        predictor = v,
        value = plot_value,
        class = names(mean_probs),
        mean_prob = as.numeric(mean_probs)
      )

      k <- k + 1L
    }
    message("Completed: ", v)
  }

  # Heads up: expect this to take a good long while to complete. You should see informative messages when each predictor is done.
  pdp_df <- do.call(rbind, pdp_list)

  write.csv(pdp_df, output_file(cfg, "Figure7_partial_dependence.csv"), row.names = FALSE)
  saveRDS(pdp_model, output_file(cfg, "probability_model.rds"))
  draw_partial_dependence(pdp_df, cfg$output_dir, pdp_vars)
  invisible(pdp_df)
}

# Source this file to define both functions. This function redraws a saved CSV
# without a model fit, terra objects, or recomputing marginal predictions.
draw_partial_dependence <- function(pdp_df, output_dir,
  pdp_vars = c("aspect", "DEM", "difrad", "dirrad", "profcurv", "tancurv",
               "slope", "veg", "snow", "hypso")) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (is.character(pdp_df) && length(pdp_df) == 1L) pdp_df <- read.csv(pdp_df)
  stopifnot(all(c("predictor", "value", "class", "mean_prob") %in% names(pdp_df)))
  display_names <- c(DEM = "Elevation (m)",
    dirrad = "Direct solar radiation (kWh/m\u00b2)",
    profcurv = "Profile curvature (1/m)", snow = "Snow-cover duration (days)",
    hypso = "Hypsometric position (m)", aspect = "Aspect (\u00b0)",
    difrad = "Diffuse solar radiation (kWh/m\u00b2)",
    tancurv = "Tangential curvature (1/m)", slope = "Slope (\u00b0)",
    veg = "Vegetation (land-cover code)")
  # 6. Format class and predictor labels ------------------------

  class_labels <- c(
    "Unfrozen" = "Permafrost-free",
    "Ice-poor" = "Ice-poor",
    "Ice-rich" = "Ice-rich",
    "Very Ice-rich" = "Very ice-rich",
    "Ice.poor" = "Ice-poor",
    "Ice.rich" = "Ice-rich",
    "Very.Ice.rich" = "Very ice-rich"
  )

  pdp_df$class_display <- factor(
    unname(class_labels[pdp_df$class]),
    levels = c(
      "Permafrost-free",
      "Ice-poor",
      "Ice-rich",
      "Very ice-rich"
    )
  )

  stopifnot(!anyNA(pdp_df$class_display))

  pdp_df$predictor_display <- factor(
    unname(display_names[pdp_df$predictor]),
    levels = unname(display_names[pdp_vars])
  )

  # 7. Publication-quality plot ---------------------------------

  class_colors <- c(
    "Permafrost-free" = "#009E73",
    "Ice-poor" = "#333333",
    "Ice-rich" = "#D55E00",
    "Very ice-rich" = "#0072B2"
  )

  p <- ggplot2::ggplot(
    pdp_df,
    ggplot2::aes(
      x = value,
      y = mean_prob,
      color = class_display
    )
  ) +
    ggplot2::geom_line(linewidth = 1.15) +
    ggplot2::facet_wrap(
      ~predictor_display,
      ncol = 2,
      scales = "free_x"
    ) +
    ggplot2::scale_color_manual(values = class_colors) +
    ggplot2::expand_limits(y = 0) +
    ggplot2::labs(
      x = NULL,
      y = "Mean predicted class probability",
      color = NULL
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 12),
      axis.title.y = ggplot2::element_text(size = 14),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 12),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "grey90",
        linewidth = 0.3
      )
    )


  # 8. Save results ---------------------------------------------

  ggplot2::ggsave(
    file.path(output_dir, "Figure7_PDP_w_snow+hypso.png"),
    p,
    width = 9,
    height = 13,
    dpi = 450,
    bg = "white"
  )

  ggplot2::ggsave(file.path(output_dir, "Figure7_PDP_w_snow+hypso.pdf"),
    p, width = 9, height = 13, bg = "white")
  invisible(p)
}
