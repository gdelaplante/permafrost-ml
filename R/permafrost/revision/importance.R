plot_importance <- function(res, cfg) {
  # Variable importance plots #################

  model <- res$best_model
  test <- as.data.frame(res$testing)

  # spatPredict stores the response in the first column
  response <- names(test)[1]

  # Get the actual predictors used by the trained ranger model
  predictors <- model$finalModel$forest$independent.variable.names
  if (is.null(predictors)) {
    predictors <- setdiff(names(test), response)
  }

  # Remove incomplete observations
  test <- test[
    complete.cases(test[, c(response, predictors), drop = FALSE]),
    ,
    drop = FALSE
  ]

  # Optional: reduce testing sample for speed, up to 2,000 per class
  set.seed(cfg$reference_seed)
  idx <- sample_by_class(test[[response]], cfg$importance_per_class)
  test <- test[idx, , drop = FALSE]

  x <- test[, predictors, drop = FALSE]
  truth <- as.character(test[[response]])
  classes <- model$levels

  stopifnot(all(classes %in% truth))

  # One-versus-rest balanced accuracy for each class
  class_ba <- function(pred, truth, cl) {
    positive <- truth == cl

    sensitivity <- mean(pred[positive] == cl)
    specificity <- mean(pred[!positive] != cl)

    (sensitivity + specificity) / 2
  }

  # Baseline performance without shuffling
  pred0 <- as.character(predict(model, newdata = x))
  baseline <- setNames(
    vapply(classes, function(cl) {
      class_ba(pred0, truth, cl)
    }, numeric(1)),
    classes
  )

  # Permutation importance: repeat each shuffle 5 times
  set.seed(cfg$importance_seed)
  n_perm <- cfg$permutations
  results <- vector("list", length(predictors) * n_perm)
  k <- 1L

  for (v in predictors) {
    for (j in seq_len(n_perm)) {
      xp <- x
      xp[[v]] <- xp[[v]][sample.int(nrow(xp))]

      pred_perm <- as.character(predict(model, newdata = xp))

      loss <- vapply(classes, function(cl) {
        baseline[[cl]] - class_ba(pred_perm, truth, cl)
      }, numeric(1))

      results[[k]] <- data.frame(
        class = classes,
        predictor = v,
        repeat_id = j,
        importance = as.numeric(loss)
      )

      k <- k + 1L
    }
  }

  # Average across permutations
  imp <- aggregate(
    importance ~ class + predictor,
    data = do.call(rbind, results),
    FUN = mean
  )

  # Normalize all four panels to the SAME global maximum
  # Negative estimates are displayed as zero
  positive_imp <- pmax(imp$importance, 0)
  stopifnot(max(positive_imp) > 0)
  imp$relative <- 100 * positive_imp / max(positive_imp)

  # Readable labels: customize for your predictor names
  labels <- c(
    aspect = "Aspect",
    DEM = "Elevation",
    difrad = "Diffuse solar radiation",
    dirrad = "Direct solar radiation",
    profcurv = "Profile curvature",
    tancurv = "Tangential curvature",
    slope = "Slope",
    veg = "Vegetation",
    snow = "Snow cover duration",
    hypso = "Hypsometric position"
  )

  matched <- labels[imp$predictor]
  imp$label <- ifelse(
    is.na(matched),
    imp$predictor,
    unname(matched)
  )

  # Map model class names to publication labels
  class_labels <- c(
    "Unfrozen" = "Permafrost-free",
    "Ice-poor" = "Ice-poor",
    "Ice-rich" = "Ice-rich",
    "Very Ice-rich" = "Very ice-rich"
  )

  # Apply display labels
  imp$class_display <- unname(class_labels[as.character(imp$class)])

  # Set the panel order you want in the figure
  imp$class_display <- factor(
    imp$class_display,
    levels = c("Permafrost-free", "Ice-poor", "Ice-rich", "Very ice-rich")
  )

  # Unique identifiers permit independent ordering within panels
  imp$term <- paste(imp$class, imp$predictor, sep = "___")
  axis_labels <- setNames(imp$label, imp$term)
  imp$term <- factor(
    imp$term,
    levels = imp$term[order(imp$relative)]
  )

  # Four-panel horizontal lollipop plot
  p <- ggplot2::ggplot(imp, ggplot2::aes(x = relative, y = term)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0, xend = relative, yend = term),
      linewidth = 0.8,
      colour = "grey55"
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::facet_wrap(~class_display, ncol = 2, scales = "free_y") +
    ggplot2::scale_y_discrete(
      labels = function(z) unname(axis_labels[as.character(z)])
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 100, 20),
      limits = c(0, 105)
    ) +
    ggplot2::labs(
      x = "Relative importance (0\u2013100)",
      y = NULL
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 14),
      axis.text.x = ggplot2::element_text(size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(size = 14)
    )


  ggplot2::ggsave(
    output_file(cfg, "Figure6_variable_importance_w_snow+hypso.png"),
    p,
    width = 11,
    height = 7,
    dpi = 400
  )

  write.csv(imp, output_file(cfg, "Figure6_importance_w_snow+hypso.csv"), row.names = FALSE)
  invisible(imp)
}
