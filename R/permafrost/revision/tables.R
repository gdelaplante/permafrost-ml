# Convert caret Prediction-by-Reference counts to observed-row publication tables.
# Source this file to define the function; no table is written until it is called.
export_confusion_tables <- function(confusion, output_dir) {
  if (!requireNamespace("caret", quietly = TRUE)) stop("The caret package is required.")
  if (is.character(confusion) && length(confusion) == 1L) confusion <- read.csv(confusion)
  if (is.data.frame(confusion)) {
    stopifnot(all(c("Prediction", "Reference", "Freq") %in% names(confusion)))
    confusion <- stats::xtabs(Freq ~ Prediction + Reference, data = confusion)
  }
  confusion <- unclass(as.matrix(confusion))
  stopifnot(nrow(confusion) == ncol(confusion), all(is.finite(confusion)),
            all(confusion >= 0), all(confusion == floor(confusion)))
  # Stable paper ordering, independent of caret's factor order.
  order <- c("Ice-poor", "Ice-rich", "Very Ice-rich", "Unfrozen")
  stopifnot(setequal(rownames(confusion), order),setequal(colnames(confusion), order))
  confusion <- confusion[order,order,drop=FALSE]
  perf <- caret::confusionMatrix(as.table(confusion))
  observed_rows <- t(confusion)
  proportions <- sweep(observed_rows,1,rowSums(observed_rows),"/")
  display <- c("Ice-poor","Ice-rich","Very ice-rich","Permafrost-free")
  dimnames(observed_rows) <- dimnames(proportions) <- list(Observed=display,Predicted=display)
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  write.csv(data.frame(Observed=display,observed_rows,check.names=FALSE),
    file.path(output_dir,"paper_SI_table1_counts.csv"),row.names=FALSE)
  write.csv(data.frame(Observed=display,proportions,check.names=FALSE),
    file.path(output_dir,"paper_table3_proportions.csv"),row.names=FALSE)
  metrics <- c("Accuracy","Sensitivity","Specificity","Pos Pred Value",
               "Neg Pred Value","Prevalence","Detection Rate","Detection Prevalence")
  # Class accuracy treats each class as positive and all other classes as negative.
  n <- sum(confusion)
  tp <- diag(confusion)
  tn <- n - rowSums(confusion) - colSums(confusion) + tp
  class_accuracy <- (tp + tn) / n
  by_class <- cbind(Accuracy=class_accuracy, perf$byClass)
  class_metrics <- t(by_class[,metrics,drop=FALSE])
  colnames(class_metrics) <- display
  write.csv(data.frame(Metric=metrics,class_metrics,check.names=FALSE),
    file.path(output_dir,"paper_table2_metrics.csv"),row.names=FALSE)
  write.csv(data.frame(metric=names(perf$overall),value=unname(perf$overall)),
    file.path(output_dir,"paper_overall_metrics.csv"),row.names=FALSE)
  invisible(list(counts=observed_rows,proportions=proportions,metrics=class_metrics,
                 overall=perf$overall))
}
