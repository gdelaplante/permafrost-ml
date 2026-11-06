# Walk through this file in RStudio, VSCode, or Positron.
# Sourcing defines reproduce_published_figures(); it does not fit or plot anything.
# At the repository root, first source download_inputs.R (defines acquisition
# functions) and call download_published_results() for the optional archived grids.
# Then source this file and call reproduce_published_figures().
reproduce_published_figures <- function(root = getwd(), output_dir = NULL) {
  root <- normalizePath(root,winslash="/",mustWork=TRUE)
  # Loads the documented analysis modules and defines run_permafrost locally.
  # This source call does not fit a model or reload a spatPredict result.
  source(file.path(root,"R/permafrost/permafrost_model_revision.R"),local=TRUE)
  m <- environment(run_permafrost)$modules
  if(is.null(output_dir)) output_dir <- file.path(root,"data/permafrost/outputs/published_figures")
  if(dir.exists(output_dir) && length(list.files(output_dir,all.files=TRUE,no..=TRUE)))
    stop("Choose an empty output_dir to preserve existing figures.")
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  published <- file.path(root,"data/permafrost/published_results")
  cfg <- m$permafrost_config(root,output_dir=output_dir,raster_dir=published)
  m$export_confusion_tables(file.path(published,"final_holdout_confusion.csv"),output_dir)
  m$draw_partial_dependence(file.path(published,"final_predictor_pdp.csv"),output_dir)
  m$plot_prediction_map(cfg)
  m$plot_external_comparison(cfg)
  m$plot_hypsometric_predictor(root,output_dir)
  used <- c(m$input_paths(cfg,"maps"),
    file.path(published,c("prediction_w_snow_hypso.tif","MESS_snow_hypso.tif",
      "final_holdout_confusion.csv","final_predictor_pdp.csv")))
  write.csv(m$file_manifest(used,root),file.path(output_dir,"figure_input_manifest.csv"),row.names=FALSE)
  writeLines(capture.output(sessionInfo()),file.path(output_dir,"sessionInfo.txt"))
  message("Published figure/table reproductions: ",output_dir)
  invisible(cfg)
}
