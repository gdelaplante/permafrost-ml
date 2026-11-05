# Fast checks that do not fit a model or process raster cells.
for (f in list.files("R/permafrost", "[.]R$", recursive = TRUE, full.names = TRUE)) {
  parse(f, encoding = "UTF-8")
}
source("R/permafrost/permafrost_model_revision.R")
m <- environment(run_permafrost)$modules
cfg <- m$permafrost_config(getwd(), "quick")
stopifnot(cfg$model$samples_per_class == 10000L, cfg$model$folds == 2L,
          cfg$model$repeats == 2L)
pub <- m$permafrost_config(getwd(), "publication")
stopifnot(pub$model$samples_per_class == 80000L, pub$model$folds == 10L,
          pub$model$repeats == 10L)
e <- tryCatch(m$run_pipeline(cfg, "pdp", check_only = TRUE),
              error = function(e) conditionMessage(e))
stopifnot(grepl("require model", e))
pointer <- tempfile(fileext = ".tif")
writeLines(c("version https://git-lfs.github.com/spec/v1", "oid sha256:example", "size 1"), pointer)
e <- tryCatch(m$assert_files(pointer), error = function(e) conditionMessage(e))
unlink(pointer)
stopifnot(grepl("git lfs pull", e, fixed = TRUE))
cat("\nAll R sources parse; profiles and live-model stage guard pass.\n")
