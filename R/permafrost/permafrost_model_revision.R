# Reproducible permafrost workflow. See README.txt in this directory.
# CLI: Rscript --vanilla R/permafrost/permafrost_model_revision.R --profile=quick --check
# R:   source("R/permafrost/permafrost_model_revision.R")
#      run <- run_permafrost(profile = "quick")
# Sourcing defines functions only; it never starts an expensive run.

run_permafrost <- local({
  source_files <- lapply(sys.frames(), function(f) f$ofile)
  source_files <- Filter(function(x) !is.null(x), source_files)
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  entry <- if (length(source_files)) tail(source_files, 1L)[[1L]] else
    if (length(arg)) sub("^--file=", "", arg[1L]) else
      stop("Source this file or execute it with Rscript.")
  script_dir <- dirname(normalizePath(entry, winslash = "/", mustWork = TRUE))
  # Module lookup never depends on getwd(), .RData, or user-defined functions.
  modules <- new.env(parent = baseenv())
  for (pkg in c("stats", "utils", "graphics", "grDevices")) {
    for (name in getNamespaceExports(pkg)) {
      assign(name, getExportedValue(pkg, name), envir = modules)
    }
  }
  for (file in c("config.R", "helpers.R", "inputs.R", "model.R", "importance.R",
                 "partial_dependence.R", "mess.R", "validation.R", "maps.R",
                 "climate.R", "pipeline.R")) {
    source(file.path(script_dir, "revision", file), local = modules, encoding = "UTF-8")
  }
  default_root <- normalizePath(file.path(script_dir, "../.."), winslash = "/")
  function(root = default_root, profile = c("publication", "quick"),
           stages = c("model", "predict", "importance", "pdp", "mess",
                      "validation", "maps", "climate"),
           output_dir = NULL, raster_dir = NULL, check_only = FALSE,
           checksums = TRUE) {
    cfg <- modules$permafrost_config(root, match.arg(profile), output_dir, raster_dir)
    modules$run_pipeline(cfg, stages, check_only, checksums)
  }
})

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  known <- grepl("^--(profile|root|output-dir|raster-dir|stages)=", args) |
    args %in% c("--check", "--help")
  if (any(!known)) stop("Unknown arguments: ", paste(args[!known], collapse = " "))
  if ("--help" %in% args) {
    cat("Usage: Rscript --vanilla R/permafrost/permafrost_model_revision.R\n",
        "  --profile=publication|quick --check\n",
        "  --root=PATH --output-dir=PATH --raster-dir=PATH\n",
        "  --stages=model,predict,importance,pdp,mess,validation,maps,climate\n",
        "Sourcing the script defines run_permafrost() without executing it.\n")
  } else {
    value <- function(key, default = NULL) {
      hit <- args[startsWith(args, paste0("--", key, "="))]
      if (length(hit) > 1L) stop("Repeated argument: ", key)
      if (length(hit)) substring(hit, nchar(key) + 4L) else default
    }
    stages <- value("stages", "model,predict,importance,pdp,mess,validation,maps,climate")
    do.call(run_permafrost, Filter(Negate(is.null), list(
      root = value("root"), profile = value("profile", "publication"),
      stages = strsplit(stages, ",", fixed = TRUE)[[1L]],
      output_dir = value("output-dir"), raster_dir = value("raster-dir"),
      check_only = "--check" %in% args)))
  }
}
