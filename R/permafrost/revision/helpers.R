require_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "),
    ". See R/permafrost/README.txt for environment restoration.", call. = FALSE)
}

output_file <- function(cfg, name) file.path(cfg$output_dir, name)
raster_file <- function(cfg, name) file.path(cfg$raster_dir, name)

assert_files <- function(paths) {
  missing <- paths[!file.exists(paths)]
  if (length(missing)) stop("Missing inputs:\n", paste(missing, collapse = "\n"),
    "\nSee R/permafrost/README.txt and data_sources.csv.", call. = FALSE)
  candidates <- paths[grepl("\\.(tiff?|gpkg)$", paths, ignore.case = TRUE)]
  candidates <- candidates[!dir.exists(candidates) & file.info(candidates)$size < 1024]
  prefix <- charToRaw("version https://git-lfs.github.com/spec/v1")
  pointers <- candidates[vapply(candidates, function(f) {
    identical(readBin(f, what = "raw", n = length(prefix)), prefix)
  }, logical(1))]
  if (length(pointers)) stop("Git LFS pointers have not been downloaded:\n",
    paste(pointers, collapse = "\n"),
    "\nRun git lfs pull in the publication repository.", call. = FALSE)
}

# Avoid sample(x, 1)'s special interpretation when x contains one integer.
sample_by_class <- function(y, limit) {
  unlist(lapply(split(seq_along(y), y, drop = TRUE), function(i) {
    i[sample.int(length(i), min(length(i), limit))]
  }), use.names = FALSE)
}

binary_model <- function(r) {
  terra::classify(r, matrix(c(1, 1, 2, 1, 3, 0, 4, 1), ncol = 2, byrow = TRUE),
                  others = NA)
}

mask_mess <- function(prediction, mess) {
  if (!terra::compareGeom(prediction, mess, stopOnError = FALSE)) {
    stop("Prediction and MESS grids differ; regenerate them together.")
  }
  terra::ifel(!is.na(mess) & mess >= 0, prediction, NA)
}

assert_class_codes <- function(model, cfg) {
  if (!identical(as.character(model$levels), cfg$class_levels)) {
    stop("Model class order differs from the configured prediction class codes.")
  }
}

casino_polygons <- function(path) {
  r <- terra::rast(path)
  if (terra::nlyr(r) != 1L || names(r) != "pmfst_int") {
    stop("Casino input must be a one-layer binary raster named pmfst_int.")
  }
  f <- terra::freq(r)
  if (any(!f$value %in% 0:1)) stop("Casino reference contains non-binary codes.")
  terra::as.polygons(r, aggregate = TRUE, na.rm = TRUE)
}

# Record exact inputs, including shapefile sidecars and files inside a GDB.
file_manifest <- function(paths, root, checksums = TRUE) {
  paths <- unique(unlist(lapply(paths, function(p) {
    if (dir.exists(p)) return(list.files(p, recursive = TRUE, full.names = TRUE))
    if (grepl("\\.shp$", p, ignore.case = TRUE)) {
      stem <- tools::file_path_sans_ext(basename(p))
      return(list.files(dirname(p), full.names = TRUE)[
        tools::file_path_sans_ext(list.files(dirname(p))) == stem])
    }
    # GDAL sidecars can carry band descriptions (notably Casino's pmfst_int).
    if (grepl("\\.tiff?$", p, ignore.case = TRUE)) {
      return(c(p, paste0(p, ".aux.xml"))[file.exists(c(p, paste0(p, ".aux.xml")))])
    }
    p
  }), use.names = FALSE))
  paths <- sort(paths[file.exists(paths) & !dir.exists(paths)])
  info <- file.info(paths)
  resolved <- normalizePath(paths, winslash = "/")
  inside <- startsWith(tolower(resolved), paste0(tolower(root), "/"))
  relative <- resolved
  relative[inside] <- substring(resolved[inside], nchar(root) + 2L)
  data.frame(path = relative,
    bytes = info$size,
    md5 = if (checksums) unname(tools::md5sum(paths)) else NA_character_,
    stringsAsFactors = FALSE)
}

run_stage <- function(name, cfg, code) {
  start <- Sys.time()
  devices <- grDevices::dev.list()
  on.exit({
    for (d in setdiff(grDevices::dev.list(), devices)) grDevices::dev.off(d)
    cat(sprintf("%s\t%.1f seconds\n", name,
                as.numeric(difftime(Sys.time(), start, units = "secs"))),
        file = output_file(cfg, "stages.tsv"), append = TRUE)
  }, add = TRUE)
  message("Starting ", name)
  force(code)
}

record_environment <- function(cfg) {
  saveRDS(cfg, output_file(cfg, "run_config.rds"))
  dput(cfg, file = output_file(cfg, "run_config.R"))
  writeLines(capture.output(sessionInfo()), output_file(cfg, "sessionInfo.txt"))
  ip <- as.data.frame(installed.packages(fields = "Repository"), stringsAsFactors = FALSE)
  utils::write.csv(ip[, c("Package", "Version", "Built", "Repository")],
                   output_file(cfg, "installed_packages.csv"), row.names = FALSE)
  if (requireNamespace("SAiVE", quietly = TRUE)) {
    writeLines(capture.output(SAiVE::spatPredict), output_file(cfg, "spatPredict_implementation.R"))
  }
}
