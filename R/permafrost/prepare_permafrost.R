# Optional input preparation; the main analysis never downloads data.
# source("R/permafrost/prepare_permafrost.R") then call the functions below.
# CLI: Rscript --vanilla R/permafrost/prepare_permafrost.R --context [--download]
#      Rscript --vanilla R/permafrost/prepare_permafrost.R --climate
#      Rscript --vanilla R/permafrost/prepare_permafrost.R --bio1

download_cached_archive <- function(url, target) {
  part <- paste0(target, ".part")
  old <- options(timeout = max(7200, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  on.exit(if (file.exists(part)) unlink(part), add = TRUE)
  status <- utils::download.file(url, part, mode = "wb", method = "libcurl")
  if (status != 0L || file.info(part)$size == 0) stop("Download failed: ", url)
  if (!file.rename(part, target)) stop("Could not finalize downloaded archive: ", target)
  invisible(target)
}

prepare_context <- function(root, download = FALSE) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Install terra first.")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  dest <- file.path(root, "data/permafrost/context")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  spec <- utils::read.csv(file.path(root, "R/permafrost/context_sources.csv"),
                          stringsAsFactors = FALSE)
  dem <- terra::rast(file.path(root, "data/permafrost/predvars/DEMClipped.tif"))
  aoi <- terra::as.polygons(terra::ext(dem), crs = terra::crs(dem))
  provenance <- list()
  for (i in seq_len(nrow(spec))) {
    target <- file.path(dest, spec$output[i])
    if (file.exists(target)) {
      message("Keeping existing context snapshot: ", target)
      next
    }
    source <- file.path(root, spec$local_source[i])
    if (!file.exists(source)) {
      if (!download) stop("Missing ", source, ". Re-run with download = TRUE or --download.")
      raw <- file.path(root, "data/permafrost/raw/context", spec$key[i])
      dir.create(raw, recursive = TRUE, showWarnings = FALSE)
      zip <- file.path(raw, basename(spec$download_url[i]))
      if (!file.exists(zip)) {
        download_cached_archive(spec$download_url[i], zip)
      }
      utils::unzip(zip, exdir = raw)
      candidates <- list.files(raw, pattern = spec$source_pattern[i], recursive = TRUE,
                                full.names = TRUE, include.dirs = TRUE, ignore.case = TRUE)
      if (length(candidates) != 1L) stop("Expected one dataset after extraction: ", raw)
      source <- candidates[1L]
    }
    message("Clipping context layer: ", spec$key[i])
    # Filter in the source CRS before loading/projecting the selected features.
    proxy <- terra::vect(source, proxy = TRUE)
    box <- terra::project(aoi, terra::crs(proxy))
    v <- terra::vect(source, extent = terra::ext(box))
    v <- terra::project(v, dem)
    if (spec$key[i] != "boundary") v <- terra::crop(v, aoi)
    if (spec$key[i] == "glacial_limits") v <- v[, "GLACIAL_DESCRIPTION"]
    if (spec$key[i] == "boundary") v <- v[as.character(v$PRUID) == "60", "PRUID"]
    if (nrow(v) == 0L) stop("No context features intersect the study area: ", source)
    terra::writeVector(v, target, filetype = "GPKG", overwrite = FALSE)
    source_files <- if (dir.exists(source)) list.files(source, recursive = TRUE, full.names = TRUE) else {
      if (grepl("\\.shp$", source, ignore.case = TRUE)) {
        stem <- tools::file_path_sans_ext(basename(source))
        all <- list.files(dirname(source), full.names = TRUE)
        all[tools::file_path_sans_ext(basename(all)) == stem]
      } else source
    }
    provenance[[spec$key[i]]] <- data.frame(layer = spec$key[i],
      source_file = basename(source_files), md5 = unname(tools::md5sum(source_files)),
      metadata_url = spec$metadata_url[i], download_url = spec$download_url[i],
      prepared_utc = format(Sys.time(), tz = "UTC", usetz = TRUE))
  }
  if (length(provenance)) {
    path <- file.path(dest, paste0("preparation_", format(Sys.time(), "%Y%m%dT%H%M%S"), ".csv"))
    utils::write.csv(do.call(rbind, provenance), path, row.names = FALSE)
  }
  invisible(dest)
}

prepare_bio1 <- function(root, archive = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  relative <- "data/permafrost/climate/ERALClim_bio1_1991-2020.tif"
  target <- file.path(root, relative)
  if (file.exists(target)) {
    message("Keeping existing BIO1 snapshot: ", target)
    return(invisible(target))
  }
  cache <- file.path(root, "data/permafrost/raw/climate")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  if (is.null(archive)) archive <- file.path(cache, "ERALClim_baselines.zip")
  if (!file.exists(archive)) {
    message("Downloading the upstream climate archive (~1.85 GB), once only.")
    download_cached_archive(paste0("https://zenodo.org/records/8124385/files/",
      "ERALClimBaselines_individualVariablesAndYears.zip?download=1"), archive)
  }
  entries <- utils::unzip(archive, list = TRUE)$Name
  nested <- entries[basename(entries) == "bio1_baselines.zip"]
  if (length(nested) != 1L) stop("Expected one bio1_baselines.zip in the upstream archive.")
  utils::unzip(archive, files = nested, exdir = cache)
  nested <- file.path(cache, nested)
  entries <- utils::unzip(nested, list = TRUE)$Name
  member <- entries[basename(entries) == basename(target)]
  if (length(member) != 1L) stop("Expected one 1991-2020 BIO1 TIFF in the nested archive.")
  utils::unzip(nested, files = member, exdir = cache)
  extracted <- file.path(cache, member)
  manifest <- utils::read.csv(file.path(root, "R/permafrost/input_manifest.csv"),
                              stringsAsFactors = FALSE)
  expected <- manifest[manifest$path == relative, ]
  if (nrow(expected) != 1L || file.info(extracted)$size != expected$bytes ||
      unname(tools::md5sum(extracted)) != expected$md5) {
    stop("Upstream BIO1 bytes differ from the recorded analysis snapshot.")
  }
  dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
  if (!file.copy(extracted, target, overwrite = FALSE)) stop("Could not stage BIO1: ", target)
  invisible(target)
}

prepare_climate <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  dest <- file.path(root, "data/permafrost/climate")
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  normal_file <- file.path(dest, "eccc_normals_1991_2020.csv")
  station_file <- file.path(dest, "eccc_stations.csv")
  if (file.exists(normal_file) && file.exists(station_file)) {
    message("Keeping existing ECCC snapshots.")
    return(invisible(dest))
  }
  if (file.exists(normal_file) || file.exists(station_file)) {
    stop("Incomplete ECCC snapshot pair. Preserve the existing file and prepare a new pair in an empty directory.")
  }
  if (!requireNamespace("weathercan", quietly = TRUE)) stop("Install weathercan first.")
  ids <- c(Dawson = "2100LRP", Carmacks = "2100301", Mayo = "2100701")
  normals <- stations <- list()
  for (name in names(ids)) {
    id <- ids[[name]]
    s <- as.data.frame(weathercan::stations_search(name))
    s <- s[s$climate_id == id, c("climate_id", "lat", "lon")]
    s <- unique(s)
    if (nrow(s) != 1L) stop("Station coordinates are ambiguous for ", name)
    s$station <- name
    stations[[name]] <- s
    n <- as.data.frame(weathercan::normals_dl(id, normals_years = "1991-2020"))
    n <- n[, c("period_of_record", "period", "daily_average_c")]
    n$climate_id <- id
    normals[[name]] <- n
  }
  utils::write.csv(do.call(rbind, normals), normal_file, row.names = FALSE)
  utils::write.csv(do.call(rbind, stations), station_file, row.names = FALSE)
  writeLines(c(paste("Retrieved UTC:", format(Sys.time(), tz = "UTC", usetz = TRUE)),
    paste("weathercan version:", as.character(utils::packageVersion("weathercan"))),
    "ECCC 1991-2020 normals; climate IDs 2100LRP, 2100301, 2100701.",
    "Source: https://climate.weather.gc.ca/climate_normals/"),
    file.path(dest, "eccc_provenance.txt"))
  # BIO1 is staged separately from the Zenodo archive, not downloaded every run.
  raster <- file.path(dest, "ERALClim_bio1_1991-2020.tif")
  if (!file.exists(raster)) message("Stage ERALClim_bio1_1991-2020.tif here: ", dest,
    " (https://zenodo.org/records/8124385). See README.txt for nested ZIP extraction.")
  invisible(dest)
}

prepare_snow <- function(root) {
  if (!requireNamespace("terra", quietly = TRUE)) stop("Install terra first.")
  years <- c("1819", "1920", "2021", "2122", "2223", "2324", "2425")
  paths <- file.path(root, "data/permafrost/staged", paste0(
    "HLS_Fmask_v1_1_snow_lengthT_winterYear", years, "_Canada_yt_clipped.tif"))
  if (any(!file.exists(paths))) stop("Missing annual snow rasters:\n",
    paste(paths[!file.exists(paths)], collapse = "\n"))
  target <- file.path(root, "data/permafrost/predvars",
    "HLS_Fmask_v1_1_snow_lengthT_winterYear1819_2425_Canada_yt_clipped_median.tif")
  if (file.exists(target)) stop("Median already exists; preserve the archived snapshot: ", target)
  terra::app(terra::rast(paths), median, na.rm = TRUE, filename = target, overwrite = FALSE)
  invisible(target)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  root <- normalizePath(file.path(dirname(sub("^--file=", "", arg[1L])), "../.."), winslash = "/")
  if (!length(args) || any(!args %in% c("--context", "--download", "--climate", "--bio1", "--snow"))) {
    stop("Use --context [--download], --climate, --bio1, and/or --snow. See README.txt.")
  }
  if ("--context" %in% args) prepare_context(root, download = "--download" %in% args)
  if ("--climate" %in% args) prepare_climate(root)
  if ("--bio1" %in% args) prepare_bio1(root)
  if ("--snow" %in% args) prepare_snow(root)
}
