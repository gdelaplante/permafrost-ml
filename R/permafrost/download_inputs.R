# Optional exact-input downloads. A cached local input is never replaced.
# Populate download_url for any externally hosted processed inputs, then run:
# Rscript --vanilla R/permafrost/download_inputs.R --download

download_snapshot_inputs <- function(root, registry = NULL, check_only = FALSE) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (is.null(registry)) registry <- file.path(root, "R/permafrost/input_downloads.csv")
  sources <- utils::read.csv(registry, stringsAsFactors = FALSE, na.strings = "")
  manifest <- utils::read.csv(file.path(root, "R/permafrost/input_manifest.csv"),
                              stringsAsFactors = FALSE)
  if (anyDuplicated(sources$path) || any(!sources$path %in% manifest$path)) {
    stop("Download registry must contain unique paths from input_manifest.csv.")
  }
  missing <- sources[!file.exists(file.path(root, sources$path)), , drop = FALSE]
  if (!nrow(missing)) {
    message("All registered inputs are already cached locally.")
    return(invisible(missing))
  }
  print(missing[, c("path", "download_url"), drop = FALSE], row.names = FALSE)
  unavailable <- missing$path[is.na(missing$download_url) | !nzchar(missing$download_url)]
  if (check_only) return(invisible(missing))
  if (length(unavailable)) stop("No exact-file download URL is registered for:\n",
    paste(unavailable, collapse = "\n"),
    "\nUse Git LFS for these custom grids, or supply their hosted URLs in input_downloads.csv.",
    "\nprepare_permafrost.R handles upstream context and climate archives separately.")
  for (i in seq_len(nrow(missing))) {
    url <- missing$download_url[i]
    if (!startsWith(url, "https://")) stop("Expected an HTTPS input URL: ", url)
    relative <- missing$path[i]
    if (grepl("(^|[/\\])\\.\\.([/\\]|$)", relative) || grepl("^[/\\]|^[A-Za-z]:", relative)) {
      stop("Download path must remain within the project: ", relative)
    }
    target <- file.path(root, relative)
    expected <- manifest[match(relative, manifest$path), ]
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    part <- paste0(target, ".part")
    old <- options(timeout = max(7200, getOption("timeout")))
    tryCatch({
      status <- utils::download.file(url, part, mode = "wb", method = "libcurl")
      if (status != 0L || file.info(part)$size != expected$bytes ||
          unname(tools::md5sum(part)) != expected$md5) {
        stop("Downloaded bytes do not match the recorded snapshot: ", relative)
      }
      if (!file.rename(part, target)) stop("Could not finalize input: ", relative)
    }, finally = {
      options(old)
      if (file.exists(part)) unlink(part)
    })
  }
  invisible(missing)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) != 1L || !args %in% c("--check", "--download")) {
    stop("Use --check to list missing inputs or --download to retrieve registered HTTPS files.")
  }
  entry <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  root <- normalizePath(file.path(dirname(entry), "../.."), winslash = "/")
  download_snapshot_inputs(root, check_only = args == "--check")
}
