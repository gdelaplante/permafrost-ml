# Exact-input downloads for R-console use. Real cached inputs are never replaced.
# Source this file to define download_snapshot_inputs(), then call it from R.
# The supplied registry contains fixed URLs for the published input snapshot.

download_snapshot_inputs <- function(root, registry = NULL, check_only = FALSE, manifest = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (is.null(registry)) registry <- file.path(root, "R/permafrost/input_downloads.csv")
  sources <- utils::read.csv(registry, stringsAsFactors = FALSE, na.strings = "")
  if (is.null(manifest)) manifest <- file.path(root, "R/permafrost/input_manifest.csv")
  manifest <- utils::read.csv(manifest, stringsAsFactors = FALSE)
  if (anyDuplicated(sources$path) || any(!sources$path %in% manifest$path)) {
    stop("Download registry must contain unique paths from input_manifest.csv.")
  }
  is_pointer <- function(path) {
    if (!file.exists(path) || file.info(path)$size >= 1024) return(FALSE)
    prefix <- charToRaw("version https://git-lfs.github.com/spec/v1")
    identical(readBin(path, "raw", n = length(prefix)), prefix)
  }
  paths <- file.path(root, sources$path)
  cached <- file.exists(paths) & !vapply(paths, is_pointer, logical(1))
  missing <- sources[!cached, , drop = FALSE]
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
      if (file.exists(target)) {
        if (!is_pointer(target)) stop("An input appeared during download; preserving it: ", target)
        unlink(target) # Replace only a verified LFS pointer, never an existing dataset.
      }
      if (!file.rename(part, target)) stop("Could not finalize input: ", relative)
    }, finally = {
      options(old)
      if (file.exists(part)) unlink(part)
    })
  }
  invisible(missing)
}

# Optional archived outputs and interpretation forest are downloaded separately.
# Sourcing defines this function; the call requests the additional snapshot.
download_published_results <- function(root = getwd(), check_only = FALSE) {
  download_snapshot_inputs(root,
    registry = file.path(root,"R/permafrost/published_results_downloads.csv"),
    check_only = check_only,
    manifest = file.path(root,"R/permafrost/published_results_manifest.csv"))
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
