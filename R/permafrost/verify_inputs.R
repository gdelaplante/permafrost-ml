# Sourcing this file defines the verifier; it does not start verification.
verify_permafrost_inputs <- function(root = getwd(), available_only = FALSE, manifest = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  if (is.null(manifest)) manifest <- file.path(root, "R/permafrost/input_manifest.csv")
  manifest <- utils::read.csv(manifest, stringsAsFactors = FALSE)
  paths <- file.path(root, manifest$path)
  missing <- manifest$path[!file.exists(paths)]
  if (length(missing) && !available_only) stop("Missing release inputs:\n", paste(missing, collapse="\n"),
    "\nSource download_inputs.R and call download_snapshot_inputs(root).")
  if (available_only) manifest <- manifest[file.exists(paths), , drop=FALSE]
  paths <- file.path(root, manifest$path)
  changed <- manifest$path[file.info(paths)$size != manifest$bytes |
    unname(tools::md5sum(paths)) != manifest$md5]
  if (length(changed)) stop("Inputs differ from the recorded snapshot:\n", paste(changed, collapse="\n"),
    "\nMaterialize any Git LFS pointers with download_snapshot_inputs(root).")
  cat("Verified",nrow(manifest),"release input files.\n")
  if (available_only && length(missing)) cat(length(missing),"required files are absent.\n")
  invisible(manifest)
}
if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly=TRUE)
  if (any(!args %in% "--available")) stop("Unknown argument.")
  verify_permafrost_inputs(available_only = "--available" %in% args)
}
