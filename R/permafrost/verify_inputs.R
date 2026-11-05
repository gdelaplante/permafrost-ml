# Verify materialized Git/LFS data against the recorded release snapshot.
# Rscript --vanilla R/permafrost/verify_inputs.R (from the project root)
manifest <- utils::read.csv("R/permafrost/input_manifest.csv", stringsAsFactors = FALSE)
args <- commandArgs(trailingOnly = TRUE)
if (any(!args %in% "--available")) stop("Use --available to verify only files present in this checkout.")
available_only <- "--available" %in% args
missing <- manifest$path[!file.exists(manifest$path)]
if (length(missing) && !available_only) stop("Missing release files:\n", paste(missing, collapse = "\n"),
  "\nThe full data release is required for analysis. See README.md for Git LFS and download options.")
if (available_only) manifest <- manifest[file.exists(manifest$path), , drop = FALSE]
actual_size <- file.info(manifest$path)$size
actual_md5 <- unname(tools::md5sum(manifest$path))
changed <- manifest$path[actual_size != manifest$bytes | actual_md5 != manifest$md5]
if (length(changed)) stop("Files differ from the release snapshot:\n", paste(changed, collapse = "\n"),
  "\nRun git lfs pull. Fresh upstream downloads can differ; use the recorded Git/LFS snapshot for an exact replication.")
cat("Verified", nrow(manifest), "release input files.\n")
if (available_only && length(missing)) cat(length(missing), "required files are not present; the full analysis cannot run yet.\n")
