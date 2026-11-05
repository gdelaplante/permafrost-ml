# Create the checksum manifest for the exact Git/LFS inputs accompanying a release.
# Run from the project root after staging all inputs.
source("R/permafrost/permafrost_model_revision.R")
m <- environment(run_permafrost)$modules
cfg <- m$permafrost_config(getwd(), "publication")
stages <- c("model", "predict", "importance", "pdp", "mess", "validation", "maps", "climate")
paths <- m$input_paths(cfg, stages)
m$assert_files(paths)
inventory <- m$file_manifest(paths, cfg$root)
inventory$storage <- ifelse(grepl("\\.(tiff?|gpkg)$", inventory$path, ignore.case = TRUE),
                             "Git LFS", "Git")
utils::write.csv(inventory, "R/permafrost/input_manifest.csv", row.names = FALSE)
cat("Manifest:", nrow(inventory), "files;", round(sum(inventory$bytes) / 1024^3, 2), "GiB.\n")

spec <- utils::read.csv("R/permafrost/context_sources.csv", stringsAsFactors = FALSE)
originals <- file.path(cfg$root, spec$local_source)
if (all(file.exists(originals))) {
  manifest <- m$file_manifest(originals, cfg$root)
  utils::write.csv(manifest, "R/permafrost/context_original_manifest.csv", row.names = FALSE)
}
