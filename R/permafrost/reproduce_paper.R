# Reproduce the Dawson Range, Yukon Territory, Canada permafrost analysis in RStudio, VSCode, or Positron.
# Open permafrost-ml.Rproj (or set the IDE working directory to this repository).
# Run the numbered sections in order using the IDE's Run/Send Selection command.
# Running the entire file executes the selected analysis profile.

# 1. Packages ---------------------------------------------------------------
# Run these three lines once if the analysis environment has not been restored:
# install.packages("renv")
# renv::restore(lockfile = "R/permafrost/renv.lock", prompt = FALSE)
# renv::load()
# In later sessions using that project library, run renv::load() before analysis.

project_root <- getwd()
stopifnot(file.exists(file.path(project_root, "permafrost-ml.Rproj")))

# 2. Inputs -----------------------------------------------------------------
# This source() defines download_snapshot_inputs(); it does not download anything.
source("R/permafrost/download_inputs.R")
# Fetch missing inputs, or replace LFS pointer files, using fixed snapshot URLs.
# Already cached real data is preserved. The first full download is about 4.93 GiB.
download_snapshot_inputs(project_root)

# This source() defines verify_permafrost_inputs(); it does not check files yet.
source("R/permafrost/verify_inputs.R")
verify_permafrost_inputs(project_root)

# 3. Load the analysis functions --------------------------------------------
# This source() defines run_permafrost() and loads the modules listed, with their
# roles, inside that file. It does not fit a model or produce a figure.
source("R/permafrost/permafrost_model_revision.R")
# Read revision/pipeline.R for the execution order. Read the individual revision/
# files to inspect the complete calculations for each scientific task.

# 4. Choose the run ----------------------------------------------------------
profile <- "quick" # Change to "publication" for 80,000/class and 10 folds x 10 repeats.
# quick uses 10,000/class and 2 folds x 2 repeats; both use the whole regional grid.
output_dir <- file.path(project_root, "data/permafrost/outputs", profile)
# Use a new output_dir for another run: a directory containing results is refused.
run_permafrost(profile = profile, output_dir = output_dir, check_only = TRUE)

# 5. Execute the analysis ----------------------------------------------------
# The pipeline loads/aligns predictors, fits the model, predicts the regional grid,
# computes importance/PDP/MESS, validates predictions, then makes maps and climate
# comparisons. res is passed live between functions in this single R session.
run <- run_permafrost(profile = profile, output_dir = output_dir)

# 6. Examine outputs ---------------------------------------------------------
print(run$res$best_model)
print(run$res$selected_model_performance)
read.csv(file.path(output_dir, "holdout_accuracy_definitions.csv"))
read.csv(file.path(output_dir, "casino_agreement.csv"))
read.csv(file.path(output_dir, "thermistors_performance_by_region.csv"))
# See README.md for the mapping from manuscript figures/tables to output files.
# Do not save/reload run or run$res: their terra objects contain external pointers.

# Optional: draw the SI hypsometric map directly from its processed input, no fit.
# This source() defines only plot_hypsometric_predictor(); the call saves PNG/PDF.
# source("R/permafrost/revision/hypsometric_map.R")
# plot_hypsometric_predictor(project_root)
# Default location: data/permafrost/outputs/SI/FigureS3_hypsometric_predictor.png

# Optional: fit/interpret without regional prediction (use a separate new folder).
# run <- run_permafrost(profile = profile, stages = c("model", "importance", "pdp"),
#   output_dir = file.path(project_root, "data/permafrost/outputs/interpretation"))
# Optional: regenerate maps/validation from existing GeoTIFFs without refitting.
# maps <- run_permafrost(stages = c("validation", "maps"), raster_dir = output_dir,
#   output_dir = file.path(project_root, "data/permafrost/outputs/maps_2"))

# Optional: regenerate published figures without a new model fit.
# The first source defines acquisition functions; the second defines the figure runner.
# source("R/permafrost/download_inputs.R")
# download_published_results(project_root)
# source("R/permafrost/reproduce_published_figures.R")
# reproduce_published_figures(project_root)
