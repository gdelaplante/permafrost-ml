Detailed analysis and reproduction instructions

Purpose and entry point

Run permafrost_model_revision.R to fit the final four-class model, predict the
regional surface, calculate environmental similarity, and produce interpretation
and validation outputs. Run commands from the repository root. Sourcing the file
only defines run_permafrost(); it does not execute an analysis. Paths are resolved
relative to the source file, so the entry point can also be sourced by absolute path.

Inputs and environment

Open the repository in your R IDE and run reproduce_paper.R section by section.
No command-line execution is required. From the R console at the repository root:
  source("R/permafrost/download_inputs.R") # Defines the downloader only.
  download_snapshot_inputs(getwd())       # Downloads missing files or LFS pointers.
  source("R/permafrost/verify_inputs.R")   # Defines the verifier only.
  verify_permafrost_inputs()              # Checks every required file.

input_manifest.csv records 34 required files (4.93 GiB), including shapefile
companions and raster auxiliary metadata. Scientific input bytes are preserved
without automatic line-ending conversion. Unmaterialized LFS pointers are detected.
Use available_only=TRUE with the verifier only when diagnosing a partial checkout.

data_sources.csv describes source products and preprocessing boundaries.
context_sources.csv lists public map downloads. The snapshot contains processed
predictors rather than complete recipes for their original GIS derivation.

Restore R dependencies in an R session at the project root:
  install.packages("renv") # only if needed
  renv::restore(lockfile = "R/permafrost/renv.lock", prompt = FALSE)
  renv::load()
  source("R/permafrost/permafrost_model_revision.R") # Defines the runner and its modules.

The lockfile records R 4.5.1 and the analysis dependencies. Installation may require
platform-specific build/geospatial libraries. weathercan is an optional preparation
dependency, not an analysis dependency: frozen ECCC CSVs are supplied.

Run settings

  run_permafrost(profile = "publication", check_only = TRUE)
  run <- run_permafrost(profile = "publication")

publication requests 80,000 points per class and 10-fold CV repeated 10 times.
quick requests 10,000 points per class and 2-fold CV repeated twice. Both profiles
use the full prediction grid. The four requested classes are Ice-poor, Ice-rich,
Unfrozen, and Very Ice-rich. Integer raster codes are 1, 2, 3, 4 respectively.
Binary comparisons map codes 1, 2, 4 to frozen and code 3 to permafrost-free.

Configuration is centralized in revision/config.R. The master seed is 20261105;
separate seeds control interpretation samples and shuffles. Sequential caret
resampling and one ranger thread are used. The historical model's random state
was not archived, so this seeded refit need not reproduce its exact statistics.

run_permafrost() accepts profile, root, output_dir, raster_dir, stages, check_only,
and checksums as ordinary R arguments. Every execution example here can be run
from the IDE console or sent from the walkthrough script.

check_only=TRUE validates packages, required files, training labels, and predictor geometry.
It does not fit the model or process the regional cells. The regional grid has
approximately 185 million cells; a complete run requires substantial runtime and
space for raster intermediates. No measured full-run resource estimate is supplied.

Stage selection

Available stages are model, predict, importance, pdp, mess, validation, maps, climate.
All are run by default. Model-dependent stages share the same live spatPredict result:
  run <- run_permafrost(profile = "quick",
    stages = c("model", "importance", "pdp"),
    output_dir = "data/permafrost/outputs/quick_interpretation")

predict, importance, and pdp require model in the same run. For maps and validation
from previously exported rasters without fitting another model:
  run <- run_permafrost(stages = c("validation", "maps"),
    raster_dir = "data/permafrost/outputs/publication",
    output_dir = "data/permafrost/outputs/publication_maps_2")

The rasters expected in raster_dir are prediction_w_snow_hypso.tif and
MESS_snow_hypso.tif. A combined prediction/map run should use the default raster_dir,
which is the same as output_dir.

Computational methods

The ten predictor names are aspect, DEM, difrad, dirrad, profcurv, tancurv, slope,
veg, snow, and hypso. The hypsometric band used is hypsometric_range50_m. Snow is
projected bilinearly onto the DEM template and masked. The other layers must already
match its CRS, extent, resolution, and alignment. Vegetation retains its integer codes.

SAiVE 1.0.6 samples the four classes and constructs a 200 x 200 grid over the sampled
outcome extent, assigning approximately 70% of occupied grid cells to training and
the remainder to testing. This spatial holdout is separate from caret's repeated
cross-validation within training. The model is ranger, with fastCompare and
thinFeatures FALSE. Caret's default tuning grid and Accuracy objective are used.

holdout_overall.csv contains ordinary overall Accuracy and its caret statistics.
holdout_by_class.csv contains sensitivity, specificity, and one-versus-rest balanced
accuracy for each class. holdout_accuracy_definitions.csv defines overall accuracy
as the proportion of correctly classified observations. holdout_confusion.csv
uses caret's explicit Prediction and Reference fields; transpose/pivot appropriately
when presenting observed classes as rows.

Permutation importance uses up to 2,000 testing observations per class, five shuffles
per predictor, and the decrease in each class's one-versus-rest balanced accuracy.
The largest positive mean decrease across all classes is scaled to 100; negative
estimates are displayed as zero. Raw mean decreases remain in the output CSV.

Partial dependence averages class probabilities over up to 1,000 training records
per class. A probability forest is fitted using the selected model's tuning values
and number of trees when the classifier does not support probability predictions.
Continuous predictors use 35 grid points between their 2nd and 98th percentiles;
vegetation uses observed codes. Radiation is displayed in kWh/m^2. The plots share
a probability-axis scale and allow predictor-specific x scales. These curves describe
model associations and may evaluate uncommon combinations of correlated predictors.

MESS compares regional values with complete predictor records inside the training
polygons, using nine continuous variables. Vegetation is excluded. Predictions with
negative or missing MESS are excluded from filtered validation and maps. The MESS
surface and negative-cell proportion use the full grid; the histogram uses up to
one million sampled cells. Waterbodies are additionally removed in the regional map.

Thermistor validation reads the supplied Yes/No/Undetermined observations and reports
coverage separately from accuracy. Regions are the actual Coffee training polygon
footprint, Casino testing polygon footprint (excluding Coffee if they overlap),
and every observation outside both map footprints. Region assignment is independent
of prediction availability. Performance includes all classified observations with
valid MESS-filtered predictions, without a radius or nearest-site selection.
Coverage reports retain all observations and explain unavailable predictions.

The climate output compares BIO1 with the ECCC annual Year normal. It also exports
the earlier mean over Jan-Dec plus Year as a separate audit column. Difference signs
are ERA5-Land minus ECCC. These two ECCC aggregations are distinct quantities.

Standalone hypsometric predictor map

The maps stage saves FigureS3_hypsometric_predictor.png (300 dpi) and a PDF in the
run output directory. To make only this map from the processed predictor in an IDE:
  source("R/permafrost/revision/hypsometric_map.R") # Defines the map function only.
  plot_hypsometric_predictor()
These files default to data/permafrost/outputs/SI/. The exact hypsometric_range50_m
band used by the model is plotted; no forest fit or saved spatPredict result is needed.

Outputs and scope

The root README maps paper figures/tables to output filenames. Default output paths
are data/permafrost/outputs/publication or quick. Each output directory must be empty;
select another output_dir for another run. CSV metrics are not typeset paper tables.
Each run also records configuration, input/code hashes, installed package versions,
sessionInfo, the installed spatPredict implementation, stage timings, and run status.
checksums=FALSE skips input hashes for diagnostics; retain hashes for reported runs.

Do not save/reload run or run$res. Their spatial objects contain external pointers.
Portable outputs include GeoTIFFs, training/testing sample CSVs and sample_crs.wkt,
fitted_model.rds, and probability_model.rds. The small synthetic smoke test checks
these computational paths; its results are not estimates of study performance.

The maintained workflow does not reproduce the historical multi-algorithm comparison,
sample-size search, raw GIS preprocessing, or the external-map comparison in paper
Figure 7. legacy/ contains historical code for context, not a complete reproduction
of those experiments. Photograph/composite artwork also lies outside this workflow.

Optional data preparation

  source("R/permafrost/prepare_permafrost.R") # Defines all preparation functions only.
  prepare_context(getwd(), download = TRUE)
  prepare_bio1(getwd())
  prepare_climate(getwd())
  prepare_snow(getwd())

Context preparation clips public source layers into GeoPackages. A rebuilt package
can differ from the archived bytes even if geometries agree. BIO1 preparation caches
the provider's ~1.85 GB nested ZIP and verifies the extracted 1991-2020 TIFF. Climate
preparation obtains missing ECCC snapshots using weathercan. Snow preparation requires
seven separately supplied annual clipped rasters for winter 2018/19 through 2024/25
and calculates their median with na.rm=TRUE. Those annual rasters are not distributed;
the resulting median is supplied through LFS. Existing snapshots are preserved.

For direct downloads, immutable GitHub URLs are supplied in input_downloads.csv.
Alternative exact-file mirrors can be entered there. After sourcing download_inputs.R, use
  download_snapshot_inputs(getwd(), check_only = TRUE)
  download_snapshot_inputs(getwd())
Downloaded files are cached and checked against expected sizes and MD5 hashes before
finalization. Routine analysis needs no downloads once inputs and packages are present.

Reading the module sources

The entry point's module_files list states the purpose of every source() invocation.
Those sources only define functions in a common analysis environment; none starts
sampling, fitting, download, or plotting. pipeline.R calls these functions in scientific
execution order. Separate modules keep the individual methods readable; the IDE
walkthrough explains the three public utility/entry-point sources inline.
