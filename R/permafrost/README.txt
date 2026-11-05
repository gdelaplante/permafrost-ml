Reproducible permafrost analysis for the revised manuscript

The entry point is permafrost_model_revision.R. It runs from staged data without
loading .RData, downloading files, or reloading a saved spatPredict result.
The publication repository retains the earlier SI script in legacy/SI_original.R.
The MSc working directory also retains its pre-refactor script in R/permafrost/archive/.

Run from R

  source("R/permafrost/permafrost_model_revision.R")
  run_permafrost(profile = "quick", check_only = TRUE)
  run <- run_permafrost(profile = "quick")

Sourcing only defines run_permafrost(); it does not launch an expensive run.
The source file locates the project root relative to itself. You can therefore
source it by an absolute path from another working directory. No setwd() edit
or machine-specific path in the analysis is required.

Run from the command line

  Rscript --vanilla R/permafrost/permafrost_model_revision.R --profile=quick --check
  Rscript --vanilla R/permafrost/permafrost_model_revision.R --profile=quick
  Rscript --vanilla R/permafrost/permafrost_model_revision.R --profile=publication

Rscript must be on PATH; otherwise use the full path to Rscript.exe.
--check validates packages, files, labels, and raster geometry without fitting
or processing raster cells. The main raster grid contains about 185 million cells;
even the quick profile still predicts over the entire domain and calculates MESS.
For a short execution check, use checks/smoke_test.R instead.

Profiles and settings

  publication: 80,000 samples PER CLASS; 10-fold CV repeated 10 times.
  quick:       10,000 samples PER CLASS; 2-fold CV repeated twice.

With four classes these request approximately 320,000 and 40,000 samples before
the spatial train/test split. SAiVE supplies its existing spatial holdout split
(70% of a 200 x 200 grid's occupied cells for training). Caret's repeated CV
operates inside the training portion. These are separate evaluation procedures.
The random forest method remains ranger; fastCompare and thinFeatures are FALSE.
Caret's default tuning grid and default Accuracy optimization are preserved.
CV prediction rows are no longer retained: downstream steps do not use them,
and retaining every row for every tuning candidate and resample costs many GB.

revision/config.R holds the choices, paths, class codes, seeds, and plot settings.
The fixed master seed is 20261105. Caret resampling and ranger use one worker/thread
by default to avoid dependence on an external parallel backend. A publication run
will take substantial time. Keep package versions fixed when comparing runs.
The old script did not set a seed before sampling/training, so its exact historical
fit cannot be recovered from these settings alone. Refit and check the publication
tables before replacing previously reported results.

Dependencies

renv.lock captures R 4.5.1 and the analysis packages installed during this refactor,
including SAiVE 1.0.6, terra 1.8-54, caret 7.0-1, and ranger 0.18.0. A separate
source checkout of SAiVE 1.0.7 was present locally; it is not the installed version
used for validation. Do not substitute that checkout without a deliberate version
change. No existing package library or project startup settings were modified.

To restore into a project library, start R at the project root and run:

  install.packages("renv")  # only if needed; requires internet
  renv::restore(project = getwd(), lockfile = "R/permafrost/renv.lock", prompt = FALSE)
  renv::load(project = getwd())
  source("R/permafrost/permafrost_model_revision.R")
  run <- run_permafrost(profile = "quick")

Restoration can require platform-specific build tools and geospatial libraries.
The lockfile was generated and validated against installed dependencies; a clean
download-and-restore into a separate library has not been performed.
The optional climate downloader uses weathercan and is separate from this lockfile.
The CSV snapshots allow the complete analysis to run without weathercan. The local
weathercan 1.0.1 installation worked, but declares cli >= 3.6.6 while local cli is
3.6.5; resolve that requirement in a separate preparation environment if refreshing
normals. No user library packages were upgraded.

Data and GitHub

input_manifest.csv records paths, sizes, MD5 checksums, and Git/LFS storage for the
34 staged input files (4.93 GiB), including .aux.xml sidecars and shapefile companions.
The publication code branch includes 20 small inputs/metadata files. Fourteen larger
files (4.93 GiB together) are prepared on a local LFS data branch and are not uploaded
from the current slow connection. The main README identifies this incomplete remote
data release explicitly. Do not treat a successful clone as a complete analysis dataset.

  git lfs install
  git clone https://github.com/gdelaplante/permafrost-ml.git
  cd permafrost-ml
  git lfs pull
  Rscript --vanilla R/permafrost/verify_inputs.R --available

After the data branch is published, git lfs pull and the full verifier (without
--available) retrieve/check the complete snapshot. TIFFs and GeoPackages are assigned
to Git LFS by .gitattributes; small input bytes are excluded from line-ending conversion.
The analysis detects unmaterialized LFS pointers and tells you to run git lfs pull.

If exact processed inputs are hosted elsewhere, add HTTPS file URLs to
input_downloads.csv, then run download_inputs.R --download. It caches files locally,
checks size and MD5 before finalizing each download, and never replaces an existing
input. No hosted URLs for the custom derived grids were available during preparation.
Use download_inputs.R --check to list missing registered inputs without network use.
Public-source context and BIO1 downloads are supported separately:

  Rscript --vanilla R/permafrost/prepare_permafrost.R --context --download
  Rscript --vanilla R/permafrost/prepare_permafrost.R --bio1

The BIO1 provider supplies a ~1.85 GB parent ZIP containing a nested BIO1 ZIP rather
than a direct TIFF. It is downloaded once, then only the required 1991-2020 TIFF is
staged after checksum verification. Existing local inputs avoid these downloads.
Rebuilt context files may differ at the byte level from the recorded GeoPackages;
use the frozen data release when exact file-level replication is required.

The publication repository ignores generated outputs, raw download caches, original
GDBs, saved workspaces, and local renv libraries. Large publication working-tree files
were staged locally as hard links to immutable MSc inputs to avoid a second working-tree
copy; create new files rather than editing an existing linked input in place.

The exact processed grids are intended to be versioned alongside the code in the data release. data_sources.csv documents
the original products, preparation steps, and provenance gaps: original terrain,
radiation, and hypsometric processing parameters are not reconstructed by this workflow.
context_sources.csv provides public source/download links; context_original_manifest.csv
records the original local GDB and boundary snapshot hashes. The compact boundary
snapshot is included; the original Statistics Canada direct download returned HTTP 403
in verification. Current LFS snapshots avoid requiring that download to run the maps.

Prepare map context

  Rscript --vanilla R/permafrost/prepare_permafrost.R --context
  Rscript --vanilla R/permafrost/prepare_permafrost.R --context --download

The first command uses existing data/plotting_layers GDBs/shapefile. The second
downloads missing sources explicitly from GeoYukon (about 309 MB compressed for
the two CanVec layers and glacial limits). Both write study-area GeoPackages in
data/permafrost/context and keep existing snapshots. The original ~421 MB of GDBs
becomes ~41 MB of context layers. Original files are kept in place.
The streams layer is required for the Casino figure, avoiding a silent change in
figure content when a file is absent. Context download URLs were verified with
HTTP HEAD; the download-and-unzip fallback has not been run for the large sources.
New upstream data or a newly written GeoPackage can differ from archived bytes;
use the exact Git/LFS release snapshot for checksum-identical inputs.

Prepare snow and climate

The staged snow median is used directly by default. The former unconditional median
calculation depended on seven commented-out raster loads. To rebuild explicitly,
place the seven annual clipped TIFFs in data/permafrost/staged, following the names
in prepare_snow(), and run:

  Rscript --vanilla R/permafrost/prepare_permafrost.R --snow

Those annual files are not in the project; the original median is preserved.
The seven years are winter 2018/19 through 2024/25, using median(..., na.rm=TRUE).
Snow is projected once onto the DEM template, masked, and named explicitly.
All other predictors must already match the DEM grid; mismatches stop the run.
The selected hypsometric band remains hypsometric_range50_m (named hypso).
Vegetation retains its original integer encoding in model fitting. For partial
dependence it is evaluated at observed codes, rather than interpolated codes.

ECCC normals and station coordinate snapshots are already in data/permafrost/climate.
To prepare them explicitly if missing, use:

  Rscript --vanilla R/permafrost/prepare_permafrost.R --climate

Place ERALClim_bio1_1991-2020.tif in that same directory. It was copied from the
existing eralclim directory and checked against the original. To obtain it again,
download ERALClimBaselines_individualVariablesAndYears.zip from Zenodo record
8124385, extract its bio1_baselines.zip, and extract only the 1991-2020 TIFF.
The main analysis never downloads the 1.85 GB parent archive.

Stages and live spatPredict objects

revision/ separates input handling, model fitting, prediction, importance, partial
dependence, MESS, independent validation, maps, and the climate comparison into
functions. All model-dependent stages receive the same live res within one R process.
Do not save/reload run or run$res. Their terra objects have external pointers.
The analysis does not save the full spatPredict object. It exports the fitted
non-spatial caret model separately as fitted_model.rds, sample attributes and
coordinates as CSV, and the sample CRS as WKT. GeoTIFFs are portable raster outputs.
The independently refitted probability forest is exported separately for inspection.

To fit and inspect without regional prediction or MESS:

  run <- run_permafrost(profile = "quick", stages = c("model", "importance", "pdp"),
                        output_dir = "data/permafrost/outputs/quick_interpretation")

predict, importance, and pdp require model in the same run. To make maps or validate
existing portable rasters without retraining, specify their folder explicitly:

  run <- run_permafrost(stages = c("validation", "maps"),
    raster_dir = "data/permafrost/outputs/publication",
    output_dir = "data/permafrost/outputs/publication_maps_2")

All outputs go into an empty output directory, defaulting to outputs/publication
or outputs/quick. An existing directory containing files is refused. Select a new
output_dir for another run. --output-dir and --raster-dir provide the CLI equivalents.
Outputs record settings, code/input checksums, installed packages, sessionInfo,
the installed spatPredict implementation, stage times, and completion status.
Input hashing reads several GB once per run; checksums=FALSE is available for local
diagnostics, and is recorded as missing hashes in that run's manifest.

Corrections and manuscript checks

The refactor fixes the hypso/hypsometric naming mismatch, workspace-dependent input
loads, the Casino raster's binary pmfst_int field handling, and a missing graphics
device closure. A redundant rast(existing SpatRaster) call discarded raster values;
that call is removed. The unused 185-million-row data-frame conversion is removed.
MESS is calculated over the same nine continuous predictors; vegetation is excluded
as before. Only its figure histogram uses at most one million sampled cells to
avoid loading the full raster into RAM; its surface and negative-cell proportion
are still computed on the full grid. Missing MESS cells are consistently excluded.
The right-edge latitude labels are corrected for terra 1.8-54's ordering behavior.
Unicode plot symbols are escaped in the R sources so Windows locale settings do
not corrupt degree signs, superscripts, or the importance-axis label.
All ten partial-dependence predictors are now included, as stated in the manuscript.
Historical Figure6/Figure7 output names are retained; confirm final figure numbering.

The climate output contains both the ECCC annual "Year" normal and the original
mean of Jan-Dec plus Year. The latter was used by the old script but is not an
annual-normal selection. Recheck the reported temperature offsets before publication.

The saved model report's 0.9137 is caret's overall Accuracy. The macro average of
one-versus-rest balanced accuracies is a different statistic. The model report and
holdout_accuracy_definitions.csv expose both; reconcile the manuscript's labels.
This refactor preserves Accuracy-based model tuning rather than changing the analysis.

The thermistor selection rule is preserved: intersect the training footprint;
otherwise select sites within 15 km of Casino; then identify the nearest three
classified sites north of the training area. north_row_ids in config.R can pin those
three original CSV row IDs. This is not the same as reporting all remaining sites.
The tested existing raster gave 32 MESS-valid observations: 14 in the training
footprint, 12 near Casino, and 6 elsewhere. The manuscript currently states 16 in
training and also discusses six elsewhere; resolve the intended groups before release.
The coverage table and point-level CSV include all observations for that audit.

Validation performed

  Rscript --vanilla R/permafrost/checks/smoke_test.R

The small synthetic check covers input loading, repeated-seed model sampling and
fitting, raster class codes, standalone fitted-model serialization, importance,
ten-predictor partial dependence, MESS, and graphics cleanup. Its outputs are clearly
separated under outputs/checks. It is not an estimate of publication performance.
Preflight was also run with the actual input files in a clean R process. Maps and
thermistor/Casino tables were regenerated from the existing prediction/MESS rasters
in separate refactor_* check directories, and their PNGs were visually inspected.
The Casino agreement was 75.1434%. The offline climate comparison was executed.
A full 80,000-per-class fit and whole-region prediction have not been rerun here.
