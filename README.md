# Extending high resolution field permafrost mapping by random forest

R code and analysis inputs for the Dawson Range permafrost mapping study. The workflow uses the Coffee Gold Project field map to train four ground-condition classes, predicts their regional distribution, and evaluates predictions using environmental similarity, the Casino reference map, and YGS thermistor observations.

## Obtain the code and inputs

Clone this repository using GitHub Desktop or your IDE with Git LFS enabled, or download the source ZIP from GitHub and extract it. Open `permafrost-ml.Rproj` in RStudio, or open the repository folder in VSCode/Positron and use it as the R working directory. No terminal commands are required.

Open [`R/permafrost/reproduce_paper.R`](R/permafrost/reproduce_paper.R) and run its numbered sections in order. It includes package setup, data download and verification, profile selection, execution, and inspection of outputs. The following data steps also work directly in the R console:

```r
# Defines the downloader; sourcing does not start a download.
source("R/permafrost/download_inputs.R")
# Retrieves missing data or materializes LFS pointers from fixed snapshot URLs.
download_snapshot_inputs(getwd())
# Defines the verifier; the following call checks every input's size and MD5.
source("R/permafrost/verify_inputs.R")
verify_permafrost_inputs()
```

The complete snapshot has **34 files totaling 4.93 GiB**. TIFFs and GeoPackages use Git LFS; the R downloader can also retrieve them without an LFS client. Existing real inputs are preserved. Keep shapefile companions and raster `.aux.xml` files, which contain geometry or metadata. [`input_manifest.csv`](R/permafrost/input_manifest.csv) records the exact files and checksums.

## Software and execution

The analysis environment is recorded in [`renv.lock`](R/permafrost/renv.lock), including R 4.5.1, SAiVE 1.0.6, terra 1.8-54, caret 7.0-1, and ranger 0.18.0. From R in the repository root:

```r
install.packages("renv") # if not already installed
renv::restore(lockfile = "R/permafrost/renv.lock", prompt = FALSE)
renv::load()
source("R/permafrost/permafrost_model_revision.R")
run_permafrost(profile = "publication", check_only = TRUE)
run <- run_permafrost(profile = "publication")
```

Restoring packages requires internet access and may require platform-specific geospatial/build libraries. Once packages and inputs are present, the analysis runs offline. The `source()` call above loads function definitions and does not start a model run. The `run_permafrost()` calls perform the checks and analysis explicitly. There is no need to run this code through a terminal.

| Profile | Requested samples per class | Repeated cross-validation |
| --- | ---: | --- |
| `publication` | 80,000 | 10 folds, 10 repeats |
| `quick` | 10,000 | 2 folds, 2 repeats |

Four classes give approximately 320,000 or 40,000 sampled observations before the spatial training/holdout split. The quick profile changes model size and resampling; it still predicts across the full regional grid. That grid has approximately 185 million cells, so allow substantial runtime and disk space for raster intermediates. For a small execution check that does not use the regional dataset:

```r
# Executes a small synthetic model/plotting test, without regional inputs.
source("R/permafrost/checks/smoke_test.R")
```

Sourcing the entry point defines `run_permafrost()` without starting an analysis. Configuration is in [`revision/config.R`](R/permafrost/revision/config.R). Results default to `data/permafrost/outputs/publication/` or `quick/`. Use a new, empty `output_dir` for each run; existing results are preserved.

## How to read the code

The walkthrough explains each `source()` call where it occurs. Sourcing `permafrost_model_revision.R` loads the following files into a shared analysis environment; these calls define functions and do not process data. The descriptions are also beside the actual loading calls in that file.

| Module under `R/permafrost/revision/` | Purpose |
| --- | --- |
| `config.R` | Profiles, seeds, input paths, predictor names, class codes |
| `helpers.R` | Input/package checks, manifests, masking and stage records |
| `inputs.R` | Predictor geometry, snow alignment and polygon loading |
| `model.R` | Forest fitting, holdout metrics, portable exports and prediction |
| `importance.R` | Class-specific permutation importance |
| `partial_dependence.R` | Probability forest and partial-dependence curves |
| `mess.R` | Nine-variable environmental similarity and its figure |
| `validation.R` | Thermistor groups/metrics and Casino agreement |
| `maps.R` | Regional and Casino comparison maps |
| `climate.R` | Station-normal/BIO1 comparison |
| `pipeline.R` | Execution order and live objects passed between functions |

Read `pipeline.R` to follow the full calculation, then the corresponding module for each method. `download_inputs.R`, `verify_inputs.R`, and `prepare_permafrost.R` are separately sourced utilities: sourcing defines their functions; subsequent named calls perform acquisition, verification, or preparation.

## Analyses and paper outputs

| Paper component | Generated file or analysis |
| --- | --- |
| Table 2: holdout classification metrics | `holdout_by_class.csv`, `holdout_overall.csv`, `holdout_accuracy_definitions.csv` |
| Table 3 and SI Table 1: confusion counts | `holdout_confusion.csv` (long format; explicit Prediction and Reference columns) |
| Figure 2: environmental similarity | `MESS_snow_hypso.tif`, `MESS_summary.csv`, `mess_publication_figure.png` |
| Figure 3: Casino comparison | `casino_comparison_map_revised.png`, `casino_agreement.csv` |
| Figure 4: regional predictions | `prediction_w_snow_hypso.tif`, `prediction_classes.csv`, `prediction_map.png` |
| Figure 5: permutation importance | `Figure6_importance_w_snow+hypso.csv`, `Figure6_variable_importance_w_snow+hypso.png` |
| Figure 6: partial dependence | `Figure7_partial_dependence.csv`, `Figure7_PDP_w_snow+hypso.png` |
| Thermistor validation | `thermistors_point_comparison.csv`, coverage/performance/confusion tables, and regional comparison PNG |
| Climate comparison | `climate_comparison.csv` |

The mapping above relates the current manuscript numbering to output filenames. Confusion tables and thermistor summaries are analysis outputs rather than typeset copies of the paper tables. In particular, current thermistor summaries use the training footprint, a 15-km Casino vicinity, and the nearest three classified northern sites; they do not yet provide the manuscript's combined six-site outside group.

## Interpretation and reproducibility scope

The model predicts ice-poor, ice-rich, permafrost-free, and very ice-rich ground. SAiVE supplies the spatial holdout partition; caret performs repeated cross-validation within its training portion. Tuning currently uses caret's default classification Accuracy. Overall accuracy and mean class-specific one-versus-rest balanced accuracy are exported separately and should not be interchanged.

MESS uses nine continuous predictors, excluding vegetation. Permutation importance uses five shuffles per predictor and one-versus-rest balanced accuracy. Partial dependence uses a separately fitted probability forest when necessary, a class-balanced reference sample, and the central 2nd–98th percentile range for continuous predictors. Vegetation is evaluated at observed codes. See the [detailed methods and running instructions](R/permafrost/README.txt).

The publication profile uses the reported sample and cross-validation settings and a recorded seed. It refits the model; the historical fit's random state was not archived, so exact agreement with its reported numerical results is not guaranteed. Each run records configuration, input/code checksums, package versions, stage times, and completion status.

The maintained workflow starts from the supplied processed predictor grids. It does not reconstruct all raw DEM, radiation, curvature, or hypsometric preprocessing. Historical algorithm-comparison/sample-size experiments are retained as legacy code, but are not runnable end to end from this distribution. Figure 7's comparison with external regional permafrost maps and Figure 1's photographs/composite artwork are not generated by this workflow.

## Data provenance and optional acquisition

[`data_sources.csv`](R/permafrost/data_sources.csv) describes the input products and derivation limits. [`context_sources.csv`](R/permafrost/context_sources.csv) links to public map sources. Normal analysis uses the frozen LFS snapshot. Optional commands rebuild missing context or obtain the climate raster from its public archive:

```r
# Defines preparation functions only; calls below explicitly request preparation.
source("R/permafrost/prepare_permafrost.R")
prepare_context(getwd(), download = TRUE)
prepare_bio1(getwd())
```

The BIO1 source is a nested archive of approximately 1.85 GB; the required TIFF is checksum-verified and cached. Newly rebuilt GeoPackages may differ from the frozen snapshot at the byte level. Fixed URLs for the archived LFS objects are supplied in input_downloads.csv. Alternative exact-file HTTPS mirrors can be entered in [`input_downloads.csv`](R/permafrost/input_downloads.csv) and retrieved using `download_snapshot_inputs(getwd())`.

**Keep `spatPredict` results in the R session that created them.** Do not save/reload `run` or `run$res`, which contain terra external pointers. Use exported GeoTIFFs, CSVs, and the separate fitted-model RDS for portable artifacts. See [LICENSE](LICENSE) and the input-source attributions.
