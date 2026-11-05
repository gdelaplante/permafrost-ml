# High resolution permafrost mapping by machine learning

This repository contains the R workflow and data snapshot accompanying the Dawson Range permafrost mapping study. The revised analysis trains a four-class random forest from Coffee Gold Project permafrost polygons, predicts across the regional grid, measures environmental similarity, and produces interpretation figures, independent validation tables, and maps.

The current entry point is [`R/permafrost/permafrost_model_revision.R`](R/permafrost/permafrost_model_revision.R). The earlier root-level `SI.R` is preserved in [`legacy/SI_original.R`](legacy/SI_original.R).

## Data release status

**The code branch includes 20 small input and metadata files. Fourteen larger inputs remain prepared locally for Git LFS upload and are not yet hosted.** The complete 34-file snapshot totals 4.93 GiB. The full regional analysis requires that pending data release or exact hosted equivalents; the synthetic smoke test runs without it.

Git LFS is configured. The local `codex/publication-input-data` branch is reserved for the complete data commit and can be pushed from a faster connection. The code branch does not contain pointers to unavailable LFS objects.

## Get the code and available data

Install [Git LFS](https://git-lfs.com/) before cloning:

```sh
git lfs install
git clone https://github.com/gdelaplante/permafrost-ml.git
cd permafrost-ml
git lfs pull
Rscript --vanilla R/permafrost/verify_inputs.R --available
```

The required full input snapshot contains **34 files totaling 4.93 GiB**; it is not all available on this code branch yet. TIFFs and GeoPackages use Git LFS; small observations, shapefile companions, and metadata use ordinary Git. [`input_manifest.csv`](R/permafrost/input_manifest.csv) records every required file's path, size, checksum, and storage type. Include `.aux.xml` files: some raster band descriptions are stored there.

Use Git with LFS to obtain the data. A source ZIP may contain pointer files depending on the repository's archive settings. Run the verifier without `--available` once the full data release has been obtained. The input verifier checks the materialized data, and the analysis reports a clear error if it encounters an LFS pointer. [GitHub's LFS documentation](https://docs.github.com/en/repositories/working-with-files/managing-large-files/about-git-large-file-storage) explains how the pointers and file downloads work.

## Restore R packages and run

The full-data commands below require all 34 inputs. Until the data release is uploaded, use the synthetic smoke test to exercise the workflow.

R 4.5.1 and the tested analysis package versions are recorded in [`renv.lock`](R/permafrost/renv.lock). From R in the repository root:

```r
install.packages("renv") # only if needed
renv::restore(lockfile = "R/permafrost/renv.lock", prompt = FALSE)
renv::load()
source("R/permafrost/permafrost_model_revision.R")
run_permafrost(profile = "quick", check_only = TRUE)
run <- run_permafrost(profile = "quick")
```

If the required packages are already available in the R library used by `Rscript`:

```sh
Rscript --vanilla R/permafrost/permafrost_model_revision.R --profile=quick --check
Rscript --vanilla R/permafrost/permafrost_model_revision.R --profile=quick
Rscript --vanilla R/permafrost/permafrost_model_revision.R --profile=publication
```

| Profile | Samples per class | Cross-validation |
| --- | ---: | --- |
| `quick` | 10,000 | 2 folds, 2 repeats |
| `publication` | 80,000 | 10 folds, 10 repeats |

There are four classes, so these request about 40,000 and 320,000 observations before the spatial training/holdout split. Repeated cross-validation runs inside the training portion. The quick profile still processes the full regional raster; use the synthetic smoke test below for a short execution check.

Sourcing the entry point defines `run_permafrost()` without starting a run. Settings are centralized in [`revision/config.R`](R/permafrost/revision/config.R). Fixed seeds and sequential resampling are used by default. Each run writes into an empty output directory under `data/permafrost/outputs/`, recording settings, input/code checksums, package versions, stage timings, and status. Generated outputs are ignored by Git.

## Workflow and data provenance

Functions under [`revision/`](R/permafrost/revision/) handle inputs, model fitting, raster prediction, permutation importance, partial dependence, MESS, validation, maps, and the climate comparison. Model-dependent stages share the same live `spatPredict` result in one R process. **Do not save and reload `run` or `run$res`**: their `terra` objects contain external pointers. Portable outputs include GeoTIFFs, CSV sample/metric tables, and a separately exported non-spatial fitted model.

The prepared full snapshot includes the processed predictors, training polygons, Casino binary reference raster, YGS thermistor observations, compact map-context layers, and frozen climate inputs. See [`data_sources.csv`](R/permafrost/data_sources.csv) for sources and preparation limits. Versioning the processed grids preserves the analysis inputs; the original terrain, solar-radiation, and hypsometric preprocessing recipes are not fully reconstructed here.

[`context_sources.csv`](R/permafrost/context_sources.csv) links to GeoYukon and Statistics Canada sources. Optional preparation can rebuild missing context from local sources or explicit downloads:

```sh
Rscript --vanilla R/permafrost/prepare_permafrost.R --context --download
```

The climate comparison raster can also be retrieved directly from its public upstream archive:

```sh
Rscript --vanilla R/permafrost/prepare_permafrost.R --bio1
```

The provider offers a ~1.85 GB ZIP containing a nested BIO1 ZIP. The script caches the download, extracts the required 1991-2020 TIFF, and checks it against the recorded size and checksum. It does not download anything when the input is already present.

For exact processed files hosted elsewhere, add HTTPS URLs to [`input_downloads.csv`](R/permafrost/input_downloads.csv) and run:

```sh
Rscript --vanilla R/permafrost/download_inputs.R --check
Rscript --vanilla R/permafrost/download_inputs.R --download
```

No direct hosted URLs for the custom terrain/radiation grids were available during this preparation. Public upstream products cannot substitute for the exact derived grids without their missing preprocessing recipes. The downloader rejects bytes that do not match the manifest and keeps successful downloads for later offline use. Rebuilt map-context GeoPackages can differ from the frozen snapshot at the byte level.

Routine analysis uses locally cached inputs offline. It does not download the large climate archive or refresh observations. Preparation keeps existing snapshots; newly downloaded upstream data should be treated as a new version.

## Validation and release status

```sh
Rscript --vanilla R/permafrost/checks/validate_workflow.R
Rscript --vanilla R/permafrost/checks/smoke_test.R
```

The smoke test uses small synthetic rasters and covers disk-backed input loading, repeated-seed fitting, class codes, standalone fitted-model serialization, importance, all ten partial-dependence predictors, MESS, and graphics cleanup. Its artifacts are separated under the ignored `outputs/checks/` directory. It does not estimate publication accuracy.

The refactor also passed preflight against the actual inputs; maps, climate comparisons, and independent validation were checked against the existing regional outputs. A complete publication-profile fit and regional prediction have not been rerun. A clean dependency download/restore has not been tested. Before a publication release, reconcile the reported accuracy terminology, thermistor groups, and climate offsets described in the [detailed workflow notes](R/permafrost/README.txt).

[`legacy/perm_plots.R`](legacy/perm_plots.R) and the original SI script retain historical algorithm-comparison and sample-size work. Those older experiments are not part of the new one-command workflow; their saved model objects remain in the local MSc research archive and require separate recovery of portable metrics before inclusion in a fully reproducible SI release.

See the [detailed instructions](R/permafrost/README.txt) for stage selection, maps from existing rasters, output contents, and methodological caveats. See the [license](LICENSE) and the separately recorded data-source attributions.
