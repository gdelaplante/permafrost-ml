Historical model-selection analyses

SI_original.R is the earlier supplementary script. perm_plots.R contains plotting
code for historical algorithm comparisons and sample-size experiments. These files
help identify the analyses supporting SI Section 4 and Supplementary Figure 2.

They retain placeholder/older paths and depend on original experiment objects that
are not distributed in this repository. They are not runnable end to end from a fresh
checkout and should not be used as the entry point for the final model workflow.
Saved spatPredict spatial objects cannot safely be restored because they contain
external pointers. Reproduction of the historical comparisons requires exporting
portable metrics from their original runs or implementing new experiment runs.

Use R/permafrost/permafrost_model_revision.R for the maintained final-model analysis.
