# Supplementary information for paper 'Regional-Scale High-Resolution Permafrost Mapping by Scalable Machine Learning'

# Model parametrization and sample size selection ###################

# Find the 10 models most similar to ranger (11 because one of these is not able to run)
models <- SAiVE::modelMatch("ranger", similarity = 0.7)
# Order the result, select top 10
models <- models[
  order(models$`Similarity to Random Forest (ranger)`, decreasing = TRUE),
][1:11, ]

# Resample vegetation raster to the same resolution and extent as other layers
vegetation <- terra::rast("path/CAN_NALCMS_landcover_2020_30m.tif")
DEM <- terra::rast("path/DEM.tif")
terra::crs(vegetation) <- DEM # Assign the same crs as other layers

vegetation <- terra::resample(vegetation, Elevation, method = "near") # resample to the same resolution and extent as other layers
# terra::writeRast(vegetation, "path/vegetation_resampled.tif", overwrite = TRUE)
# vegetation <- terra::rast("path/vegetation_resampled.tif")

# Load the predictor and outcome variables, make trainControl
features <- c(
  aspect = terra::rast("path/Aspect.tif"),
  DEM = DEM,
  difrad = terra::rast("path/DiffuseRad.tif"),
  dirrad = terra::rast("path/DirectRad.tif"),
  profcurv = terra::rast("path/ProfileCurv.tif"),
  tancurv = terra::rast("path/TangentCurv.tif.tif"),
  slope = terra::rast("path/SlopeDeg.tif"),
  veg = vegetation
)
outcome <- terra::vect("path/Tetra_pmfst.shp")[, 2]
outcome$Type <- as.factor(outcome$Type)

trainControl <- caret::trainControl(
  method = "repeatedcv",
  number = 10, # 10-fold Cross-validation
  repeats = 10, # repeated ten times
  verboseIter = FALSE,
  returnResamp = "final",
  savePredictions = "all",
  allowParallel = TRUE
)

# Run spatPredict using the 10 models to find the best one
res <- SAiVE::spatPredict(
  features,
  outcome,
  10000, # 10 000 sample points just to find the best performing method
  trainControl = trainControl, # Same trainControl for all models (otherwise matched one to one)
  methods = models$`Model Abbreviation`,
  predict = FALSE, # Don't bother predicting across the whole raster for now
  fastCompare = FALSE, # We don't want to reduce the training set size further
  thinFeatures = FALSE # Don't attempt to trim features with VSURF (already trimmed, and just tries to leave everything in anyways)
)
# Accuracy vs sample size
test <- list()
samp <- c(
  thousand = 1000,
  twothousand = 2000,
  fourthousand = 4000,
  sixthousand = 6000,
  eightthousand = 8000
  # ... and so on
)

for (i in 1:length(samp)) {
  test[[names(samp[i])]] <- SAiVE::spatPredict(
    features,
    outcome,
    samp[i],
    trainControl = trainControl,
    methods = res$selected_method, # Only use best performing model from previous step
    thinFeatures = FALSE
  )
}

# Save the output in case anything goes wrong… the previous step is very long
save(test, file = "path.Rdata")

# Reload
# load("path.Rdata")

df <- data.frame(
  points = samp,
  accuracy = c(
    test$thousand$selected_model_performance$overall[1],
    test$twothousand$selected_model_performance$overall[1],
    test$fourthousand$selected_model_performance$overall[1],
    test$sixthousand$selected_model_performance$overall[1],
    test$eightthousand$selected_model_performance$overall[1]
    # … and so on
  )
)

# Plot the result
ggplot2::ggplot(df, ggplot2::aes(x = points, y = accuracy)) +
  ggplot2::geom_point() +
  ggplot2::labs() +
  ggplot2::geom_smooth(method = "loess")


# Training and applying the model ###################

# Load the predictor and outcome variables
features <- c(
  aspect = terra::rast("path/Aspect.tif"),
  DEM = terra::rast("path/DEM.tif"),
  difrad = terra::rast("path/DiffuseRad.tif"),
  dirrad = terra::rast("path/DirectRad.tif"),
  profcurv = terra::rast("path/FocalStatProfileCurv.tif"),
  tancurv = terra::rast("path/FocalStatTangentCurv.tif"),
  slope = terra::rast("path/SlopeDeg.tif"),
  veg = terra::rast("path/vegetation_resampled.tif")
)

outcome <- terra::vect(
  "data/permafrost/training/MineSitePermafrost_TetraTech_20170224.dbf"
)
outcome$Type <- as.factor(outcome$Type)
# Retain only the column with the class labels, drop the rest
outcome <- outcome[, "Type"]

# make trainControl object for caret
trainControl <- caret::trainControl(
  method = "repeatedcv",
  number = 10, # 10-fold Cross-validation
  repeats = 10, # repeated ten times
  verboseIter = FALSE,
  returnResamp = "final",
  savePredictions = "all",
  allowParallel = TRUE
)

# Run spatPredict to get a quick model used for making PDP plots
res <- SAiVE::spatPredict(
  features, # predictor variables as a list of terra::spatRasters
  outcome, # outcome variable as a terra::vect with the class labels as a factor
  10000, # 10 000 sample points for a quick but reasonably accurate run; we ended up with 80 000 for the best performing model
  trainControl = trainControl, # Same trainControl for all methods (otherwise matched one to one)
  methods = "ranger", # The best performing methods from an iterative run with 10 methods
  predict = FALSE, # Don't predict across the whole raster for now
  fastCompare = FALSE, # We don't want to reduce the training set size further (and no comparison between methods is being done anyways)
  thinFeatures = FALSE # Don't attempt to trim features with VSURF (already trimmed)
)
# > names(res)
# [1] "training" "testing" "selected_method" "best_model" "selected_model_performance"
# Training = the training data used to train the model as a spatVector
# Testing = the testing data used to test the model as a spatVector
# selected_method = the method that performed best in the training/testing phase as a character string (e.g. "ranger")
# best_model = the best performing model as a caret 'train' object
# selected_model_performance = the performance of the best model on the testing data as a list

# To predict across the whole raster, use:
features <- terra::rast(features) # make sure features is a SpatRaster, not a list of SpatRasters
prediction <- terra::predict(
  object = features,
  model = res$best_model,
  na.rm = TRUE,
  progress = "text",
  filename = "output_file.tif",
  overwrite = TRUE,
  cores = 8
)


# Comparison of model output to Casino Mining Project map ##################
# This script was used to compare the model output to the Casino Mining Project's map.
# To replicate, replace the paths in the script with the appropriate paths on your system.

casino <- terra::vect("path_to_casino_map.shp")

# Turn the Permafrost column into a binary column
# column pmfts_int gets 1 if column Permafrost == 'yes' else 0
casino$pmfst_int <- ifelse(casino$Permafrost == 'yes', 1, 0)

# Write the vector to disk for later use
# terra::writeVector(casino, "path_to_write_reclassed_vector", overwrite = TRUE)

# Load the predicted surface, generated earlier:
model <- terra::rast("path_to_predicted_surface.tif")

# Make the casino vector file into a raster of same resolution as 'model'
casino <- terra::project(casino, model)
casino_rast <- terra::rasterize(casino, model, field = "pmfst_int")
# terra::writeRaster(casino_rast, "path_to_write_raster", overwrite = TRUE)

# Reclassify the model raster to binary using the same threshold as the casino map (1 = permafrost, 0 = no permafrost)
tbl <- data.frame(id = c(1:4), c(1, 1, 0, 1))

model_bin <- terra::classify(model, tbl)
# terra::writeRaster(model_bin, "path_to_write_reclassed_model”, overwrite = TRUE)

# Determine the agreement between the model and the casino_rast data
agreement <- model_bin == casino_rast
# Find the percentage of cells that agree
agreement_perc <- sum(values(agreement), na.rm = TRUE) /
  terra::ncell(agreement) *
  100

# Print the agreement percentage
agreement_perc


# Creating PDP plots ##############
# pdp_vars <- c("dirrad", "difrad", "aspect", "slope")
pdp_vars <- names(features)

training_df <- terra::as.data.frame(res$training)

response_col <- "Type"

train_y <- factor(make.names(as.character(training_df[[response_col]])))

train_x <- training_df[,
  setdiff(names(training_df), response_col),
  drop = FALSE
]

pdp_model <- caret::train(
  x = train_x,
  y = train_y,
  method = "ranger",
  trControl = caret::trainControl(
    method = "none",
    classProbs = TRUE
  ),
  tuneGrid = res$best_model$bestTune
)

model <- pdp_model

class_names <- colnames(stats::predict(
  model,
  newdata = train_x[1, , drop = FALSE],
  type = "prob"
))


pdp_list <- list()
i <- 1L

for (pdp_var in pdp_vars) {
  x <- train_x[[pdp_var]]

  if (is.numeric(x)) {
    pdp_grid <- seq(
      stats::quantile(x, 0.02, na.rm = TRUE),
      stats::quantile(x, 0.98, na.rm = TRUE),
      length.out = 40
    )
  } else if (is.factor(x)) {
    pdp_grid <- levels(x)
  } else {
    pdp_grid <- sort(unique(x))
  }

  for (pdp_value in pdp_grid) {
    newdata <- train_x

    if (is.factor(x)) {
      newdata[[pdp_var]] <- factor(pdp_value, levels = levels(x))
    } else {
      newdata[[pdp_var]] <- pdp_value
    }

    prob <- stats::predict(
      model,
      newdata = newdata,
      type = "prob"
    )

    mean_prob <- colMeans(prob[, class_names, drop = FALSE], na.rm = TRUE)

    pdp_list[[i]] <- data.frame(
      predictor = pdp_var,
      value = as.character(pdp_value),
      class = names(mean_prob),
      mean_prob = as.numeric(mean_prob),
      stringsAsFactors = FALSE
    )

    i <- i + 1L
  }
}

pdp_df <- do.call(rbind, pdp_list)


# Check that things are as expected
head(pdp_df) # Should be a data frame with columns predictor, value, class, and mean_prob
unique(pdp_df$class) # Should be the same as the class names in the training data
unique(pdp_df$predictor) # Should be the same as the pdp_vars vector


old_par <- graphics::par(no.readonly = TRUE)
on.exit(graphics::par(old_par))

# vars <- unique(pdp_df$predictor)

# Limit to only the top 5 most important variables for the sake of making a nice figure
vars <- c("aspect", "DEM", "dirrad", "profcurv", "slope")
display_names <- c(
  aspect = "Aspect (degrees)",
  DEM = "Elevation (m)",
  dirrad = "Direct solar radiation (W/m²)",
  profcurv = "Profile curvature (1/m)",
  slope = "Slope (degrees)"
)
classes <- unique(pdp_df$class)

graphics::par(
  mfrow = grDevices::n2mfrow(length(vars)),
  mar = c(4, 4, 3, 1)
)

cols <- seq_along(classes)
names(cols) <- classes

# legend <- TRUE
legend <- FALSE # For no legend at all
even <- TRUE

panel_labels <- LETTERS[seq_along(vars)]
panel_i <- 1L

for (v in vars) {
  d <- pdp_df[pdp_df$predictor == v, ]

  # detect numeric vs categorical
  x_num <- suppressWarnings(as.numeric(d$value))
  is_num <- !any(is.na(x_num))

  if (is_num) {
    x <- x_num
    xlim <- range(x)
  } else {
    x_levels <- unique(d$value)
    x <- match(d$value, x_levels)
    xlim <- c(1, length(x_levels))
  }

  graphics::plot(
    NA,
    xlim = xlim,
    ylim = range(d$mean_prob),
    xlab = display_names[v],
    cex.axis = 1.5, # increase axis tick size
    cex.lab = 1.5, # increase axis label size
    ylab = if (even) "Mean predicted probability" else "",
    main = NULL,
    xaxt = if (is_num) "s" else "n"
  )

  if (!is_num) {
    graphics::axis(1, at = seq_along(x_levels), labels = x_levels, las = 2)
  }

  for (cl in classes) {
    dc <- d[d$class == cl, ]

    if (is_num) {
      xx <- as.numeric(dc$value)
    } else {
      xx <- match(dc$value, x_levels)
    }

    ord <- order(xx)

    graphics::lines(
      xx[ord],
      dc$mean_prob[ord],
      col = cols[cl],
      lwd = 2
    )
  }

  graphics::mtext(
    panel_labels[panel_i],
    side = 3, # top
    adj = 0, # left align
    line = 0.2, # distance from plot edge (smaller = closer)
    cex = 2, # size of the label
    font = 2 # bold
  )
  panel_i <- panel_i + 1L

  # Only plot a legend for the first one
  if (legend) {
    graphics::legend(
      "topright",
      legend = classes,
      col = cols,
      lwd = 2,
      bty = "n",
      cex = 1.2,
      inset = c(0.1, 0)
    )
    legend <- FALSE
  }

  if (even) {
    even <- FALSE
  } else {
    even <- TRUE
  }
}

# Adding a legend to empty panel (assuming the last panel is empty)
# Empty panel for legend
graphics::plot.new()

graphics::legend(
  "center",
  legend = classes,
  col = cols,
  lwd = 2,
  bty = "n",
  cex = 1.5
)


# Calculation of environmental similarity (MESS) values and map/plot generation ##################

# Load predictor variables
Elevation <- terra::rast("data/permafrost/predvars/DEMClipped.tif")
SlopeDeg <- terra::rast("data/permafrost/predvars/SlopeDeg.tif")
AspectDeg <- terra::rast("data/permafrost/predvars/Aspect.tif")
TangentCurv <- terra::rast("data/permafrost/predvars/FocalStatTangentCurv.tif")
ProfileCurv <- terra::rast("data/permafrost/predvars/FocalStatProfileCurv.tif")
DiffuseSolarRad <- terra::rast("data/permafrost/predvars/DiffuseRad.tif")
DirectSolarRad <- terra::rast("data/permafrost/predvars/DirectRad.tif")

features <- list(
  Elevation = Elevation,
  SlopeDeg = SlopeDeg,
  AspectDeg = AspectDeg,
  TangentCurv = TangentCurv,
  ProfileCurv = ProfileCurv,
  DiffuseSolarRad = DiffuseSolarRad,
  DirectSolarRad = DirectSolarRad
)

# stack features into a single SpatRaster
features <- terra::rast(features)

# Downsample features by a factor of 10 for speed if desired (optional)
# Adjust cores to what you have available
features <- terra::aggregate(
  features,
  fact = 10,
  fun = mean,
  na.rm = TRUE,
  cores = 6
)

# Get a vector of the coffee creek region
PermMap <- terra::vect(
  "data/permafrost/training/MineSitePermafrost_TetraTech_20170224.dbf"
)
PermMap <- PermMap[, "Type"]
PermMap <- terra::project(PermMap, features) # ensure the crs is the same)

# Extract the predictor values at the locations of PermMap
predvals_coffee <- terra::crop(features, PermMap)
predvals_coffee <- terra::mask(predvals_coffee, PermMap)
predvals_coffee <- terra::as.data.frame(
  predvals_coffee,
  xy = TRUE,
  na.rm = TRUE
)

predvals_all <- terra::as.data.frame(features, xy = TRUE, na.rm = TRUE)

# install.packages("dismo")
# With dismo, first argument is a raster, second is a matrix/data.frame
# Convert predvals_coffee to a raster::stack
# Drop vegetation, dismo::mess doesn't play well with categorical variables
features_stack <- raster::stack(terra::subset(
  features,
  c(
    "Elevation",
    "SlopeDeg",
    "AspectDeg",
    "TangentCurv",
    "ProfileCurv",
    "DiffuseSolarRad",
    "DirectSolarRad"
  )
))
# Expect warnings aabout 'no non-missing arguments' because of NA values
dismo <- dismo::mess(features_stack, predvals_coffee[, c(3:9)]) # Leave out x and y columns

# Save dismo as a raster
raster::writeRaster(dismo, "path.tif", overwrite = TRUE)
# dismo <- raster::raster("path.tif")

# Turn it back to a terra object for easier plotting
dismo <- terra::rast(dismo)


# define colour-blind friendly palettes
mess_cols <- grDevices::hcl.colors(100, "Viridis")
neg_cols <- grDevices::hcl.colors(100, "Inferno")

# negative raster
neg <- dismo
neg[neg >= 0] <- NA

# proportion negative
prop_neg <- terra::global(dismo < 0, "sum", na.rm = TRUE)[1, 1] /
  terra::global(!is.na(dismo), "sum", na.rm = TRUE)[1, 1]

# publication figure for MESS values
png(
  "mess_publication_figure.png",
  width = 3000,
  height = 1000,
  res = 300
)

par(
  mfrow = c(1, 3), # 1 row, 3 columns
  oma = c(0, 0, 2, 0) # outer margins (bottom, left, top, right)
)

# 1. Full MESS raster (left panel)
terra::plot(
  dismo,
  col = mess_cols,
  axes = FALSE,
  box = FALSE,
  main = "",
)
mtext(
  "A: MESS values\n",
  side = 3,
  line = 2,
  cex = 1
)

# 2. Histogram (middle panel)
hist(
  terra::values(dismo),
  breaks = 50,
  col = "grey75",
  border = "grey35",
  main = "",
  xlab = "MESS values",
  ylab = "Frequency"
)
# Add line at 0 to show the threshold for extrapolation
abline(v = 0, lty = 2, lwd = 2)
mtext(
  "B: Distribution\n",
  side = 3,
  line = 2,
  cex = 1
)

# 3. Negative MESS values only (right panel)
terra::plot(
  neg,
  col = neg_cols,
  axes = FALSE,
  box = FALSE,
  main = ""
)


mtext(
  paste0("C: Negative MESS values\n", round(prop_neg * 100, 1), "% of cells"),
  side = 3,
  line = 2,
  cex = 1
)

dev.off()
