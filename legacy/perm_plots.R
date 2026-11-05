load("data/permafrost/model optimization/model_comparison.RData")


plot_data <- data.frame(method = NA,
                        lower = NA,
                        mean = NA,
                        upper = NA)
for (i in 1:length(res$trained_models_performance)) {
  dat <- c(names(res$trained_models_performance)[[i]],
           res$trained_models_performance[[i]]$overall[[3]],
           res$trained_models_performance[[i]]$overall[[1]],
           res$trained_models_performance[[i]]$overall[[4]])
  plot_data <- rbind(plot_data, dat)
}
plot_data <- plot_data[-1,]
plot_data[,2] <- as.numeric(plot_data[,2])
plot_data[,3] <- as.numeric(plot_data[,3])
plot_data[,4] <- as.numeric(plot_data[,4])

plot_data2 <- data.frame(method = NA,
                        lower = NA,
                        mean = NA,
                        upper = NA)
for (i in 1:length(res2$trained_models_performance)) {
  dat <- c(names(res2$trained_models_performance)[[i]],
           res2$trained_models_performance[[i]]$overall[[3]],
           res2$trained_models_performance[[i]]$overall[[1]],
           res2$trained_models_performance[[i]]$overall[[4]])
  plot_data2 <- rbind(plot_data2, dat)
}
plot_data2 <- plot_data2[-1,]
plot_data2[,2] <- as.numeric(plot_data2[,2])
plot_data2[,3] <- as.numeric(plot_data2[,3])
plot_data2[,4] <- as.numeric(plot_data2[,4])

# Load ggplot2 if not already loaded
library(ggplot2)

ggplot(plot_data, aes(x = method, y = mean)) +  # Convert row numbers to factors for x-axis
  geom_point() +  # Plot the mean values as points
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +  # Add error bars
  labs(x = "Method", y = "Balanced Accuracy", title = "Machine Learning Method Accuracy") +
  theme_classic() +
  geom_point(data = plot_data2, aes(x = method, y = mean), col = "blue") +
  geom_errorbar(data = plot_data2, aes(ymin = lower, ymax = upper), width = 0.2, col = "blue")




accuracy_classes <- data.frame()
for (i in 1:length(res$trained_models_performance)) {
  data <- as.data.frame(res$trained_models_performance[[i]]$byClass)
  sub <- data.frame()
  for (j in 1:nrow(data)) {
    s <- data.frame(Method = names(res$trained_models_performance)[[i]],
                      Class = rownames(data)[[j]],
                      Sensitivity = data[j,1],
                      Specificity = data[j,2],
                      Precision = data[j,5],
                      "Balanced accuracy" = data[j,11],
                    check.names = FALSE)
    sub <- rbind(sub, s)
  }
  accuracy_classes <- rbind(accuracy_classes, sub)
}

# Load required libraries
library(ggplot2)
library(reshape2)

# Convert the data frame from wide to long format
accuracy_long <- melt(accuracy_classes, id.vars = c("Method", "Class"))

# Create a heatmap
ggplot(accuracy_long, aes(x = Method, y = Class, fill = value)) +
  geom_tile(color = "white") +
  facet_wrap(~variable, scales = "free_y") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme_minimal() +
  labs(title = "Performance Metrics Heatmap", x = "Method", y = "Class")

# Create bar plots for each metric
metrics <- c("sensitivity", "specificity", "precision", "balanced_accuracy")

for (metric in metrics) {
  ggplot(accuracy_classes, aes(x = "method", y = metric, fill = "class")) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_minimal() +
    labs(title = paste("Comparison of", metric, "across Methods"), x = "Method", y = metric) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Create line plots for each metric
ggplot(accuracy_long, aes(x = Method, y = value, color = Class, group = Class)) +
  geom_point(size = 2) +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  labs(x = "Method", y = "Value") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.title=element_blank())



# Accuracy vs sample size ############################
test <- list()
samp <- c(twothousand = 2000, fourthousand = 4000, sixthousand = 6000, eightthousand = 8000, tenthousand = 10000, twelvethousand = 12000, fourteenthousand = 14000, sixteenthousand = 16000, eighteenthousand = 18000, twentythousand = 20000, twentytwothousand = 22000, twentyfourthousand = 24000, twentysixthousand = 26000, twentyeightthousand = 28000, thirtytwothousand = 32000, thirtyeightthousand = 38000, fortyfourthousand = 44000, fiftythousand = 50000, fiftysixthousand = 56000, seventythousand = 70000, eightythousand = 80000, ninetythousand = 90000, hundredthousand = 100000)

# for (i in c(23)) {
#   test[[names(samp[i])]] <- SAiVE::spatPredict(features, outcome, samp[i], trainControl = trainControl, methods = 'ranger', thinFeatures = FALSE)
# }
# save(test, file = "H:\\gtdelapl\\n_test7.RData")

load("data/model optimization/n_test7.RData")

df <- data.frame(points = samp[names(samp) %in% names(test)],
                 accuracy_mean = NA,
                 accuracy_min = NA,
                 accuracy_max = NA)
for (i in 1:nrow(df)) {
  df$accuracy_mean[i] <- test[[rownames(df)[i]]]$selected_model_performance$overall[[1]]
  df$accuracy_min[i] <- test[[rownames(df)[i]]]$selected_model_performance$overall[[3]]
  df$accuracy_max[i] <- test[[rownames(df)[i]]]$selected_model_performance$overall[[4]]
}

ggplot2::ggplot(df, ggplot2::aes(x = points, y = accuracy_mean)) +
  ggplot2::xlim(2000, max(df$points)) +
  ggplot2::ylim(0.78, max(df$accuracy_max)) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = accuracy_min, ymax = accuracy_max), width = 0.2) +  # Add error bars
  ggplot2::labs() +
  ggplot2::theme_classic() +
  ggplot2::geom_smooth(method = "loess", span = 2, level = 0.5, se = FALSE) +
  ggplot2::labs(x = "Number of sample points", y = "Balanced Accuracy")


# Load the final, best performing model (ran on work computer) to create PDPs
load("data/permafrost/model optimization/eighty.RData")

# Calculate partial dependence functions
pdp <- pdp::partial(eighty$best_model, pred.var = "veg", progress = TRUE)

alluv <- easyalluvial::alluvial_model_response_caret(eighty$best_model, eighty$best_model$trainingData, method = "pdp", parallel = TRUE)
