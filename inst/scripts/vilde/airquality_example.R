
data("airquality")
airquality <- airquality[complete.cases(airquality), ]
# Split data into test- and training data
train <- head(airquality, -50)
test <- tail(airquality, 50)

# Fit a linear model
model <- lm(Ozone ~ Solar.R + Wind+ Temp + Month, data = x_train)

p <- mean(train$Ozone)

x <- explain(
  train,
  test,
  model = model,
  approach = "empirical",
  prediction_zero = p
)

if (requireNamespace("ggplot2", quietly = TRUE)) {
  # The default plotting option is a bar plot of the Shapley values
  # We draw bar plots for the first 4 observations
  plot(x, index_x_explain = 1:4)

  # We can also make waterfall plots
  plot(x, plot_type = "waterfall", index_x_explain = 1:4)
  plot(x, plot_type = "waterfall", index_x_explain = 1:4, top_k_features = 2) # top_k_features = 2 shows the 2 features with largest contribution

  # Or scatter plots showing the distribution of the shapley values and feature values
  plot(x, plot_type = "scatter")
  plot(x, plot_type = "scatter", scatter_features = "Temp") # if we only want the scatter plot for a specific feature

  # Or a beeswarm plot summarising the Shapley values and feature values for all features
  plot(x, plot_type = "beeswarm")
  plot(x, plot_type = "beeswarm", col = c("red", "black")) # we can change colors
}
