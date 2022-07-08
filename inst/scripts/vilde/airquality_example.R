library(xgboost)
library(shapr)

data("airquality")
airquality <- airquality[complete.cases(airquality), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_test <- 1:50
x_train <- as.matrix(airquality[-ind_x_test, x_var])
y_train <- airquality[-ind_x_test, y_var]
x_test <- as.matrix(airquality[ind_x_test, x_var])

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
x <- explain(
  x_train,
  x_test,
  model = model,
  approach = "empirical",
  prediction_zero = p
)

# Finally we plot the resulting explanations
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

