# Setup -----------------------------------------------------------------------------------------------------------
# Load necessary libraries
library(xgboost)
library(data.table)

# Get the data
data("airquality")
data = data.table::as.data.table(airquality)
data = data[complete.cases(data), ]

# Define the features and the response
x_var = c("Solar.R", "Wind", "Temp", "Month")
y_var = "Ozone"

# Split data into test and training data set
ind_x_explain = 1:12
x_train = data[-ind_x_explain, ..x_var]
y_train = data[-ind_x_explain, get(y_var)]
x_explain = data[ind_x_explain, ..x_var]

# Fitting a basic xgboost model to the training data
model = xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
prediction_zero = mean(y_train)

# Independence approach
explanation_independence = explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence",
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Empirical approach
explanation_empirical = explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Gaussian 1e1 approach
explanation_gaussian_1e1 = explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 1e1
)

# Gaussian 1e2 approach
explanation_gaussian_1e2 = explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Combined approach
explanation_combined = explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = c("gaussian", "ctree", "empirical"),
  prediction_zero = prediction_zero,
  n_samples = 1e2
)

# Create a list of explanations with names
explanation_list = list(
  "Ind." = explanation_independence,
  "Emp." = explanation_empirical,
  "Gaus. 1e1" = explanation_gaussian_1e1,
  "Gaus. 1e2" = explanation_gaussian_1e2,
  "Combined" = explanation_combined
)


# Plots -----------------------------------------------------------------------------------------------------------
# The function uses the provided names.
plot_SV_several_approaches(explanation_list)

# We can change the number of columns in the grid of plots and add other visual alterations
plot_SV_several_approaches(explanation_list,
                           facet_ncol = 3,
                           facet_scales = "free_y",
                           add_zero_line = TRUE,
                           digits = 2,
                           brewer_palette = "Paired",
                           geom_col_width = 0.6) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 0))


# We can specify which explicands to plot to get less chaotic plots and make the bars vertical
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           horizontal_bars = FALSE,
                           axis_labels_rotate_angle = 45)


# We can change the order of the features by specifying the order using the `only_these_features` parameter.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R", "Month", "Wind"))

# We can also remove certain features if we are not interested in them or want to focus on, e.g., two features.
# The function will give a message to if the user specifies non-valid feature names.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R"),
                           plot_phi0 = TRUE)

# We can specify which explicands to plot to get less chaotic plots.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10))

# We can make the bars vertical by setting `horizontal_bars = FALSE`.
# Will then get message about long label names on the x-axis and how to fix it.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           horizontal_bars = FALSE)

# We can change the order of the features by specifying the order using the `only_these_features` parameter.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R", "Month", "Wind"))

# We can also remove certain features if we are not interested in them or want to focus on, e.g., two features.
# The function will give a message to if the user specifies non-valid feature names.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R"))

# To more easily compare the magnitude of the Shapley values for different explicands we can fix the x-axis
# by specifying that only the scales on the y-axis are to be free.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R"),
                           facet_scales = "free_y")

# If we rather want vertical bars and fix the y-axis, then we specify that the scales are only free on the x-axis.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R"),
                           facet_scales = "free_x",
                           axis_labels_rotate_angle = 0,
                           horizontal_bars = FALSE)

# By default the function does not plot the phi0, but we can change that by setting `plot_phi0 = TRUE`.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R"),
                           plot_phi0 = TRUE)

# Or we can include "none" in the `only_these_features` parameter. Note that phi0 will always be the first bars.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           only_these_features = c("Temp", "Solar.R", "none"))

# We can add a line at the Shapley value of zero and ensure non overlapping labels by setting `axis_labels_n_dodge`.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           add_zero_line = TRUE,
                           axis_labels_n_dodge = 2,
                           horizontal_bars = FALSE)

# We can increase the space between the features to make it easier to distinguish them from each other
# by lowering `geom_col_width`. Note that default is 0.85.
plot_SV_several_approaches(explanation_list,
                           index_explicands = c(1:2, 5, 10),
                           geom_col_width = 0.6)

