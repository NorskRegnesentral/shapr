library(xgboost)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Look at the dependence between the features
cor(x_train)

# Fit a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specify phi_0, i.e., the expected prediction without any features
p0 <- mean(y_train)

# Compute Shapley values with Kernel SHAP, accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  phi0 = p0,
  seed = 1
)

# In global script, plot(explanation) works as before, displaying the plot, regardless of
# how we set print_ggplot = FALSE
plot(explanation)
plot(explanation, print_ggplot = TRUE) # Equivalent to plot(explanation)

plot(explanation, print_ggplot = FALSE) # This corresponds the previous behavior. The plot is displayed as it is returned globally

# NEW BEHAVIOR: assigning the output to plot.shapr now also displays the plot by default
plt = plot(explanation)
plt
# To avoid displaying the plot when assigning the output, set print_ggplot = TRUE.
# This is likely what you want if you want to adjust the plot before displaying it
plt = plot(explanation,print_ggplot = FALSE)
plt + ggplot2::ggtitle("My custom title")

# Using print_ggplot = FALSE is important since for instance this combination becomes a
# bit wierd: It displays the plot first without the modified title, while the stored plt
# includes the custom title.
# Use plt = plot(explanation,print_ggplot = FALSE) + ggplot2::ggtitle("My custom title") instead
plt = plot(explanation) + ggplot2::ggtitle("My custom title")
plt # Displayes the output from shapr.plot + the adjusted title

# Inside loops and function, the plot is now displayed by default,
# which is different from before

# Displays the plot
for(i in 1:3){
  plot(explanation)
}

# Do not display the plot (as before)
for(i in 1:3){
  plot(explanation,print_ggplot = FALSE)
}

# Function example

my_plot_function_display <- function(explanation){
  # Displays the plot
  plot(explanation)
  return("Plot displayed")
}

my_plot_function_display(explanation)

my_plot_function_no_display <- function(explanation){
  # Displays the plot
  plot(explanation,print_ggplot = FALSE)
  return("No plot displayed")

}

my_plot_function_no_display(explanation)











