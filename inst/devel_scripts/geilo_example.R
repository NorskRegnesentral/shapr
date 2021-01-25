remotes::install_github("NorskRegnesentral/shapr",ref = "groupSHAP")

# Loading the Boston housing data set
data("Boston", package = "MASS")
x_var <- c("lstat", "rm","dis","indus" ,"nox","tax")
y_var <- "medv"

x_train <- as.matrix(Boston[1:50, x_var])
y_train <- Boston[1:500, y_var]
x_test <- as.matrix(Boston[501:504, x_var])

# Fitting a basic xgboost model
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Define the feature groups
group <- list(c("lstat","rm","dis"),
              c("indus","nox"),
              "tax")

# Prepare the data for explanation
explainer <- shapr::shapr(x_train, model, group = group)

# Run the explainer
explanation <- shapr::explain(x_test,
                              explainer,
                              approach = "empirical",
                              prediction_zero = mean(y_train))

# Plot the group-wise Shapley values
plot(explanation,plot_phi0 = F)
