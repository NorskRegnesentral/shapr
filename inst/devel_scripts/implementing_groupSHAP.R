
### experimenting while implementation groupSHAP

library(shapr)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm","dis",
           "indus","nox",
           "tax")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Just looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

group1 <- list(c(1,2,3),
              c(4,5),
              c(6))

group2 <- list(c(1),
              c(4,5),
              c(6,2,3))


# Prepare the data for explanation
explainer0 <- shapr(x_train, model, group = NULL)
explainer1 <- shapr(x_train, model, group = group1)
explainer2 <- shapr(x_train, model, group = group2)


# Spedifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation0 <- explain(x_test, explainer0, approach = "empirical", prediction_zero = p0)
explanation1 <- explain(x_test, explainer1, approach = "empirical", prediction_zero = p0)
explanation2 <- explain(x_test, explainer2, approach = "empirical", prediction_zero = p0)

# Printing the Shapley values for the test data
explanation0$dt
explanation1$dt
explanation2$dt


explanation <- explain(x_test, explainer, approach = "gaussian", prediction_zero = p0)
explanation$dt


explanation <- explain(x_test, explainer, approach = "copula", prediction_zero = p0)
explanation$dt

# All are quite differnet


# TODO in the end
# 1. Add possibility for the group list to include the column names instead, then to be transformed to a list as below
# 3. decide whether to sort the group ids or not (is it necessary anywhere in the code?)
# 4. Consider chainging the m/n_features argument to length(group) if is.null(group) is FALSE early on
#    to simplify later code (this needs to be carefully checked as I may need both version in different functions)
# 6. Fix plot.shapr Currently, does not work properly.
# 7. Test the approach on the dnb data, and see what kind of results you get for the two cases we
#    use in the paper
# 8. Try out group shapley as a way to explain categorical data by grouping one-hot-encoded variables,
#    and using the empirical approach to estimate the necassary conditional explectations.

# Finally we plot the resulting explanations
plot(explanation)
