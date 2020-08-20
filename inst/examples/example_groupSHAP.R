
library(shapr)

# xgboost example
data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

group1 <- list(c(1,2),
               c(3,4)) # c(4,5),c(6)

group1_names = lapply(group1, function(x){x_var[x]})

explainer <- shapr(x_train, model, group = group1_names)

p0 <- mean(y_train)

explanation1 <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)


# lm example
data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus")

x_train <- tail(Boston, -6)
y_train <- tail(Boston, -6)
x_test <- head(Boston, 6)

model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)

group1 <- list(c(1,2),
               c(3,4)) # c(4,5),c(6)

group1_names = lapply(group1, function(x) {
  x_var[x]
  })

explainer <- shapr(x_train, model, group = group1_names)

p0 <- mean(x_train$medv)

explanation1 <- explain(x_test, explainer, approach = "empirical", prediction_zero = p0)


# TODO in the end
# 6. Fix plot.shapr Currently, does not work properly. (DONE)
# 7. Test the approach on the dnb data, and see what kind of results you get for the two cases we
#    use in the paper. (DONE)
# 8. Try out group shapley as a way to explain categorical data by grouping one-hot-encoded variables,
#    and using the empirical approach to estimate the necassary conditional explectations. (DONE.)
# 9. Create/update tests whereever the behavior is different for groups. (DONE.)
# 10. Generally need to check the order of x_train and x_test. Should reorder everything to feature_labels
# and then before returning results in the end, bring it back to original of x_test.

## TODO 2:
# 1.	Modify the plotting functionality in the package to work for groups.
# 2.	Create/update tests for the new code in the package.
# 3.	Try out the approach on the dnb-data and see what kind of results you get for the two cases we are looking at in our first paper. The code used to plot the figures for the two cases is attached (rather messy, but the information needed should be there).
# 4.	Try out the approach as a way to explain categorical data by grouping one-hot-encoding variables into separate groups and using e.g. the empirical approach to estimate the conditional expectations. This is an idea I just came up with. I am not saying it is better than the ctree approach, but it is an alternative which gives you shapley values for categorical features in an efficient way.
# 5.	Some kind of experiments to see how the method “performs” (there is no truth here) compare to post-grouping of feature wise shapley values as we introduce in our first paper. We should investigate different types of grouping for this, I believe. I would like others, like Kjersti, to be involved in the discussion on how to do this the best way.


## May 26th
## Categorical data

# 1. Simulate continuous data
# 2. Converte to categorical
# 3. One-hot encode the categorical data
# 4.1 Fit the regular empirical method to the one-hot encoded data
# 4.2 Group one-hot encoded categorical data and fit the grouped empirical to the origianl variables
# 4.3 Use ctree on the categorical data


