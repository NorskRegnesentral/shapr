
# non-exact feature grouping:

feature_labels = c("A","B","C","D","E")
m <- length(feature_labels)
group = list(g1=c("A","D"),g2=c("C","B"),g3="E")

group_num <- lapply(group, FUN = function(x) {
  match(x, feature_labels)
})



feat_exact <- shapr:::feature_exact(m)

feat_not_exact <- shapr:::feature_not_exact(m,n_combinations = 10)

group_exact <- shapr:::feature_group(group_num)


#### Testing grouping function

library(xgboost)
library(shapr)
library(data.table)

data("Boston", package = "MASS")

x_var <- c("lstat", "rm", "dis", "indus","age","tax","ptratio","black","nox")
y_var <- "medv"

x_train <- as.matrix(Boston[-1:-6, x_var])
y_train <- Boston[-1:-6, y_var]
x_test <- as.matrix(Boston[1:6, x_var])

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = x_train,
  label = y_train,
  nround = 20,
  verbose = FALSE
)

group <- list(A=x_var[1:3],B=x_var[4:5],C=x_var[7],D=x_var[c(6,8)],E=x_var[9])

explainer1 <- shapr(x_train, model,group = group,n_combinations=10^ 6)

explainer2 <- shapr(x_train, model,group = group)

explanation1 <- explain(
  x_test,
  approach = "independence",
  explainer = explainer1,
  prediction_zero = p
)

explanation2 <- explain(
  x_test,
  approach = "independence",
  explainer = explainer2,
  prediction_zero = p
)
explanation2$dt-explanation1$dt # All small differences!


# Prepare the data for explanation
exp_list <- list()
set.seed(123)
for(i in 1:200){

  explainer <- shapr(x_train, model,group = group,n_combinations=20)

  # Specifying the phi_0, i.e. the expected prediction without any features
  p <- mean(y_train)

  # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
  # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
  set.seed(i)
  explanation <- explain(
    x_test,
    approach = "independence",
    explainer = explainer,
    prediction_zero = p
  )

  exp_list[[i]] <- copy(explanation$dt)

  print(i)
}



explainer <- shapr(x_train, model,group = group)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  x_test,
  approach = "independence",
  explainer = explainer,
  prediction_zero = p
)

### Well, this is biased, but that is probably just a "problem" with KernelSHAP.
exp_list2 <- lapply(exp_list,as.matrix)
Reduce("+",exp_list2)/length(exp_list2)-as.matrix(explanation$dt)


# Prepare the data for explanation
exp_feature_list <- list()
set.seed(123)
for(i in 1:100){

  set.seed(i)
  explainer <- shapr(x_train, model,n_combinations=300)

  # Specifying the phi_0, i.e. the expected prediction without any features
  p <- mean(y_train)

  # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
  # the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
  set.seed(i)
  explanation <- explain(
    x_test,
    approach = "independence",
    explainer = explainer,
    prediction_zero = p
  )

  exp_feature_list[[i]] <- copy(explanation$dt)

  print(i)
}


explainer0 <- shapr(x_train, model)

# Specifying the phi_0, i.e. the expected prediction without any features
p <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation0 <- explain(
  x_test,
  approach = "independence",
  explainer = explainer0,
  prediction_zero = p
)

### Well, this is biased, but that is probably just a "problem" with KernelSHAP.
exp_feature_list2 <- lapply(exp_feature_list,as.matrix)
Reduce("+",exp_feature_list2)/length(exp_feature_list2)-as.matrix(explanation0$dt)


