rm(list = ls())

library(shapr)
library(MASS)

data("Boston")

x_var <- c("lstat", "rm", "dis", "indus")
y_var <- "medv"

x_train <- as.matrix(tail(Boston[, x_var], -6))
y_train <- tail(Boston[, y_var], -6)
x_test <- as.matrix(head(Boston[, x_var], 6))

# Fitting a basic xgboost model to the training data
model <- xgboost::xgboost(
  data = x_train,
  label = y_train,
  nround = 20
)

# Prepare the data for explanation
explainer <- shapr(x_train, model)

# Explain predictions (empirical, AICc)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "AICc_each_k")
explanation

explainer.orig <- prepare_kshap(x_train,x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  empirical_settings = list(type =
                              "AICc_each_k", fixed_sigma_vec = 0.1, AICc_no_samp_per_optim = 1000,
                            AIC_optim_max_eval = 20, AIC_optim_startval = 0.1, w_threshold = 0.95)
)
explanation.orig$Kshap



# Empirical with independence:
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "independence")
explanation

explainer.orig <- prepare_kshap(x_train, x_test)
explanation.orig <- compute_kshap(
  model = model,
  l = explainer.orig,
  pred_zero = mean(y_train),
  empirical_settings = list(type =
                              "independence", fixed_sigma_vec = 0.1, w_threshold = 0.95)
)
explanation.orig$Kshap

# Explain predictions (empirical, fixed sigma)
explanation <- explain(x_test, explainer, approach = "empirical", prediction_zero = mean(y_train), type = "fixed_sigma")
explanation



# Explain predictions (gaussian)
explanation <- explain(x_test, explainer, approach = "gaussian", prediction_zero = mean(y_train))
explanation

# Explain predictions (copula)
explanation <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))
explanation

# Explain predictions (combined)
explanation <- explain(x_test, explainer, approach = "copula", prediction_zero = mean(y_train))
explanation

