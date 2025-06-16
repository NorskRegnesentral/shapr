library(shapr)


source("tests/testthat/helper-lm.R")
############


x <- explain(
  testing = TRUE,
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "regression_separate",
  phi0 = p0,
  seed = 1,
  regression.model = parsnip::linear_reg(),
  iterative = TRUE
)


x <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "independence",
  phi0 = p0,
  seed = 1,
  iterative = FALSE
)

yes <- get_results(x, what=c("shapley_est", "shapley_sd", "MSEv", "MSEv_explicand", "MSEv_coalition", "sdsad"))



print(x,"MSEv")

cli_startup(x$internal,model_class = "lm",c("basic","vS_details"))


summary(x)


