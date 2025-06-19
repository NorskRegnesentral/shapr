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
  iterative = TRUE,
  verbose = c("basic","convergence", "shapley")
)

aa=summary(x)

yes <- get_results(x, what=c("shapley_est", "shapley_sd"))

yes2 <- get_results(x, what="shapley_sd")


print(x,"MSEv")

yes <- get_results(x)



