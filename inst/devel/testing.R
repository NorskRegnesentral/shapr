library(shapr)


source("tests/testthat/helper-lm.R")
############


x <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "regression_separate",
  phi0 = p0,
  seed = 1,
  regression.model = parsnip::linear_reg(),
  iterative = TRUE,
  group = list(
    c("Solar.R","Wind"),
    c("Temp","Month"),
    "Day"
  ),
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


#############


x <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric,
  x_train = x_train_numeric,
  approach = "gaussian",
  phi0 = p0,
  seed = 1,
  asymmetric = TRUE,
  causal_ordering = list(1:2, 3),
  group = list(
    c("Solar.R","Wind"),
    c("Temp","Month"),
    "Day"
  ),
  confounding = NULL,
  n_MC_samples = 5 # Just for speed
)

future::plan("multisession", workers = 2)

future::plan("multisession", workers = 7)

future::plan("sequential")

x <- explain(
  model = model_lm_numeric,
  x_explain = x_explain_numeric[rep(1:3,100)],
  x_train = x_train_numeric,
  approach = "ctree",
  phi0 = p0,
  seed = 1,
  asymmetric = TRUE,
  causal_ordering = list(1:3, 4:5),
  confounding = NULL,
  n_MC_samples = 1000 # Just for speed
)
x$timing$total_time_secs
