# In this file, we compare the causal and conditional Shapley values for a categorical dataset.
# We see that "categorical" approach sometimes produce Shapley values of the opposite sign than
# the other approaches, but this happens for both causal and conditional Shapley values.
# I.e., there is likely no mistake in the cateogical causal Shapley value code.
{
  options(digits = 5) # To avoid round off errors when printing output on different systems

  set.seed(12345)

  data <- data.table::as.data.table(airquality)
  data[, Month_factor := as.factor(Month)]
  data[, Ozone_sub30 := (Ozone < 30) * 1]
  data[, Ozone_sub30_factor := as.factor(Ozone_sub30)]
  data[, Solar.R_factor := as.factor(cut(Solar.R, 10))]
  data[, Wind_factor := as.factor(round(Wind))]

  data_complete <- data[complete.cases(airquality), ]
  data_complete <- data_complete[sample(seq_len(.N))]
  y_var_numeric <- "Ozone"
  x_var_categorical <- c("Month_factor", "Ozone_sub30_factor", "Solar.R_factor", "Wind_factor")
  data_train <- head(data_complete, -10)
  data_explain <- tail(data_complete, 10)
  x_train_categorical <- data_train[, ..x_var_categorical]
  x_explain_categorical <- data_explain[, ..x_var_categorical]
  lm_formula_categorical <- as.formula(paste0(y_var_numeric, " ~ ", paste0(x_var_categorical, collapse = " + ")))
  model_lm_categorical <- lm(lm_formula_categorical, data = data_complete)
  p0 <- data_train[, mean(get(y_var_numeric))]
}

# Causal Shapley values -----
causal_independence <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "independence",
  phi0 = p0,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE
)

causal_categorical <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "categorical",
  phi0 = p0,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE,
  iterative = FALSE
)

# Warning CTREE is the slowest approach by far
causal_ctree <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "ctree",
  phi0 = p0,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE,
  iterative = FALSE
)

causal_vaeac <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "vaeac",
  vaeac.epochs = 20,
  phi0 = p0,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE,
  iterative = FALSE
)

shapr::plot_SV_several_approaches(list(
  ind = causal_independence,
  cat = causal_categorical,
  ctree = causal_ctree,
  vaeac = causal_vaeac
))

# Conditional Shapley values ------
conditional_independence <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "independence",
  phi0 = p0,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE,
  iterative = FALSE
)

conditional_categorical <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "categorical",
  phi0 = p0,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE,
  iterative = FALSE
)

# Warning CTREE is the slowest approach by far
conditional_ctree <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "ctree",
  phi0 = p0,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE,
  iterative = FALSE
)

conditional_vaeac <- explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "vaeac",
  vaeac.epochs = 20,
  phi0 = p0,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_MC_samples = 50, # Just for speed
  verbose = c("basic", "convergence", "shapley", "vS_details"),
  keep_samp_for_vS = TRUE,
  iterative = FALSE
)

shapr::plot_SV_several_approaches(list(
  ind = conditional_independence,
  cat = conditional_categorical,
  ctree = conditional_ctree,
  vaeac = conditional_vaeac
))
