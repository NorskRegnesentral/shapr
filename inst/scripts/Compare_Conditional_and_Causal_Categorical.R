# In this file, we compare the causal and conditional Shapley values for a categorical dataset.
# We see that "categorical" approach sometimes produce Shapley values of the opposite sign than
# the other approaches, but this happens for both causal and conditional Shapley values.
# I.e., there is likely no mistake in the cateogical causal Shapley value code.

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


# Causal Shapley values -----
causal_independence = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "independence",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

causal_categorical = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "categorical",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

causal_ctree = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "ctree",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

causal_vaeac = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "vaeac",
  vaeac.epochs = 20,
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  asymmetric = FALSE,
  causal_ordering = list(3:4, 2, 1),
  confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

plot_SV_several_approaches(list(ind = causal_independence,
                                cat = causal_categorical,
                                ctree = causal_ctree,
                                vaeac = causal_vaeac))

# Conditional Shapley values ------
conditional_independence = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "independence",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

conditional_categorical = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "categorical",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

conditional_ctree = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "ctree",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

conditional_vaeac = shapr::explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "vaeac",
  vaeac.epochs = 20,
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE,
  # asymmetric = FALSE,
  # causal_ordering = list(3:4, 2, 1),
  # confounding = c(TRUE, FALSE, FALSE),
  n_samples = 1000, # Just for speed
  verbose = 2,
  keep_samp_for_vS = TRUE
)

plot_SV_several_approaches(list(ind = conditional_independence,
                                cat = conditional_categorical,
                                ctree = conditional_ctree,
                                vaeac = conditional_vaeac))
