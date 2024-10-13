# Continuous data -------------------------------------------------------------------------------------------------
test_that("output_asymmetric_conditional", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = NULL,
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asymmetric_conditional"
  )
})

test_that("output_asym_cond_reg", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      regression.model = parsnip::linear_reg(),
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = NULL,
      paired_shap_sampling = FALSE
    ),
    "output_asym_cond_reg"
  )
})

test_that("output_asym_cond_reg_adaptive", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "regression_separate",
      regression.model = parsnip::linear_reg(),
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = NULL,
      paired_shap_sampling = FALSE,
      adaptive = TRUE
    ),
    "output_asym_cond_reg_adaptive"
  )
})

test_that("output_symmetric_conditional", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5), # Does not matter when asymmetric = TRUE and confounding = NULL
      confounding = NULL,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_conditional"
  )
})

test_that("output_symmetric_marginal_independence", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "independence",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_marginal_independence"
  )
})

test_that("output_symmetric_marginal_gaussian", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_symmetric_marginal_gaussian"
  )
})

test_that("output_asym_caus_conf_TRUE", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = TRUE,
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asym_caus_conf_TRUE"
  )
})



test_that("output_asym_caus_conf_FALSE", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = FALSE,
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asym_caus_conf_FALSE"
  )
})

test_that("output_asym_caus_conf_mix", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asym_caus_conf_mix"
  )
})

test_that("output_asym_caus_conf_mix_n_coal", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5, # Just for speed
      paired_shap_sampling = FALSE,
      max_n_coalitions = 6
    ),
    "output_asym_caus_conf_mix_n_coal"
  )
})

test_that("output_asym_caus_conf_mix_empirical", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "empirical",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asym_caus_conf_mix_empirical"
  )
})

test_that("output_asym_caus_conf_mix_ctree", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "ctree",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_asym_caus_conf_mix_ctree"
  )
})

test_that("output_sym_caus_conf_TRUE", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_sym_caus_conf_TRUE"
  )
})

test_that("output_sym_caus_conf_FALSE", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_sym_caus_conf_FALSE"
  )
})

test_that("output_sym_caus_conf_mix", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_sym_caus_conf_mix"
  )
})


## Group-wise  -----------------------------------------------------------------------------------------------------
test_that("output_sym_caus_conf_TRUE_group", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3),
      confounding = TRUE,
      group = list("A" = c("Solar.R", "Wind"), B = "Temp", C = c("Month", "Day")),
      n_MC_samples = 5 # Just for speed
    ),
    "output_sym_caus_conf_TRUE_group"
  )
})


test_that("output_sym_caus_conf_mix_group", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1, 2, 3),
      confounding = c(TRUE, TRUE, FALSE),
      group = list("A" = c("Solar.R"), B = c("Wind", "Temp"), C = c("Month", "Day")),
      n_MC_samples = 5 # Just for speed
    ),
    "output_sym_caus_conf_mix_group"
  )
})

test_that("output_sym_caus_conf_mix_group_adaptive", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_numeric,
      x_explain = x_explain_numeric,
      x_train = x_train_numeric,
      approach = "gaussian",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1, 2, 3),
      confounding = c(TRUE, TRUE, FALSE),
      group = list("A" = c("Solar.R"), B = c("Wind", "Temp"), C = c("Month", "Day")),
      n_MC_samples = 5, # Just for speed,
      verbose = c("convergence"),
      adaptive = TRUE
    ),
    "output_sym_caus_conf_mix_group_adaptive"
  )
})





# Mixed data ------------------------------------------------------------------------------------------------------
test_that("output_mixed_sym_caus_conf_TRUE", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = TRUE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_mixed_sym_caus_conf_TRUE"
  )
})

test_that("output_mixed_sym_caus_conf_TRUE_adaptive", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = TRUE,
      n_MC_samples = 5, # Just for speed
      adaptive = TRUE
    ),
    "output_mixed_sym_caus_conf_TRUE_adaptive"
  )
})

test_that("output_mixed_asym_caus_conf_mixed", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(TRUE, FALSE, FALSE),
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_mixed_sym_caus_conf_mixed"
  )
})

test_that("output_mixed_asym_caus_conf_mixed_2", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "ctree",
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      confounding = c(FALSE, TRUE, TRUE),
      paired_shap_sampling = FALSE,
      n_MC_samples = 5 # Just for speed
    ),
    "output_mixed_sym_caus_conf_mixed_2"
  )
})


test_that("output_mixed_asym_cond_reg", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_mixed,
      x_explain = x_explain_mixed,
      x_train = x_train_mixed,
      approach = "regression_separate",
      regression.model = parsnip::linear_reg(),
      prediction_zero = p0,
      asymmetric = TRUE,
      causal_ordering = list(1:2, 3, 4:5),
      paired_shap_sampling = FALSE,
      confounding = NULL,
      adaptive = TRUE
    ),
    "output_mixed_asym_cond_reg"
  )
})



# Categorical data ------------------------------------------------------------------------------------------------
test_that("output_categorical_asym_causal_mixed_cat", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical[1:2],# Temp [1:2] as [1:3] give different sample on GHA-macOS for unknown reason
      x_train = x_train_categorical,
      approach = "categorical",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(3:4, 2, 1),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5, # Just for speed
      keep_samp_for_vS = TRUE
    ),
    "output_categorical_asym_causal_mixed_cat"
  )
})



test_that("output_cat_asym_causal_mixed_cat_ad", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "categorical",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(3:4, 2, 1),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5, # Just for speed
      adaptive = TRUE
      ),
    "output_cat_asym_causal_mixed_cat_ad"
  )
})

test_that("output_categorical_asym_causal_mixed_ctree", {
  expect_snapshot_rds(
    explain(
      testing = TRUE,
      model = model_lm_categorical,
      x_explain = x_explain_categorical,
      x_train = x_train_categorical,
      approach = "ctree",
      prediction_zero = p0,
      asymmetric = FALSE,
      causal_ordering = list(3:4, 2, 1),
      confounding = c(TRUE, FALSE, FALSE),
      n_MC_samples = 5 # Just for speed
    ),
    "output_categorical_asym_causal_mixed_ctree"
  )
})
