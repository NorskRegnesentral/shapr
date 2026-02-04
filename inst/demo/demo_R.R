# Demo script for shapr
# Requires the following R packages (from CRAN)
# shapr, xgboost, data.table, future, progressr, knitr, ggplot2, patchwork

#### Loads packages, reads data and models created by R_prep_data_and_model.R ####

library(xgboost)
library(data.table)
library(shapr)

x_explain <- fread(file.path("data_and_models", "x_explain.csv"))
x_train <- fread(file.path("data_and_models", "x_train.csv"))
y_train <- unlist(fread(file.path("data_and_models", "y_train.csv")))
model <- readRDS(file.path("data_and_models", "model.rds"))

x_train[,trend:=as.numeric(trend)]
x_explain[,trend:=as.numeric(trend)]

# Load packages and sets up parallel processing
library(future)
library(progressr)
future::plan(multisession, workers = 4)
progressr::handlers(global = TRUE)
progressr::handlers("cli")


# Looking at the data

hist(y_train)
head(x_train)

# Required components in explain()
# model
# x_explain
# x_train
# phi0
# x_train
phi0 <- mean(y_train)

expl_default_gaussian <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  phi0 = phi0)

expl_default_gaussian # Print Shapley values

expl_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  verbose = c("basic", "convergence", "shapley"), # "vS_details", "progress"
  phi0 = phi0,
  seed = 123)

expl_ctree_higher_tol <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  verbose = c("basic", "convergence"),
  phi0 = phi0,
  seed = 123,
  iterative_args = list(
    initial_n_coalitions = 40,
    convergence_tol = 0.025,
    n_coal_next_iter_factor_vec = rep(0.8, 10)
  )
)

# Non-iterative versions with fixed number of coalitions

expl_ctree_40 <- explain(
  model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "ctree",
    phi0 = phi0,
    iterative = FALSE,
    verbose = c("basic", "shapley"),
    max_n_coalitions = 40,
    seed = 123)

expl_ctree_40_MCsamp_100 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  phi0 = phi0,
  iterative = FALSE,
  verbose = c("basic", "shapley"),
  max_n_coalitions = 40,
  n_MC_samples = 100,
  seed = 123)

expl_ctree_40$timing$summary
expl_ctree_40_MCsamp_100$timing$summary

expl_copula_40 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "copula",
  max_n_coalitions = 40,
  iterative = FALSE,
  phi0 = phi0,
  seed = 123)

### Regression approaches

## Separate
exp_reg_sep_40 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  phi0 = phi0,
  approach = "regression_separate",
  max_n_coalitions = 40,
  iterative = FALSE,
  regression.model = parsnip::boost_tree(
    engine = "xgboost",
    mode = "regression"
  ),
  verbose = c("basic", "shapley", "vS_details"),
  seed = 123)

# Separate tuned
tree_vals <- c(10, 15, 25, 50, 100, 500)
exp_reg_sep_40_tuned <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  phi0 = phi0,
  approach = "regression_separate",
  max_n_coalitions = 40,
  iterative = FALSE,
  regression.model = parsnip::boost_tree(
    trees = hardhat::tune(),
    engine = "xgboost",
    mode = "regression"
  ),
  regression.tune_values = expand.grid(
    trees = tree_vals
  ),
  regression.vfold_cv_para = list(v = 5),
  verbose = c("basic", "shapley", "vS_details"),
  seed = 123)

## Surrogate
exp_reg_surr_40 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  phi0 = phi0,
  approach = "regression_surrogate",
  max_n_coalitions = 40,
  iterative = FALSE,
  regression.model = parsnip::boost_tree(
    engine = "xgboost",
    mode = "regression"
  ),
  verbose = c("basic", "shapley"),
  seed = 123)



plot_MSEv_eval_crit(list(
  ctree_40 = expl_ctree_40,
  ctree_40_MCsamp_100 = expl_ctree_40_MCsamp_100,
  copula_40 = expl_copula_40,
  reg_sep_40 = exp_reg_sep_40,
  reg_sep_40_tuned = exp_reg_sep_40_tuned,
  reg_surr_40 = exp_reg_surr_40
), index_x_explain = 1:100)




#### Continued estimation ####

expl_copula_60 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "copula",
  max_n_coalitions = 60,
  iterative = TRUE,
  phi0 = phi0,
  seed = 123,
  prev_shapr_object = expl_copula_40)

### Summary

summary(expl_default_gaussian) # Summary of Shapley value computation

summary_expl <- summary(expl_default_gaussian)

attributes(summary_expl)

summary_expl$pred_explain
summary_expl$approach
summary_expl$MSEv
summary_expl$dt_vS[,1:5]
summary_expl$iterative_info
summary_expl$timing_summary


### Plotting

plot(
  expl_default_gaussian,
  plot_type = "bar",
  index_x_explain = c(1, 22, 45, 89),
  bar_plot_phi0 = FALSE
)

plot(
  expl_default_gaussian,
  plot_type = "waterfall",
  index_x_explain = c(1, 22, 45, 89)
)

plot(
  expl_default_gaussian,
  plot_type = "scatter")

plot(
  expl_default_gaussian,
  plot_type = "scatter",
  scatter_features = c("trend", "temp")
)

plot(
  expl_default_gaussian,
  plot_type = "beeswarm")

plot(
  expl_default_gaussian,
  plot_type = "beeswarm",
  col = c("pink", "blue")
)


### Grouping

group <- list(
  temp = c("temp", "atemp"),
  time = c("trend", "cosyear", "sinyear"),
  weather = c("hum", "windspeed")
)

expl_group_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  phi0 = phi0,
  group = group,
  approach = "ctree",
  seed = 123)

future::plan(sequential) # Disable parallelization to avoid conflict with tidyverse

expl_group_vaeac <- explain(
  model = model,
  x_explain = head(x_explain),
  x_train = x_train,
  phi0 = phi0,
  group = group,
  approach = "vaeac",
  vaeac.n_vaeacs_initialize = 2,
  vaeac.lr = 0.1,
  vaeac.epochs = 20,
  vaeac.width = 8,
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details"),
  seed = 123)



### Other models

data_train <- cbind(x_train, cnt = y_train)


library(gbm)

# Fitting a gbm model
model_gbm <- gbm::gbm(
  cnt~.,
  data = data_train,
  distribution = "gaussian"
)

#### Full feature versions of the three required model functions ####
MY_predict_model <- function(x, newdata) {
  gbm::predict.gbm(x, as.data.frame(newdata), n.trees = x$n.trees)
}

MY_get_model_specs <- function(x) {
  feature_specs <- list()
  feature_specs$labels <- labels(x$Terms)
  m <- length(feature_specs$labels)
  feature_specs$classes <- attr(x$Terms, "dataClasses")[-1]
  feature_specs$factor_levels <- setNames(vector("list", m), feature_specs$labels)
  feature_specs$factor_levels[feature_specs$classes == "factor"] <- NA # model object doesn't contain factor levels info
  return(feature_specs)
}

expl_custom <- explain(
  model = model_gbm,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  max_n_coalitions = 40,
  iterative = FALSE,
  phi0 = phi0,
  seed = 123,
  predict_model = MY_predict_model,
  get_model_specs = MY_get_model_specs
)

#### Tidymodels/parsnip/workflow

library(parsnip)
model_tidymodels <- workflows::workflow() %>%
  workflows::add_model(parsnip::mlp(engine = "nnet", mode = "regression")) %>%
  #   workflows::add_model(parsnip::boost_tree(trees = 20, engine = "xgboost", mode = "regression")) %>% # alternative
  workflows::add_recipe(recipes::recipe(cnt ~ ., data = data_train)) %>%
  parsnip::fit(data = data_train)

# Create the Shapley values for the tidymodels version

future::plan(sequential) # Disable parallelization to avoid conflict with tidyverse

expl_tidymodels <- explain(
  model = model_tidymodels,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  max_n_coalitions = 40,
  iterative = FALSE,
  verbose = c("basic", "shapley"),
  phi0 = phi0,
  seed = 123
)

future::plan(multisession, workers = 4)




### Asymmetric and causal Shapley values

causal_order <- list(
  "trend",
  c("cosyear", "sinyear"),
  c("temp", "atemp", "windspeed", "hum")
)

confounding <- c(FALSE, TRUE, FALSE)

expl_asym_cond <- explain(
    model = model,
    x_train = x_train,
    x_explain = x_explain,
    approach = "copula",
    phi0 = phi0,
    iterative = FALSE,
    max_n_coalitions = 40,
    asymmetric = TRUE,
    causal_ordering = causal_order,
    confounding = NULL,
    seed = 123)

expl_asym_caus <- explain(
    model = model,
    x_train = x_train,
    x_explain = x_explain,
    approach = "copula",
    phi0 = phi0,
    iterative = FALSE,
    max_n_coalitions = 40,
    asymmetric = TRUE,
    causal_ordering = causal_order,
    confounding = confounding,
    seed = 123)

expl_indep_40 <- explain(
    model = model,
    x_train = x_train,
    x_explain = x_explain,
    approach = "independence",
    phi0 = phi0,
    iterative = FALSE,
    max_n_coalitions = 40,
    asymmetric = FALSE,
    causal_ordering = NULL,
    confounding = NULL,
    seed = 123)


expl_list <- list(expl_asym_cond, expl_asym_caus, expl_copula_40, expl_indep_40)
names(expl_list) <- c(
  "Asymmetric conditional",
  "Asymmetric causal",
  "Symmetric conditional",
  "Symmetric marginal"
)

# Explain the four variations and create beeswarm plots
plot_list <- list()
for (i in seq_along(expl_list)) {
  plot_list[[i]] <- plot(
    expl_list[[i]],
    plot_type = "beeswarm",
    print_ggplot = FALSE
  ) +
    ggplot2::ggtitle(names(expl_list)[i]) +
    ggplot2::ylim(-3700, 3700)
}

library(patchwork)
patchwork::wrap_plots(plot_list, nrow = 1) +
  patchwork::plot_layout(guides = "collect")

### Forecasting

x_full <- fread(file.path("data_and_models", "x_full.csv"))
ts <- x_full[, temp]

plot(ts, type = "l")

# Basic AR(2) model
model_ar <- ar(ts[seq_len(700)], order = 2)
phi0_ar <- rep(mean(ts), 3)

expl_fc_ar <- explain_forecast(
  model = model_ar,
  y = ts,
  train_idx = 2:700,
  explain_idx = 701,
  explain_y_lags = 2,
  horizon = 3,
  approach = "copula",
  phi0 = phi0_ar,
  group_lags = FALSE,
  seed = 1
)
expl_fc_ar
expl_fc_ar$pred_explain

# ARIMAX-model

model_arimax <- arima(
  x_full[seq_len(700), temp],
  order = c(2, 1, 1),
  xreg = x_full[seq_len(700), windspeed]
)
phi0_arimax <- rep(mean(ts), 2)

expl_fc_arimax <- explain_forecast(
  model = model_arimax,
  y = x_full[, "temp"],
  xreg = x_full[, "windspeed"],
  train_idx = 2:700,
  explain_idx = 701:702,
  explain_y_lags = 2,
  explain_xreg_lags = 1,
  horizon = 2,
  approach = "copula",
  n_MC_samples = 100,
  phi0 = phi0_arimax,
  group_lags = FALSE,
  seed = 1
)

expl_fc_arimax

expl_fc_arimax_grouplags <- explain_forecast(
  model = model_arimax,
  y = x_full[, "temp"],
  xreg = x_full[, "windspeed"],
  train_idx = 2:700,
  explain_idx = 701:702,
  explain_y_lags = 2,
  explain_xreg_lags = 1,
  horizon = 2,
  approach = "copula",
  n_MC_samples = 100,
  phi0 = phi0_arimax,
  group_lags = TRUE,
  seed = 1
)

expl_fc_arimax_grouplags




