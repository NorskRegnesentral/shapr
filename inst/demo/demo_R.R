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
head(x_explain)

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
  verbose = c("basic", "progress", "convergence", "vS_details"),
  phi0 = phi0,
  seed = 123)

expl_ctree_higher_tol <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  verbose = c("basic", "progress", "convergence", "vS_details"),
  phi0 = phi0,
  seed = 123,
  iterative_args = list(
    convergence_tol = 0.01,
    n_coal_next_iter_factor_vec = rep(0.8, 10)
  )
)


expl_ctree_40 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  phi0 = phi0,
  iterative = FALSE,
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details"),
  max_n_coalitions = 40,
  seed = 123)

expl_ctree_40_MCsamp_100 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  phi0 = phi0,
  iterative = FALSE,
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details"),
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

### Regression

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
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details"),
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
))



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

### VAEAC approach

expl_vaeac <- explain(
  model = model,
  x_explain = head(x_explain),
  x_train = x_train,
  approach = "vaeac",
  phi0 = phi0,
  iterative = FALSE,
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details"),
  max_n_coalitions = 20,
  seed = 123)


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

plot(expl_default_gaussian, plot_type = "scatter")

plot(
  expl_default_gaussian,
  plot_type = "scatter",
  scatter_features = c("trend", "temp")
)

plot(expl_default_gaussian, plot_type = "beeswarm")

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
  verbose = c("basic", "progress", "convergence", "shapley", "vS_details"),
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
ts <- x_full[seq_len(700), temp]

# Basic AR(2) model
model_ar <- ar(ts, order = 2)
phi0_ar <- rep(mean(ts), 3)

exp_fc_ar <- explain_forecast(
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
exp_fc_ar
exp_fc_ar$pred_explain

# ARIMAX-model

model_arimax <- arima(
  x_full[seq_len(700), temp],
  order = c(2, 1, 1),
  xreg = x_full[seq_len(700), windspeed]
)
phi0_arimax <- rep(mean(ts), 2)

exp_fc_arimax <- explain_forecast(
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

exp_fc_arimax


### Other models

data_train <- cbind(x_train, cnt = y_train)
data_train[,trend:=as.numeric(trend)]


library(gbm)

formula_gbm <- as.formula(paste0(cnt, "~", paste0(x_var, collapse = "+")))

# Fitting a gbm model
set.seed(825)
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
  workflows::add_recipe(recipes::recipe(cnt ~ ., data = data_train)) %>%
  parsnip::fit(data = data_train)

model_tidymodels <- workflows::workflow() %>%
  workflows::add_model(parsnip::boost_tree(trees = 20, engine = "xgboost", mode = "regression")) %>%
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




######## OLD ##############


plot(
  expl_default,
  plot_type = "scatter"
)


                        max_n_coalitions = 40,

                        verbose = c("basic", "progress", "convergence", "vS_details"),
                        seed = 1)


# 40 ctree
exp_40_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  max_n_coalitions = 40,
  approach = "ctree",
  phi0 = mean(y_train),
  verbose = NULL,
  ctree.sample = FALSE,
  seed = 1
)


print(exp_40_indep, what = "MSEv")
print(exp_40_ctree, what = "MSEv")

print(exp_40_ctree)

summary(exp_40_ctree)

### Continued estimation
exp_iter_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  phi0 = mean(y_train),
  prev_shapr_object = exp_40_ctree,
  ctree.sample = FALSE,
  verbose = c("basic", "convergence"),
  seed = 1
)


### Plotting

library(ggplot2)

#+ fig-scatter_ctree, fig.width=7, fig.height=3
plot(
  exp_iter_ctree,
  plot_type = "scatter",
  scatter_features = c("atemp", "windspeed")
)

#+ echo=FALSE
# Produce the pdf used in Figure 3 in the paper
ggplot2::ggsave(file.path("paper_figures", "scatter_ctree.pdf"), width = 7, height = 3)

#+
### Grouping
group <- list(
  temp = c("temp", "atemp"),
  time = c("trend", "cosyear", "sinyear"),
  weather = c("hum", "windspeed")
)

exp_g_reg <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  phi0 = mean(y_train),
  group = group,
  approach = "regression_separate",
  regression.model = parsnip::boost_tree(
    engine = "xgboost",
    mode = "regression"
  ),
  verbose = NULL,
  seed = 1
)

tree_vals <- c(10, 15, 25, 50, 100, 500)
exp_g_reg_tuned <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  phi0 = mean(y_train),
  group = group,
  approach = "regression_separate",
  regression.model = parsnip::boost_tree(
    trees = hardhat::tune(),
    engine = "xgboost",
    mode = "regression"
  ),
  regression.tune_values = expand.grid(
    trees = tree_vals
  ),
  regression.vfold_cv_para = list(v = 5),
  verbose = NULL,
  seed = 1
)

print(exp_g_reg, what = "MSEv")
print(exp_g_reg_tuned, what = "MSEv")

print(exp_g_reg, what = "timing_summary")
print(exp_g_reg_tuned, what = "timing_summary")


#+ fig-waterfall_group, fig.width=7, fig.height=4
# Waterfall plot for the best one
plot(
  exp_g_reg_tuned,
  index_x_explain = 6,
  plot_type = "waterfall"
)

#+ echo=FALSE
# Produce the pdf used in Figure 3 in the paper
ggplot2::ggsave(file.path("paper_figures", "waterfall_group.pdf"), width = 7, height = 4)

#+
#### Causal and asymmetric Shapley values ####

# Specify the causal ordering and confounding
causal_order0 <- list(
  "trend",
  c("cosyear", "sinyear"),
  c("temp", "atemp", "windspeed", "hum")
)

confounding0 <- c(FALSE, TRUE, FALSE)

# Specify the parameters of four different Shapley value variations
exp_names <- c("Asymmetric causal", "Asymmetric conditional",
               "Symmetric conditional", "Symmetric marginal")

causal_ordering_list <- list(causal_order0, causal_order0, NULL, NULL)
confounding_list <- list(confounding0, NULL, NULL, TRUE)
asymmetric_list <- list(TRUE, TRUE, FALSE, FALSE)

# Explain the four variations and create beeswarm plots
plot_list <- list()
for (i in seq_along(exp_names)) {
  exp_tmp <- explain(
    model = model,
    x_train = x_train,
    x_explain = x_explain,
    approach = "gaussian",
    phi0 = mean(y_train),
    asymmetric = asymmetric_list[[i]],
    causal_ordering = causal_ordering_list[[i]],
    confounding = confounding_list[[i]],
    seed = 1,
    verbose = NULL
  )

  plot_list[[i]] <- plot(
    exp_tmp,
    plot_type = "beeswarm",
    print_ggplot = FALSE
  ) +
    ggplot2::ggtitle(exp_names[i]) +
    ggplot2::ylim(-3700, 3700)
}

#+ fig-beeswarm_caus_asym, fig.width=14, fig.height=4, fig.scale=0.9
# Use the patchwork package to combine the plots
library(patchwork)
patchwork::wrap_plots(plot_list, nrow = 1) +
  patchwork::plot_layout(guides = "collect")

#+ echo=FALSE
# Produce the pdf used in Figure 6 in the paper
ggplot2::ggsave(
  file.path("paper_figures", "beeswarm_caus_asym.pdf"),
  scale = 0.9,
  width = 14,
  height = 4
)

#+
#### Example code in Section 6 ####

# Read additional data
x_full <- fread(file.path("data_and_models", "x_full.csv"))
data_fit <- x_full[seq_len(729), ]

# Fit AR(2)-model
model_ar <- ar(data_fit$temp, order = 2)

phi0_ar <- rep(mean(data_fit$temp), 3)

exp_fc_ar <- explain_forecast(
  model = model_ar,
  y = x_full[, "temp"],
  train_idx = 2:729,
  explain_idx = 730:731,
  explain_y_lags = 2,
  horizon = 3,
  approach = "empirical",
  phi0 = phi0_ar,
  group_lags = FALSE,
  seed = 1
)

# Summary of Shapley value computation
print(exp_fc_ar)

# Fit ARIMA(2,0,0)-model
model_arimax <- arima(
  data_fit$temp,
  order = c(2, 0, 0),
  xreg = data_fit$windspeed
)
phi0_arimax <- rep(mean(data_fit$temp), 2)

exp_fc_arimax <- explain_forecast(
  model = model_arimax,
  y = x_full[, "temp"],
  xreg = x_full[, "windspeed"],
  train_idx = 2:728,
  explain_idx = 729,
  explain_y_lags = 2,
  explain_xreg_lags = 1,
  horizon = 2,
  approach = "empirical",
  phi0 = phi0_arimax,
  group_lags = TRUE,
  seed = 1
)

# Print the Shapley values
print(exp_fc_arimax)

#### Wrapping up ####

#+ echo=FALSE
# Make sure any open connections opened by future::plan(multisession, workers = 4) are closed afterwards
future::plan("sequential")

#+
sessionInfo()
