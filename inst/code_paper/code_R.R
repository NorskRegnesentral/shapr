## Replication script for the R-examples used in the paper
# shapr: Explaining Machine Learning Models with Conditional Shapley Values in R and Python

# Requires the following R packages (from CRAN)
# shapr, xgboost, data.table, future, progressr, knitr, ggplot2, patchwork

# /*
# The lines below have already been run to save data/models for eased reproducibility:
# source("R_prep_data_and_model.R")
# */

# /*
# Run the below command in R from this script's folder to generate the code_R.html from code_R.R
# knitr::spin("code_R.R")
# */

#+ echo=FALSE
knitr::opts_chunk$set(fig.path = "html_figures/")
#+

#### Loads packages, reads data and models created by R_prep_data_and_model.R ####

library(xgboost)
library(data.table)
library(shapr)

x_explain <- fread(file.path("data_and_models", "x_explain.csv"))
x_train <- fread(file.path("data_and_models", "x_train.csv"))
y_train <- unlist(fread(file.path("data_and_models", "y_train.csv")))
model <- readRDS(file.path("data_and_models", "model.rds"))


# Load packages and sets up parallel processing
library(future)
library(progressr)
future::plan(multisession, workers = 4)
#+ echo=FALSE
# When calling progressr::handlers(global = TRUE) from within knitr::spin("code_R.R"), we get
# 'Error in globalCallingHandlers(condition = global_progression_handler): should not be called with handlers on the stack',
# which is a known limitation of progressr/R.
# This line is therefore not evaluated here, but still shown in the rendered document
# The output is not affected by this, as the progressr::handlers(global = TRUE) is only used to show progress bars,
# but we want to include it in the code since it is helpful for users when running interactively,
# as opposed to more cumbersome with_progress() wrapper which would work with knitr::spin("code_R.R").
#+ eval = FALSE
progressr::handlers(global = TRUE)
#+

#### Example code in Section 3 ####

# 40 indep
exp_40_indep <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 40,
                        approach = "independence",
                        phi0 = mean(y_train),
                        verbose = NULL,
                        seed = 1)


# 40 ctree
exp_40_ctree <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 40,
                        approach = "ctree",
                        phi0 = mean(y_train),
                        verbose = NULL,
                        ctree.sample = FALSE,
                        seed = 1)


print(exp_40_indep, what = "MSEv")
print(exp_40_ctree, what = "MSEv")

print(exp_40_ctree)

summary(exp_40_ctree)

### Continued estimation
exp_iter_ctree <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          approach = "ctree",
                          phi0 = mean(y_train),
                          prev_shapr_object = exp_40_ctree,
                          ctree.sample = FALSE,
                          verbose = c("basic", "convergence"),
                          seed = 1)


### Plotting

library(ggplot2)

#+ fig-scatter_ctree, fig.width=7, fig.height=3
plot(exp_iter_ctree,
     plot_type = "scatter",
     scatter_features = c("atemp", "windspeed"))

#+ echo=FALSE
# Produce the pdf used in Figure 3 in the paper
ggplot2::ggsave(file.path("paper_figures", "scatter_ctree.pdf"), width = 7, height = 3)

#+
### Grouping
group <- list(temp = c("temp", "atemp"),
              time = c("trend", "cosyear", "sinyear"),
              weather = c("hum", "windspeed"))

exp_g_reg <- explain(model = model,
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
                     seed = 1)

tree_vals <- c(10, 15, 25, 50, 100, 500)
exp_g_reg_tuned <- explain(model = model,
                           x_explain = x_explain,
                           x_train = x_train,
                           phi0 = mean(y_train),
                           group = group,
                           approach = "regression_separate",
                           regression.model =
                             parsnip::boost_tree(
                               trees = hardhat::tune(),
                               engine = "xgboost",
                               mode = "regression"
                             ),
                           regression.tune_values = expand.grid(
                             trees = tree_vals
                           ),
                           regression.vfold_cv_para = list(v = 5),
                           verbose = NULL,
                           seed = 1)

print(exp_g_reg, what = "MSEv")
print(exp_g_reg_tuned, what = "MSEv")

print(exp_g_reg, what = "timing_summary")
print(exp_g_reg_tuned, what = "timing_summary")


#+ fig-waterfall_group, fig.width=7, fig.height=4
# Waterfall plot for the best one
plot(exp_g_reg_tuned,
     index_x_explain = 6,
     plot_type = "waterfall")

#+ echo=FALSE
# Produce the pdf used in Figure 3 in the paper
ggplot2::ggsave(file.path("paper_figures", "waterfall_group.pdf"), width = 7, height = 4)

#+
#### Causal and asymmetric Shapley values ####

# Specify the causal ordering and confounding
causal_order0 <- list("trend",
                      c("cosyear", "sinyear"),
                      c("temp", "atemp", "windspeed", "hum"))

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
  exp_tmp <- explain(model = model,
                     x_train = x_train,
                     x_explain = x_explain,
                     approach = "gaussian",
                     phi0 = mean(y_train),
                     asymmetric = asymmetric_list[[i]],
                     causal_ordering = causal_ordering_list[[i]],
                     confounding = confounding_list[[i]],
                     seed = 1,
                     verbose = NULL)

  plot_list[[i]] <- plot(exp_tmp,
                         plot_type = "beeswarm",
                         print_ggplot = FALSE) +
    ggplot2::ggtitle(exp_names[i]) + ggplot2::ylim(-3700, 3700)
}

#+ fig-beeswarm_caus_asym, fig.width=14, fig.height=4, fig.scale=0.9
# Use the patchwork package to combine the plots
library(patchwork)
patchwork::wrap_plots(plot_list, nrow = 1) +
  patchwork::plot_layout(guides = "collect")

#+ echo=FALSE
# Produce the pdf used in Figure 6 in the paper
ggplot2::ggsave(file.path("paper_figures", "beeswarm_caus_asym.pdf"),
                scale = 0.9,
                width = 14,
                height = 4)

#+
#### Example code in Section 6 ####

# Read additional data
x_full <- fread(file.path("data_and_models", "x_full.csv"))
data_fit <- x_full[seq_len(729), ]

# Fit AR(2)-model
model_ar <- ar(data_fit$temp, order = 2)

phi0_ar <- rep(mean(data_fit$temp), 3)

exp_fc_ar <- explain_forecast(model = model_ar,
                              y = x_full[, "temp"],
                              explain_idx = 730:731,
                              explain_y_lags = 2,
                              horizon = 3,
                              approach = "empirical",
                              phi0 = phi0_ar,
                              group_lags = FALSE,
                              seed = 1)

# Summary of Shapley value computation
print(exp_fc_ar)

# Fit ARIMA(2,0,0)-model
model_arimax <- arima(data_fit$temp,
                      order = c(2, 0, 0),
                      xreg = data_fit$windspeed)
phi0_arimax <- rep(mean(data_fit$temp), 2)

exp_fc_arimax <- explain_forecast(model = model_arimax,
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
                                  seed = 1)

# Print the Shapley values
print(exp_fc_arimax)

#### Wrapping up ####

#+ echo=FALSE
# Make sure any open connections opened by future::plan(multisession, workers = 4) are closed afterwards
future::plan("sequential")

#+
sessionInfo()
