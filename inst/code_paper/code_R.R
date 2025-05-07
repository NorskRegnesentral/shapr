## Replication script for the R-examples used in the paper
# shapr: Explaining Machine Learning Models with Conditional Shapley Values in R and Python

# Requires the following R packages (from CRAN)
# shapr, xgboost, data.table, future, progressr, ggplot2, ggpubr

# /*
# The lines below have already been run to save data/models for eased reproducibility:
# source("R_prep_data_and_model.R")
# */

# /*
# Run the below command in R from this script's folder to generate the code_R.html from code_R.R
# knitr::spin("code_R.R")
# */

#### Loads packages, Reads data and models created by R_prep_data_and_model.R ####

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

# 30 indep
exp_30_indep <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 30,
                        approach = "independence",
                        phi0 = mean(y_train),
                        verbose = NULL,
                        seed = 1)


# 30 ctree
exp_30_ctree <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 30,
                        approach = "ctree",
                        phi0 = mean(y_train),
                        verbose = NULL,
                        ctree.sample = FALSE,
                        seed = 1)


exp_30_indep$MSEv$MSEv
exp_30_ctree$MSEv$MSEv

print(exp_30_ctree)

### Continued estimation
exp_iter_ctree <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          approach = "ctree",
                          phi0 = mean(y_train),
                          prev_shapr_object = exp_30_ctree,
                          ctree.sample = FALSE,
                          verbose = c("basic","convergence"),
                          seed = 1)


### PLotting

library(ggplot2)

#+ fig-scatter_ctree, fig.width=7, fig.height=3
plot(exp_iter_ctree, plot_type = "scatter", scatter_features = c("atemp", "windspeed"))

#+ echo=FALSE
# Produce the pdf used in Figure 3 in the paper
ggplot2::ggsave(file.path("paper_figures", "scatter_ctree.pdf"), width = 7, height = 3)

#+
### Grouping

group <- list(temp = c("temp", "atemp"),
              time = c("trend", "cosyear", "sinyear"),
              weather = c("hum","windspeed"))

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
                               engine = "xgboost", mode = "regression"
                             ),
                           regression.tune_values = expand.grid(
                             trees = tree_vals
                           ),
                           regression.vfold_cv_para = list(v = 5),
                           verbose = NULL,
                           seed = 1)

exp_g_reg$MSEv$MSEv
exp_g_reg_tuned$MSEv$MSEv

#+ echo=FALSE
# Print Shapley value for the best one (not shown in the paper)
head(exp_g_reg_tuned$shapley_values_est)

#+ fig-waterfall_group, fig.width=7, fig.height=4
# Waterfall plot for the best one
plot(exp_g_reg_tuned,
     index_x_explain = 6,
     plot_type="waterfall")

#+ echo=FALSE
# Produce the pdf used in Figure 3 in the paper
ggplot2::ggsave(file.path("paper_figures", "waterfall_group.pdf"), width = 7, height = 4)


#+
#### Figure 6 in the paper ####

#+ fig-beeswarm_caus_asym, echo=FALSE, message=FALSE, fig.width=14, fig.height=4, fig.scale=1.1
library(ggplot2)
library(ggpubr)

# Specify the causal ordering and confounding
causal_ordering <- list("trend",
                        c("cosyear", "sinyear"),
                        c("temp", "atemp", "windspeed", "hum"))

confounding <- c(FALSE, TRUE, FALSE)

# Asymmetric conditional
exp_asym_cond <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = TRUE,
  causal_ordering = causal_ordering,
  confounding = NULL,
  seed = 1
)

# Symmetric causal
exp_sym_cau <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = FALSE,
  causal_ordering = causal_ordering,
  confounding = confounding,
  seed = 1
)

# Symmetric marginal
exp_sym_marg_gaus <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = FALSE,
  causal_ordering = NULL,
  confounding = TRUE,
  seed = 1
)

# Symmetric marginal
exp_sym_marg_ind <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "independence",
  asymmetric = FALSE,
  causal_ordering = NULL,
  confounding = NULL,
  seed = 1
)

## Plotting
# Combine the explanations
explanation_list = list("Asymmetric conditional" = exp_asym_cond,
                        "Symmetric causal" = exp_sym_cau,
                        "Symmetric marginal (gaussian)" = exp_sym_marg_gaus,
                        "Symmetric marginal (indep)" = exp_sym_marg_ind)

# Make the beeswarm plots
grobs <- lapply(seq(length(explanation_list)), function(explanation_idx) {
  gg <- plot(explanation_list[[explanation_idx]], plot_type = "beeswarm") +
    ggplot2::ggtitle(gsub("_", " ", names(explanation_list)[[explanation_idx]]))

  # Flip the order such that the features comes in the right order
  gg <- gg +
    ggplot2::scale_x_discrete(limits = rev(levels(gg$data$variable)[levels(gg$data$variable) != "none"]))+
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = 12),
                   axis.text.x = ggplot2::element_text(size = 12))
})

fig = ggpubr::ggarrange(grobs[[1]], grobs[[2]], grobs[[3]], grobs[[4]],
                        ncol=4, nrow=1, common.legend = TRUE, legend="right")
fig

# Produce the pdf used in Figure 6 in the paper
ggplot2::ggsave(file.path("paper_figures", "beeswarm_caus_asym.pdf"),
                scale = 1.1,
                width = 14,
                height = 4)

#+ eval=FALSE, echo=TRUE
#### Generic example code for Section 4 (not ran for paper) ####

# Specify the causal ordering and confounding
causal_ordering <- list("trend",
                        c("cosyear", "sinyear"),
                        c("temp", "atemp", "windspeed", "hum"))

confounding <- c(FALSE, TRUE, FALSE)

explanation <- explain(
  model = model,
  x_train = x_train,
  x_explain = x_explain,
  phi0 = mean(y_train),
  approach = "gaussian",
  asymmetric = TRUE,
  causal_ordering = causal_ordering,
  confounding = confounding,
  seed = 1
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

# Print Shapley values
print(exp_fc_ar)

# Fit ARIMA(2,0,0)-model
model_arimax <- arima(data_fit$temp,
                      order = c(2, 0, 0),
                      xreg = data_fit$windspeed)
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

# Print Shapley values
print(exp_fc_arimax)

#### Wrapping up ####

#+ echo=FALSE
# Make sure any open connections opened future::plan(multisession, workers = 4) are closed afterwards
future::plan("sequential")

#+
sessionInfo()


