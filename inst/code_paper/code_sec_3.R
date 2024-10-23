library(xgboost)
library(data.table)
library(shapr)

path <- "inst/code_paper/"
x_explain <- fread(paste0(path, "x_explain.csv"))
x_train <- fread(paste0(path, "x_train.csv"))
y_train <- unlist(fread(paste0(path, "y_train.csv")))
model <- readRDS(paste0(path, "model.rds"))


# We compute the SHAP values for the test data.
library(future)
library(progressr)
future::plan(multisession, workers = 4)
progressr::handlers(global = TRUE)


# 20 indep
exp_20_indep <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 20,
                        approach = "independence",
                        phi0 = mean(y_train),
                        verbose = NULL)


# 20 ctree
exp_20_ctree <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 20,
                        approach = "ctree",
                        phi0 = mean(y_train),
                        verbose = NULL,
                        ctree.sample = FALSE)



exp_20_indep$MSEv$MSEv
exp_20_ctree$MSEv$MSEv

##### OUTPUT ####
#> exp_20_indep$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1805368 123213.6
#> exp_20_ctree$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1224818 101680.4

exp_20_ctree

### Continued estimation

exp_iter_ctree <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          approach = "ctree",
                          phi0 = mean(y_train),
                          prev_shapr_object = exp_20_ctree,
                          ctree.sample = FALSE,
                          verbose = c("basic","convergence"))


### PLotting ####

library(ggplot2)

plot(exp_iter_ctree, plot_type = "scatter",scatter_features = c("atemp","windspeed"))

ggplot2::ggsave("inst/code_paper/scatter_ctree.pdf",width = 7, height = 4)

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
                     verbose = NULL)

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
                           verbose = NULL)


exp_g_reg$MSEv$MSEv
exp_g_reg_tuned$MSEv$MSEv

#> exp_group_reg_sep_xgb$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1547240 142123.2
#> exp_group_reg_sep_xgb_tuned$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1534033 142277.4

# Plot the best one

plot(exp_group_reg_sep_xgb_tuned,index_x_explain = 6,plot_type="waterfall")

ggplot2::ggsave("inst/code_paper/waterfall_group.pdf",width = 7, height = 4)

# Print Shapley value for the best ones

head(exp_group_reg_sep_xgb_tuned$shapley_values_est)



