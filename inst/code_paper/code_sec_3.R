library(xgboost)
library(data.table)
library(future)
library(progressr)
library(shapr)
library(ggplot2)

# Bike sharing data
bike <- fread("inst/extdata/day.csv")
#temp <- tempfile()
#url <- "https://archive.ics.uci.edu/static/public/275/bike+sharing+dataset.zip"
#download.file(url, temp)
#unlink(temp)


bike[,trend := as.numeric(difftime(dteday,
                                  dteday[1],
                                  units = "days"))]

bike[,cosyear :=cospi(trend / 365 * 2)]
bike[,sinyear :=sinpi(trend / 365 * 2)]
bike[,temp := temp * (39 - (-8)) + (-8)]
bike[,atemp := atemp * (50 - (-16)) + (-16)]
bike[,windspeed := 67 * windspeed]
bike[,hum := 100 * hum]


# We specify the features and the response variable.
x_var <- c("trend", "cosyear", "sinyear",
           "temp", "atemp", "windspeed", "hum")
y_var <- "cnt"

# We split the data into a training ($80\%$) and test ($20\%$) data set, and we compute $\phi_0$.
set.seed(123)
train_index <- sample(x = nrow(bike), size = round(0.8*nrow(bike)))


x_train <- bike[train_index, mget(x_var)]
y_train <- bike[train_index, get(y_var)]

x_explain <- bike[-train_index, mget(x_var)]
y_explain <- bike[-train_index, get(y_var)]

#### Writing data back to disk for ease with python code

fwrite(x_train, file="inst/jss_paper/x_train.csv")
fwrite(as.data.table(y_train), file="inst/jss_paper/y_train.csv")
fwrite(x_explain, file="inst/jss_paper/x_explain.csv")
fwrite(as.data.table(y_explain), file="inst/jss_paper/y_explain.csv")

phi0 <- mean(y_train)
#%

# Then we fit the \code{xgboost} model to the training data.
model <- xgboost::xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 100,
  verbose = FALSE
)

xgb.save(model, "inst/jss_paper/xgb.model")


# We compute the SHAP values for the test data.
future::plan(multisession, workers = 4)
progressr::handlers(global = TRUE)

#### Story

# full (independence)
exp_full_indep <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          iterative = FALSE,
                          approach = "independence",
                          prediction_zero = phi0,
                          verbose = NULL)


# full (gaussian)
exp_full_gauss <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          iterative = FALSE,
                          approach = "gaussian",
                          prediction_zero = phi0,
                          verbose = NULL)

exp_full_indep$MSEv$MSEv
exp_full_gauss$MSEv$MSEv

##### OUTPUT ####
#> exp_full_indep$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1737089 113694.2
#> exp_full_gauss$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1306199 104003.5



# Should find better x_indexes to plot (with more differences)
pl_full_indep <- plot(exp_full_indep, index_x_explain = 1:2,bar_plot_phi0 = FALSE)+ggplot2::ggtitle("Independence")
pl_full_gauss <- plot(exp_full_gauss, index_x_explain = 1:2,bar_plot_phi0 = FALSE)+ggplot2::ggtitle("Gaussian")

library(patchwork)

pl_full_indep/pl_full_gauss


########## ITERATIVE #################
# Showcasing iterative approach, with ctree, and with verbose basic + convergence
exp_iter_ctree <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          prediction_zero = phi0,
                          approach = "ctree",
                          verbose = c("basic","convergence"))

#### OUTPUT FORM THE ABOVE CALL ####

# Note: Feature classes extracted from the model contains NA.
# Assuming feature classes from the data are correct.
#
# Success with message:
#   max_n_coalitions is NULL or larger than or 2^n_features = 128,
# and is therefore set to 2^n_features = 128.
#
#
# ── Starting `shapr::explain()` at 2024-10-21 22:14:06 ──────────────────────────────────────────────────────────────────────────────────────────────────────────────
# • Model class: <xgb.Booster>
#   • Approach: ctree
# • Iterative estimation: TRUE
# • Number of feature-wise Shapley values: 7
# • Number of observations to explain: 146
# • Computations (temporary) saved at: C:\Users\jullum\AppData\Local\Temp\RtmpqsXn1a\shapr_obj_3d98173516a.rds
#
# ── iterative computation started ──
#
# ── Iteration 1 ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ℹ Using 13 of 128 coalitions, 13 new.
#
# ── Convergence info
# ℹ Not converged after 14 coalitions:
#   Current convergence measure: 0.25 [needs 0.02]
# Estimated remaining coalitions: 112
# (Concervatively) adding 10% of that (12 coalitions) in the next iteration.
#
# ── Iteration 2 ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ℹ Using 26 of 128 coalitions, 12 new.
#
# ── Convergence info
# ℹ Not converged after 26 coalitions:
#   Current convergence measure: 0.19 [needs 0.02]
# Estimated remaining coalitions: 100
# (Concervatively) adding 10% of that (10 coalitions) in the next iteration.
#
# ── Iteration 3 ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ℹ Using 36 of 128 coalitions, 10 new.
#
# ── Convergence info
# ℹ Not converged after 36 coalitions:
#   Current convergence measure: 0.12 [needs 0.02]
# Estimated remaining coalitions: 82
# (Concervatively) adding 20% of that (18 coalitions) in the next iteration.
#
# ── Iteration 4 ─────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# ℹ Using 54 of 128 coalitions, 18 new.
#
# ── Convergence info
# ✔ Converged after 54 coalitions:
#   Convergence tolerance reached!
#

#### NED OUPUT #####

pl_iter_ctree <- plot(exp_iter_ctree, index_x_explain = 1,plot_type = "waterfall")

pl_iter_ctree


#### Grouping

group <- list(temp = c("temp", "atemp"),
              time = c("trend", "cosyear", "sinyear"),
              weather = c("hum","windspeed"))

exp_group_reg_sep_xgb <- explain(model = model,
                                 x_explain = x_explain,
                                 x_train = x_train,
                                 prediction_zero = phi0,
                                 group = group,
                                 approach = "regression_separate",
                                 regression.model = parsnip::boost_tree(engine = "xgboost", mode = "regression"))

exp_group_reg_sep_xgb_tuned <- explain(model = model,
                                       x_explain = x_explain,
                                       x_train = x_train,
                                       prediction_zero = phi0,
                                       group = group,
                                       approach = "regression_separate",
                                       regression.model =
                                         parsnip::boost_tree(trees = hardhat::tune(), engine = "xgboost", mode = "regression"),
                                       regression.tune_values = expand.grid(trees = c(10, 15, 25, 50, 100, 500)),
                                       regression.vfold_cv_para = list(v = 5))


### Add vaeac here as well
exp_group_reg_sep_xgb$MSEv$MSEv
exp_group_reg_sep_xgb_tuned$MSEv$MSEv

#> exp_group_reg_sep_xgb$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1547240 142123.2
#> exp_group_reg_sep_xgb_tuned$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1534033 142277.4

# Print Shapley value for the best ones

head(exp_group_reg_sep_xgb_tuned$shapley_values_est)

#explain_id     none      temp      time   weather
#<int>    <num>     <num>     <num>     <num>
#1:          1 4536.598  -371.659 -2757.175 -661.8197
#2:          2 4536.598 -1041.262 -1609.387 -412.9517
#3:          3 4536.598 -1118.937 -1560.695 -585.7902
#4:          4 4536.598 -1361.832 -1781.578 -415.2823
#5:          5 4536.598 -1887.654 -1745.006  125.1834
#6:          6 4536.598 -1810.055 -1927.635  478.3566






### OLD CODE

exp_iter_ctree$timing$total_time_secs
exp_full_gauss$timing$total_time_secs


exp_full_indep <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          iterative = FALSE,
                          approach = "independence",
                          prediction_zero = phi0)

pl_full_indep <- plot(exp_full_indep, index_x_explain = 1:2,bar_plot_phi0 = FALSE)+ggtitle("Independence")



parsnip::boost_tree(trees = hardhat::tune(), engine = "xgboost", mode = "regression"),
regression.tune_values = expand.grid(trees = c(10, 15, 25, 50, 100, 500)),
regression.vfold_cv_para = list(v = 5)
)


# Run later
exp_full_vaeac <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          iterative = FALSE,
                          approach = "vaeac",
                          prediction_zero = phi0)

exp_iter_reg_sep_xgb <- explain(model = model,
                                x_explain = x_explain,
                                x_train = x_train,
                                prediction_zero = phi0,
                                approach = "regression_separate",
                                regression.model = parsnip::boost_tree(engine = "xgboost", mode = "regression"))

