
# 20 indep
exp_20_indep <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 20,
                        approach = "independence",
                        phi0 = mean(y_train),
                        verbose = NULL,
                        seed = 1)


# 20 ctree
exp_20_ctree <- explain(model = model,
                        x_explain = x_explain,
                        x_train = x_train,
                        max_n_coalitions = 20,
                        approach = "ctree",
                        phi0 = mean(y_train),
                        verbose = NULL,
                        ctree.sample = FALSE,
                        seed = 1)



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

print(exp_20_ctree)
### Continued estimation

exp_iter_ctree <- explain(model = model,
                          x_explain = x_explain,
                          x_train = x_train,
                          approach = "ctree",
                          phi0 = mean(y_train),
                          prev_shapr_object = exp_20_ctree,
                          ctree.sample = FALSE,
                          verbose = c("basic","convergence"),
                          seed = 1)


### PLotting ####

library(ggplot2)

plot(exp_iter_ctree, plot_type = "scatter",scatter_features = c("atemp","windspeed"))

ggplot2::ggsave(file.path(path_output,"scatter_ctree.pdf"),width = 7, height = 3)

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

#> exp_g_reg$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1547240 142123.2
#> exp_g_reg_tuned$MSEv$MSEv
#MSEv  MSEv_sd
#<num>    <num>
#  1: 1534033 142277.4

# Plot the best one

exp_g_reg_tuned$shapley_values_est[6,]
x_explain[6,]

plot(exp_g_reg_tuned,index_x_explain = 6,plot_type="waterfall")

ggplot2::ggsave(file.path(path_output,"waterfall_group.pdf"),width = 7, height = 4)

# Print Shapley value for the best ones

head(exp_g_reg_tuned$shapley_values_est)

#explain_id     none      temp      time   weather
#<int>    <num>     <num>     <num>     <num>
#  1:          1 4536.598  -371.659 -2757.175 -661.8197
#2:          2 4536.598 -1041.262 -1609.387 -412.9517
#3:          3 4536.598 -1118.937 -1560.695 -585.7902
#4:          4 4536.598 -1361.832 -1781.578 -415.2823
#5:          5 4536.598 -1887.654 -1745.006  125.1834
#6:          6 4536.598 -1810.055 -1927.635  478.3566
