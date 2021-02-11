library(shapr)
library(MASS)
library(data.table)

source("inst/paper_experiments/model_definitions.R")
source("inst/paper_experiments/1-general_experiment-function.R")
source("inst/devel_scripts/3-calculate_true_shapley_withdatatable.R")

seed = 1
No_test_obs = 100

lm_function = function(form, train_data){
  lm(formula = form, data = train_data)
}

gam_function = function(form, train_data){
  mgcv::gam(formula = form, data = train_data)
}

corr_vector = c(0, 0.1, 0.3, 0.7, 0.9)
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     form = form_lm1,
                     model_function = lm_function,
                     model_name = "experiment_lm1")
}
corr_vector = c(0, 0.1, 0.3, 0.7, 0.9)
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     form = form_lm2,
                     model_function = lm_function,
                     model_name = "experiment_lm2")
}
corr_vector = c(0, 0.1, 0.3, 0.7, 0.9)
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     form = form_lm3,
                     model_function = lm_function,
                     model_name = "experiment_lm3")
}