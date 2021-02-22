# This script runs the categorical experiment which did not end up being in the paper.
# It follows very similarly to experiment 1 in the paper but with 10 categorical features (3 levels each).
# It is not in the paper because the results (MAD) were strange (no variance) and we did not have enough time
# to figure out why. 

library(shapr)
library(MASS)
library(data.table)

source("inst/paper_experiments/cat-model_definitions.R")
source("inst/paper_experiments/1-cat-general_experiment-function.R")
source("inst/paper_experiment/3-cat-calculate_true_shapley_withdatatable.R")

seed = 1
No_test_obs = 100

lm_function = function(form, train_data){
  lm(formula = form, data = train_data)
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
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     form = form_lm2,
                     model_function = lm_function,
                     model_name = "experiment_lm2")
}
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     form = form_lm3,
                     model_function = lm_function,
                     model_name = "experiment_lm3")
}