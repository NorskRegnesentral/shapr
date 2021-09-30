## This is the script to run all the lm1, lm2, lm3 experiments (from paper) with fixed beta and response functions.
# Takes perhaps 4 hours to run the whole thing.

library(shapr)
library(MASS)
library(data.table)

source("inst/paper_experiments/model_definitions.R")
source("inst/paper_experiments/1-general_experiment-function.R")

seed = 1
# set.seed(seed)
# beta <- round(rnorm(18), 1)
beta = c(-0.6,  0.2, -0.8,  1.6,  0.3, -0.8,  0.5,  0.7,  0.6, -0.3,  1.5,  0.4, -0.6, -2.2,  1.1,  0.0,  0.0,  0.9)
No_test_obs = 100
corr_vector = c(0, 0.1, 0.3, 0.7, 0.9)

lm_function = function(form, train_data){
  lm(formula = form, data = train_data)
}

gam_function = function(form, train_data){
  mgcv::gam(formula = form, data = train_data)
}


for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response_lm1,
                     form = form_lm1,
                     model_function = lm_function,
                     model_name = "experiment_lm1")
  }

for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response_lm2,
                     form = form_lm2,
                     model_function = lm_function,
                     model_name = "experiment_lm2")
}
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response_lm3,
                     form = form_lm3,
                     model_function = lm_function,
                     model_name = "experiment_lm3")
}

