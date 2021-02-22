## This is the script to run GAM2 experiment (from paper) with fixed beta and response functions.
# Takes perhaps 12 * 5 hours to run the whole thing.

library(shapr)
library(MASS)
library(data.table)

source("inst/paper_experiments/model_definitions.R")
source("inst/paper_experiments/1-general_experiment-function.R")

seed = 1
set.seed(seed)
beta <- round(rnorm(50), 1)
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
                     make_response_function = make_response_gam2,
                     form = form_gam2,
                     model_function = gam_function,
                     model_name = "experiment_gam2")
}
