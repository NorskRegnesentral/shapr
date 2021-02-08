library(shapr)
library(MASS)
library(data.table)

source("inst/paper_experiments/model_definitions.R")
source("inst/paper_experiments/1-general_experiment-function.R")

seed = 1
set.seed(seed)
beta <- round(rnorm(50), 1)
No_test_obs = 10
corr_vector = c(0, 0.1, 0.3, 0.7, 0.9)

lm_function = function(form, train_data){
  lm(formula = form, data = train_data)
}

gam_function = function(form, train_data){
  mgcv::gam(formula = form, data = train_data)
}


corr = 0

for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response_lm1,
                     form = form_lm1,
                     make_response_function_onehot = make_response_onehot_lm1,
                     form_onehot = form_onehot_lm1,
                     model_function = lm_function,
                     model_name = "experiment_lm1")
}
