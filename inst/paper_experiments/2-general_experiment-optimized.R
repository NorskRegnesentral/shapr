library(shapr)
library(MASS)
library(data.table)

setwd("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/")

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

for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response_gam1,
                     form = form_gam1,
                     model_function = gam_function,
                     model_name = "experiment_gam1")
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
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response_gam3,
                     form = form_gam3,
                     model_function = gam_function,
                     model_name = "experiment_gam3")
}
