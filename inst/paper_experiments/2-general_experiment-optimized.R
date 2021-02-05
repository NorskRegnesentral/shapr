library(shapr)
library(MASS)
library(data.table)

setwd("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/")

source("inst/paper_experiments/model_definitions.R")
source("inst/paper_experiments/1-general_experiment-function.R")

seed = 1
set.seed(seed)
beta <- round(rnorm(50), 1)
No_test_obs = 1
corr_vector = c(0, 0.1)

lm_function = function(form, train_data){
  lm(formula = form, data = train_data)
}

for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response1,
                     form = form1,
                     model_function = lm_function,
                     model_name = "experiment1")
  }

for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response2,
                     form = form2,
                     model_function = lm_function,
                     model_name = "experiment2")
}
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response3,
                     form = form3,
                     model_function = lm_function,
                     model_name = "experiment3")
}
