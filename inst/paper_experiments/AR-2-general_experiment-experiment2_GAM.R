# This is to run experiment 3 for GAM2 and grouping A. Annabelle ran this for the paper. Took about 12 * 5 hours to finish.
# In these experiments, we have to run groupShapley and post-grouped Shapley for EVERY grouping (not just for a given model.)

library(shapr)
library(MASS)
library(data.table)

source("inst/paper_experiments/model_definitions.R")
source("inst/paper_experiments/AR-1-general_experiment-function_GAM.R")

seed = 1
set.seed(seed)
beta <- c(-0.6,  0.2, -0.8,  1.6,  0.3, -0.8,  0.5,  0.7,  0.6, -0.3,  1.5,  0.4, -0.6, -2.2,  1.1,  0.0,  0.0,  0.9)
No_test_obs = 100
n_samples = 1000

gam_function = function(form, train_data){
  mgcv::gam(formula = form, data = train_data)
}

# group A {1, 2, 3, 4}, {5, 6, 7, 8}, {9, 10}
corr_vector = c(0, 0.1, 0.3, 0.7, 0.9)
for(corr in corr_vector){
  general_experiment(No_test_obs = No_test_obs,
                     corr = corr,
                     seed = 1,
                     beta = beta,
                     make_response_function = make_response_gam2,
                     form = form_gam2,
                     model_function = gam_function,
                     model_name = "AR-groupA-experiment_gam_diff_corr",
                     n_samples = n_samples)
}
