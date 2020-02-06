library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans

# source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley.R")

source("inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

tod_date <- '17_01_20'

response_mod <- function(mod_matrix_full,beta,epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}

parameters_list <- list(Sigma_diag = 1,
                        corr = 0,
                        mu = c(0, 0, 0),
                        beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                        N_shapley = 10000,
                        N_training = 1000,
                        N_testing = 1000,
                        cutoff = c(-200, 0, 1, 200),
                        noise = FALSE,
                        response_mod = response_mod,
                        fit_mod = "regression",
                        methods = c("empirical"), # "gaussian", "ctree"
                        name = 'corr0')

set.seed(123)
 ll <- simulate_data(parameters_list)

 head(ll$true_shapley)
 head(ll$true_linear)
 MAE(ll$true_shapley, ll$true_linear) # 0.00095050 for 10000 obs and 1000 for training and testing / 0.0009436
 MAE(ll$true_shapley, ll$methods[['empirical']]$dt_sum) # 0.0179 # 1.06 minutes

