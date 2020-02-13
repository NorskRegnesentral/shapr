### Exploring cond_expec_scripts etc


library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

source("inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}
corr <- 0.5
no_categories <- 3
set.seed(1)
dim <- 10

beta <- round(rnorm(dim * no_categories + 1), 1)

parameters_list <- list(Sigma_diag = 1,
                        corr = corr,
                        mu = rep(0, dim),
                        beta = beta, # c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1)
                        N_shapley = 10^6,
                        noise = TRUE,
                        response_mod = response_mod,
                        fit_mod = "regression",
                        methods = c("ctree"),
                        name = paste0('corr', corr),
                        cutoff = c(-200, 0, 1, 200),
                        no_categories = no_categories,
                        N_training = 100,
                        N_testing = 100,
                        seed = 1)

simulate_data(parameters_list)

