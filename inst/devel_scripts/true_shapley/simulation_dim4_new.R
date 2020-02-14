library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

# source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

source("inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable_diff_alphas.R")

tod_date <- '13_02_20_noise'
dim <- 4

clock_seed_0 <- round(as.numeric(Sys.time())*1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1,5)
print(rand_string)
tod_date <- paste0(tod_date,"_",rand_string)
##

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}



parameters_list <- list()

# seed <- 1
corr <- c(0, 0.1, 0.5, 0.8)
no_categories <- 3
set.seed(1)
beta <- round(rnorm(dim * no_categories + 1), 1)

k <- 1
for(j in corr){
  parameters_list[[k]] <- list(Sigma_diag = 1,
                               corr = j,
                               mu = rep(0, dim),
                               beta = beta,
                               N_shapley = 10000000,
                               noise = TRUE,
                               response_mod = response_mod,
                               fit_mod = "regression",
                               methods = c("empirical_ind", "empirical", "gaussian", "ctree"),
                               name = paste0('corr', j),
                               cutoff = c(-200, 0, 1, 200),
                               no_categories = no_categories,
                               N_training = 100,
                               N_testing = 1000,
                               seed = 1)
  k <- k + 1
}


tm <- Sys.time()
all_methods <- list()
for(i in 1:length(parameters_list)){
  source("inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable_diff_alphas.R")
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(tod_date, '_results_', i, "_dim_" , dim, ".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm, sep = ""))
}
tm2 <- Sys.time()
print(tm2 - tm)
