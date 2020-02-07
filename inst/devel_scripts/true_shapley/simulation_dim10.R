library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

tod_date <- '7_02_20'
dim <- 10
##

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}



parameters_list <- list()

# seed <- 1
corr <- c(0, 0.1, 0.5, 0.8, 0.9)
no_categories <- 3
set.seed(1)
beta <- round(rnorm(dim * no_categories + 1), 1)

k <- 1
for(j in corr){
  parameters_list[[k]] <- list(Sigma_diag = 1,
                               corr = j,
                               mu = rep(0, dim),
                               beta = beta, # c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1)
                               N_shapley = 100000,
                               noise = TRUE,
                               response_mod = response_mod,
                               fit_mod = "regression",
                               methods = c("ctree"),
                               name = paste0('corr', j),
                               cutoff = c(-200, 0, 1, 200),
                               no_categories = no_categories,
                               N_training = 100,
                               N_testing = 100,
                               seed = 1)
  k <- k + 1
}


tm <- Sys.time()
all_methods <- list()
for(i in 1:length(parameters_list)){
  nm0 <- paste(tod_date, '_results_', i, "_dim_" , dim, sep = "")
  all_methods[[i]] <- simulate_data(parameters_list[[i]],tmp_folder = nm0)
  nm <- paste0(nm0,".rds")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm, sep = ""))
}
tm2 <- Sys.time()
print(tm2 - tm)
