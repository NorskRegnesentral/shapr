
clock_seed_0 <- round(as.numeric(Sys.time()) * 1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1, 5)
print(rand_string)
folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/", folder, sep = ""))
dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations/", folder, sep = ""))

##
response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}


parameters_list <- list()

if(test){
  print("Testing.")
  set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
  corr <- c(0, 0.1)
  k <- 1
  for(j in corr){
    parameters_list[[k]] <- list(Sigma_diag = 1,
                                 corr = j,
                                 mu = rep(0, dim),
                                 beta = beta,
                                 N_shapley = 1e+03,
                                 noise = TRUE,
                                 response_mod = response_mod,
                                 fit_mod = "regression",
                                 methods = methods,
                                 name = paste0('corr', j),
                                 cutoff = cutoff,
                                 Sample_test = TRUE, # Can be FALSE as well, then No_test_sample not used.
                                 No_test_sample = 20,
                                 No_train_obs = 20,
                                 N_sample_gaussian = c(50),
                                 seed = 1,
                                 no_categories = no_categories)
    k <- k + 1
  }

} else{
  set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
  corr <- c(0, 0.1, 0.5, 0.8, 0.9)
  k <- 1
  for(j in corr){
    parameters_list[[k]] <- list(Sigma_diag = 1,
                                 corr = j,
                                 mu = rep(0, dim),
                                 beta = beta,
                                 N_shapley = 1e+07,
                                 noise = TRUE,
                                 response_mod = response_mod,
                                 fit_mod = "regression",
                                 methods = methods,
                                 name = paste0('corr', j),
                                 cutoff = cutoff,
                                 Sample_test = TRUE, # Can be FALSE as well, then No_test_sample not used.
                                 No_test_sample = 1000,
                                 No_train_obs = 1000,
                                 N_sample_gaussian = c(100, 1000),
                                 seed = 1,
                                 no_categories = no_categories)
    k <- k + 1
  }

}

all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(folder, '_rho_', parameters_list[[i]]$corr, ".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))
}
