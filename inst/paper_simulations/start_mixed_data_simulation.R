
library(shapr)
library(data.table)
library(MASS)
library(ggplot2)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/paper_simulations/source_mixed_data.R")



## Start simulation study
tod_date0 <- format(Sys.Date(), "%d_%m_%y")

dim <- 4
no_categories <- 4

clock_seed_0 <- round(as.numeric(Sys.time()) * 1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1,5)
folder <- paste0(tod_date0, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/", folder, sep = ""))

##

parameters_list <- list()

corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
k <- 1
for(j in corr){
  parameters_list[[k]] <- list(methods = c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP"),
                          No_sample_gaussian = c(100),
                          No_cont_var = 2,
                          No_cat_var = 2,
                          No_levels = 4,
                          Sigma_diag = 1,
                          corr = j,
                          No_train_obs = 1000,
                          No_test_obs = 500,
                          cat_cutoff = c(-200, -0.5, 0, 1, 200),
                          noise = FALSE,
                          name = 'testing',
                          seed = 123)
  k <- k + 1
}

all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- compute_shapley_mixed_data(parameters_list[[i]])

  nm = paste(folder, '_rho_', parameters_list[[i]]$corr, ".rds", sep = "")

  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))
}
