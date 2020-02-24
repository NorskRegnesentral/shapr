library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

tod_date = "21_01_20"

response_mod <- function(mod_matrix_full,beta,epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}


parameters_list <- list()

seed <- 1
# corr <- c(seq(0, 0.9, by = 0.2))
corr <- c(0, 0.01, 0.1, 0.2, 0.5, 0.8, 0.9)

k <- 1
for(i in seed){
  for(j in corr){
    parameters_list[[k]] <- list(Sigma_diag = 1,
                                 corr = j,
                                 mu = c(0, 0, 0),
                                 beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                                 N_shapley = 10000000,
                                 noise = TRUE,
                                 response_mod = response_mod,
                                 fit_mod = "regression",
                                 methods = c("empirical_ind", "empirical", "gaussian", "ctree_onehot", "ctree"),
                                 name = paste0('corr', j),
                                 cutoff = c(-200, 0, 1, 200),
                                 N_training = 1000,
                                 N_testing = 1000,
                                 seed = i,
                                 no_categories = 3)
    k <- k + 1
  }
}



tm <- Sys.time()
all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(tod_date, '_results_', i , ".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))
}


MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL
MAE_seed <- NULL

for(i in 1:length(parameters_list)){
  if(!is.null(all_methods[[i]][['true_linear']])){
    MAE_truth <- c(MAE_truth, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']]))
  }
  for(m in parameters_list[[i]]$methods){
    
    if(m != 'ctree'){
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt_sum))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, parameters_list[[i]]$name)
    } else{
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, parameters_list[[i]]$name)
    }
    MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
  }
}


results <- data.table(MAE_methods, MAE_methods_names, MAE_parameters, MAE_seed)
results[, correlation := paste0("", str_sub(MAE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)

nm = paste(tod_date, '_results', '.rds', sep = "")
saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))

nm = paste(tod_date, '_all_methods', '.rds', sep = "")
saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))


# source data

nm <- paste0(tod_date, "_results_finerscale.rds")
results0 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))


p1 <- ggplot(data = results0, aes(y = MAE_methods, x = correlation, col = as.factor(MAE_seed))) + 
  geom_point(size = 4, stroke = 3.5, aes(shape = as.factor(MAE_methods_names))) +
  scale_x_continuous(breaks = seq(0,  0.5, by = 0.05)) + # scale_x_continuous(breaks = seq(0,  1, by = 0.2)
  theme_bw(base_size = 34) + 
  xlab("correlation") + 
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Seed") +
  scale_shape_discrete(name = "Method", labels = c("Ctree", "Empirical independence")) + 
  ggtitle("Dim: 3, N_shapley = 1e+07, N_train = 1000, N_test = 1000")

nm = paste(tod_date, '_MAE_with_seed', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/categorical_shapley/", nm, sep = ""), plot = p1, device = NULL, path = NULL,
       scale = 1, width = 60, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)

