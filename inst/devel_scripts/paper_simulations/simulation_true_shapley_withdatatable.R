library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

tod_date0 <- '24_02_20'
dim <- 3

clock_seed_0 <- round(as.numeric(Sys.time())*1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1,5)
print(rand_string)
tod_date <- paste0(tod_date0, "_", rand_string, "_dim_", dim)

dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/", tod_date, sep = ""))
dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations/", tod_date, sep = ""))

##

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}



parameters_list <- list()

no_categories <- 3
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
                               methods = c("empirical_ind", "empirical", "gaussian", "ctree_onehot", "ctree"),
                               name = paste0('corr', j),
                               cutoff = c(-200, 0, 1, 200),
                               Sample_test = TRUE, # Can be FALSE as well, then No_test_sample not used.
                               No_test_sample = 1000,
                               No_train_obs = 1000,
                               N_sample_gaussian = c(100, 1000),
                               seed = 1,
                               no_categories = 3)
  k <- k + 1
}
# parameters_list = parameters_list[[1]]


tm <- Sys.time()
all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(tod_date, '_results_', i , ".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", tod_date, nm, sep = "/"))
}

# to read old data
# all_methods <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/19_02_20_6kbYr_results_5.rds")


MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL
MAE_seed <- NULL

for(i in 1:length(all_methods)){
  if(!is.null(all_methods[[i]][['true_linear']])){
    MAE_truth <- c(MAE_truth, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']]))
  }
  for(m in parameters_list[[i]]$methods){

    if(m == 'gaussian'){
      for(gauss in parameters_list[[i]]$N_sample_gaussian){
        MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[paste0('gaussian_nsamples', gauss)]]$dt_sum))
        MAE_methods_names <- c(MAE_methods_names, paste0('gaussian_nsamples', gauss))
        MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
        MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
      }
    } else if(m != 'ctree'){
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt_sum))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
      MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
    } else{
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
      MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
    }

  }
}


results <- data.table(MAE_methods, MAE_methods_names, MAE_parameters, MAE_seed)
results[, correlation := paste0("", str_sub(MAE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)

nm = paste(tod_date, '_results', '.rds', sep = "")
saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", tod_date, nm, sep = "/"))

nm = paste(tod_date, '_all_methods', '.rds', sep = "")
saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", tod_date, nm, sep = "/"))


p1 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names ))) +
  geom_point(size = 4, stroke = 1.5) +
  scale_x_discrete(labels = c("corr0" = "0", "corr0.05" = "0.05", "corr0.1" = "0.1", "corr0.3" = "0.3", "corr0.5" = "0.5", "corr0.8" = "0.8", "corr0.9" = "0.9")) +
  theme_bw(base_size = 22) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian_100", "Gaussian_1000") ) +
  ggtitle("")


nm = paste(tod_date, '_MAE', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations", tod_date, nm, sep = "/"), plot = p1, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)
tm2 <- Sys.time()
print(tm2 - tm)



## BOXPLOTS
p2 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names ))) +
  geom_point(size = 4, stroke = 1.5) +
  scale_x_discrete(labels = c("corr0" = "0", "corr0.05" = "0.05", "corr0.1" = "0.1", "corr0.3" = "0.3", "corr0.5" = "0.5", "corr0.8" = "0.8", "corr0.9" = "0.9")) +
  theme_bw(base_size = 22) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian_100", "Gaussian_1000") ) +
  ggtitle("")


nm = paste(tod_date, '_MAE', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/categorical_shapley/", nm, sep = ""), plot = p2, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)
tm2 <- Sys.time()
print(tm2 - tm)





