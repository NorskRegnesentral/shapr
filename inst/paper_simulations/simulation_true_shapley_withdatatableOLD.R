library(stringr)
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

tod_date0 <- format(Sys.Date(), "%d_%m_%y")

dim <- 3
no_categories <- 3

clock_seed_0 <- round(as.numeric(Sys.time())*1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1,5)
print(rand_string)
tod_date <- paste0(tod_date0, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations_Annabelle/", tod_date, sep = ""))
dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations_Annabelle/", tod_date, sep = ""))

##

response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}



parameters_list <- list()

set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
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
                               methods = c("empirical_ind", "empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP"),
                               name = paste0('corr', j),
                               cutoff = c(-200, 0, 1, 200),
                               Sample_test = TRUE,
                               No_train_obs = 1000,
                               No_test_sample = 1000,
                               N_sample_gaussian = c(100, 1000),
                               seed = 1,
                               no_categories = no_categories)
  k <- k + 1
}

# parameters_list = parameters_list[[1]]

all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(tod_date, '_rho_', parameters_list[[i]]$corr, ".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations_Annabelle/", tod_date, nm, sep = "/"))
}


# for gaussian stuff
gauss_mat <- list()
for(i in 1:length(all_methods)){
  gauss_mat[[i]] <- all_methods[[i]]$methods$gaussian_nsamples100$dt_sum[, id := rep(1:dim^no_categories, each = 50)]
  gauss_mat[[i]] <- gauss_mat[[i]][, .(none = mean(none), feat_1_ = mean(feat_1_), feat_2_ = mean(feat_2_), feat_3_ = mean(feat_3_)), by = .(id)]
  gauss_mat[[i]] = gauss_mat[[i]][, id := NULL]
}

gauss_mat2 <- list()
for(i in 1:length(all_methods)){
  gauss_mat2[[i]] <- all_methods[[i]]$methods$gaussian_nsamples1000$dt_sum[, id := rep(1:dim^no_categories, each = 50)]
  gauss_mat2[[i]] <- gauss_mat2[[i]][, .(none = mean(none), feat_1_ = mean(feat_1_), feat_2_ = mean(feat_2_), feat_3_ = mean(feat_3_)), by = .(id)]
  gauss_mat2[[i]] = gauss_mat2[[i]][, id := NULL]
}

MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL
MAE_seed <- NULL

for(i in 1:length(all_methods)){
  if(!is.null(all_methods[[i]][['true_linear']])){
    MAE_truth <- c(MAE_truth, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']], weights = all_methods[[i]]$join_prob_true[[dim + 1]]))
  }
  for(m in names(all_methods[[1]]$methods)){

    if(m == 'gaussian_nsamples100'){
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], gauss_mat[[i]], weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
      MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
    } else if (m == 'gaussian_nsamples1000'){
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], gauss_mat2[[i]], weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
      MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
    } else if(m != 'ctree' & m != 'kernelSHAP'){
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt_sum,
                                        weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
      MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
    } else{
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt,
                                        weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
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


p1 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names ))) +
  geom_point(size = 4, stroke = 1.5) +
  scale_x_discrete(labels = c("corr0" = "0", "corr0.05" = "0.05", "corr0.1" = "0.1", "corr0.3" = "0.3", "corr0.5" = "0.5", "corr0.8" = "0.8", "corr0.9" = "0.9")) +
  theme_bw(base_size = 22) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method" ) +
  ggtitle("")
#  labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian_100", "Gaussian_1000", "kernelSHAP")

nm = paste(tod_date, '_MAE', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations", tod_date, nm, sep = "/"), plot = p1, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)




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
