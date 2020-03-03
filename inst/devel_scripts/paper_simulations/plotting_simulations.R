library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(stringr)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

tod_date <- "28_02_20"
rand_string <- "WbMlD"
dim <- 5
no_categories <- 6

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

##

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations_Annabelle", folder, nm, sep = "/"))

# for gaussian stuff

if(any(grepl("gaussian", names(all_methods[[1]]$methods)))){
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
folder <- paste0(tod_date, "_WbMlD", "_dim", dim, "_nbcat", no_categories)
saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations_Annabelle", folder, nm, sep = "/"))


p1 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names ))) +
  geom_point(size = 4, stroke = 1.5) +
  scale_x_discrete(labels = c("corr0" = "0", "corr0.05" = "0.05", "corr0.1" = "0.1", "corr0.3" = "0.3", "corr0.5" = "0.5", "corr0.8" = "0.8", "corr0.9" = "0.9")) +
  theme_bw(base_size = 22) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method" ) +
  ggtitle("")
#  labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian_100", "Gaussian_1000", "kernelSHAP")

nm = paste(tod_date, '_MAE', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations_Annabelle", folder, nm, sep = "/"), plot = p1, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)



