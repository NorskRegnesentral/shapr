library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

tod_date <- '9_02_20'
dim <- 5
##

# response_mod <- function(mod_matrix_full, beta, epsilon){
#   as.vector(mod_matrix_full %*% beta) + epsilon
# }



# parameters_list <- list()

# corr <- c(0, 0.1, 0.5, 0.8, 0.9)
# no_categories <- 3
# set.seed(1)
# beta <- round(rnorm(dim * no_categories + 1), 1)
#
# k <- 1
# for(j in corr){
#   parameters_list[[k]] <- list(Sigma_diag = 1,
#                                corr = j,
#                                mu = rep(0, dim),
#                                beta = beta,
#                                N_shapley = 100000,
#                                noise = TRUE,
#                                response_mod = response_mod,
#                                fit_mod = "regression",
#                                methods = c("empirical_ind", "empirical", "gaussian", "ctree_onehot", "ctree"),
#                                name = paste0('corr', j),
#                                cutoff = c(-200, 0, 1, 200),
#                                no_categories = no_categories,
#                                N_training = 100,
#                                N_testing = 100,
#                                seed = 1)
#   k <- k + 1
# }



## load data

nm <- paste0(tod_date, "_results_", 5, "_dim_", dim, ".rds")
all_methods <- readRDS(paste0("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm))



MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL
MAE_seed <- NULL

for(i in 1:length(all_methods)){
  if(!is.null(all_methods[[i]][[1]][['true_linear']])){
    MAE_truth <- c(MAE_truth, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']]))
  }
  for(m in names(all_methods[[1]]$methods)){

    if(m != 'ctree'){
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt_sum))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
    } else{
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
    }
    MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
  }
}



results <- data.table(MAE_methods, MAE_methods_names, MAE_parameters, MAE_seed)
results[, correlation := paste0("", str_sub(MAE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)

nm = paste(tod_date, '_results_dim_', dim, '.rds', sep = "")
saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm, sep = ""))

nm = paste(tod_date, '_all_methods_dim_', dim, '.rds', sep = "")
saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm, sep = ""))


## plotting
p0 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names ))) +
  geom_point(size = 4, stroke = 1.5) +
  scale_x_discrete(labels = c("corr0" = "0", "corr0.1" = "0.1", "corr0.5" = "0.5", "corr0.8" = "0.8", "corr0.9" = "0.9")) +
  theme_bw(base_size = 22) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian") ) +
  ggtitle(paste("Dim:", dim, "N_shapley = ", all_methods[[1]]$parameters$N_shapley,
                "N_train = ", all_methods[[1]]$parameters$N_training, sep = " "))


nm = paste(tod_date, '_MAE_dim_', dim, '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/higher_dimensions/", nm, sep = ""), plot = p0, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)

##
AE_truth <- NULL
AE_methods <- NULL
AE_methods_names <- NULL
AE_parameters <- NULL
AE_seed <- NULL
corr <- NULL

for(i in 1:length(all_methods)){
  if(!is.null(all_methods[[i]][['true_linear']])){
    AE_truth <- c(AE_truth, AE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']]))
  }
  corr <- c(corr, all_methods[[i]]$parameters$corr)
  AE_seed <- c(AE_seed, all_methods[[i]]$seed)
  for(m in names(all_methods[[1]]$methods)){

    if(m != 'ctree'){
      AE_methods <- c(AE_methods, AE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt_sum))
      # AE_methods_names <- c(AE_methods_names, m)
      # AE_parameters <- c(AE_parameters, all_methods[[i]]$parameters$name)
    } else{
      AE_methods <- c(AE_methods, AE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt))
      # AE_methods_names <- c(AE_methods_names, m)
      # AE_parameters <- c(AE_parameters, all_methods[[i]]$parameters$name)
    }
    # AE_seed <- c(AE_seed, all_methods[[i]]$seed)
  }
}


N_testing <- all_methods[[1]]$parameters$N_testing

results <- data.table(AE_methods, rep(names(all_methods[[1]]$methods), each = N_testing),
                      rep(corr, each = N_testing), rep(AE_seed, each = N_testing))

setnames(results, c("AE_methods", "V2", "V3", "V4"), c("AE_methods", "AE_methods_names", "correlation", "seed"))
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)
results0[, 'group' := as.factor(correlation)]


p1 <- ggplot(data = results0, aes(y = AE_methods, x = group, fill = as.factor(AE_methods_names))) +
  geom_boxplot() + facet_wrap(~group, scale = "free")



    scale_x_discrete(labels = c("corr0" = "0", "corr0.1" = "0.1", "corr0.5" = "0.5", "corr0.8" = "0.8", "corr0.9" = "0.9")) +
  theme_bw(base_size = 22) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian") ) +
  ggtitle(paste("Dim:", dim, "N_shapley = ", all_methods[[1]]$parameters$N_shapley,
                "N_train = ", all_methods[[1]]$parameters$N_training, sep = " "))


