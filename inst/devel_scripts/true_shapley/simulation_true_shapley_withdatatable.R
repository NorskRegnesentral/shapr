library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans

# source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley.R")

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

tod_date <- '17_01_20'

response_mod = function(tbl, beta, mod_matrix){
  # beta <- c(1.0, 1,  0.0,  0,  0,  0,  0,  0,  0, 0)
  nms <- colnames(mod_matrix)[-1]

  for(i in 1:length(nms)){
    assign(nms[i], tbl[, nms[i], with = FALSE])
  }

  epsilon <- tbl[, "epsilon"]

  return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
           beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
           beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)
}


# parameters_list <- list(Sigma_diag = 1,
#                         corr = 0,
#                         mu = c(0, 0, 0),
#                         beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
#                         N_shapley = 10000,
#                         N_training = 1000,
#                         N_testing = 1000,
#                         cutoff = c(-200, 0, 1, 200),
#                         noise = FALSE,
#                         response_mod = response_mod,
#                         fit_mod = "regression",
#                         methods = c("empirical"), # "gaussian", "ctree"
#                         name = 'corr0')
#
# ll <- simulate_data(parameters_list)
# head(ll$true_shapley)
# head(ll$true_linear)
# MAE(ll$true_shapley, ll$true_linear) # 0.00095050 for 10000 obs and 1000 for training and testing / 0.0009436
# MAE(ll$true_shapley, ll$methods[['gaussian']]$dt_sum) # 0.01729 # 2.68 minutes
# MAE(ll$true_shapley, ll$methods[['empirical']]$dt_sum) # 0.0179 # 1.06 minutes
# MAE(ll$true_shapley, ll$methods[['ctree']]$dt) # 0.016423 #
# MAE(ll$true_shapley, ll$methods[['ctree_onehot']]$dt_sum) # 0.013 5.46 minutes
# MAE(ll$true_shapley, ll$methods[['empirical_ind']]$dt_sum) # 0.01876 # 2.10 minutes
#

##

parameters_list <- list()

parameters_list[[1]] <- list(Sigma_diag = 1,
                             corr = 0,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = response_mod,
                             fit_mod = "regression",
                             methods = c("empirical", "empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr0',
                             cutoff = c(-200, 0, 1, 200),
                             N_training = 1000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles

parameters_list[[2]] <- list(Sigma_diag = 1,
                             corr = 0.01,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = response_mod,
                             fit_mod = "regression",
                             methods = c("empirical", "empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr01',
                             cutoff = c(-200, 0, 1, 200),
                             N_training = 1000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles



parameters_list[[3]] <- list(Sigma_diag = 1,
                             corr = 0.1,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = response_mod,
                             fit_mod = "regression",
                             methods = c("empirical", "empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr1',
                             cutoff = c(-200, 0, 1, 200),
                             N_training = 1000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles

parameters_list[[4]] <- list(Sigma_diag = 1,
                             corr = 0.2,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = response_mod,
                             fit_mod = "regression",
                             methods = c("empirical", "empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr2',
                             cutoff = c(-200, 0, 1, 200),
                             N_training = 1000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles

parameters_list[[5]] <- list(Sigma_diag = 1,
                             corr = 0.5,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = response_mod,
                             fit_mod = "regression",
                             methods = c("empirical", "empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr5',
                             cutoff = c(-200, 0, 1, 200),
                             N_training = 1000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles


parameters_list[[6]] <- list(Sigma_diag = 1,
                             corr = 0.8,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = response_mod,
                             fit_mod = "regression",
                             methods = c("empirical", "empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr8',
                             cutoff = c(-200, 0, 1, 200),
                             N_training = 1000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles


parameters_list[[7]] <- list(Sigma_diag = 1,
                             corr = 0.9,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = response_mod,
                             fit_mod = "regression",
                             methods = c("empirical", "empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr9',
                             cutoff = c(-200, 0, 1, 200),
                             N_training = 1000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles


tm <- Sys.time()
all_methods <- list()
for(i in 1:length(parameters_list)){ # length(parameters_list)
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(tod_date, '_results_', i,".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))
}

mydata <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/17_01_20_results_7.rds")


MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL

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
  }
}


results <- data.table(MAE_methods, MAE_methods_names, MAE_parameters)
results[, correlation := paste0(".", str_sub(MAE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)

nm = paste(tod_date, '_results', '.rds', sep = "")
saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))

nm = paste(tod_date, '_all_methods', '.rds', sep = "")
saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))

# mydata <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/1_12_19_results.rds")

library(ggplot2)
p1 <- ggplot(data = results0, aes(y = MAE_methods, x = correlation, col = as.factor(MAE_methods_names))) +
  geom_point(size = 2, stroke = 1.5, shape = 16) + scale_x_continuous(breaks = seq(0,  1, by = 0.2)) +
  theme_grey(base_size = 22) + xlab("correlation") + ylab("Mean aveerage error (MAE)") +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian"))

nm = paste(tod_date, '_MAE', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/categorical_shapley/", nm, sep = ""), plot = p1, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)
tm2 <- Sys.time()
print(tm2 - tm)

