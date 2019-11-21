
source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley.R")

# parameters_list <- list(Sigma_diag = 1,
#                         corr = 0,
#                         mu = c(0, 0, 0),
#                         beta = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 3),
#                         N_shapley = 1000000,
#                         noise = FALSE,
#                         response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
#                           return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
#                                    beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
#                                    beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
#                         fit_mod = "regression",
#                         methods = c("empirical","empirical_ind", "gaussian", "ctree_onehot", "ctree"), # "empirical","empirical_ind", "gaussian", "ctree_onehot", "ctree"
#                         name = 'corr0, mu0',
#                         cutoff = NULL, # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles
#                         N_training = 10000,
#                         N_testing = 1000)
#
# ll <- simulate_data(parameters_list)
# # 1:21 - 1:40
# print(Sys.time())
#
#
# ll$true_shapley
# ll$true_linear
#
# MAE(ll$true_shapley, ll$true_linear) # 0.0056
# MAE(ll$true_shapley, ll$methods[['gaussian']]$dt_sum) # 0.01729
# MAE(ll$true_shapley, ll$methods[['empirical']]$dt_sum) # 0.0179
# MAE(ll$true_shapley, ll$methods[['ctree']]$dt) # 0.016423
# MAE(ll$true_shapley, ll$methods[['ctree_onehot']]$dt_sum) # 0.013
# MAE(ll$true_shapley, ll$methods[['empirical_ind']]$dt_sum) # 0.01876
#
#
# MAE(ll$true_linear, ll$methods[['empirical_ind']]$dt_sum) # 0.01312 / 0.01548235


parameters_list <- list()
parameters_list[[1]] <- list(Sigma_diag = 1,
                             corr = 0,
                             mu = c(0, 0, 0),
                             beta = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                             N_shapley = 1000000,
                             noise = FALSE,
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, noise, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + noise)},
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr0',
                             cutoff = NULL,
                             N_training = 10000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles

parameters_list[[2]] <- list(Sigma_diag = 1,
                             corr = 0.1,
                             mu = c(0, 0, 0),
                             beta = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                             N_shapley = 1000000,
                             noise = FALSE,
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, noise, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + noise)},
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr1',
                             cutoff = NULL,
                             N_training = 10000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles

parameters_list[[3]] <- list(Sigma_diag = 1,
                             corr = 0.2,
                             mu = c(0, 0, 0),
                             beta = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                             N_shapley = 1000000,
                             noise = FALSE,
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, noise, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + noise)},
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr2',
                             cutoff = NULL,
                             N_training = 10000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles

parameters_list[[4]] <- list(Sigma_diag = 1,
                             corr = 0.5,
                             mu = c(0, 0, 0),
                             beta = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                             N_shapley = 1000000,
                             noise = FALSE,
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, noise, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + noise)},
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr5',
                             cutoff = NULL,
                             N_training = 10000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles


parameters_list[[5]] <- list(Sigma_diag = 1,
                             corr = 0.9,
                             mu = c(0, 0, 0),
                             beta = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                             N_shapley = 1000000,
                             noise = FALSE,
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, noise, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + noise)},
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree_onehot", "ctree"), # "gaussian", "ctree"
                             name = 'corr9',
                             cutoff = NULL,
                             N_training = 10000,
                             N_testing = 1000) # cutoff = c(-200, 0, 1, 200) ## if null, makes cutoffs based on 0.33 and 0.66 quantiles


all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
}

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


results <- data.frame(MAE_methods, MAE_methods_names, MAE_parameters)

saveRDS(results, file = "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/21_11_19results.rds")

library(ggplot2)
ggplot(data = results, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names))) + geom_point()

