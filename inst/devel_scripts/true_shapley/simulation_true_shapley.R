
source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley.R")

parameters_list <- list(Sigma_diag = 1,
                        corr = 0,
                        mu = c(0, 0, 0),
                        beta = c(1, 1, 2, 3, 1, 2, 3, 1, 2, 3),
                        N_data = 10000,
                        N_shapley = 10000000,
                        noise = FALSE,
                        response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, noise, beta){
                          return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                   beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                   beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + noise)},
                        fit_mod = "regression",
                        methods = c("empirical", "gaussian", "ctree"),
                        name = 'corr0, mu0',
                        cutoff = NULL) # cutoff = c(-200, 0, 1, 200)


# response_mod = function(feat1, feat2, feat3, noise, beta){
#   return(beta[1] + beta[2] * (1 - feat12 * feat13) +  beta[3] * feat12 + beta[4] * feat13 +
#            beta[5] * (1 - feat22 * feat23) + beta[6] * feat22 + beta[7] * feat23 +
#            beta[8] * (1 - feat32 * feat33) + beta[9] * feat32 + beta[10] * feat33 + noise)
# }


parameters_list <- list()
parameters_list[[1]] <- list(Sigma_diag = 1,
                        corr = 0,
                        mu = c(1, -2, 0.5),
                        beta = c(1, 1, 1, 1),
                        N_data = 10000,
                        N_shapley = 100000,
                        noise = FALSE,
                        response_mod = function(x1, x2, x3, noise, beta){
                          return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise)
                        },
                        fit_mod = "regression",
                        methods = c("empirical", "gaussian", "ctree"),
                        name = 'corr0, mu_non0')

parameters_list[[2]] <- list(Sigma_diag = 1,
                             corr = 0.1,
                             mu = c(1, -2, 0.5),
                             beta = c(1, 1, 1, 1),
                             N_data = 10000,
                             N_shapley = 100000,
                             noise = FALSE,
                             response_mod = function(x1, x2, x3, noise, beta){
                               return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise) # x1 * x2
                             },
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree"),
                             name = 'corr0.1, mu_non0')

parameters_list[[3]] <- list(Sigma_diag = 1,
                             corr = 0.2,
                             mu = c(1, -2, 0.5),
                             beta = c(1, 1, 1, 1),
                             N_data = 10000,
                             N_shapley = 100000,
                             noise = FALSE,
                             response_mod = function(x1, x2, x3, noise, beta){
                               return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise) # x1 * x2
                             },
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree"),
                             name = 'corr0.2, mu_non0')

parameters_list[[4]] <- list(Sigma_diag = 1,
                             corr = 0,
                             mu = c(0, 0, 0),
                             beta = c(1, 1, 1, 1),
                             N_data = 10000,
                             N_shapley = 100000,
                             noise = FALSE,
                             response_mod = function(x1, x2, x3, noise, beta){
                               return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise) # x1 * x2
                             },
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree"),
                             name = 'corr0, mu0')

parameters_list[[5]] <- list(Sigma_diag = 1,
                             corr = 0.5,
                             mu = c(0, 0, 0),
                             beta = c(1, 1, 1, 1),
                             N_data = 10000,
                             N_shapley = 100000,
                             noise = FALSE,
                             response_mod = function(x1, x2, x3, noise, beta){
                               return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise) # x1 * x2
                             },
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree"),
                             name = 'corr0.5, mu0')

parameters_list[[6]] <- list(Sigma_diag = 1,
                             corr = 0.8,
                             mu = c(1, 1, 1),
                             beta = c(1, 1, 1, 1),
                             N_data = 10000,
                             N_shapley = 100000,
                             noise = FALSE,
                             response_mod = function(x1, x2, x3, noise, beta){
                               return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise) # x1 * x2
                             },
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree"),
                             name = 'corr0.8, mu1')

parameters_list[[7]] <- list(Sigma_diag = 1,
                             corr = 0.9,
                             mu = c(1, 1, 1),
                             beta = c(1, 1, 1, 1),
                             N_data = 10000,
                             N_shapley = 100000,
                             noise = FALSE,
                             response_mod = function(x1, x2, x3, noise, beta){
                               return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise) # x1 * x2
                             },
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree"),
                             name = 'corr0.9, mu1')

parameters_list[[8]] <- list(Sigma_diag = 1,
                             corr = 0,
                             mu = c(3, 3, 3),
                             beta = c(1, 1, 1, 1),
                             N_data = 10000,
                             N_shapley = 100000,
                             noise = FALSE,
                             response_mod = function(x1, x2, x3, noise, beta){
                               return(beta[1] + beta[2] * x1 + beta[3] * x2 + beta[4] * x3 + noise)
                             },
                             fit_mod = "regression",
                             methods = c("empirical", "gaussian", "ctree"),
                             name = 'corr0, mu3')


all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
}

MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL

for(i in 1:length(parameters_list)){
  MAE_truth <- c(MAE_truth, MAE(all_methods[[i]][[1]], all_methods[[i]][[2]]))
  for(j in 1:length(parameters_list[[i]]$methods)){
    MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][[1]], all_methods[[i]][[3]][[j]]$dt))
    MAE_methods_names <- c(MAE_methods_names, parameters_list[[i]]$methods[j])
    MAE_parameters <- c(MAE_parameters, parameters_list[[i]]$name)
  }
}


results <- data.frame(MAE_methods, MAE_methods_names, MAE_parameters)

# saveRDS(results, file = "/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/18_11_19results.rds")

library(ggplot2)
ggplot(data = results, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names))) + geom_point()

