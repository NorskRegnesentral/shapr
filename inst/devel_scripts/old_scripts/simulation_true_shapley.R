
library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley.R")

tod_date <- '13_01_20'

parameters_list <- list(Sigma_diag = 1,
                        corr = 0,
                        mu = c(0, 0, 0),
                        beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                        N_shapley = 10000000,
                        noise = FALSE,
                        response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                          return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                   beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                   beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
                        fit_mod = "regression",
                        methods = c("empirical", "empirical_ind"), # "gaussian", "ctree"
                        name = 'corr0',
                        cutoff = c(-200, 0, 1, 200),
                        N_training = 1000,
                        N_testing = 1000)

ll <- simulate_data(parameters_list)
head(ll$true_shapley)
head(ll$true_linear)
MAE(ll$true_shapley, ll$true_linear) # 0.00095050 for 10000 obs / 0.0009436
MAE(ll$true_shapley, ll$methods[['gaussian']]$dt_sum) # 0.01729
MAE(ll$true_shapley, ll$methods[['empirical']]$dt_sum) # 0.0179
MAE(ll$true_shapley, ll$methods[['ctree']]$dt) # 0.016423
MAE(ll$true_shapley, ll$methods[['ctree_onehot']]$dt_sum) # 0.013
MAE(ll$true_shapley, ll$methods[['empirical_ind']]$dt_sum) # 0.01876


MAE(ll$true_linear, ll$methods[['empirical_ind']]$dt_sum) # 0.01312 / 0.01548235


parameters_list <- list()

parameters_list[[1]] <- list(Sigma_diag = 1,
                             corr = 0,
                             mu = c(0, 0, 0),
                             beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                             N_shapley = 10000000,
                             noise = TRUE,
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
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
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
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
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
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
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
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
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
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
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
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
                             response_mod = function(feat12, feat13, feat22, feat23, feat32, feat33, epsilon = 0, beta){
                               return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
                                        beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
                                        beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)},
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

mydata <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/02_12_19_results_7.rds")


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

nm = paste(tod_date, '_results', '.rds', sep = "")
saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))

nm = paste(tod_date, '_all_methods', '.rds', sep = "")
saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))

# mydata <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/1_12_19_results.rds")

library(ggplot2)
p1 <- ggplot(data = results, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names))) + geom_point(size = 2.5, stroke = 1.5, shape = 16) +
  theme_grey(base_size = 26)

nm = paste(tod_date, '_MAE', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/categorical_shapley/", nm, sep = ""), plot = p1, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)
tm2 <- Sys.time()
print(tm2 - tm)


results$MAE_methods2 <- round(results$MAE_methods, 4)

### -------------------------- figure out timng -----------------------------
timing_mat <- matrix(NA, nrow = length(all_methods), ncol = 5)

for(i in 1:length(all_methods)){
  for(j in 1:5){
    timing_mat[i, j] <- all_methods[[i]]$timing[j][[1]]
  }
}

timing_mat[, 1:4] <- timing_mat[, 1:4] * 60

# timing_mat[4, 1] <- timing_mat[4, 1] / 60

##      76.03504 169.8718 218.0687 381.1239 43.34351
## [2,] 61.69069 160.8733 209.9902 377.6845 42.66945
## [3,] 63.50596 155.2695 204.4956 368.1923 41.64979
## [4,] 59.41624 148.8400 208.6320 359.0781 41.49687
## [5,] 60.55542 158.4871 222.8267 384.4653 41.95226
## [6,] 63.54166 156.8506 212.8682 375.4930 42.68414
## [7,] 68.89645 167.2157 208.9224 395.6664 41.00328

#### other stuff

## load data ----------------
# all_methods <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/1_12_19_all_methods.rds")

# true_shapley <- all_methods[[1]][['true_shapley']]
# shapley_method <- all_methods[[1]][['methods']][['empirical']]$dt_sum

MAE <- function(true_shapley, shapley_method){
  mean(apply(abs(true_shapley - shapley_method), 2, mean)[-1])
}


AE_abs <- function(df){
  rho0_mat <- data.table(apply(abs(df$methods[[1]]$dt_sum - df$true_shapley)[, -1], 1, mean))
  rho0_mat <- cbind(rho0_mat,  names(df$methods)[1])

  for(i in 2:length(names(df$methods))){
    if(names(df$methods)[i] == 'ctree'){
      rho0_mat1 <- data.table(apply(abs(df$methods[[i]]$dt - df$true_shapley)[, -1], 1, mean))
      rho0_mat1 <- cbind(rho0_mat1, names(df$methods)[i])
      rho0_mat <- rbind(rho0_mat, rho0_mat1)
    } else{
      rho0_mat1 <- data.table(apply(abs(df$methods[[i]]$dt_sum - df$true_shapley)[, -1], 1, mean))
      rho0_mat1 <- cbind(rho0_mat1, names(df$methods)[i])
      rho0_mat <- rbind(rho0_mat, rho0_mat1)
    }
  }
  setnames(rho0_mat, c("V1", "V2"), c("avg_error", "method"))
  return(rho0_mat)
}


AE <- function(df){
  rho0_mat <- data.table(apply((df$methods[[1]]$dt_sum - df$true_shapley)[, -1], 1, mean))
  rho0_mat <- cbind(rho0_mat,  names(df$methods)[1])

  for(i in 2:length(names(df$methods))){
    if(names(df$methods)[i] == 'ctree'){
      rho0_mat1 <- data.table(apply((df$methods[[i]]$dt - df$true_shapley)[, -1], 1, mean))
      rho0_mat1 <- cbind(rho0_mat1, names(df$methods)[i])
      rho0_mat <- rbind(rho0_mat, rho0_mat1)
    } else{
      rho0_mat1 <- data.table(apply((df$methods[[i]]$dt_sum - df$true_shapley)[, -1], 1, mean))
      rho0_mat1 <- cbind(rho0_mat1, names(df$methods)[i])
      rho0_mat <- rbind(rho0_mat, rho0_mat1)
    }
  }
  setnames(rho0_mat, c("V1", "V2"), c("avg_error", "method"))
  return(rho0_mat)
}

AE1 <- AE(df = all_methods[[1]])
AE2 <- AE(df = all_methods[[2]])
AE3 <- AE(df = all_methods[[3]])
AE4 <- AE(df = all_methods[[4]])
AE5 <- AE(df = all_methods[[5]])
AE6 <- AE(df = all_methods[[6]])
AE7 <- AE(df = all_methods[[7]])

AEabs1 <- AE_abs(df = all_methods[[1]])
AEabs2 <- AE_abs(df = all_methods[[2]])
AEabs3 <- AE_abs(df = all_methods[[3]])
AEabs4 <- AE_abs(df = all_methods[[4]])
AEabs5 <- AE_abs(df = all_methods[[5]])
AEabs6 <- AE_abs(df = all_methods[[6]])
AEabs7 <- AE_abs(df = all_methods[[7]])

p11 <- ggplot(data = AE1, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average error (no absolute value), rho = 0') +
  theme_grey(base_size = 20) # + geom_hline(yintercept = 0, col = 'red')
p12 <- ggplot(data = AE2, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average error (no absolute value), rho = 0.01') + theme_grey(base_size = 20)
p13 <- ggplot(data = AE3, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average error (no absolute value), rho = 0.1') + theme_grey(base_size = 20)
p14 <- ggplot(data = AE4, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average error (no absolute value), rho = 0.2') + theme_grey(base_size = 20)
p15 <- ggplot(data = AE5, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average error (no absolute value), rho = 0.5') + theme_grey(base_size = 20)
p16 <- ggplot(data = AE6, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average error (no absolute value), rho = 0.8') + theme_grey(base_size = 20)
p17 <- ggplot(data = AE7, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average error (no absolute value), rho = 0.9') + theme_grey(base_size = 20)


p21 <- ggplot(data = AEabs1, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average absolute error, rho = 0') + theme_grey(base_size = 20) + ylim(c(0, 0.1))
p22 <- ggplot(data = AEabs2, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average absolute error, rho = 0.01') + theme_grey(base_size = 20) + ylim(c(0, 0.1))
p23 <- ggplot(data = AEabs3, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average absolute error, rho = 0.1') + theme_grey(base_size = 20) + ylim(c(0, 0.1))
p24 <- ggplot(data = AEabs4, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average absolute error, rho = 0.2') + theme_grey(base_size = 20) + ylim(c(0, 0.1))
p25 <- ggplot(data = AEabs5, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average absolute error, rho = 0.5') + ylim(c(0, 0.2)) + theme_grey(base_size = 20)
p26 <- ggplot(data = AEabs6, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average absolute error, rho = 0.8') + ylim(c(0, 0.2)) + theme_grey(base_size = 20)
p27 <- ggplot(data = AEabs7, aes(x = method, y = avg_error)) + geom_boxplot() + ggtitle(label = 'average absolute error, rho = 0.9') + ylim(c(0, 0.2)) + theme_grey(base_size = 20)

for(i in 1:7){
  for(j in 1:2){
    nm = paste(tod_date, '_MAE_p', j, i, '.png', sep = "")
    ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/categorical_shapley/", nm, sep = ""), plot = get(paste('p',j, i, sep = "")), device = NULL, path = NULL,
           scale = 1, width = 25, height = 30, units = "cm",
           dpi = 300, limitsize = TRUE)
  }
}




##
xx <- AE(df = all_methods[[1]])
xx[, mean(avg_error), by = 'method']

xx <- AE(df = all_methods[[2]])
xx[, mean(avg_error), by = 'method']





