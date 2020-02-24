library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable_OLDCODE.R")

tod_date <- '03_12_19'

response_mod = function(tbl, beta, mod_matrix){
  nms <- colnames(mod_matrix)[-1]

  for(i in 1:length(nms)){
    assign(nms[i], tbl[, nms[i], with = FALSE])
  }

  epsilon <- tbl[, "epsilon"]

  return(beta[1] + beta[2] * ((1 - feat12) * (1 - feat13)) +  beta[3] * feat12 + beta[4] * feat13 +
           beta[5] * ((1 - feat22) * (1 - feat23)) + beta[6] * feat22 + beta[7] * feat23 +
           beta[8] * ((1 - feat32) * (1 - feat33)) + beta[9] * feat32 + beta[10] * feat33 + epsilon)
}


parameters_list <- list()

seed <- 1
corr <- c(0, 0.1)

k <- 1
for(i in seed){
  for(j in corr){
    parameters_list[[k]] <- list(Sigma_diag = 1,
                                 corr = j,
                                 mu = c(0, 0, 0),
                                 beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                                 N_shapley = 100000,
                                 noise = FALSE,
                                 response_mod = response_mod,
                                 fit_mod = "regression",
                                 methods = c("empirical_ind", "empirical", "ctree"),
                                 name = paste0('corr', j),
                                 cutoff = c(-200, 0, 1, 200),
                                 N_training = 20,
                                 N_testing = 20,
                                 seed = i)
    k <- k + 1
  }
}

tm <- Sys.time()
all_methods <- list()
for(i in 1:length(parameters_list)){ # length(parameters_list)
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(tod_date, '_results_', i,".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/test/", nm, sep = ""))
}


MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL

for(i in 1:length(parameters_list)){
  if(!is.null(all_methods[[i]][['true_linear']])){
    MAE_truth <- c(MAE_truth, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']]))
  }
  for(m in names(all_methods[[1]]$methods)){

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
results[, correlation := paste0(str_sub(MAE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)


nm = paste(tod_date, '_results', '.rds', sep = "")
saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/test/", nm, sep = ""))

nm = paste(tod_date, '_all_methods', '.rds', sep = "")
saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/test/", nm, sep = ""))

## source data

nm <- "03_12_19_results.rds"
results0 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))

p1 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names))) +
  geom_point(size = 4, stroke = 3.5, shape = 16) +
  theme_bw(base_size = 34) +
  xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_x_discrete(labels = c("corr0" = "0","corr01" = "0.01",
                              "corr1" = "0.1",
                              "corr2" = "0.2",
                              "corr5" = "0.5",
                              "corr8" = "0.8",
                              "corr9" = "0.9")) +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian")) +
  ggtitle("Dim: 3, N_shapley = 1e+07, N_train = 1000, N_test = 1000")

nm = paste(tod_date, "_MAE_larger.png", sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/categorical_shapley/", nm, sep = ""), plot = p1,
       device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)

