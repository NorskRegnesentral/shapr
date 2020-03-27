
## This script gets the table for the original simulation with 2 continuous and 2 categorical (3 levels)
## Done over Mar 12-15
## THIS IS NOT THE CURRENT SCRIPT GO TO get_tables_cond_cat2.R!!!!!!!!

library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(stringr)
library(xtable)

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

##------------------------------------------------------
## DIMENSION 3
## NB CATEGORIES 4
## CORR = 0
tod_date <- "11_03_20"
rand_string <- "GmjIT"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))


for(i in 1:length(all_methods)){

  for(m in names(all_methods[[1]]$methods)){
    if(m != 'kernelSHAP' & m != 'ctree'){

      all_methods[[i]]$methods[[m]]$dt_sum2 <- cbind(all_methods[[i]]$methods[[m]]$dt[, 1:3],
                                                     all_methods[[i]]$methods[[m]]$dt[, 4] + all_methods[[i]]$methods[[m]]$dt[, 5],
                                                     all_methods[[i]]$methods[[m]]$dt[, 6] + all_methods[[i]]$methods[[m]]$dt[, 7])

      names(all_methods[[i]]$methods[[m]]$dt_sum2) <- c("none", "cont_1_", "cont_2_", "cat_1_", "cat_2_")
    }
  }
}

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}



for(j in 1:length(all_methods)){
  for(m in names(all_methods[[j]]$methods)){

    if(m == 'ctree' | m == 'kernelSHAP'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt

      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[[m]])]] <- all_methods[[j]]$methods[[m]]$dt

    } else if(m == 'empirical' | m == 'empirical_ind' | m == 'ctree_onehot'){
      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt_sum2

    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = 1/nrow(dt_sum_list[[i]][[m]]))  )
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0",  sep = "/"))



## CORR = 0.1
tod_date <- "12_03_20"
rand_string <- "Z9ICO"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.1.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))


for(i in 1:length(all_methods)){

  for(m in names(all_methods[[1]]$methods)){
    if(m != 'kernelSHAP' & m != 'ctree'){

      all_methods[[i]]$methods[[m]]$dt_sum2 <- cbind(all_methods[[i]]$methods[[m]]$dt[, 1:3],
                                                     all_methods[[i]]$methods[[m]]$dt[, 4] + all_methods[[i]]$methods[[m]]$dt[, 5],
                                                     all_methods[[i]]$methods[[m]]$dt[, 6] + all_methods[[i]]$methods[[m]]$dt[, 7])

      names(all_methods[[i]]$methods[[m]]$dt_sum2) <- c("none", "cont_1_", "cont_2_", "cat_1_", "cat_2_")
    }
  }
}

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}



for(j in 1:length(all_methods)){
  for(m in names(all_methods[[j]]$methods)){

    if(m == 'ctree' | m == 'kernelSHAP'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt

      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[[m]])]] <- all_methods[[j]]$methods[[m]]$dt

    } else if(m == 'empirical' | m == 'empirical_ind' | m == 'ctree_onehot'){
      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt_sum2

    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = 1/nrow(dt_sum_list[[i]][[m]]))  )
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.1",  sep = "/"))




## CORR = 0.3
tod_date <- "12_03_20"
rand_string <- "jGcDN"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.3.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))


for(i in 1:length(all_methods)){

  for(m in names(all_methods[[1]]$methods)){
    if(m != 'kernelSHAP' & m != 'ctree'){

      all_methods[[i]]$methods[[m]]$dt_sum2 <- cbind(all_methods[[i]]$methods[[m]]$dt[, 1:3],
                                                     all_methods[[i]]$methods[[m]]$dt[, 4] + all_methods[[i]]$methods[[m]]$dt[, 5],
                                                     all_methods[[i]]$methods[[m]]$dt[, 6] + all_methods[[i]]$methods[[m]]$dt[, 7])

      names(all_methods[[i]]$methods[[m]]$dt_sum2) <- c("none", "cont_1_", "cont_2_", "cat_1_", "cat_2_")
    }
  }
}

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}



for(j in 1:length(all_methods)){
  for(m in names(all_methods[[j]]$methods)){

    if(m == 'ctree' | m == 'kernelSHAP'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt

      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[[m]])]] <- all_methods[[j]]$methods[[m]]$dt

    } else if(m == 'empirical' | m == 'empirical_ind' | m == 'ctree_onehot'){
      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt_sum2

    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = 1/nrow(dt_sum_list[[i]][[m]]))  )
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.3",  sep = "/"))



## CORR = 0.5
tod_date <- "12_03_20"
rand_string <- "QOFdV"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.5.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))
#
#
# head(all_methods[[1]]$true_shapley, 10)
# head(all_methods[[1]]$methods$empirical$dt, 10)
# empirical <- cbind(all_methods[[1]]$methods$empirical$dt[, 1:3], all_methods[[1]]$methods$empirical$dt[, 4] + all_methods[[1]]$methods$empirical$dt[, 5],
#       all_methods[[1]]$methods$empirical$dt[, 6] + all_methods[[1]]$methods$empirical$dt[, 7])
# head(empirical, 10)
# head(all_methods[[1]]$methods$ctree$dt, 10)
# head(all_methods[[1]]$methods$ctree_onehot$dt, 10)
# ctree_onehot <- cbind(all_methods[[1]]$methods$ctree_onehot$dt[, 1:3], all_methods[[1]]$methods$ctree_onehot$dt[, 4] +
#                         all_methods[[1]]$methods$ctree_onehot$dt[, 5],
#                    all_methods[[1]]$methods$ctree_onehot$dt[, 6] +
#                      all_methods[[1]]$methods$ctree_onehot$dt[, 7])
# head(ctree_onehot, 10)
#
#


for(i in 1:length(all_methods)){

  for(m in names(all_methods[[1]]$methods)){
    if(m != 'kernelSHAP' & m != 'ctree'){

      all_methods[[i]]$methods[[m]]$dt_sum2 <- cbind(all_methods[[i]]$methods[[m]]$dt[, 1:3],
                                                     all_methods[[i]]$methods[[m]]$dt[, 4] + all_methods[[i]]$methods[[m]]$dt[, 5],
                                                     all_methods[[i]]$methods[[m]]$dt[, 6] + all_methods[[i]]$methods[[m]]$dt[, 7])

      names(all_methods[[i]]$methods[[m]]$dt_sum2) <- c("none", "cont_1_", "cont_2_", "cat_1_", "cat_2_")
    }
  }
}

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}



for(j in 1:length(all_methods)){
  for(m in names(all_methods[[j]]$methods)){

    if(m == 'ctree' | m == 'kernelSHAP'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt

      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[[m]])]] <- all_methods[[j]]$methods[[m]]$dt

    } else if(m == 'empirical' | m == 'empirical_ind' | m == 'ctree_onehot'){
      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt_sum2

    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = 1/nrow(dt_sum_list[[i]][[m]]))  )
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.5",  sep = "/"))


## CORR = 0.8
tod_date <- "13_03_20"
rand_string <- "YBjVt"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.8.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))


for(i in 1:length(all_methods)){

  for(m in names(all_methods[[1]]$methods)){
    if(m != 'kernelSHAP' & m != 'ctree'){

      all_methods[[i]]$methods[[m]]$dt_sum2 <- cbind(all_methods[[i]]$methods[[m]]$dt[, 1:3],
                                                     all_methods[[i]]$methods[[m]]$dt[, 4] + all_methods[[i]]$methods[[m]]$dt[, 5],
                                                     all_methods[[i]]$methods[[m]]$dt[, 6] + all_methods[[i]]$methods[[m]]$dt[, 7])

      names(all_methods[[i]]$methods[[m]]$dt_sum2) <- c("none", "cont_1_", "cont_2_", "cat_1_", "cat_2_")
    }
  }
}

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}



for(j in 1:length(all_methods)){
  for(m in names(all_methods[[j]]$methods)){

    if(m == 'ctree' | m == 'kernelSHAP'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt

      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[[m]])]] <- all_methods[[j]]$methods[[m]]$dt

    } else if(m == 'empirical' | m == 'empirical_ind' | m == 'ctree_onehot'){
      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt_sum2

    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = 1/nrow(dt_sum_list[[i]][[m]]))  )
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.8",  sep = "/"))




## CORR = 0.9
tod_date <- "13_03_20"
rand_string <- "j860L"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))


for(i in 1:length(all_methods)){

  for(m in names(all_methods[[1]]$methods)){
    if(m != 'kernelSHAP' & m != 'ctree'){

      all_methods[[i]]$methods[[m]]$dt_sum2 <- cbind(all_methods[[i]]$methods[[m]]$dt[, 1:3],
                                                     all_methods[[i]]$methods[[m]]$dt[, 4] + all_methods[[i]]$methods[[m]]$dt[, 5],
                                                     all_methods[[i]]$methods[[m]]$dt[, 6] + all_methods[[i]]$methods[[m]]$dt[, 7])

      names(all_methods[[i]]$methods[[m]]$dt_sum2) <- c("none", "cont_1_", "cont_2_", "cat_1_", "cat_2_")
    }
  }
}

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum2, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}



for(j in 1:length(all_methods)){
  for(m in names(all_methods[[j]]$methods)){

    if(m == 'ctree' | m == 'kernelSHAP'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt

      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[[m]])]] <- all_methods[[j]]$methods[[m]]$dt

    } else if(m == 'empirical' | m == 'empirical_ind' | m == 'ctree_onehot'){
      # dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt_sum2

    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = 1/nrow(dt_sum_list[[i]][[m]]))  )
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.9",  sep = "/"))








## ALL RESULTS
corr0 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0", sep = "/"))
corr0.1 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.1", sep = "/"))
corr0.3 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.3", sep = "/"))
corr0.5 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.5", sep = "/"))
corr0.8 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.8", sep = "/"))
corr0.9 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat3_corr0.9", sep = "/"))

full_table <- rbind(corr0, corr0.1, corr0.3, corr0.5, corr0.8, corr0.9)

t <- reshape(full_table, idvar = c("method_names", "dim", "no_categories"), timevar = "corr", direction = "wide")
setcolorder(t, c("dim", "no_categories", "method_names", "MAE_results.0", "MAE_results.0.1", "MAE_results.0.3", "MAE_results.0.5", "MAE_results.0.8")) # , "MAE_results.0.9"

colnames(t) <- c("dim", "nb categories", "method", "0", "0.1", "0.3", "0.5", "0.8", "0.9") #
print(xtable(t, digits = c(0, 0, 0, 0, rep(4, 6))), include.rownames = FALSE)
