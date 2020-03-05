library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(stringr)
library(xtable)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

tod_date <- "04_03_20"
rand_string <- "sg2yn"
dim <- 3
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

##

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations_Annabelle",
                             folder, nm, sep = "/"))

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- length(all_methods[[j]]$parameters$mu)^all_methods[[j]]$parameters$no_categories

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K]]$dt_sum, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean2 <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean2[, id := NULL]
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean, gaussian_nsamples1000 = gaussian_mean2)

}

for(j in 1:length(all_methods)){
  for(k in 1:length(names(all_methods[[j]]$methods))){
    if(grepl("empirical", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
    } else if(!grepl("gaussian", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt
    }
  }
}


MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
    MAE_methods_names <- c(MAE_methods_names, m)
    MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
  }
}


results <- data.table(MAE_methods, MAE_methods_names, MAE_parameters)
results[, correlation := paste0("", str_sub(MAE_parameters, start = 5, end = -1))]
results[, MAE_parameters := NULL]
t <- reshape(results, idvar = "MAE_methods_names", timevar = "correlation", direction = "wide")
rownames(t) <- t$MAE_methods_names

names(t) <- unique(results$correlation)

t[, MAE_methods_names := NULL]
xtable(t, digits = 4)

