

## This script gets the table for the NEW simulation with 2 continuous and 2 categorical (4 levels)
## Done Mar 16

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

tod_date <- "16_03_20"
rand_string <- "0oS75"
dim <- 4
no_categories <- 4

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))


# for(i in 1:length(all_methods)){
#
#   for(m in names(all_methods[[1]]$methods)){
#     if(m != 'kernelSHAP' & m != 'ctree'){
#
#       all_methods[[i]]$methods[[m]]$dt_sum2 <- cbind(all_methods[[i]]$methods[[m]]$dt[, 1:3],
#                                                      all_methods[[i]]$methods[[m]]$dt[, 4] + all_methods[[i]]$methods[[m]]$dt[, 5],
#                                                      all_methods[[i]]$methods[[m]]$dt[, 6] + all_methods[[i]]$methods[[m]]$dt[, 7])
#
#       names(all_methods[[i]]$methods[[m]]$dt_sum2) <- c("none", "cont_1_", "cont_2_", "cat_1_", "cat_2_")
#     }
#   }
# }

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- all_methods[[j]]$parameters$No_test_obs

  l = list()
  for(i in 1:10){
    l[[i]] <- cbind(all_methods[[j]]$methods[[i + K - 1]]$dt_sum, 1:top)
  }
  l_all <- rbindlist(l)
  setnames(l_all, c('V2'), c('id'))
  gaussian_mean <- l_all[, lapply(.SD, mean), by = id]
  gaussian_mean[, id := NULL]
  ##
  dt_sum_list[[j]] <- list(gaussian_nsamples100 = gaussian_mean)

}



for(j in 1:length(all_methods)){
  for(m in names(all_methods[[j]]$methods)){

    if(m == 'ctree' | m == 'kernelSHAP'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt

    } else if(m == 'empirical' | m == 'empirical_ind' | m == 'ctree_onehot'){
      dt_sum_list[[j]][[m]] <- all_methods[[j]]$methods[[m]]$dt_sum

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

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat4",  sep = "/"))

## ALL RESULTS
corr_all <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/all_correlations", "results_dim4_nocat4", sep = "/"))

full_table <- corr_all

t <- reshape(full_table, idvar = c("method_names", "dim", "no_categories"), timevar = "corr", direction = "wide")
setcolorder(t, c("dim", "no_categories", "method_names", "MAE_results.0", "MAE_results.0.1", "MAE_results.0.3", "MAE_results.0.5", "MAE_results.0.8")) # , "MAE_results.0.9"

colnames(t) <- c("dim", "nb categories", "method", "0", "0.1", "0.3", "0.5", "0.8", "0.9") #
print(xtable(t, digits = c(0, 0, 0, 0, rep(4, 6))), include.rownames = FALSE)




## TIMING
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 1:length(all_methods[[i]]$timing)){
    timing <-   rbind(timing, t(as.matrix(all_methods[[i]]$timing[[j]])))
    names <- c(names, names(all_methods[[i]]$timing)[j])
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}

timing <- data.table(timing)
timing[, method := names]
timing[, corr := corr]
timing[, dim := dim]
timing[, no_categories := no_categories]
timing1 <- timing[, c("elapsed", "method", "corr", "dim", "no_categories")]
timing2 <- timing1[, (mean_elapsed = mean(elapsed)), by = c("method", "dim", "no_categories")]
saveRDS(timing2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/", "timing_dim4_nocat4",  sep = "/"))

timing_full_table <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", "timing_dim4_nocat4", sep = "/"))

timing_full_table[, no_x_test := 500]
timing_full_table[, V2 := V1 / no_x_test]
timing_full_table[, V1 := NULL]

setcolorder(timing_full_table, c("dim", "no_categories", "no_x_test", "method", "V2"))

colnames(timing_full_table) <- c("dim", "nb categories", "nb variables",  "method", "elapsed")

print(xtable(timing_full_table, digits = c(0, 0, 0, 0, 0, 3)), include.rownames = FALSE)

