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
## NB CATEGORIES 3
tod_date <- "28_02_20"
rand_string <- "26OXL"
dim <- 3
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE", folder, nm, sep = "/"))

for(i in 1:length(all_methods)){
  all_methods[[i]]$methods$empirical_ind <- NULL
}

MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(all_methods)){
  for(m in names(all_methods[[1]]$methods)){
    if(m == 'ctree' | m == 'kernelSHAP'){

      true <- all_methods[[i]][['true_shapley']]
      meth <- all_methods[[i]]$methods[[m]]$dt
      weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

    } else if(grepl('gaussian', m)){
      tmp <- data.table(cbind(all_methods[[i]]$methods[[m]]$dt_sum, rep(1:dim^no_categories, each = 50)))
      tmp0 <- tmp[, lapply(.SD, mean), by = V2]
      tmp0[, V2 := NULL]

      true <- all_methods[[i]][['true_shapley']]
      meth <- tmp0
      weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

    }
    else {
      true <- all_methods[[i]][['true_shapley']]
      meth <- all_methods[[i]]$methods[[m]]$dt_sum
      weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

      #Hmisc::wtd.mean(x = abs_error_vec,weights = weight_vec,normwt=T) # Gives same as MAE()


    }

    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05, 0.95),type = 'quantile',normwt = T)
    quant_results <- cbind(quant_results, quants)


  }
}


results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]


results0 <- results

## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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

# timing2 <- timing[, c("elapsed", "method", "corr", "dim", "no_categories")]

# timing0 <- timing[, (mean_elapsed = mean(elapsed)), by = c("method", "dim", "no_categories")]

timing0 <- timing

## ------------------
## corr = 0.3
tod_date <- "13_03_20"
rand_string <- "jsXoK"
dim <- 3
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.3.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))

dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- (all_methods[[j]]$parameters$no_categories)^length(all_methods[[j]]$parameters$mu)

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
    if('ctree' == names(all_methods[[j]]$methods[k]) | 'kernelSHAP' == names(all_methods[[j]]$methods[k])){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt
    } else if(!grepl("gaussian", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    true <- all_methods[[i]][['true_shapley']]
    meth <- dt_sum_list[[i]][[m]]
    weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)


    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

results2 <- rbind(results0, results)
results2 <- results2[order(corr)]

saveRDS(results2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat3",  sep = "/"))


## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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


timing1 <- timing

timing2 <- rbind(timing0, timing1)

timing2 <- timing2[, c("elapsed", "method", "corr", "dim", "no_categories")]

timing2_1 <- timing2[, (mean_elapsed = mean(elapsed)), by = c("method", "dim", "no_categories")]

saveRDS(timing2_1, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim3_nocat3",  sep = "/"))


##------------------------------------------------------
## DIMENSION 3
## NB CATEGORIES 4

tod_date <- "03_03_20"
rand_string <- "bXBZr"
dim <- 3
no_categories <- 4

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))


dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- (all_methods[[j]]$parameters$no_categories)^length(all_methods[[j]]$parameters$mu)

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
    if('ctree' == names(all_methods[[j]]$methods[k]) | 'kernelSHAP' == names(all_methods[[j]]$methods[k])){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt
    } else if(!grepl("gaussian", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    true <- all_methods[[i]][['true_shapley']]
    meth <- dt_sum_list[[i]][[m]]
    weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)


    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

results0 <- results

## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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

timing0 <- timing

## ------------------
## corr = 0.3
tod_date <- "13_03_20"
rand_string <- "3dsS3"
dim <- 3
no_categories <- 4

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.3.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))


dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- (all_methods[[j]]$parameters$no_categories)^length(all_methods[[j]]$parameters$mu)

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
    if('ctree' == names(all_methods[[j]]$methods[k]) | 'kernelSHAP' == names(all_methods[[j]]$methods[k])){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt
    } else if(!grepl("gaussian", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    true <- all_methods[[i]][['true_shapley']]
    meth <- dt_sum_list[[i]][[m]]
    weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)


    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

results2 <- rbind(results0, results)
results2 <- results2[order(corr)]

saveRDS(results2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat4",  sep = "/"))

## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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


timing2 <- rbind(timing0, timing)

timing2 <- timing2[, c("elapsed", "method", "corr", "dim", "no_categories")]

timing2 <- timing2[, (mean_elapsed = mean(elapsed)), by = c("method", "dim", "no_categories")]

saveRDS(timing2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim3_nocat4",  sep = "/"))



##------------------------------------------------------
## DIMENSION 4
## NB CATEGORIES 3

tod_date <- "03_03_20"
rand_string <- "oYB36"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))


dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- (all_methods[[j]]$parameters$no_categories)^length(all_methods[[j]]$parameters$mu)

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
    if('ctree' == names(all_methods[[j]]$methods[k]) | 'kernelSHAP' == names(all_methods[[j]]$methods[k])){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt
    } else if(!grepl("gaussian", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    true <- all_methods[[i]][['true_shapley']]
    meth <- dt_sum_list[[i]][[m]]
    weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)


    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

results0 <- results

## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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

timing0 <- timing

## ---------
## corr = 0.3


tod_date <- "13_03_20"
rand_string <- "v2bcB"
dim <- 4
no_categories <- 3

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.3.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))


dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <- (all_methods[[j]]$parameters$no_categories)^length(all_methods[[j]]$parameters$mu)

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
    if('ctree' == names(all_methods[[j]]$methods[k]) | 'kernelSHAP' == names(all_methods[[j]]$methods[k])){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt
    } else if(!grepl("gaussian", names(all_methods[[j]]$methods[k]))){
      dt_sum_list[[j]][[names(all_methods[[j]]$methods[k])]] <- all_methods[[j]]$methods[[k]]$dt_sum
    }
  }
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    true <- all_methods[[i]][['true_shapley']]
    meth <- dt_sum_list[[i]][[m]]
    weight_vec <- all_methods[[i]]$joint_prob_true[[dim + 1]]

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)


    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

results2 <- rbind(results0, results)
results2 <- results2[order(corr)]
saveRDS(results2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim4_nocat3",  sep = "/"))

## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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


timing2 <- rbind(timing0, timing)

timing2 <- timing2[, c("elapsed", "method", "corr", "dim", "no_categories")]
timing2 <- timing2[, (mean_elapsed = mean(elapsed)), by = c("method", "dim", "no_categories")]
saveRDS(timing2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim4_nocat3",  sep = "/"))



##------------------------------------------------------
## DIMENSION 5
## NB CATEGORIES 6

tod_date <- "10_03_20" # "03_03_20"
rand_string <- "luQN8" # "FPqLx"
dim <- 5
no_categories <- 6

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))

true_shapley_list <- list()
dt_sum_list <- list()
for(j in 1:length(all_methods)){
  ct <- all_methods[[j]]$methods[['ctree']]$dt
  KS <- all_methods[[j]]$methods[['kernelSHAP']]$dt

  estimated_shap = all_methods[[j]]$methods$ctree$x_test
  true_shap = all_methods[[j]]$joint_prob_true

  col_names = names(all_methods[[j]]$methods$ctree$model$model)[-1]

  rmerge <- true_shap[estimated_shap, on = col_names, allow.cartesian = TRUE]

  feat_comb_id <- rmerge[['feat_comb_id']]
  TS <- all_methods[[j]][['true_shapley']]

  dt_sum_list[[j]] <- list(ctree = cbind(ct, feat_comb_id), kernelSHAP = cbind(KS, feat_comb_id))
  true_shapley_list[[j]] <- list(TS)
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(dt_sum_list)){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    true <- true_shapley_list[[i]][[1]]
    meth <- dt_sum_list[[i]][[m]][, feat_comb_id := NULL]
    weight_vec <- weights0

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)


    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)  }
}

results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]


saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim5_nocat6",  sep = "/"))



## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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

timing2 <- timing[, c("elapsed", "method", "corr", "dim", "no_categories")]
timing2 <- timing2[, (mean_elapsed = mean(elapsed)), by = c("method", "dim", "no_categories")]

saveRDS(timing2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim5_nocat6",  sep = "/"))


##------------------------------------------------------
## DIMENSION 7
## NB CATEGORIES 5

tod_date <- "10_03_20" # "03_03_20"
rand_string <- "l9ZQk" # "mdXqD"
dim <- 7
no_categories <- 5

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.9.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))

true_shapley_list <- list()
dt_sum_list <- list()
for(j in 1:length(all_methods)){

  ct <- all_methods[[j]]$methods[['ctree']]$dt
  KS <- all_methods[[j]]$methods[['kernelSHAP']]$dt

  estimated_shap = all_methods[[j]]$methods$ctree$x_test
  true_shap = all_methods[[j]]$joint_prob_true

  col_names = names(all_methods[[j]]$methods$ctree$model$model)[-1]

  rmerge <- true_shap[estimated_shap, on = col_names, allow.cartesian = TRUE]

  feat_comb_id <- rmerge[['feat_comb_id']]
  TS <- all_methods[[j]][['true_shapley']]

  dt_sum_list[[j]] <- list(ctree = cbind(ct, feat_comb_id), kernelSHAP = cbind(KS, feat_comb_id))
  true_shapley_list[[j]] <- list(TS)
}


MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL

for(i in 1:length(dt_sum_list)){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    true <- true_shapley_list[[i]][[1]]
    meth <- dt_sum_list[[i]][[m]][, feat_comb_id := NULL]
    weight_vec <- weights0

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)


    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
    }
}

results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim7_nocat5",  sep = "/"))


## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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

timing2 <- timing[, c("elapsed", "method", "corr", "dim", "no_categories")]
timing2 <- timing2[, (mean_elapsed = mean(elapsed)), by = c("method", "dim", "no_categories")]

saveRDS(timing2, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim7_nocat5",  sep = "/"))

##------------------------------------------------------
## DIMENSION 10
## NB CATEGORIES 4

## PART 1
tod_date <- "14_03_20"
rand_string <- "7XhAD"
dim <- 10
no_categories <- 4
folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho0.8_part38.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))


## PART 2
tod_date <- "16_03_20"
rand_string <- "miNZ8"
dim <- 10
no_categories <- 4
folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho0.9_part10.rds")
all_methods2 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))


all_methods[[39]] <- all_methods2[[1]]
all_methods[[40]] <- all_methods2[[2]]
all_methods[[41]] <- all_methods2[[3]]
all_methods[[42]] <- all_methods2[[4]]
all_methods[[43]] <- all_methods2[[5]]
all_methods[[44]] <- all_methods2[[6]]
all_methods[[45]] <- all_methods2[[7]]
all_methods[[46]] <- all_methods2[[8]]
all_methods[[47]] <- all_methods2[[9]]
all_methods[[48]] <- all_methods2[[10]]



true_shapley_list <- list()
dt_sum_list <- list()
for(j in 1:6){
  ct <- NULL
  KS <- NULL
  feat_comb_id <- NULL
  TS <- NULL
  for(i in ((j - 1) * 8 + 1):(j * 8)){
    ct <- rbind(ct, all_methods[[i]]$methods[['ctree']]$dt)
    KS <- rbind(KS, all_methods[[i]]$methods[['kernelSHAP']]$dt)

    estimated_shap = all_methods[[i]]$methods$ctree$x_test
    true_shap = all_methods[[i]]$joint_prob_true

    col_names = names(all_methods[[i]]$methods$ctree$model$model)[-1]

    rmerge <- true_shap[estimated_shap, on = col_names, allow.cartesian = TRUE]

    feat_comb_id <- c(feat_comb_id, rmerge[['feat_comb_id']])
    TS <- rbind(TS, all_methods[[i]][['true_shapley']])
  }
  dt_sum_list[[j]] <- list(ctree = cbind(ct, feat_comb_id), kernelSHAP = cbind(KS, feat_comb_id))
  true_shapley_list[[j]] <- list(TS)
}



##

MAE_results <- NULL
method_names <- NULL
corr <- NULL
quant_results <- NULL


for(i in 1:6){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    true <- true_shapley_list[[i]][[1]]
    meth <- dt_sum_list[[i]][[m]][, feat_comb_id := NULL]
    weight_vec <- weights0

    MAE_results <- c(MAE_results, MAE(true, meth, weights = weight_vec))
    abs_error_vec <- rowMeans(abs(true-meth)[,-1])
    quants <- Hmisc::wtd.quantile(x = abs_error_vec,weights = weight_vec,
                                  probs = c(0.05,0.95),type='quantile',normwt=T)
    quant_results <- cbind(quant_results,quants)

    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i * 8]]$parameters$corr)
  }
}

results <- data.table(MAE_results, q_005=quant_results[1,],q_095=quant_results[2,],method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim10_nocat4",  sep = "/"))






## timing
timing <- NULL
names <- NULL
corr <- NULL
for(i in 1:length(all_methods)){
  for(j in 6:length(all_methods[[i]]$timing)){
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

timing0 <- timing[, c("elapsed", "method", "corr", "dim", "no_categories")]

timing2 <- timing0[, .(V1 = sum(elapsed)), by = c("method", "dim", "no_categories", "corr")]

timing3 <- timing2[, .(V1 = mean(V1)), by = c("method", "dim", "no_categories")]

saveRDS(timing3, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim10_nocat4",  sep = "/"))


## only for dim 10
timing2 <- timing0[, .(elapsed = sum(elapsed)), by = c("method", "dim", "no_categories", "corr")]
t <- reshape(timing2, idvar = c("method", "dim", "no_categories"), timevar = "corr", direction = "wide")
t0 <- copy(t)
t0[, no_x_test := 2000]

setcolorder(t0, c("dim", "no_categories", "no_x_test", "method", "elapsed.0", "elapsed.0.1", "elapsed.0.5", "elapsed.0.8", "elapsed.0.9"))

colnames(t0) <- c("dim", "nb categories", "nb variables",  "method", "0", "0.1", "0.3", "0.5", "0.8", "0.9")
print(xtable(t0, digits = c(0, 0, 0, 0, 0, rep(1, 6))), include.rownames = FALSE)




##------------------------------------------------------
## Load all data


dim3_nocat3 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat3", sep = "/"))
dim3_nocat4 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat4", sep = "/"))
dim4_nocat3 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim4_nocat3", sep = "/"))
dim5_nocat6 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim5_nocat6", sep = "/"))
dim7_nocat5 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim7_nocat5", sep = "/"))
dim10_nocat4 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim10_nocat4", sep = "/"))


full_table <- rbind(dim3_nocat3, dim3_nocat4, dim4_nocat3, dim5_nocat6, dim7_nocat5, dim10_nocat4) #

full_table0 <- full_table[, q_005 := NULL]
full_table0[, q_095 := NULL]

t <- reshape(full_table, idvar = c("dim", "no_categories", "method_names"), timevar = "corr", direction = "wide")
setcolorder(t, c("dim", "no_categories", "method_names", "MAE_results.0", "MAE_results.0.1", "MAE_results.0.3", "MAE_results.0.5", "MAE_results.0.8", "MAE_results.0.9"))

library(xtable)
colnames(t) <- c("dim", "nb categories", "method", "0", "0.1", "0.3", "0.5", "0.8", "0.9")
print(xtable(t, digits = c(0, 0, 0, 0, rep(4, 6))), include.rownames = FALSE)


## ------------------------------------------------------
## Timing of methods

timing_dim3_nocat3 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim3_nocat3", sep = "/"))
timing_dim3_nocat4 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim3_nocat4", sep = "/"))
timing_dim4_nocat3 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim4_nocat3", sep = "/"))
timing_dim5_nocat6 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim5_nocat6", sep = "/"))
timing_dim7_nocat5 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim7_nocat5", sep = "/"))
timing_dim10_nocat4 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "timing_dim10_nocat4", sep = "/"))


timing_full_table <- rbind(timing_dim3_nocat3, timing_dim3_nocat4, timing_dim4_nocat3, timing_dim5_nocat6, timing_dim7_nocat5, timing_dim10_nocat4) # timing_dim10_nocat4
timing_full_table[, no_x_test := timing_full_table[['no_categories']]^timing_full_table[['dim']]]

timing_full_table[,no_x_test := c(rep(27, 7), rep(64, 6), rep(81, 6), rep(2000, 6))]
timing_full_table[, V2 := V1/no_x_test]
timing_full_table[, V1 := NULL]

setcolorder(timing_full_table, c("dim", "no_categories", "no_x_test", "method", "V2"))

colnames(timing_full_table) <- c("dim", "nb categories", "nb variables",  "method", "elapsed")

print(xtable(timing_full_table, digits = c(0, 0, 0, 0, 0, 3)), include.rownames = FALSE)
