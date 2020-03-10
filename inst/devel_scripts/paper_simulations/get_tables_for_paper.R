library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(stringr)
library(xtable)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

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

for(i in 1:length(all_methods)){
  for(m in names(all_methods[[1]]$methods)){
    if(m == 'ctree' | m == 'kernelSHAP'){
      MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]]$methods[[m]]$dt, weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
    } else if(grepl('gaussian', m)){
      tmp <- data.table(cbind(all_methods[[i]]$methods[[m]]$dt_sum, rep(1:dim^no_categories, each = 50)))
      tmp0 <- tmp[, lapply(.SD, mean), by = V2]
      tmp0[, V2 := NULL]

      MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], tmp0, weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
    }
    else {
      MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]]$methods[[m]]$dt_sum, weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
    }

    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]
saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat3",  sep = "/"))


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

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]
saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat4",  sep = "/"))


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

for(i in 1:length(dt_sum_list)){
  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]], weights = all_methods[[i]]$joint_prob_true[[dim + 1]]))
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]
saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim4_nocat3",  sep = "/"))


##------------------------------------------------------
## DIMENSION 5
## NB CATEGORIES 6

tod_date <- "03_03_20"
rand_string <- "FPqLx"
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

for(i in 1:length(dt_sum_list)){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(true_shapley_list[[i]][[1]], dt_sum_list[[i]][[m]][, feat_comb_id := NULL], weights = weights0)) # 0.4759115
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}

results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]
saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim5_nocat6",  sep = "/"))


##------------------------------------------------------
## DIMENSION 7
## NB CATEGORIES 5

tod_date <- "03_03_20"
rand_string <- "mdXqD"
dim <- 7
no_categories <- 5

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.5.rds")
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

for(i in 1:length(dt_sum_list)){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(true_shapley_list[[i]][[1]], dt_sum_list[[i]][[m]][, feat_comb_id := NULL], weights = weights0)) # 0.4759115
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}

results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

##

tod_date <- "04_03_20"
rand_string <- "b5p4T"
dim <- 7
no_categories <- 5

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho_0.8.rds")
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

for(i in 1:length(dt_sum_list)){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(true_shapley_list[[i]][[1]], dt_sum_list[[i]][[m]][, feat_comb_id := NULL], weights = weights0)) # 0.4759115
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results2 <- data.table(MAE_results, method_names, corr)
results2[, dim := dim]
results2[, no_categories := no_categories]

##

tod_date <- "04_03_20"
rand_string <- "uc757"
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

for(i in 1:length(dt_sum_list)){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(true_shapley_list[[i]][[1]], dt_sum_list[[i]][[m]][, feat_comb_id := NULL], weights = weights0)) # 0.4759115
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results3 <- data.table(MAE_results, method_names, corr)
results3[, dim := dim]
results3[, no_categories := no_categories]

results <- rbind(results, results2, results3)

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim7_nocat5",  sep = "/"))


##------------------------------------------------------
## DIMENSION 10
## NB CATEGORIES 4

tod_date <- "06_03_20"
rand_string <- "W34c9"
dim <- 10
no_categories <- 4

folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

## load data
nm <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories, "_rho0.9_part40.rds")
all_methods <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations", folder, nm, sep = "/"))

true_shapley_list <- list()
dt_sum_list <- list()
for(j in 1:(length(all_methods)/8)){
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


MAE_results <- NULL
method_names <- NULL
corr <- NULL

for(i in 1:length(dt_sum_list)){
  weights <- merge(dt_sum_list[[i]][[1]],  all_methods[[i]]$joint_prob_true[, c('joint_prob', 'feat_comb_id')], by = 'feat_comb_id')
  weights <- weights[['joint_prob']]
  weights0 <- weights/(sum(weights))

  for(m in names(dt_sum_list[[1]])){
    MAE_results <- c(MAE_results, MAE(true_shapley_list[[i]][[1]], dt_sum_list[[i]][[m]][, feat_comb_id := NULL], weights = weights0)) # 0.4759115
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i * 8]]$parameters$corr)
  }
}

results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]

saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim10_nocat4",  sep = "/"))


##------------------------------------------------------
## Load all data


dim3_nocat3 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat3", sep = "/"))
dim3_nocat4 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim3_nocat4", sep = "/"))
dim4_nocat3 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim4_nocat3", sep = "/"))
dim5_nocat6 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim5_nocat6", sep = "/"))
dim7_nocat5 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim7_nocat5", sep = "/"))
dim10_nocat4 <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_tables", "results_dim10_nocat4", sep = "/"))


full_table <- rbind(dim3_nocat3, dim3_nocat4, dim4_nocat3, dim5_nocat6, dim7_nocat5, dim10_nocat4)

t <- reshape(full_table, idvar = c("method_names", "dim", "no_categories"), timevar = "corr", direction = "wide")
colnames(t) <- c("method", "dim", "number of categories", "0", "0.1", "0.5", "0.8", "0.9")
xtable(t, digits = c(0, 0, 0, 0, rep(4, 5)))

