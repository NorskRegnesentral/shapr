library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

tod_date <- '6_02_20'

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


parameters_list <- list(Sigma_diag = 1,
                        corr = 0,
                        mu = c(0, 0, 0),
                        beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                        N_shapley = 10000,
                        N_training = 1000,
                        N_testing = 1000,
                        cutoff = c(-200, 0, 1, 200),
                        noise = FALSE,
                        response_mod = response_mod,
                        fit_mod = "regression",
                        methods = c("empirical"), # "gaussian", "ctree"
                        name = 'corr0')

ll <- simulate_data(parameters_list)
head(ll$true_shapley)
head(ll$true_linear)
MAE(ll$true_shapley, ll$true_linear) # 0.00095050 for 10000 obs and 1000 for training and testing / 0.0009436
MAE(ll$true_shapley, ll$methods[['gaussian']]$dt_sum) # 0.01729 # 2.68 minutes
MAE(ll$true_shapley, ll$methods[['empirical']]$dt_sum) # 0.0179 # 1.06 minutes
MAE(ll$true_shapley, ll$methods[['ctree']]$dt) # 0.016423 #
MAE(ll$true_shapley, ll$methods[['ctree_onehot']]$dt_sum) # 0.013 5.46 minutes
MAE(ll$true_shapley, ll$methods[['empirical_ind']]$dt_sum) # 0.01876 # 2.10 minutes


##

parameters_list <- list()

seed <- 1
# corr <- c(seq(0, 0.9, by = 0.2))
corr <- c(0, 0.01, 0.1, 0.2, 0.5, 0.8, 0.9)

k <- 1
for(i in seed){
  for(j in corr){
    parameters_list[[k]] <- list(Sigma_diag = 1,
                                 corr = j,
                                 mu = c(0, 0, 0),
                                 beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1),
                                 N_shapley = 10000000,
                                 noise = TRUE,
                                 response_mod = response_mod,
                                 fit_mod = "regression",
                                 methods = c("empirical_ind", "empirical", "gaussian", "ctree_onehot", "ctree"),
                                 name = paste0('corr', j),
                                 cutoff = c(-200, 0, 1, 200),
                                 N_training = 1000,
                                 N_testing = 1000,
                                 seed = i)
    k <- k + 1
  }
}



tm <- Sys.time()
all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- simulate_data(parameters_list[[i]])
  nm = paste(tod_date, '_results_', i, "_finerscale" , ".rds", sep = "")
  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm, sep = ""))
}

# mydata <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/1_12_19_results.rds")


MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL
MAE_seed <- NULL

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
    MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
  }
}


results <- data.table(MAE_methods, MAE_methods_names, MAE_parameters, MAE_seed)
results[, correlation := paste0("", str_sub(MAE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)

nm = paste(tod_date, '_results_finerscale', '.rds', sep = "")
saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))

nm = paste(tod_date, '_all_methods_finerscale', '.rds', sep = "")
saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/", nm, sep = ""))


p1 <- ggplot(data = results0, aes(y = MAE_methods, x = correlation, col = as.factor(MAE_seed))) +
  geom_point(size = 2, stroke = 1.5, shape = as.factor(MAE_methods_names))  + scale_x_continuous(breaks = seq(0,  0.5, by = 0.05)) + # scale_x_continuous(breaks = seq(0,  1, by = 0.2)
  theme_grey(base_size = 22) + xlab("correlation") + ylab("Mean aveerage error (MAE)") +
  scale_color_discrete(name = "Seed") +
  scale_shape_discrete(name = "Method") + # labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian")
  ggtitle("circle = ctree, triangle = empirical independence") + geom_hline(yintercept = 0.045)




# library(ggplot2)
## Old plot

mydata <- readRDS("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/MAE/1_12_19_results.rds")


p2 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names ))) +
  geom_point(size = 4, stroke = 1.5) +
  scale_x_discrete(labels = c("corr0" = "0", "corr01" = "0.01", "corr1" = "0.1", "corr2" = "0.2", "corr5" = "0.5", "corr8" = "0.8", "corr9" = "0.9")) +
  theme_bw(base_size = 22) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian") ) +
  ggtitle("")

# tod_date = '6_02_20'

nm = paste(tod_date, '_MAE', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/categorical_shapley/", nm, sep = ""), plot = p1, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)
tm2 <- Sys.time()
print(tm2 - tm)

