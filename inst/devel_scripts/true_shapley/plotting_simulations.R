library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)
library(stringr)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/true_shapley/calculate_true_shapley_withdatatable.R")

<<<<<<< Updated upstream
tod_date <- '8_02_20'
dim <- 6
=======
tod_date <- '10_02_20'
dim <- 10
>>>>>>> Stashed changes
##

## load data

nm1 <- "7_02_20_results_2_dim_10_corr0_01.rds"
nm2 <- "7_02_20_results_1_dim_10_corr09.rds"
nm3 <- "7_02_20_results_1_dim_10.rds"

all_methods_1 <- readRDS(paste0("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm1))
all_methods_2 <- readRDS(paste0("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm2))
all_methods_3 <- readRDS(paste0("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm3))

all_methods <- list(all_methods_1[[1]],
                    all_methods_1[[2]],
                    all_methods_2[[1]],
                    all_methods_3[[1]])



## Mean average error

MAE_truth <- NULL
MAE_methods <- NULL
MAE_methods_names <- NULL
MAE_parameters <- NULL
MAE_seed <- NULL

for(i in 1:length(all_methods)){
  if(!is.null(all_methods[[i]][['true_linear']])){
    MAE_truth <- c(MAE_truth, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']]))
  }
  for(m in names(all_methods[[1]]$methods)){

    if(m != 'ctree'){
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt_sum))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
    } else{
      MAE_methods <- c(MAE_methods, MAE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt))
      MAE_methods_names <- c(MAE_methods_names, m)
      MAE_parameters <- c(MAE_parameters, all_methods[[i]]$parameters$name)
    }
    MAE_seed <- c(MAE_seed, all_methods[[i]]$seed)
  }
}


results <- data.table(MAE_methods, MAE_methods_names, MAE_parameters, MAE_seed)
results[, correlation := paste0("", str_sub(MAE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)

nm = paste(tod_date, '_results_dim_', dim, '.rds', sep = "")
# saveRDS(results0, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm, sep = ""))

nm = paste(tod_date, '_all_methods_dim_', dim, '.rds', sep = "")
# saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/higher_dimensions/", nm, sep = ""))


## plotting
p0 <- ggplot(data = results0, aes(y = MAE_methods, x = MAE_parameters, col = as.factor(MAE_methods_names ))) +
  geom_point(size = 4, stroke = 3.5) +
  scale_x_discrete(labels = c("corr0" = "0", "corr0.1" = "0.1", "corr0.5" = "0.5", "corr0.8" = "0.8", "corr0.9" = "0.9")) +
  theme_bw(base_size = 34) + xlab("correlation") +
  ylab("Mean average error (MAE)") +
  scale_color_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian") ) +
  ggtitle(paste("Dim:", dim, ", N_shapley = ", all_methods[[1]]$parameters$N_shapley,
                "N_train/N_test = ", all_methods[[1]]$parameters$N_training, sep = " ")) +
  ylim(0, 0.5)


nm = paste(tod_date, '_MAE_dim_', dim, '_same_axis', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/higher_dimensions/", nm, sep = ""), plot = p0, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)

## Average error (box plots)

AE_truth <- NULL
AE_methods <- NULL
AE_methods_names <- NULL
AE_parameters <- NULL
AE_seed <- NULL
corr <- NULL
N_testing <- all_methods[[1]]$parameters$N_testing

for(i in 1:length(all_methods)){
  if(!is.null(all_methods[[i]][['true_linear']])){
    AE_truth <- c(AE_truth, AE(all_methods[[i]][['true_shapley']], all_methods[[i]][['true_linear']]))
  }
  corr <- c(corr, all_methods[[i]]$parameters$corr)
  AE_seed <- c(AE_seed, all_methods[[i]]$seed)
  for(m in names(all_methods[[1]]$methods)){

    if(m != 'ctree'){
      AE_methods <- c(AE_methods, AE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt_sum))
      AE_methods_names <- c(AE_methods_names, rep(m, N_testing))
      AE_parameters <- c(AE_parameters, rep(all_methods[[i]]$parameters$name, N_testing))
    } else{
      AE_methods <- c(AE_methods, AE(all_methods[[i]][['true_shapley']], all_methods[[i]][['methods']][[m]]$dt))
      AE_methods_names <- c(AE_methods_names, rep(m, N_testing))
      AE_parameters <- c(AE_parameters, rep(all_methods[[i]]$parameters$name, N_testing))
    }
    # AE_seed <- c(AE_seed, all_methods[[i]]$seed)
  }
}


results <- data.table(AE_methods, AE_methods_names, AE_parameters)
results[, correlation := paste0("", str_sub(AE_parameters, start = 5, end = -1))]
corr <- results[, lapply(.SD, FUN = as.numeric), .SDcol = "correlation"]
results0 <- cbind(results[, correlation := NULL], corr)

corr.labs <- c("corr0", "corr0.1", "corr0.5", "corr0.8", "corr0.9")
names(corr.labs) <- c("correlation 0", "correlation 0.1", "correlation 0.5", "correlation 0.8", "correlation 0.9")

p1 <- ggplot(data = results0, aes(x = AE_parameters, y = AE_methods, fill = AE_methods_names)) +
  geom_boxplot()

p2 <- ggplot(data = results0, aes(x = AE_parameters, y = AE_methods, fill = AE_methods_names)) +
  geom_boxplot() + facet_wrap(~ AE_parameters, scale = "free_x") + #
  theme_bw(base_size = 34) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.text.x = element_text(size = 34, face = "bold")) +
  ylab("Average error (AE)")+
  scale_fill_discrete(name = "Method", labels = c("Ctree", "Ctree one-hot", "Empirical", "Empirical independence", "Gaussian") ) +
  ggtitle(paste("Dim:", dim, "N_shapley = ", all_methods[[1]]$parameters$N_shapley, ", N_train/N_test = ", all_methods[[1]]$parameters$N_training, sep = " "))


nm = paste(tod_date, '_MAE_dim_', dim, '_boxplot', '.png', sep = "")
ggsave(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/higher_dimensions/", nm, sep = ""), plot = p2, device = NULL, path = NULL,
       scale = 1, width = 45, height = 30, units = "cm",
       dpi = 300, limitsize = TRUE)


