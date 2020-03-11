
library(shapr)
library(data.table)
library(MASS)
library(ggplot2)

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_mixed_data.R")



## Start simulation study
tod_date0 <- format(Sys.Date(), "%d_%m_%y")

dim <- 4
no_categories <- 3

clock_seed_0 <- round(as.numeric(Sys.time()) * 1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1,5)
folder <- paste0(tod_date0, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/", folder, sep = ""))

##

parameters_list <- list()

set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
k <- 1
for(j in corr){
  parameters_list[[k]] <- list(methods = c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP"),
                          No_sample_gaussian = c(100, 1000),
                          No_cont_var = 2,
                          No_cat_var = 2,
                          No_levels = 3,
                          Sigma_diag = 1,
                          corr = j,
                          No_train_obs = 1000,
                          No_test_obs = 2000,
                          cat_cutoff = c(-200, 0, 1, 200),
                          noise = FALSE,
                          name = 'testing',
                          seed = 123)
  k <- k + 1
}

all_methods <- list()
for(i in 1:length(parameters_list)){
  all_methods[[i]] <- compute_shapley_mixed_data(parameters_list[[i]])

  nm = paste(tod_date, '_rho_', parameters_list[[i]]$corr, ".rds", sep = "")

  saveRDS(all_methods, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data", folder, nm, sep = "/"))
}

## End simulation study

## save/plot results
dt_sum_list <- list()

for(j in 1:length(all_methods)){
  K <- which(grepl("gaussian", names(all_methods[[j]]$methods)))[1]
  top <-all_methods[[j]]$parameters$No_test_obs

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
    MAE_results <- c(MAE_results, MAE(all_methods[[i]][['true_shapley']], dt_sum_list[[i]][[m]]))
    method_names <- c(method_names, m)
    corr <- c(corr, all_methods[[i]]$parameters$corr)
  }
}


results <- data.table(MAE_results, method_names, corr)
results[, dim := dim]
results[, no_categories := no_categories]
saveRDS(results, file = paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/", folder, "results_dim4_nocat3",  sep = "/"))


results <- readRDS(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/cont_and_cat_data/09_03_20_eCMjG_dim4_nbcat3", "results_dim4_nocat3", sep = "/"))

p1 <- ggplot(data = results, aes(y = MAE_results, x = as.character(corr), col = as.factor(method_names))) +
  geom_point(size = 4, stroke = 1.5) +
  theme_bw(base_size = 22) +
  xlab("correlation") +
  ylab("Mean absolute error (MAE)") +
  scale_color_discrete(name = "Method" ) +
  ggtitle("")








#### OLD FUNCTIONS ####
pmvnorm_no_checking <- function(lower, upper,mean, sigma, algorithm){

  corr <- cov2cor(sigma)

  ret = mvtnorm:::mvt(lower = lower,upper = upper,df = 0,corr = corr, delta = mean,algorithm = algorithm)

  return(ret)
}

# Here is the computation function, taking the preparation values as input
compute_dens_x_given_S_is_C_func <- function(x,ret_list) {

  mean_above <- as.vector(ret_list$mean_above_mult%*%x+ret_list$mean_above_add)


  above <- mvtnorm::pmvnorm(lower = ret_list$C_lower,upper = ret_list$C_upper,
                            mean = mean_above,
                            sigma = ret_list$sigma_above,
                            algorithm = algorithm)[1]

  left <- dnorm(x,mean=ret_list$left_mean,sd = ret_list$left_sd)

  dens <- left*above/ret_list$below

  return(dens)

}


# Vectorizing the two functions and checking that they give the same result
vec_dens_x_given_S_is_C_func <- Vectorize(dens_x_given_S_is_C_func,vectorize.args="x")

vec_compute_dens_x_given_S_is_C_func = Vectorize(compute_dens_x_given_S_is_C_func,vectorize.args = "x")

vec_compute_dens_x_given_S_is_C_func_2 = Vectorize(compute_dens_x_given_S_is_C_func)

# aa = outer(x_int_grid,prep_list_all_x_test_C, FUN = vec_compute_dens_x_given_S_is_C_func_2)

vec_compute_dens_x_given_S_is_C_func_rev <- function(ret_list,x){
  vec_compute_dens_x_given_S_is_C_func(x,ret_list)
}
