library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

test <- FALSE
special_dim7 <- FALSE
special_dim10 <- FALSE
special_dim10_2 <- FALSE

tod_date <- format(Sys.Date(), "%d_%m_%y")


##
special_dim7 <- FALSE
special_dim10 <- FALSE
dim <- 3
no_categories <- 3
cutoff <- c(-200, 0, 1, 200)
corr <- c(0)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")

clock_seed_0 <- round(as.numeric(Sys.time()) * 1000)
clock_seed <- signif(clock_seed_0) - clock_seed_0
set.seed(clock_seed)
rand_string <- stringi::stri_rand_strings(1, 5)
folder <- paste0(tod_date, "_", rand_string, "_dim", dim, "_nbcat", no_categories)

#dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/paper_simulations/", folder, sep = ""))
# dir.create(paste("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/results/figures/paper_simulations/", folder, sep = ""))

##
response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}


set.seed(3); beta <- round(rnorm(dim * no_categories + 1), 1)
k <- 1
j = 0.1
for (aa in 1:100){
  seed = aa
  parameters_list <- list(Sigma_diag = 1,
                          corr = j,
                          mu = rep(0, dim),
                          beta = beta,
                          N_shapley = 1e+07,
                          noise = TRUE,
                          response_mod = response_mod,
                          fit_mod = "regression",
                          methods = methods,
                          name = paste0('corr', j),
                          cutoff = cutoff,
                          Sample_test = TRUE, # Can be FALSE as well, then No_test_sample not used.
                          No_test_sample = ifelse(exists("No_test_sample"), No_test_sample, 1000),
                          No_train_obs = 100,
                          x_test_dt <- NULL,
                          N_sample_gaussian = c(100, 1000),
                          seed = ifelse(exists("seed"), seed, 1),
                          no_categories = no_categories)



  res = simulate_data(parameters_list)

  colMeans(res$methods$ctree$dt-res$methods$kernelSHAP$dt)

  for(i in 2:7){
    print(length(unique(all_trees[[2]]$tree@where)))
  }

}

seed = 51
parameters_list <- list(Sigma_diag = 1,
                        corr = j,
                        mu = rep(0, dim),
                        beta = beta,
                        N_shapley = 1e+07,
                        noise = TRUE,
                        response_mod = response_mod,
                        fit_mod = "regression",
                        methods = methods,
                        name = paste0('corr', j),
                        cutoff = cutoff,
                        Sample_test = TRUE, # Can be FALSE as well, then No_test_sample not used.
                        No_test_sample = ifelse(exists("No_test_sample"), No_test_sample, 1000),
                        No_train_obs = 100,
                        x_test_dt <- NULL,
                        N_sample_gaussian = c(100, 1000),
                        seed = ifelse(exists("seed"), seed, 1),
                        no_categories = no_categories)



res = simulate_data(parameters_list)

colMeans(res$methods$ctree$dt-res$methods$kernelSHAP$dt)

for(i in 2:7){
  print(length(unique(all_trees[[2]]$tree@where)))
}


dt_keep

dt_keep_ctree

colnam = paste0("feat_",1:3,"_")

dt_keep_2 <- dt_keep[, sum(w), by = c("id_combination",colnam, "id")]
setnames(dt_keep_2, "V1", "w")

setkey(dt_keep_2)
dt_keep_2

dt_keep_ctree_2 <- dt_keep_ctree[, sum(w), by = c("id_combination",colnam, "id")]
setnames(dt_keep_ctree_2, "V1", "w")

setkey(dt_keep_ctree_2)
dt_keep_ctree_2

all_trees[[6]]$tree

head(dt_keep_ctree_2[id_combination!=1 & id ==1,],100)

head(dt_keep_2[id_combination!=1 & id ==1,],100)
