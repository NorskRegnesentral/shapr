library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

test <- FALSE
special <- FALSE
tod_date <- format(Sys.Date(), "%d_%m_%y")


# dim <- 7
# no_categories <- 5
# cutoff = c(-200, -0.5, -0.25, 0, 1, 200)
# seed <- 90#99
# corr <- 0.8#0.9#c(0,0.1,0.5,0.8, 0.9) # 0.8 leads to the error: Error in model@fit(data, ...) : error code 1 from Lapack routine 'dgesdd'
# methods <- c("ctree", "kernelSHAP")
#
# source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
# source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")

#source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
#seed_vec <- 1:10 + 90
# for (seed in seed_vec){
#   tryCatch( expr = {
#     source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")
#   }, error = function(e){
#     message("Error for seed",seed)
#     print(seed)
#   }
#   )
# }



##

# dim <- 3
# no_categories <- 4
# cutoff <- c(-200, -0.5, 0, 1, 200)
# corr <- c(0,0.1,0.5,0.8, 0.9)
# methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")
#
# source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
# source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")
#
#
# ##
# #
# dim <- 4
# no_categories <- 3
# cutoff = c(-200, 0, 1, 200)
# corr <- c(0,0.1,0.5,0.8, 0.9)
#
# methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")
#
# source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
# source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")
#
# ##
#
# dim <- 5
# no_categories <- 6
# cutoff = c(-200, -0.5, -0.25, 0, 0.9, 1, 200)
# corr <- c(0,0.1,0.5,0.8, 0.9)
# methods <- c("ctree", "kernelSHAP")
#
# source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
# source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")





#
# ### Not yet run#
#
special <- TRUE
dim <- 10
no_categories <- 4
cutoff = c(-200, -0.5, 0, 1, 200)
corr <- c(0, 0.1, 0.5, 0.8, 0.9)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")
#
# ##
