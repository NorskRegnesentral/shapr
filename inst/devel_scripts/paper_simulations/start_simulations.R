library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

test <- TRUE
tod_date <- format(Sys.Date(), "%d_%m_%y")

##

dim <- 10
no_categories <- 4
cutoff = c(-200, -0.5, 0, 1, 200)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")

##

dim <- 7
no_categories <- 5
cutoff = c(-200, -0.5, -0.25, 0, 1, 200)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")


##

dim <- 5
no_categories <- 6
cutoff = c(-200, -0.5, -0.25, 0, 0.9, 1, 200)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")

##

dim <- 3
no_categories <- 4
cutoff = c(-200, -0.5, 0, 1, 200)
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")


##

dim <- 4
no_categories <- 3
cutoff = c(-200, 0, 1, 200)
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")



##

dim <- 3
no_categories <- 3
cutoff <- c(-200, 0, 1, 200)
methods <- c("gaussian", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")



