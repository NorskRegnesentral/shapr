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
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")


##

dim <- 5
no_categories <- 6
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")


##

dim <- 7
no_categories <- 5
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")

##

dim <- 4
no_categories <- 3
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")


##

dim <- 3
no_categories <- 4
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")



