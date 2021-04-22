library(shapr)
library(data.table)
library(MASS)
library(lqmm) ## to check if Sigma is positive definite
library(rapportools) # for testing booleans
library(ggplot2)

test <- FALSE
special_dim7 <- FALSE
special_dim10 <- FALSE

tod_date <- format(Sys.Date(), "%d_%m_%y")

##
special_dim7 <- FALSE
special_dim10 <- FALSE

dim <- 3
no_categories <- 3
cutoff <- c(-200, 0, 1, 200)
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/paper_simulations/3-calculate_true_shapley_withdatatable.R")
source("inst/paper_simulations/2-source_paper_simulations.R")


##
special_dim7 <- FALSE
special_dim10 <- FALSE

dim <- 3
no_categories <- 4
cutoff <- c(-200, -0.5, 0, 1, 200)
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")


##
special_dim7 <- FALSE
special_dim10 <- FALSE

dim <- 4
no_categories <- 3
cutoff = c(-200, 0, 1, 200)
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
methods <- c("empirical", "gaussian", "ctree_onehot", "ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")

##
special_dim7 <- TRUE
special_dim10 <- FALSE

dim <- 5
no_categories <- 6
cutoff = c(-200, -0.5, -0.25, 0, 0.9, 1, 200)
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")

##
special_dim7 <- TRUE
special_dim10 <- FALSE

dim <- 7
no_categories <- 5
cutoff = c(-200, -0.5, -0.25, 0, 1, 200)
corr <- c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")


##
special_dim7 <- FALSE
special_dim10 <- TRUE

dim <- 10
no_categories <- 4
cutoff = c(-200, -0.5, 0, 1, 200)
corr <-c(0, 0.1, 0.3, 0.5, 0.8, 0.9)
methods <- c("ctree", "kernelSHAP")

source("inst/devel_scripts/paper_simulations/calculate_true_shapley_withdatatable.R")
source("inst/devel_scripts/paper_simulations/source_paper_simulations.R")

