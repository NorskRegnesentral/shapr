
#### Script for a simulation study with mixed data (i.e. categorical + continuous) ####

# Assuming a linear model

library(shapr)
library(data.table)


beta <- c()

parameters_list <- list(Sigma_diag = 1,
                        corr = 0,
                        mu = c(0, 0, 0, 0, 0),
                        beta = c(1, -1, 0, 1, 1, 1, 0.5, 0.5, 1, -1,rep(1,3),rep(1,3)),
                        N_shapley = 10000,
                        N_training = 20,
                        N_testing = 20,
                        cutoff = c(-200, 0, 1, 200),
                        no_categories = 3,
                        noise = FALSE,
                        response_mod = response_mod,
                        fit_mod = "regression",
                        methods = c("empirical"), # "gaussian", "ctree"
                        name = 'corr0')

### The resonse model
response_mod <- function(mod_matrix_full,beta,epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}
