

# Testing simulations by copying and pasting from Lars original scripts

distribution = "mvn" #  "burr", "gh" # Currently only mvn is supported
pred_function = "gam"  # "rf
n_train = 1000
experiments_arg = if (pred_function == "gam") 1:14 else 10:14

approaches <- c("reg_sep-lm","reg_sep-gam", "arf", "gaussian", "ctree")

#rhos = 0.5
rhos = c(0, seq(0.1, 0.9, 0.2))
#burr_p_vec = 1.5
burr_p_vec = seq(0.5, 3, 0.5)

if (distribution == "mvn") {
  distribution_params = rhos
} else if (distribution == "burr") {
  distribution_params = burr_p_vec
} else {
  stop(sprintf("Do not recognise the provided distribution '%s'.", distribution))
}

future::plan("multisession", workers = 10)

progressr::handlers(global = TRUE)



### Libraries etc

library(shapr)
library(stringr)
library(ranger)
library(xgboost)
library(mgcv)
library(ggplot2)
library(tidyr)
library(scales)
library(caret)
library(progress)

library(tidyverse)

library("grid")
library("patchwork")


source("inst/simtest/source_Lars_funcs.R") # TODO: Simplify to only the function I need
source("inst/simtest/source_Martin_funcs.R") # TODO: Simplify to only the function I need




# Parameters --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Number of features
#p = 10
#p = 4
#p = 6
p = 8 # READY FOR FINAL EXPERIMENTS

# Check for valid value for p.
if (p<1 || 10<p) {
  stop(sprintf("The number of features has to be between 1 and 10, inclusive."))
}

# Define the true model coefficients.
# Beta are the coefficients of the first order terms, and first value is the intercept.
beta_vec = c(-0.6, 0.2, -0.8, 1.6, 0.3, -0.8, 0.5, 0.7, 0.6, -0.3, 1.5)[1:(p+1)]
beta_vec =  rep(1, p+1)
beta_vec =  c(0, 0.2, -0.8, 1.6, 0.3, -0.8, 0.5, 0.7, 0.6, -0.3, 1.5)[1:(p+1)] # OLD
beta_vec =  c(1, 0.2, -0.8, 1.0, 0.5, -0.8, 0.6, -0.7, -0.6, -0.5, 0.9)[1:(p+1)]
beta_vec

# gamma are the coefficients of the second order terms (interactions).
gamma_vec = rep(1, p/2)
gamma_vec = c(0.4, -0.6, -2.2, 1.1, 0.0)[1:(p/2)] # OLD
gamma_vec = c(0.8, -1, -2, 1.5, 1)[1:(p/2)]
gamma_vec
length(gamma_vec)

## Define the parameters of the distributions, but only use the ones that
# correspond to the associated distribution.
# Define the mean of the MVN feature to be zero
mean_vec = rep(0, p)

# Define the covariance matrix to be the equicorrelation matrix with off-diagonal entries 'rho'.
# rhos = c(0.0, 0.1, 0.3, 0.5, 0.7, 0.9)
# rho = 0
rho_equicorrelation = TRUE
rho_equicorrelation = FALSE

# Define the parameters to the multivariate Burr distribution, except the scalar kappa (p)
burr_r_vec = c(4, 3, 5, 2, 5, 3, 5, 1, 6, 5, 1, 1)[1:p]
burr_b_vec = c(5, 4, 6, 5, 3, 6, 5, 5, 4, 3, 2, 4)[1:p]

# Define the standard deviation of the noise added to the generated response.
noise_sigma = 1
# Kanskje legge inn mer dynamisk slik at Var(eps) = 0.05Var(Y), slik som i SHAFF.

# Define the number of training and test observations
# n_train = 1000 FERDIG
# n_train = 100  FERDIG
# n_train = 5000
n_test = 250

# Define seed values used when generating the training and test data for reproducibility
seed_train = 2010#2010 # TODO: Fjern sånn at det bare står 2010 og 2011
seed_test = 2011#2011

# The number of Monte Carlo samples to use when we compute the true Shapley values
num_samples_monte_carlo_true_Shapley_values = 2500L#1000L # Increase to 5000L later

# The number of Monte Carlo samples to use when we estimate the Shapley values using the sampling approaches
num_samples_monte_carlo_sample_Shapley_values = 250L


# Generate Formulas for All Simulation Studies ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set the names of the features to be Var1, Var2, ..., and so on.
# Note that this might be necessary when I create the custom models
# in Shapr, as I need to do some extra tweaking to estimate the Shapley values.

feature_names = paste("Var", seq(p), sep = "")

# Set the name of response variable
response_name = "y"

# Define which non-linear function we are to use on some of the features
non_linear_function = "cos"

# Define the predictive function type to use for the experiments with non-linear functions and interactions
predictive_function_type_non_linear_or_interactions = "gam" # "rf" or "ppr"
if (!is.null(pred_function)) {
  predictive_function_type_non_linear_or_interactions = pred_function
}

# Create formulas for all the simulation studies
all_experiments = generate_all_formulas(
  feature_names = feature_names,
  response_name = response_name,
  non_linear_function = non_linear_function,
  predictive_function_type_non_linear_or_interactions = predictive_function_type_non_linear_or_interactions)

all_experiments = all_experiments[experiments_arg]

names_all_experiments = names(all_experiments)
print(names_all_experiments)



# Iterate over Dist Param ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Iterate over the provided distributions parameters.
# I.e., the correlations if the distribution is the mvn,
# or the scale parameters p in the burr distribution.

res <- list()


#distribution_param_idx = 4
distribution_params <- distribution_params
all_experiments <- all_experiments

distribution_param_idx <- 4
for (distribution_param_idx in seq(1, length(distribution_params))) {

  # Get the current distribution parameter
  distribution_param = distribution_params[distribution_param_idx]

  # Small printout to the user.
  cat(sprintf("Distribution '%s', Parameter '%g' (%d/%d).\n",
              distribution, distribution_param, distribution_param_idx, length(distribution_params)))

  # Get a name_string for the current distribution we are looking at
  if (distribution == "mvn") {
    current_distribution_name = paste(distribution, "rho", distribution_param, "equi", rho_equicorrelation, sep = "_")
  } else if (distribution == "burr") {
    current_distribution_name = paste(distribution, "p", distribution_param, sep = "_")
  } else {
    stop(sprintf("Do not recognise the provided distribution '%s'.", distribution))
  }


  ## Iterate over Experiments ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  # Iterate over all the experiments
  experiment_idx = 12
  for (experiment_idx in seq(length(all_experiments))) {
    # Get the current experiment
    current_formula = all_experiments[[experiment_idx]]

    # Small printout to the user.
    cat(sprintf("Distribution '%s'. Parameter '%g' (%d/%d). Experiment '%s' (%d/%d).\n",
                distribution,
                distribution_param, distribution_param_idx, length(distribution_params),
                paste(current_formula$name, current_formula$predictive_function_type, sep = "_"),
                experiment_idx, length(all_experiments)))


    # Extract what type of predictive function we are going to use
    current_predictive_function_name = current_formula$predictive_function_type

    # Create the save name which are going to use when we save the results
    ### ADD BETA AND GAMMA HERE AS IN 'parameter_setup_list_save_name' IF I EXPLORE DIFFERENT VALUES FOR THE BETA AND GAMMA COEFFICIENTS
    current_save_name = paste("EXPT", current_formula$name, "FUNC", current_predictive_function_name, "DIST", current_distribution_name, "DIM", p, "Ntrain", n_train, "Ntest", n_test, sep = "_")
    current_save_name


    # Small printout to the user.
    cat(sprintf("Distribution '%s'. Parameter '%g' (%d/%d). Experiment '%s' (%d/%d). Creating data, predictive model, and explainer object.\n",
                distribution,
                distribution_param, distribution_param_idx, length(distribution_params),
                paste(current_formula$name, current_formula$predictive_function_type, sep = "_"),
                experiment_idx, length(all_experiments)))

    # Extract the current gamma coefficients, which are used in front of the interaction terms
    current_gamma_vec = gamma_vec[seq(0, current_formula$n_linear_interactions)]

    # If the formula contains non-linear interaction terms, then we update the gamma coefficients.
    # Repeat by three as the current used non-linear interaction function creates three terms.
    # Not that this MUST be updated if we change the non-linear interaction function.
    if (current_formula$n_non_linear_interactions > 0) {
      current_gamma_vec = rep(gamma_vec[seq(0, current_formula$n_non_linear_interactions)], each = 3)
    }

    # Create the training and test data for the current experiment
    current_data = create_train_and_test_data(
      n_train = n_train,
      n_test = n_test,
      p = p,
      formula = current_formula$formula_features_as_formula,
      beta_vec = beta_vec,
      gamma_vec = current_gamma_vec,
      distribution = distribution,
      noise_sigma = noise_sigma,
      mean_vec = mean_vec,
      rho = distribution_param,
      rho_equicorrelation = rho_equicorrelation,
      burr_p = distribution_param,
      burr_r_vec = burr_r_vec,
      burr_b_vec = burr_b_vec,
      seed_train = seed_train,
      seed_test = seed_test)
    #pairs(current_data$training_x)

    current_model <- fit_model(current_predictive_function_name,current_formula, current_data, tune = FALSE)

    # Compute training and test MSE of the predictive model.
    # It should be close to noise_sigma^2, when properly fitted as the model
    # is of the same structure as the underlying data generating process
    current_training_MSE = mean((as.matrix(predict(current_model, current_data$training_df)) - current_data$training_y)^2)
    current_test_MSE = mean((as.matrix(predict(current_model, current_data$test_df)) - current_data$test_y)^2)
    print(c(current_training_MSE, current_test_MSE))

    # Compute the Phi_0 value, i.e., the expected prediction without any features
    current_phi_0 = mean(current_data$training_y)


    # List to save
    current_setup = list(
      current_data = current_data,
      current_model = current_model,
      current_training_MSE = current_training_MSE,
      current_test_MSE = current_test_MSE,
      current_phi_0 = current_phi_0
    )

    #### Compute true vS and Shapley values
    model <- current_setup$current_model
    x_explain <- current_setup$current_data$test_x
    x_train <- current_setup$current_data$training_x
    phi0 <- current_setup$current_phi_0
    predict_model <- NULL
    if(current_predictive_function_name=="gam") predict_model = mgcv::predict.bam

    expl_true2 <- shapr::explain(
      model = model,
      x_explain = x_explain,
      x_train = x_train,
      predict_model = predict_model,
      approach = "gaussian",
      phi0 = phi0,
      iterative = FALSE,
      seed = 123,
      n_MC_samples  = num_samples_monte_carlo_true_Shapley_values
    )

    res_list <- list()
    res_list$data_dist_name <- distribution
    res_list$data_dist_param <- distribution_param
    res_list$model_name <- current_formula$name
    res_list$true_explainer <- expl_true

    ### Use different approaches
    expl_list <- list()
    for(i in seq_along(approaches)){
      approach <- approaches[i]

      if(approach %in% c("arf","independence","ctree","gaussian","copula","VAEAC")){
        expl <- shapr::explain(
          model = model,
          x_explain = x_explain,
          x_train = x_train,
          predict_model = predict_model,
          approach = approach,
          phi0 = phi0,
          iterative = FALSE,
          seed = 123,
          n_MC_samples = num_samples_monte_carlo_sample_Shapley_values
        )
        expl_list[[approach]] <- expl
      } else { # if approach is regression
        approach0 <- strsplit(approach,"-")[[1]]
        approach1 <- ifelse(approach0[1]=="reg_sep","regression_separate","regression_surrogate")
        if(approach0[2]=="lm"){
          regression.model <- parsnip::linear_reg()
          regression.recipe_func <- NULL
        } else if(approach0[2] == "gam"){
          regression.model <- parsnip::linear_reg()
          regression.recipe_func = function(regression_recipe) { # Preprocessing into natural splines format
            return(recipes::step_ns(regression_recipe, recipes::all_numeric_predictors(), deg_free = 2))
          }
        } else {
          stop("Unrecognized approach")
        }

        expl <- shapr::explain(
          model = model,
          x_explain = x_explain,
          x_train = x_train,
          predict_model = predict_model,
          approach = approach1,
          regression.model = regression.model,
          regression.recipe_func = regression.recipe_func,
          phi0 = phi0,
          iterative = FALSE,
          seed = 123
        )
        expl_list[[approach]] <- expl

        gc()

      }

      res_list$est_explainer <- expl_list
      # Saving results (overwriting with more approaches for every iteration in the approach loop)
      saveRDS(res_list, file = paste0("inst/simtest/results/",current_save_name,".rds"))


    }

    rm(res_list)
    gc()

  }

}

