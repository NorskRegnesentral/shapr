#' Main function
#'
#' @description Main function
#' @export
explain <- function(x_train,
                    x_explain,
                    model = NULL,
                    approach,
                    prediction_zero,
                    n_combinations = NULL,
                    group = NULL,
                    n_samples = 1e3,
                    n_batches = 1,
                    seed = 1,
                    keep_samp_for_vS = FALSE,
                    predict_model = NULL,
                    get_model_specs = NULL,
                    ...){ # ... is further arguments passed to specific approaches

  # Sets up input parameters, data and preprocess the data if needed
  internal <- check_setup(x_train = x_train,
                          x_explain = x_explain,
                          model = model,
                          approach = approach,
                          prediction_zero = prediction_zero,
                          n_combinations = n_combinations,
                          group = group,
                          n_samples = n_samples,
                          n_batches = n_batches,
                          seed = seed,
                          keep_samp_for_vS = keep_samp_for_vS,
                          predict_model = predict_model,
                          get_model_specs = get_model_specs,...)

  # Tests that the model predicts as intended
  test_model(internal,model)

  # Check the approach (to be moved to check_setup later), setting up the Shapley (sampling) framework and prepares the
  # conditional expectation computation for the chosen approach
  # TODO: Remove the ellipsis below by extracting those parameters from internal (if not NULL) within setup_approach
  internal <- setup_computation(internal,model)# model only needed for type AICc of approach empirical, otherwise ignored

  # Accross all batches get the data we will predict on, predict on them, and do the MC integration

  # Getting the samples for the conditional distributions with specified approach
  # predicting with these samples
  # performing MC integration on these to estimate the conditional expectation
  vS_list <- compute_vS(internal,model)

  # Compute Shapley values based on conditional expectations
  # Organize function output
  output <- finalize_explanation(vS_list = vS_list,
                                 internal = internal)


  return(output)

}
