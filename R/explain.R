#' @export
explain_final <- function(x_train,
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
                          ...){ # ... is further arguments passed to setup_approach

  # NEW Overview

  # check_setup(ALL INPUT) # This is the main setup function called by explain which includes the below subfunctions
  # The subfunctions should go into files with the name setup-set_internal, setup-check_model and so on, which each
  # includes all of the necessary functions used by those functions
    # set_internal() # Creating the internal based on all input except model
    # check_model(model) # Extracting the model specification
    # check_data(internal) # Process the data (put the data scaling in here eventually)
  # test_model(internal,model) # Test whether the model predicts without error
  # setup_computation
    # check_approach(internal) #
    # setup_shapley(internal) #
    # setup_approach(internal) #
  # compute_vS(internal,model) #


  # setup_explain(ALL INPUT) # This is the main setup function called by explain which includes the below subfunctions
  # The subfunctions should go into files with the name setup-set_internal, setup-check_model and so on, which each
  # includes all of the necessary functions used by those functions
    # set_internal() # Creating the internal based on all input except model
    # check_model(model) # Extracting the model specification
    # check_data(internal) # Process the data (put the data scaling in here eventually)
    # test_model(internal,model) # Test whether the model predicts without error
    # check_approach(internal) #
    # setup_shapley(internal) #
    # setup_approach(internal) #
  # compute_vS(internal,model) #


  # TODO: create a set_internal function which incorporates the get_parameters and get_data functions below
  # This is where we store everything
  internal <- list()

  # Structure the input
  internal$parameters <- get_parameters(approach = approach,
                                        prediction_zero = prediction_zero,
                                        n_combinations = n_combinations,
                                        group = group,
                                        n_samples = n_samples,
                                        n_batches = n_batches,
                                        seed = seed,
                                        keep_samp_for_vS = keep_samp_for_vS,...)

  internal$data <- get_data(x_train,x_explain)

  ##### DATA CHECKING AND PROCESSING ###########

  # TODO: Separate model here and create a check_model function
  internal <- process_all_data(internal,model)

  # Checking that the prediction function works (duplicate in Python)
  test_model(head(internal$data$x_train, 2),model)

  # Checking the format of approach
  check_approach(internal)

  # setup the Shapley framework
  internal <- shapley_setup(internal)

  # Setup for approach
  internal <- setup_approach(internal, model = model, ...) # model only needed for type AICc of approach empirical

  # Accross all batches get the data we will predict on, predict on them, and do the MC integration
  vS_list <- future.apply::future_lapply(X = internal$objects$S_batch,
                                         FUN = run_batch,
                                         internal = internal,
                                         model = model,
                                         future.seed = internal$parameters$seed)

  output <- finalize_explanation(vS_list = vS_list,
                                 internal = internal)


  return(output)

}
