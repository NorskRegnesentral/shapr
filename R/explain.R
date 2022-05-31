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

  # Overview of what should happen in this function (and in what order)

  # Basic check of the necessary input being available
  # LATER: Make list with all parameters (n_combinations, seed, n_batches, approach, etc)
  # LATER: Make a list with all internal objects (X, S, S_batch, etc)
  # extract feature info from model if available
  # check compatability of x_train, x_explain and model
  # Merge all non-model stuff from shapr() into explain_setup

  # Setup
  #explainer <- init_explainer(environment(),...)

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
