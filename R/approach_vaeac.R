# SHAPR functions -------------------------------------------------------------------------------------------------
#' @rdname setup_approach
#'
#' @param vaeac.depth Positive integer (default is `3`). The number of hidden layers
#' in the neural networks of the masked encoder, full encoder, and decoder.
#' @param vaeac.width Positive integer (default is `32`). The number of neurons in each
#' hidden layer in the neural networks of the masked encoder, full encoder, and decoder.
#' @param vaeac.latent_dim Positive integer (default is `8`). The number of dimensions in the latent space.
#' @param vaeac.lr Positive numeric (default is `0.001`). The learning rate used in the [torch::optim_adam()] optimizer.
#' @param vaeac.activation_function An [torch::nn_module()] representing an activation function such as, e.g.,
#' [torch::nn_relu()] (default), [torch::nn_leaky_relu()], [torch::nn_selu()], or [torch::nn_sigmoid()].
#' @param vaeac.n_vaeacs_initialize Positive integer (default is `4`). The number of different vaeac models to initiate
#'  in the start. Pick the best performing one after `vaeac.extra_parameters$epochs_initiation_phase`
#'  epochs (default is `2`) and continue training that one.
#' @param vaeac.epochs Positive integer (default is `100`). The number of epochs to train the final vaeac model.
#' This includes `vaeac.extra_parameters$epochs_initiation_phase`, where the default is `2`.
#' @param vaeac.extra_parameters Named list with extra parameters to the `vaeac` approach. See
#'  [shapr::vaeac_get_extra_para_default()] for description of possible additional parameters and their default values.
#'
#' @section The vaeac approach:
#' The `vaeac` model consists of three neural network (a full encoder, a masked encoder, and a decoder) based
#' on the provided `vaeac.depth` and `vaeac.width`. The encoders map the full and masked input
#' representations to latent representations, respectively, where the dimension is given by `vaeac.latent_dim`.
#' The latent representations are sent to the decoder to go back to the real feature space and
#' provide a samplable probabilistic representation, from which the Monte Carlo samples are generated.
#' We use the `vaeac` method at the epoch with the lowest validation error (IWAE) by default, but
#' other possibilities are available but setting the `vaeac.which_vaeac_model` parameter. See
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)} for more details.
#'
#' @inheritParams default_doc_explain
#'
#' @export
#' @author Lars Henry Berge Olsen
setup_approach.vaeac <- function(internal, # add default values for vaeac here.
                                 vaeac.depth = 3,
                                 vaeac.width = 32,
                                 vaeac.latent_dim = 8,
                                 vaeac.activation_function = torch::nn_relu,
                                 vaeac.lr = 0.001,
                                 vaeac.n_vaeacs_initialize = 4,
                                 vaeac.epochs = 100,
                                 vaeac.extra_parameters = list(),
                                 ...) {
  # Check that torch is installed
  if (!requireNamespace("torch", quietly = TRUE)) stop("`torch` is not installed. Please run install.packages('torch')")
  if (!torch::torch_is_installed()) torch::install_torch()

  # Extract the objects we will use later
  S <- internal$objects$S
  X <- internal$objects$X
  parameters <- internal$parameters

  # Small printout to user
  if (parameters$verbose == 2) message("Starting 'setup_approach.vaeac'.")

  # Check if we are doing a combination of approaches
  combined_approaches <- length(internal$parameters$approach) > 1

  # Ensure that `parameters$vaeac.extra_parameters` is a named list
  if (is.null(parameters$vaeac.extra_parameters)) parameters$vaeac.extra_parameters <- list()
  if (!is.list(parameters$vaeac.extra_parameters)) stop("`vaeac.extra_parameters` must be a list.")
  if (length(parameters$vaeac.extra_parameters) > 0) vaeac_check_extra_named_list(parameters$vaeac.extra_parameters)

  # Ensure that all vaeac parameters are in their right location
  parameters <- vaeac_update_para_locations(parameters = parameters)

  # Extract the default values defined for the vaeac parameters in this function
  vaeac_main_para_names <- formalArgs(setup_approach.vaeac)
  vaeac_main_para_names <- vaeac_main_para_names[!vaeac_main_para_names %in% c("internal", "...")]
  vaeac_main_para <- mget(vaeac_main_para_names)

  # vaeac_main_para = list(vaeac.depth = 3,
  #                        vaeac.width = 32,
  #                        vaeac.latent_dim = 8,
  #                        vaeac.activation_function = torch::nn_relu,
  #                        vaeac.lr = 0.001,
  #                        vaeac.n_vaeacs_initialize = 10,
  #                        vaeac.epochs = 100,
  #                        vaeac.extra_parameters = list())

  # Add the default extra parameter values for the non-user specified extra parameters
  parameters$vaeac.extra_parameters <- utils::modifyList(vaeac_get_extra_para_default(),
    parameters$vaeac.extra_parameters,
    keep.null = TRUE
  )

  # Add the default main parameter values for the non-user specified main parameters
  parameters <- utils::modifyList(vaeac_main_para, parameters, keep.null = TRUE)

  # Reorder them such that the vaeac parameters are at the end of the parameters list
  parameters <- c(parameters[(length(vaeac_main_para) + 1):length(parameters)], parameters[seq_along(vaeac_main_para)])

  # Check if vaeac is to be applied on a subset of coalitions.
  if (!parameters$exact || parameters$is_groupwise || combined_approaches) {
    # We have either:
    # 1) sampled `n_combinations` different subsets of coalitions (i.e., not exact),
    # 2) using the coalitions which respects the groups in group Shapley values, and/or
    # 3) using a combination of approaches where vaeac is only used on a subset of the coalitions.
    # Here, objects$S contains the coalitions while objects$X contains the information about the approach.

    # Extract the the coalitions / masks which are estimated using vaeac as a matrix
    parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions <-
      S[X[approach == "vaeac"]$id_combination, , drop = FALSE]

    # Extract the weights for the corresponding coalitions / masks.
    parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions_prob <-
      X$shapley_weight[X[approach == "vaeac"]$id_combination]

    # Normalize the weights/probabilities such that they sum to one.
    parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions_prob <-
      parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions_prob /
        sum(parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions_prob)
  } else {
    # We are going to use the MCAR(`masking_ratio`) masking scheme. Set the variables to `NULL` as we do not need them.
    parameters$vaeac.mask_gen_coalitions <- parameters$vaeac.mask_gen_coalitions_prob <- NULL
  }

  # Check if user provided a pre-trained vaeac model, otherwise, we train one from scratch.
  if (is.null(parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model)) {
    # We train a vaeac model with the parameters in `parameters`, as user did not provide pre-trained vaeac model
    if (parameters$verbose == 2) message("Training a `vaeac` model with the provided parameters from scratch.")

    # Specify that a vaeac model was NOT provided
    parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model_provided <- FALSE

    # Extract all veaac parameters and remove the "vaeac." prefix as the names need to mach the parameters in "do.call"
    vaeac_all_parameters <- c(
      parameters$vaeac.extra_parameters,
      parameters[vaeac_main_para_names[vaeac_main_para_names != "vaeac.extra_parameters"]]
    )
    names(vaeac_all_parameters) <- sub("vaeac\\.", "", names(vaeac_all_parameters))
    vaeac_all_parameters <- c(vaeac_all_parameters, parameters[c("seed", "verbose")]) # Add seed and verbose

    # Fit/train the vaeac model with the provided model parameters
    vaeac_model <- do.call(vaeac_train_model, c(vaeac_all_parameters, list(x_train = internal$data$x_train)))

    # Add this to the explainer object
    parameters$vaeac <- list(
      models = vaeac_model[1:(grep("train_vlb", names(vaeac_model)) - 1)], # Models are all entries before `train_vlb`
      results = vaeac_model[c("train_vlb", "val_iwae", "val_iwae_running")], # The train & val results
      parameters = vaeac_model$parameters # List of all the parameters used to train the vaeac model
    )

    # Add `vaeac` as a class to the object. We use this to validate the input when
    # `vaeac.pretrained_vaeac_model` is given to the `shapr::explain()` function.
    class(parameters$vaeac) <- c(class(parameters$vaeac), "vaeac")
  } else {
    # User provided a pre-trained vaeac model. (Minimal checking for valid vaeac model is conducted.)
    # The pre-trained vaeac model is either:
    # 1. The explanation$internal$parameters$vaeac list of type "vaeac" from an earlier call to explain().
    # 2. A string containing the path to where the "vaeac" model is stored on disk.
    if (parameters$verbose == 2) message("Loading the provided `vaeac` model.")

    # Boolean representing that a pre-trained vaeac model was provided
    parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model_provided <- TRUE

    # Check some aspects of the pre-trained vaeac model and add it to the parameters list if it passes the checks
    parameters <- vaeac_update_pretrained_model(parameters = parameters)
  }

  # Get which vaeac model we are to use, load it and then store the checkpoint
  checkpoint <- torch::torch_load(parameters$vaeac$models[[parameters$vaeac.extra_parameters$vaeac.which_vaeac_model]])
  parameters$vaeac.checkpoint <- checkpoint

  # Set up and store the vaeac model such that it is loaded before calling the `prepare_data.vaeac()` function.
  parameters$vaeac.model <-
    vaeac_get_model_from_checkp(checkpoint = checkpoint, cuda = checkpoint$cuda, mode_train = FALSE)

  # Extract and save sampling method. That is, if we are to sample randomly from the inferred generative distributions
  # or if we are to sample the most likely values (mean for cont and class with highest prob for cat features).
  parameters$vaeac.sampler <- if (parameters$vaeac.extra_parameters$vaeac.sample_random) {
    parameters$vaeac.model$sampler_random
  } else {
    parameters$vaeac.model$sampler_most_likely
  }

  # Update/overwrite the parameters list in the internal list.
  internal$parameters <- parameters

  # Small printout to user
  if (parameters$verbose == 2) message("Done with 'setup_approach.vaeac'.\n")

  # Return the updated internal list.
  return(internal)
}

#' @inheritParams default_doc
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
#' @author Lars Henry Berge Olsen
prepare_data.vaeac <- function(internal, index_features = NULL, ...) {
  # If not provided, then set `index_features` to all non trivial coalitions
  if (is.null(index_features)) index_features <- seq(2, internal$parameters$n_combinations - 1)

  # Extract objects we are going to need later
  S <- internal$objects$S
  seed <- internal$parameters$seed
  verbose <- internal$parameters$verbose
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  vaeac.model <- internal$parameters$vaeac.model
  vaeac.sampler <- internal$parameters$vaeac.sampler
  vaeac.checkpoint <- internal$parameters$vaeac.checkpoint
  vaeac.batch_size_sampling <- internal$parameters$vaeac.extra_parameters$vaeac.batch_size_sampling

  # Small printout to the user
  if (verbose == 2) {
    message(paste0(
      "Working on batch ", internal$objects$X[id_combination == index_features[1]]$batch, " of ",
      internal$parameters$n_batches, " in `prepare_data.vaeac()`."
    ))
  }

  # Apply all coalitions to all explicands to get a data table where `vaeac` will impute the `NaN` values
  x_explain_extended <-
    vaeac_get_x_explain_extended(x_explain = x_explain, S = S, index_features = index_features)

  # Set the number of observations do generate the MC samples for at the time.
  n_explain_extended <- nrow(x_explain_extended)
  batch_size <- if (is.null(vaeac.batch_size_sampling)) n_explain_extended else vaeac.batch_size_sampling
  if (batch_size > n_explain_extended) batch_size <- n_explain_extended

  # Impute the missing entries using the vaeac approach.
  x_explain_with_MC_samples_dt <- vaeac_impute_missing_entries(
    x_explain_with_NaNs = x_explain_extended,
    n_explain = n_explain,
    n_samples = n_samples,
    vaeac_model = vaeac.model,
    checkpoint = vaeac.checkpoint,
    sampler = vaeac.sampler,
    batch_size = batch_size,
    verbose = verbose,
    seed = seed,
    index_features = index_features
  )

  # Return the generated conditional Monte Carlo samples
  return(x_explain_with_MC_samples_dt)
}


# Train vaeac model ====================================================================================================
#' Train the Vaeac Model
#'
#' @description Function that fits a vaeac model to the given dataset based on the provided parameters,
#' as described in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}. Note that
#' all default parameters specified below origin from [shapr:setup_approach.vaeac()] and
#' [shapr::vaeac_get_extra_para_default()].
#'
#' @details
#' The vaeac model consists of three neural networks, i.e., a masked encoder, a full encoder, and a decoder.
#' The networks have shared `depth`, `width`, and `activation_function`. The encoders maps the `x_train`
#' to a latent representation of dimension `latent_dim`, while the decoder maps the latent representations
#' back to the feature space. See \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}
#' for more details. The function first initiates `n_vaeacs_initialize` vaeac models with different randomly
#' initiated network parameter values to remedy poorly initiated values. After `epochs_initiation_phase` epochs, the
#' `n_vaeacs_initialize` vaeac models are compared and the function continues to only train the best performing
#' one for a total of `epochs` epochs. The networks are trained using the ADAM optimizer with the learning rate is `lr`.
#'
#' @param depth Positive integer (default is `3`). The number of hidden layers
#' in the neural networks of the masked encoder, full encoder, and decoder.
#' @param width Positive integer (default is `32`). The number of neurons in each
#' hidden layer in the neural networks of the masked encoder, full encoder, and decoder.
#' @param latent_dim Positive integer (default is `8`). The number of dimensions in the latent space.
#' @param lr Positive numeric (default is `0.001`). The learning rate used in the [torch::optim_adam()] optimizer.
#' @param activation_function An [torch::nn_module()] representing an activation function such as, e.g.,
#' [torch::nn_relu()] (default), [torch::nn_leaky_relu()], [torch::nn_selu()], or [torch::nn_sigmoid()].
#' @param n_vaeacs_initialize Positive integer (default is `4`). The number of different vaeac models to initiate
#'  in the start. Pick the best performing one after `epochs_initiation_phase`
#'  epochs (default is `2`) and continue training that one.
#' @param epochs Positive integer (default is `100`). The number of epochs to train the final vaeac model.
#' This includes `epochs_initiation_phase`, where the default is `2`.
#' @param x_train A data.table containing the training data. Categorical data must have class names \eqn{1,2,\dots,K}.
#' @param model_description String (default is `make.names(Sys.time())`). String containing, e.g., the name of the
#' data distribution or additional parameter information. Used in the save name of the fitted model. If not provided,
#' then a name will be generated based on [base::Sys.time()] to ensure a unique name. We use [base::make.names()] to
#' ensure a valid file name for all operating systems.
#' @param folder_to_save_model String (default is [base::tempdir()]). String specifying a path to a folder where
#' the function is to save the fitted vaeac model. Note that  the path will be removed from the returned
#' [shapr::explain()] object if `vaeac.save_model = FALSE`.
#' @param cuda cuda Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will be trained using cuda/GPU.
#' If [torch::cuda_is_available()] is `FALSE`, the we fall back to use CPU. If `FALSE`, we use the CPU. Often this is
#' faster for tabular data sets. Note, cuda is not not supported in the current version of the `shapr` package.
#' TODO: Update this when this is done.
#' @param epochs_initiation_phase Positive integer (default is `2`). The number of epochs to run each of the
#' `n_vaeacs_initialize` `vaeac` models before continuing to train only the best performing model.
#' @param epochs_early_stopping Positive integer (default is `NULL`). The training stops if there has been no
#' improvement in the validation IWAE for `epochs_early_stopping` epochs. If the user wants the training process
#' to be solely based on this training criterion, then `epochs` in [shapr::explain()] should be set to a large
#' number. If `NULL`, then `shapr` will internally set `epochs_early_stopping = vaeac.epochs` such that early
#' stopping does not occur.
#' @param save_every_nth_epoch Positive integer (default is `NULL`). If provided, then the vaeac model after
#' every `save_every_nth_epoch`th epoch will be saved.
#' @param val_ratio Numeric (default is `0.25`). Scalar between `0` and `1` indicating the ratio of
#' instances from the input data which will be used as validation data. That is, `val_ratio = 0.25` means
#' that `75%` of the provided data is used as training data, while the remaining `25%` is used as validation data.
#' @param val_iwae_n_samples Positive integer (default is `25`). The number of generated samples used
#' to compute the IWAE criterion when validating the vaeac model on the validation data.
#' @param batch_size Positive integer (default is `64`). The number of samples to include in each batch
#' during the training of the vaeac model. Used in [torch::dataloader()].
#' @param skip_conn_layer Logical (default is `TRUE`). If `TRUE`, we apply identity skip connections in each
#' layer, see [shapr::SkipConnection()]. That is, we add the input \eqn{X} to the outcome of each hidden layer,
#' so the output becomes \eqn{X + activation(WX + b)}.
#' @param skip_conn_masked_enc_dec Logical (default is `TRUE`). If `TRUE`, we apply concatenate skip
#' connections between the layers in the masked encoder and decoder. The first layer of the masked encoder will be
#' linked to the last layer of the decoder. The second layer of the masked encoder will be
#' linked to the second to last layer of the decoder, and so on.
#' @param batch_normalization Logical (default is `FALSE`). If `TRUE`, we apply batch normalization after the
#' activation function. Note that if `skip_conn_layer = TRUE`, then the normalization is applied after the
#' inclusion of the skip connection. That is, we batch normalize the whole quantity \eqn{X + activation(WX + b)}.
#' @param paired_sampling Logical (default is `TRUE`). If `TRUE`, we apply paired sampling to the training
#' batches. That is, the training observations in each batch will be duplicated, where the first instance will be masked
#' by \eqn{S} while the second instance will be masked by \eqn{\bar{S}}. This ensures that the training of the
#' `vaeac` model becomes more stable as the model has access to the full version of each training observation. However,
#' this will increase the training time due to more complex implementation and doubling the size of each batch. See
#' [shapr::paired_sampler()] for more information.
#' @param running_avg_n_values running_avg_n_values Positive integer (default is `5`).
#' The number of previous IWAE values to include
#' when we compute the running means of the IWAE criterion.
#' @param masking_ratio Numeric (default is `0.5`). Probability of masking a feature in the
#' [shapr::MCAR_mask_generator()] (MCAR = Missing Completely At Random). The MCAR masking scheme ensures that `vaeac`
#' model can do arbitrary conditioning as all coalitions will be trained. `masking_ratio` will be overruled if
#' `mask_gen_coalitions` is specified.
#' @param mask_gen_coalitions Matrix (default is `NULL`). Matrix containing the coalitions that the
#' `vaeac` model will be trained on, see [shapr::Specified_masks_mask_generator()]. This parameter is used internally
#' in `shapr` when we only consider a subset of coalitions/combinations, i.e., when
#' `n_combinations` \eqn{< 2^{n_{\text{features}}}}, and for group Shapley, i.e.,
#' when `group` is specified in [shapr::explain()].
#' @param mask_gen_coalitions_prob Numeric array (default is `NULL`). Array of length equal to the height
#' of `mask_gen_coalitions` containing the probabilities of sampling the corresponding coalitions in
#' `mask_gen_coalitions`.
#' @param sigma_mu Numeric (default is `1e4`). One of two hyperparameter values in the normal-gamma prior
#' used in the masked encoder, see Section 3.3.1 in
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param sigma_sigma Numeric (default is `1e-4`). One of two hyperparameter values in the normal-gamma prior
#' used in the masked encoder, see Section 3.3.1 in
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param save_data Logical (default is `FALSE`). If `TRUE`, then the data is stored together with
#' the model. Useful if one are to continue to train the model later using [shapr::vaeac_continue_train_model()].
#' @param log_exp_cont_feat Logical (default is `FALSE`). If we are to \eqn{\log} transform all
#' continuous features before sending the data to [shapr::vaeac()]. The `vaeac` model creates unbounded Monte Carlo
#' sample values. Thus, if the continuous features are strictly positive (as for, e.g., the Burr distribution and
#' Abalone data set), it can be advantageous to \eqn{\log} transform the data to unbounded form before using `vaeac`.
#' If `TRUE`, then [shapr::vaeac_postprocess_data()] will take the \eqn{\exp} of the results to get back to strictly
#' positive values when using the `vaeac` model to impute missing values/generate the Monte Carlo samples.
#' @param verbose Boolean. An integer specifying the level of verbosity. Use `0` (default) for no verbosity,
#' `1` for low verbose, and `2` for high verbose.
#' @param seed Positive integer (default is `1`). Seed for reproducibility. Specifies the seed before any randomness
#' based code is being run.
#' @param ... List of extra parameters, currently not used.
#'
#' @return A list containing the training/validation errors and paths to where the vaeac models are saved on the disk.
#' @export
#' @author Lars Henry Berge Olsen
vaeac_train_model <- function(x_train,
                              model_description,
                              folder_to_save_model,
                              cuda,
                              n_vaeacs_initialize,
                              epochs_initiation_phase,
                              epochs,
                              epochs_early_stopping,
                              save_every_nth_epoch,
                              val_ratio,
                              val_iwae_n_samples,
                              depth,
                              width,
                              latent_dim,
                              lr,
                              batch_size,
                              running_avg_n_values,
                              activation_function,
                              skip_conn_layer,
                              skip_conn_masked_enc_dec,
                              batch_normalization,
                              paired_sampling,
                              masking_ratio,
                              mask_gen_coalitions,
                              mask_gen_coalitions_prob,
                              sigma_mu,
                              sigma_sigma,
                              save_data,
                              log_exp_cont_feat,
                              which_vaeac_model,
                              verbose,
                              seed,
                              ...) {
  # Set seed for reproducibility for both R and torch
  set.seed(seed)
  torch::torch_manual_seed(seed)

  # Set epochs_early_stopping to epochs to ensure that early stopping never occurs
  if (is.null(epochs_early_stopping)) epochs_early_stopping <- epochs

  # Check all the vaeac parameters
  do.call(vaeac_check_parameters, mget(formalArgs(vaeac_train_model)))

  # Check if we can use cuda
  if (cuda) cuda <- vaeac_check_cuda(cuda)

  # Determine which mask generator to use
  mask_generator_name <- vaeac_get_mask_generator_name(
    mask_gen_coalitions = mask_gen_coalitions,
    mask_gen_coalitions_prob = mask_gen_coalitions_prob,
    masking_ratio = masking_ratio,
    verbose = verbose
  )

  # Set up the data loaders and get the save file names and load them into the local environment
  list2env(
    vaeac_get_data_objects(
      x_train = x_train,
      log_exp_cont_feat = log_exp_cont_feat,
      val_ratio = val_ratio,
      batch_size = batch_size,
      paired_sampling = paired_sampling,
      model_description = model_description,
      depth = depth,
      width = width,
      latent_dim = latent_dim,
      lr = lr,
      epochs = epochs,
      save_every_nth_epoch = save_every_nth_epoch,
      folder_to_save_model = folder_to_save_model,
      train_indices = NULL,
      val_indices = NULL
    ),
    envir = environment()
  )

  # Get information saved together with the vaeac model to make it possible to load the model from disk later.
  # Note that some of the parameters could be derived from others, but for simplicity we store all needed objects.
  state_list <- vaeac_get_full_state_list(environment())

  # Check if we are to add the training data to the state list
  if (save_data) state_list <- c(state_list, list(x_train = x_train, x_train_torch = x_train_torch))

  ## Initializing vaeac models
  # Initialize several vaeac models and keep the one with the best training variational lower bound
  # after a given number of epochs. Keep the version with highest vlb, denoted by "best_vlb".
  best_vlb <- -Inf

  # Create a `progressr::progressor()` to keep track of the overall training time of the vaeac approach
  progressr_bar <- progressr::progressor(steps = epochs_initiation_phase * (n_vaeacs_initialize - 1) + epochs)

  # Iterate over the initializations.
  initialization_idx <- 1
  for (initialization_idx in seq(n_vaeacs_initialize)) {
    # Initialize a new vaeac model
    vaeac_model <- vaeac(
      one_hot_max_sizes = one_hot_max_sizes,
      width = width,
      depth = depth,
      latent_dim = latent_dim,
      activation_function = activation_function,
      skip_conn_layer = skip_conn_layer,
      skip_conn_masked_enc_dec = skip_conn_masked_enc_dec,
      batch_normalization = batch_normalization,
      paired_sampling = paired_sampling,
      mask_generator_name = mask_generator_name,
      masking_ratio = masking_ratio,
      mask_gen_coalitions = mask_gen_coalitions,
      mask_gen_coalitions_prob = mask_gen_coalitions_prob,
      sigma_mu = sigma_mu,
      sigma_sigma = sigma_sigma
    )

    # TODO: we need to check this + we need to send the data too
    # Send the model to the GPU, if we have access to it.
    if (cuda) vaeac_model <- vaeac_model$cuda()

    # Add the number of trainable parameters in the vaeac model to the state list
    if (initialization_idx == 1) {
      state_list$n_trainable_parameters <- vaeac_model$n_train_param
      if (verbose == 2) {
        message(paste0("The vaeac model contains ", vaeac_model$n_train_param[1, 1], " trainable parameters."))
      }
    }

    # Print which initialization vaeac the function is working on
    if (verbose == 2) {
      message(paste0("Initializing vaeac number ", initialization_idx, " of ", n_vaeacs_initialize, "."))
    }

    # Create the ADAM optimizer
    optimizer <- vaeac_get_optimizer(vaeac_model = vaeac_model, lr = lr, optimizer_name = "adam")

    # Train the current initialized vaeac model
    vaeac_model_now_list <- vaeac_train_model_auxiliary(
      vaeac_model = vaeac_model,
      optimizer = optimizer,
      epochs = epochs_initiation_phase,
      epochs_start = 1, # All the vaeacs should start from scratch
      train_dataloader = train_dataloader,
      val_dataloader = val_dataloader,
      val_iwae_n_samples = val_iwae_n_samples,
      running_avg_n_values = running_avg_n_values,
      epochs_early_stopping = FALSE, # Do not want to do early stopping during initialization
      verbose = verbose,
      cuda = cuda,
      progressr_bar = progressr_bar,
      save_every_nth_epoch = save_every_nth_epoch,
      initialization_idx = initialization_idx,
      n_vaeacs_initialize = n_vaeacs_initialize,
      train_vlb = NULL, # We start from scratch
      val_iwae = NULL, # We start from scratch
      val_iwae_running = NULL # We start from scratch
    )

    # If the new initialization have lower training VLB than previous initializations, then we keep it.
    if ((best_vlb <= vaeac_model_now_list$avg_vlb)$item()) {
      vaeac_model_best_list <- vaeac_model_now_list
    }
  } # Done with initial training of all vaeac models

  # Send the model to the GPU, if we have access to it.
  # TODO: Check that this when we get access to GPU
  if (cuda) vaeac_model_best_list$model <- vaeac_model_best_listmodel$cuda()

  # Check if we are printing detailed debug information
  # Small printout to the user stating which initiated vaeac model was the best.
  if (verbose == 2) {
    message(paste0(
      "Best vaeac inititalization was number ", vaeac_model_best_list$initialization_idx, " (of ", n_vaeacs_initialize,
      ") with a training VLB = ", round(as.numeric(vaeac_model_best_list$train_vlb[-1]), 3), " after ",
      epochs_initiation_phase, " epochs. Continue to train this inititalization."
    ))
  }

  return_list <- vaeac_train_model_auxiliary(
    vaeac_model = vaeac_model_best_list$vaeac_model,
    optimizer = vaeac_model_best_list$optimizer,
    train_dataloader = train_dataloader,
    val_dataloader = val_dataloader,
    val_iwae_n_samples = val_iwae_n_samples,
    running_avg_n_values = running_avg_n_values,
    verbose = verbose,
    cuda = cuda,
    progressr_bar = progressr_bar,
    epochs = epochs,
    epochs_start = epochs_initiation_phase + 1,
    epochs_early_stopping = epochs_early_stopping,
    save_every_nth_epoch = save_every_nth_epoch,
    vaeac_save_file_names = vaeac_save_file_names, # Provide the save names for the models
    state_list = state_list, # Need to provide the state list as it will be saved together with the models
    initialization_idx = NULL, # Do not need to specify it as we are not doing the initialization now
    n_vaeacs_initialize = NULL, # Do not need to specify it as we are not doing the initialization now
    train_vlb = vaeac_model_best_list$train_vlb, # Send in the array from the best initiated vaeac model
    val_iwae = vaeac_model_best_list$val_iwae,
    val_iwae_running = vaeac_model_best_list$val_iwae_running
  )

  # Return the paths where the models are saved and the training/validation errors.
  return(return_list)
}

#' Continue to Train the vaeac Model
#'
#' @description Function that loads a previously trained vaeac model and continue the training, either
#' on new data or on the same dataset as it was trained on before. If we are given a new dataset, then
#' we assume that new dataset has the same distribution and one_hot_max_sizes as the original dataset.
#'
#' @param explanation A [shapr::explain()] object and `vaeac` must be the used approach.
#' @param epochs_new Integer. The number of extra epochs to conduct.
#' @param lr_new Numeric. If we are to overwrite the old learning rate in the adam optimizer.
#' @param x_train Matrix/data.frame containing new training data. If not present,
#' then we try to load training data from the vaeac_model.
#' @param save_data Boolean. If we are to save the training data.
#' @param verbose Boolean. If we are to print out information to the user.
#'
#' @return A list containing the training/validation errors and paths to where the vaeac models are saved on the disk.
#' @export
#' @author Lars Henry Berge Olsen
vaeac_continue_train_model <- function(explanation,
                                       epochs_new,
                                       lr_new = NULL,
                                       x_train = NULL,
                                       save_data = FALSE,
                                       verbose = 0,
                                       seed = 1) {
  # Check the input
  if (!"shapr" %in% class(explanation)) stop("`explanation` must be a list of class `shapr`.")
  if (!"vaeac" %in% explanation$internal$parameters$approach) stop("`vaeac` is not an approach in `explanation`.")
  if (!is.null(lr_new)) vaeac_check_positive_numerics(list(lr_new = lr_new))
  if (!is.null(x_train) && !data.table::is.data.table(x_train)) stop("`x_train` must be a `data.table` object.")
  vaeac_check_verbose(verbose)
  vaeac_check_positive_integers(list(epochs_new = epochs_new, seed = seed))
  vaeac_check_logicals(list(save_data = save_data))

  # Set seed for reproducibility
  set.seed(seed)

  # Extract the vaeac list and load the model at the last epoch
  vaeac_model <- explanation$internal$parameters$vaeac
  checkpoint <- torch::torch_load(vaeac_model$models$last)

  # If we applied early stopping before and are calling this function, then we turn early stopping off
  if (isTRUE(checkpoint$early_stopping_applied)) checkpoint$epochs_early_stopping <- epochs_new

  # Check for access to a single training data set and use the data from the checkpoint if `x_train` is not provided
  if (is.null(checkpoint$normalized_data) && is.null(x_train)) {
    stop("The `vaeac` model did not include data (set `vaeac.save_data = TRUE in `explain()`) and `x_train = NULL`.")
  }
  if (!is.null(checkpoint$x_train) && !is.null(x_train)) {
    message("The `vaeac` model includes data and `x_train` was provided to this function. We only use `x_train`.")
  }
  if (is.null(x_train)) x_train <- checkpoint$x_train

  # Check that the provided vaeac model is trained on a dataset with the same feature names
  vaeac_check_x_train_names(feature_names_vaeac = checkpoint$feature_list$labels, feature_names_new = names(x_train))

  # Check if we can reuse the original validation and training indices
  if (!is.null(checkpoint$x_train) || nrow(x_train) == checkpoint$n_train) {
    val_indices <- checkpoint$val_indices
    train_indices <- checkpoint$train_indices
  } else {
    val_indices <- train_indices <- NULL
  }

  # Set up the data loaders and get the save file names and load them into the local environment
  list2env(
    vaeac_get_data_objects(
      x_train = x_train,
      log_exp_cont_feat = checkpoint$log_exp_cont_feat,
      val_ratio = checkpoint$val_ratio,
      batch_size = checkpoint$batch_size,
      paired_sampling = checkpoint$paired_sampling,
      model_description = checkpoint$ model_description,
      depth = checkpoint$depth,
      width = checkpoint$width,
      latent_dim = checkpoint$latent_dim,
      lr = checkpoint$lr, # Use the old one as this parameter is used in the filenames
      epochs = checkpoint$epochs + epochs_new,
      save_every_nth_epoch = checkpoint$save_every_nth_epoch,
      folder_to_save_model = checkpoint$folder_to_save_model,
      train_indices = train_indices,
      val_indices = val_indices
    ),
    envir = environment()
  )

  # List to values saved to disk together with the vaeac models below.
  state_list_new <- list(
    norm_mean = as.array(x_train_preprocessed$norm_mean),
    norm_std = as.array(x_train_preprocessed$norm_std),
    n_train = n_train,
    epochs_new = epochs_new,
    train_indices = train_indices,
    val_indices = val_indices,
    lr_new = lr_new
  )

  # If we are also to save the data to state_list.
  if (save_data) {
    state_list_new <- c(state_list_new, list(x_train = x_train, x_train_torch = x_train_torch))

    # Give a message regarding disk usage
    vaeac_check_save_parameters(
      save_data = save_data,
      epochs = epochs_new,
      save_every_nth_epoch = checkpoint$save_every_nth_epoch,
      x_train_size = format(object.size(x_train), units = "auto")
    )
  }

  # Add the new state list as a list to the checkpoint
  n_times_continued_trained <- sum(grepl("state_list_new", names(checkpoint)))
  state_list_new_name <- paste("state_list_new", n_times_continued_trained + 1, sep = "_")
  state_list <- checkpoint
  state_list[[state_list_new_name]] <- state_list_new

  # Set up the vaeac model in training mode and based on the parameters stored in the checkpoint
  vaeac_model <- vaeac_get_model_from_checkp(checkpoint = checkpoint, cuda = checkpoint$cuda, mode_train = TRUE)

  # Specify the learning rate we will use, create the an adam optimizer, and insert the stored optimizer state.
  lr_now <- if (!is.null(lr_new)) lr_new else checkpoint$lr
  optimizer <- vaeac_get_optimizer(vaeac_model = vaeac_model, lr = lr_now, optimizer_name = "adam")
  optimizer$load_state_dict(checkpoint$optimizer_state_dict)

  # Compute the new number of epochs
  epochs_old <- checkpoint$epochs
  epochs <- epochs_old + epochs_new
  state_list$epochs <- epochs

  # Create a `progressr::progressor()` to keep track of the new training
  progressr_bar <- progressr::progressor(steps = epochs_new)

  # Train the vaeac model for `epochs_new` number of epochs
  vaeac_tmp <- vaeac_train_model_auxiliary(
    vaeac_model = vaeac_model,
    optimizer = optimizer,
    train_dataloader = train_dataloader,
    val_dataloader = val_dataloader,
    val_iwae_n_samples = checkpoint$val_iwae_n_samples,
    running_avg_n_values = checkpoint$running_avg_n_values,
    verbose = verbose,
    cuda = checkpoint$cuda,
    progressr_bar = progressr_bar,
    epochs = epochs,
    epochs_start = epochs_old + 1,
    epochs_early_stopping = checkpoint$epochs_early_stopping,
    save_every_nth_epoch = checkpoint$save_every_nth_epoch,
    vaeac_save_file_names = vaeac_save_file_names, # Provide the save names for the models
    state_list = state_list, # Need to provide the state list as it will be saved together with the models
    initialization_idx = NULL, # Do not need to specify it as we are not doing the initialization now
    n_vaeacs_initialize = NULL, # Do not need to specify it as we are not doing the initialization now
    train_vlb = checkpoint$train_vlb,
    val_iwae = checkpoint$val_iwae,
    val_iwae_running = checkpoint$val_iwae_running
  )

  # Create the return list
  return_list <- list(
    models = vaeac_tmp[1:(grep("train_vlb", names(vaeac_tmp)) - 1)], # Models are all entries before `train_vlb`
    results = vaeac_tmp[c("train_vlb", "val_iwae", "val_iwae_running")], # The train & val results
    parameters = vaeac_tmp$parameters # List of all the parameters used to train the vaeac model
  )

  # Add `vaeac` as a class to the object. We use this to validate the input when
  # `vaeac.pretrained_vaeac_model` is given to the `shapr::explain()` function.
  class(return_list) <- c(class(return_list), "vaeac")

  # Return the paths where the models are saved and the training/validation errors.
  return(return_list)
}


# Compute Imputations ==================================================================================================
#' Impute Missing Values Using Vaeac
#'
#' @details  Function that imputes the missing values in 2D matrix where each row constitute an individual.
#' The values are sampled from the conditional distribution estimated by a vaeac model.
#'
#' @inheritParams vaeac_train_model
#' @param x_explain_with_NaNs A 2D matrix, where the missing entries to impute are represented by `NaN`.
#' @param n_samples Integer. The number of imputed versions we create for each row in `x_explain_with_NaNs`.
#' @param index_features Optional integer vector. Used internally in shapr package to index the coalitions.
#' @param n_explain Positive integer. The number of explicands.
#' @param vaeac_model An initialized `vaeac` model that we are going to use to generate the MC samples.
#' @param checkpoint List containing the parameters of the `vaeac` model.
#' @param sampler A sampler object used to sample the MC samples.
#'
#' @return A data.table where the missing values (`NaN`) in `x_explain_with_NaNs` have been imputed `n_samples` times.
#' The data table will contain extra id columns if `index_features` and `n_explain` are provided.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_impute_missing_entries <- function(x_explain_with_NaNs,
                                         n_samples,
                                         vaeac_model,
                                         checkpoint,
                                         sampler,
                                         batch_size,
                                         verbose = 0,
                                         seed = NULL,
                                         n_explain = NULL,
                                         index_features = NULL) {
  # We only need `n_explain` when `index_features` is provided
  if (xor(is.null(index_features), is.null(n_explain))) {
    stop("Either none or both of `index_features` and `n_explain` must be given.")
  }

  # Set seed for reproducibility if provided by the user. Both in R and torch.
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

  if (verbose == 2) message("Preprocessing the explicands.")

  # Preprocess `x_explain_with_NaNs`. Turn factor names into numerics 1,2,...,K, (vaeac only accepts numerics) and keep
  # track of the maping of names. Optionally log-transform the continuous features. Then, finally, normalize the data
  # using the training means and standard deviations. I.e., we assume that the new data follow the same distribution as
  # the training data. If this is NOT the case, then vaeac will generate unreasonable imputations.
  x_explain_with_NaNs_processed <- vaeac_preprocess_data(
    data = x_explain_with_NaNs,
    log_exp_cont_feat = checkpoint$log_exp_cont_feat,
    normalize = TRUE,
    norm_mean = checkpoint$norm_mean, # Normalize using training data means
    norm_std = checkpoint$norm_std # Normalize using training data standard deviations
  )$data_normalized_torch

  # Create the data set object
  dataset <- vaeac_dataset(X = x_explain_with_NaNs_processed, one_hot_max_sizes = checkpoint$one_hot_max_sizes)

  # Create a data loader that load/iterate over the data set in chronological order.
  dataloader <- torch::dataloader(dataset = dataset, batch_size = batch_size, shuffle = FALSE)

  if (verbose == 2) message("Generating the MC samples.")

  # Create an auxiliary list of lists to store the imputed values combined with the original values. The structure is
  # [[i'th MC sample]][[b'th batch]], where the entries are tensors of dimension batch_size x n_features.
  results <- lapply(seq(n_samples), function(k) list())

  # Generate the conditional Monte Carlo samples for the observation `x_explain_with_NaNs`, one batch at the time.
  coro::loop(for (batch in dataloader) {
    # Make a deep copy of the batch and detach it from graph.
    batch_extended <- batch$clone()$detach()

    # If batch size is less than batch_size, extend it with objects from the beginning of the dataset.
    if (batch_extended$shape[1] < batch_size) {
      batch_extended <- vaeac_extend_batch(batch = batch_extended, dataloader = dataloader, batch_size = batch_size)
    }

    # Send the original and extended batch to GPU if applicable.
    if (checkpoint$cuda) {
      batch <- batch$cuda()
      batch_extended <- batch_extended$cuda()
    }

    # Compute the imputation mask, i.e., which entries we are to impute.
    mask_extended <- torch::torch_isnan(batch_extended)$to(dtype = torch::torch_float())

    # Do not need to keep track of the gradients, as we are not fitting the model.
    torch::with_no_grad({
      # Compute the distribution parameters for the generative models inferred by the masked encoder and decoder.
      # This is a tensor of shape [batch_size, n_samples, n_generative_parameters]. Note that, for only continuous
      # features we have that n_generative_parameters = 2*n_features, but for categorical data the number depends
      # on the number of categories.
      samples_params <- vaeac_model$generate_samples_params(batch = batch_extended, mask = mask_extended, K = n_samples)

      # Remove the parameters belonging to added instances in batch_extended.
      samples_params <- samples_params[1:batch$shape[1], , ]
    })

    # Make a deep copy of the batch with missing values set to zero.
    mask <- torch::torch_isnan(batch)
    batch_zeroed_nans <- batch$clone()$detach()
    batch_zeroed_nans[mask] <- 0

    # Iterate over the number of imputations and generate the imputed samples
    for (i in seq(n_samples)) {
      # Extract the i'th inferred generative parameters for the whole batch.
      # sample_params is a tensor of shape [batch_size, n_generative_parameters].
      sample_params <- samples_params[, i, ]

      # Generate the imputations using the generative distributions inferred by the decoder.
      sample <- sampler(sample_params)

      # Set the imputations for features in S (known features) to zero, as we do not need to generate them.
      sample[torch::torch_logical_not(mask)] <- 0

      # Combine the imputations with the original data to fill in the missing values. Shape is [batch_size, n_features].
      sample <- sample + batch_zeroed_nans

      # Make a deep copy and add it to correct location in the results list.
      results[[i]] <- append(results[[i]], sample$clone()$detach()$cpu())
    } # End of iterating over the n_samples
  }) # End of iterating over the batches. Done imputing.

  if (verbose == 2) message("Postprocessing the Monte Carlo samples.")

  # Order the MC samples into a tensor of shape [nrow(x_explain_with_NaNs), n_samples, n_features]. The lapply function
  # creates a list of tensors of shape [nrow(x_explain_with_NaNs), 1, n_features] by concatenating the batches for the
  # i'th MC sample to a tensor of shape [nrow(x_explain_with_NaNs), n_features] and then add unsqueeze to add a new
  # singleton dimension as the second dimension to get the shape [nrow(x_explain_with_NaNs), 1, n_features]. Then
  # outside of the lapply function, we concatenate the n_samples torch elements to form a final torch result of shape
  # [nrow(x_explain_with_NaNs), n_samples, n_features].
  result <- torch::torch_cat(lapply(seq(n_samples), function(i) torch::torch_cat(results[[i]])$unsqueeze(2)), dim = 2)

  # Get back to the original distribution by undoing the normalization by multiplying with the std and adding the mean
  result <- result * checkpoint$norm_std + checkpoint$norm_mean

  # Convert from a tensor of shape [nrow(x_explain_with_NaNs), n_samples, n_features]
  # to a matrix of shape [(nrow(x_explain_with_NaNs) * n_samples), n_features].
  result <- as.data.table(as.matrix(result$view(c(result$shape[1] * result$shape[2], result$shape[3]))$detach()$cpu()))

  # Post-process the data such that categorical features have original level names and convert to a data table.
  result <- vaeac_postprocess_data(data = result, vaeac_model_state_list = checkpoint)

  # If user provide `index_features`, then we add columns needed for shapr computations
  if (!is.null(index_features)) {
    # Add id, id_combination and weights (uniform for the `vaeac` approach) to the result.
    result[, c("id", "id_combination", "w") := list(
      rep(x = seq(n_explain), each = length(index_features) * n_samples),
      rep(x = index_features, each = n_samples, times = n_explain),
      1 / n_samples
    )]

    # Set the key in the data table
    setkeyv(result, c("id", "id_combination"))
  }

  return(result)
}
