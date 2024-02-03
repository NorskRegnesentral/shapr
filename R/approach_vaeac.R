# SHAPR functions -------------------------------------------------------------------------------------------------
#' @rdname setup_approach
#'
#' @param vaeac.depth Integer. The number of hidden layers in the neural networks of the masked
#'  encoder, full encoder, and decoder.
#' @param vaeac.width Integer. The number of neurons in each hidden layer in the neural networks
#'  of the masked encoder, full encoder, and decoder.
#' @param vaeac.latent_dim Integer. The number of dimensions in the latent space.
#' @param vaeac.lr Numeric. The learning rate used in the [torch::optim_adam()] optimizer.
#' @param vaeac.activation_function An [torch::nn_module()] representing an activation
#'  function such as, e.g., [torch::nn_relu()], [torch::nn_leaky_relu()],
#'  [torch::nn_selu()], and [torch::nn_sigmoid()].
#' @param vaeac.n_vaeacs_initialize Integer. The number of different vaeac models to initiate
#'  in the start. Pick the best performing one after `vaeac.extra_parameters$epochs_initiation_phase`
#'  epochs (default is `2`) and continue training that one.
#' @param vaeac.epochs Integer. The number of epochs to train the final vaeac model. This includes
#' `vaeac.extra_parameters$epochs_initiation_phase`, where the default is `2`.
#' @param vaeac.extra_parameters Named list with extra parameters to the `vaeac` approach.
#' See [shapr::vaeac_get_extra_para_default()] for description of possible additional parameters.
#'
#' @section The vaeac approach:
#' The `vaeac` model consists of three neural network (a full encoder, a masked encoder, and a decoder) based
#' on the provided `vaeac.depth` and `vaeac.width`. The encoders map the full and masked input
#' representations to latent representations, respectively, where the dimension is given by `vaeac.latent_dim`.
#' The latent representations are sent to the decoder to go back to the real feature space and
#' provide a samplable probabilistic representation, from which the Monte Carlo samples are generated.
#' We use the `vaeac` method at the epoch with the lowest validation error (IWAE) by default, but
#' other possibilities are available but setting the `vaeac.which_vaeac_model` parameter.
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
                                 vaeac.n_vaeacs_initialize = 10,
                                 vaeac.epochs = 200,
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
  vaeac_main_para_names = formalArgs(setup_approach.vaeac)
  vaeac_main_para_names = vaeac_main_para_names[!vaeac_main_para_names %in% c("internal", "...")]
  vaeac_main_para <- mget(vaeac_main_para_names)

  # vaeac_main_para = list(vaeac.depth = 3,
  #                        vaeac.width = 32,
  #                        vaeac.latent_dim = 8,
  #                        vaeac.activation_function = torch::nn_relu,
  #                        vaeac.lr = 0.001,
  #                        vaeac.n_vaeacs_initialize = 10,
  #                        vaeac.epochs = 200,
  #                        vaeac.extra_parameters = list())

  # Add the default extra parameter values for the non-user specified extra parameters
  parameters$vaeac.extra_parameters <- utils::modifyList(vaeac_get_extra_para_default(),
                                                         parameters$vaeac.extra_parameters,
                                                         keep.null = TRUE
  )

  # Add the default main parameter values for the non-user specified main parameters
  parameters <- utils::modifyList(vaeac_main_para, parameters, keep.null = TRUE)

  # Reorder them such that the vaeac parameters are at the end of the parameters list
  parameters <- c(parameters[(length(vaeac_main_para) + 1):length(parameters)], parameters[1:length(vaeac_main_para)])

  # Check if vaeac is to be applied on a subset of coalitions.
  if (!parameters$exact || parameters$is_groupwise || combined_approaches) {
    # We have either:
    # 1) sampled `n_combinations` different subsets of coalitions (i.e., not exact),
    # 2) using the coalitions which respects the groups in group Shapley values, and/or
    # 3) using a combination of approaches where vaeac is only used on a subset of the coalitions.
    # Here, objects$S contains the coalitions while objects$X contains the information about the approach.

    # Extract the the coalitions / masks which are estimated using vaeac as a matrix
    parameters$vaeac.mask_gen_these_coalitions <- S[X[approach == "vaeac"]$id_combination, , drop = FALSE]

    # Extract the weights for the corresponding coalitions / masks.
    parameters$vaeac.mask_gen_these_coalitions_prob <- X$shapley_weight[X[approach == "vaeac"]$id_combination]

    # Normalize the weights/probabilities such that they sum to one.
    parameters$vaeac.mask_gen_these_coalitions_prob <- parameters$vaeac.mask_gen_these_coalitions_prob /
      sum(parameters$vaeac.mask_gen_these_coalitions_prob)
  } else {
    # We are going to use the MCAR(`masking_ratio`) masking scheme. Set the variables to `NULL` as we do not need them.
    parameters$vaeac.mask_gen_these_coalitions <- parameters$vaeac.mask_gen_these_coalitions_prob <- NULL
  }

  # Check if user provided a pre-trained vaeac model, otherwise, we train one from scratch.
  if (is.null(parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model)) {
    # We train a vaeac model with the parameters in `parameters`, as user did not provide pre-trained vaeac model
    if (parameters$verbose == 2) message("Training a `vaeac` model with the provided parameters from scratch.")

    # Specify that a vaeac model was NOT provided
    parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model_provided <- FALSE

    # Extract all veaac parameters and remove the "vaeac." prefix as the names need to mach the parameters in "do.call"
    vaeac_all_parameters = c(parameters$vaeac.extra_parameters,
                             parameters[vaeac_main_para_names[vaeac_main_para_names != "vaeac.extra_parameters"]])
    names(vaeac_all_parameters) <- sub("vaeac\\.", "", names(vaeac_all_parameters))
    vaeac_all_parameters = c(vaeac_all_parameters, parameters[c("seed", "verbose")]) # Add seed and verbose

    # Fit/train the vaeac model with the provided model parameters
    vaeac_model <- do.call(vaeac_train_model, c(vaeac_all_parameters, list(x_train = internal$data$x_train)))

    # Add this to the explainer object
    parameters$vaeac <- list(
      models = vaeac_model[1:(grep("train_vlb", names(vaeac_model)) - 1)], # Models are all entries before `train_vlb`
      results = vaeac_model[c("train_vlb", "validation_iwae", "validation_iwae_running")], # The train & val results
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
#' as described in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
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
#' @param x_train A data.table containing the data. Categorical names
#' Categorical data must have class names \eqn{1,2,\dots,K}.
#' @param model_description String containing, e.g., the name of the data distribution or
#' additional parameter information. Used in the save name of the fitted model.
#' @param folder_to_save_model String specifying a path to a folder where
#' the function is to save the fitted vaeac model.
#' @param cuda Boolean. If we are to use cuda (GPU) if available. STILL IN DEVELOPMENT!
#' @param n_vaeacs_initialize Integer. The number of different vaeac models to initiate in the start.
#' Pick the best performing one after `epochs_initiation_phase` and continue training that one.
#' @param epochs_initiation_phase Integer. The number of epochs to run each of the `n_vaeacs_initialize`
#' vaeac models before only continuing training the best one.
#' @param epochs Integer. The number of epochs to train the final vaeac model.
#' This includes `epochs_initiation_phase`.
#' @param epochs_early_stopping Integer. The training stops if there has been no improvement in the validation IWAE
#' for `epochs_early_stopping` epochs. If the user wants the training process to be solely based on this, then `epochs`
#' should be set to a large number.
#' @param validation_ratio Scalar between 0 and 1 indicating the ratio of
#' instances from data which will be used as validation data.
#' @param validation_iwae_n_samples Integer. The number of samples used to compute the
#' IWAE when validating the vaeac model on the validation data.
#' @param depth Integer. The number of hidden layers in the neural
#' networks of the masked encoder, full encoder, and decoder.
#' @param width Integer. The number of neurons in each hidden layer in
#' the neural networks of the masked encoder, full encoder, and decoder.
#' @param latent_dim Integer. The number of dimensions in the latent space.
#' @param lr Numeric. The learning rate used in the [torch::optim_adam()] optimizer.
#' @param batch_size Integer. The number of samples to include in each batch.
#' @param running_avg_n_values Integer. How many of the previous values to include when we compute the running means.
#' @param activation_function An [torch::nn_module()] representing an activation function such as, e.g.,
#' [torch::nn_relu()], [torch::nn_leaky_relu()], [torch::nn_selu()], and
#' [torch::nn_sigmoid()].
#' @param skip_connection_layer Boolean. If we are to use skip connections in each layer. If true, then we add the input
#' to the outcome of each hidden layer, so the output becomes X + activation(WX + b). I.e., identity skip connection.
#' @param skip_connection_masked_enc_dec Boolean. If we are to apply concatenate skip
#' connections between the layers in the masked encoder and decoder.
#' @param batch_normalization Boolean. If we are to use batch normalization after the activation function.
#' Note that if `skip_connection_layer` is TRUE, then the normalization is
#' done after the adding from the skip connection. I.e, we batch normalize the whole quantity X + activation(WX + b).
#' @param paired_sampling Boolean. Default is `TRUE`. If we are doing paired sampling. I.e.,
#  each batch contains two versions of the same training observation, but where the first one is
#' masked by \eqn{S} and the second one is masked by \eqn{\bar{S}}, the complement, see
#' \href{https://arxiv.org/pdf/2107.07436.pdf}{Jethani et al. (2022)}. Training becomes more
#' stable, but slower due to more complex implementation.
#' @param masking_ratio Probability of masking a feature in the MCAR mask generator.
#' Default masking scheme which ensures that vaeac can do arbitrary conditioning.
#' This is overruled if `mask_gen_these_coalitions` is specified.
#' @param mask_gen_these_coalitions Matrix containing the different coalitions to learn.
#' @param mask_gen_these_coalitions_prob Numerics containing the probabilities for
#' sampling each mask in `mask_gen_these_coalitions`.
#' Array containing the probabilities for sampling the coalitions in `mask_gen_these_coalitions`.
#' @param sigma_mu Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder,
#' see Section 3.3.1 in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param sigma_sigma Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder,
#' see Section 3.3.1 in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param save_data Boolean. If we are to save the data together with the model. Useful if one are to continue
#' to train the model later.
#' @param log_exp_cont_feat Boolean. If we are to log transform all continuous
#' features before
#' sending the data to the vaeac using the [shapr::vaeac_postprocess_data()] function. The vaeac method creates
#' unbounded values, so if the continuous features are strictly positive, as for Burr and Abalone data, it can be
#' advantageous to log-transform the data to unbounded form before using vaeac.
#' If `TRUE`, then [shapr::vaeac_postprocess_data()] will take the exp of the results
#' to get back to strictly positive values when using the vaeac model to impute missing values.
#' @param verbose Boolean. If we are to print the progress of the initialization of different vaeac models,
#' the training of the final vaeac model, and summary of the training progress.
#' @param save_every_nth_epoch Integer. If we are to save the vaeac model after every nth epoch.
#' @param seed Positive integer (default is `1`). Seed for reproducibility.
#' @param ... List of extra parameters, currently not used.
#'
#' @return A list containing the training/validation errors and paths to where the vaeac models are saved on the disk.
#' @export
#' @author Lars Henry Berge Olsen
vaeac_train_model <- function(x_train,
                              model_description,
                              folder_to_save_model,
                              cuda = FALSE,
                              n_vaeacs_initialize = 10,
                              epochs_initiation_phase = 2,
                              epochs = 200,
                              epochs_early_stopping = NULL,
                              save_every_nth_epoch = NULL,
                              validation_ratio = 0.25,
                              validation_iwae_n_samples = 25,
                              depth = 3,
                              width = 32,
                              latent_dim = 8,
                              lr = 0.001,
                              batch_size = 64,
                              running_avg_n_values = 5,
                              activation_function = torch::nn_relu,
                              skip_connection_layer = TRUE,
                              skip_connection_masked_enc_dec = TRUE,
                              batch_normalization = FALSE,
                              paired_sampling = TRUE,
                              masking_ratio = 0.5,
                              mask_gen_these_coalitions = NULL,
                              mask_gen_these_coalitions_prob = NULL,
                              sigma_mu = 1e4,
                              sigma_sigma = 1e-4,
                              save_data = FALSE,
                              log_exp_cont_feat = FALSE,
                              which_vaeac_model = "best",
                              verbose = 0,
                              seed = 1,
                              ...) {

  # TODO: REMOVE list2env(vaeac_all_parameters, envir = .GlobalEnv)


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
    mask_gen_these_coalitions = mask_gen_these_coalitions,
    mask_gen_these_coalitions_prob = mask_gen_these_coalitions_prob,
    masking_ratio = masking_ratio,
    verbose = verbose
  )

  # Get the dimensions of the x_train
  n_train <- nrow(x_train)
  n_features <- ncol(x_train)

  # Preprocess x_train. Turn factor names into numerics 1,2,...,K, (vaeac only accepts numerics) and keep track
  # of the maping of names. Optionally log-transform the continuous features. Then, finally, normalize the data.
  x_train_preprocessed <- vaeac_preprocess_data(data = x_train, log_exp_cont_feat = log_exp_cont_feat, normalize = TRUE)

  # Extract the preprocessed and normalized x_train as a torch tensor and the one-hot feature sizes (cont have size 1)
  x_train_torch <- x_train_preprocessed$data_normalized_torch
  one_hot_max_sizes <- x_train_preprocessed$one_hot_max_sizes

  # Splitting the input into a training and validation data sets
  val_size <- ceiling(n_train * validation_ratio) # Number of observations in the validation set
  val_indices <- sample(n_train, val_size, replace = FALSE) # Sample indices for the validation set
  val_dataset <- vaeac_dataset(x_train_torch[val_indices], one_hot_max_sizes) # Create a torch::dataset() for vaeac
  train_indices <- seq(n_train)[-val_indices] # The remaining indices constitutes the training set
  train_dataset <- vaeac_dataset(x_train_torch[train_indices], one_hot_max_sizes) # Create a torch::dataset() for vaeac

  # Ensure a valid batch size
  if (batch_size > length(train_indices)) {
    message(paste0(
      "Decrease `batch_size` (", batch_size, ") to largest allowed value (", length(train_indices), "), ",
      "i.e., the number of training observations."
    ))
    batch_size <- length(train_indices)
  }

  # Create the Data Loader objects which iterate over the data in the Data Set objects
  train_dataloader <- torch::dataloader(
    dataset = train_dataset,
    batch_size = batch_size,
    shuffle = if (paired_sampling) FALSE else TRUE, # Must be `FALSE` when `sampler` is specified
    sampler = if (paired_sampling) paired_sampler(train_dataset, shuffle = TRUE) else NULL
  )

  val_dataloader <- torch::dataloader(
    dataset = val_dataset,
    batch_size = batch_size,
    shuffle = FALSE,
    sampler = if (paired_sampling) paired_sampler(val_dataset, shuffle = FALSE) else NULL
  )

  # Get all the file names for the vaeac objects we are going to save
  vaeac_save_file_names <- vaeac_get_save_file_names(
    model_description = model_description,
    n_features = n_features,
    n_train = n_train,
    depth = depth,
    width = width,
    latent_dim = latent_dim,
    lr = lr,
    epochs = epochs,
    save_every_nth_epoch = save_every_nth_epoch,
    folder_to_save_model = folder_to_save_model
  )

  ##### List that stores needed information for save and load the model
  # List of values saved to disk together with the vaeac models below.

  # TODO: GO THOUGH AND CHECK WHAT WE NEED AND DONT
  # Information saved together with the vaeac model to make it possible to load the model from disk later.
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
      skip_connection_layer = skip_connection_layer,
      skip_connection_masked_enc_dec = skip_connection_masked_enc_dec,
      batch_normalization = batch_normalization,
      paired_sampling = paired_sampling,
      mask_generator_name = mask_generator_name,
      masking_ratio = masking_ratio,
      mask_gen_these_coalitions = mask_gen_these_coalitions,
      mask_gen_these_coalitions_prob = mask_gen_these_coalitions_prob,
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
      validation_iwae_n_samples = validation_iwae_n_samples,
      running_avg_n_values = running_avg_n_values,
      epochs_early_stopping = FALSE, # Do not want to do early stopping during initialization
      verbose = verbose,
      cuda = cuda,
      progressr_bar = progressr_bar,
      save_every_nth_epoch = save_every_nth_epoch,
      initialization_idx = initialization_idx,
      n_vaeacs_initialize = n_vaeacs_initialize,
      train_vlb = NULL, # We start from scratch
      validation_iwae = NULL, # We start from scratch
      validation_iwae_running = NULL # We start from scratch
    )

    # If the new initialization have lower training VLB than previous initializations, then we keep it.
    if ((best_vlb <= vaeac_model_now_list$avg_vlb)$item()) {
      vaeac_model_best_list <- vaeac_model_now_list
    }
  } # Done with initial training of all vaeac models

  # Send the model to the GPU, if we have access to it.
  # TODO: IT should be there already?
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
    validation_iwae_n_samples = validation_iwae_n_samples,
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
    validation_iwae = vaeac_model_best_list$validation_iwae,
    validation_iwae_running = vaeac_model_best_list$validation_iwae_running
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

  # Check for access to a single training data set and use the data from the checkpoint if `x_train` is not provided
  if (is.null(checkpoint$normalized_data) && is.null(x_train)) {
    stop("The `vaeac` model did not include data (set `vaeac.save_data = TRUE in `explain()`) and `x_train = NULL`.")
  }
  if (!is.null(checkpoint$x_train) && !is.null(x_train)) {
    message("The `vaeac` model includes data and `x_train` was provided to this function. We only use `x_train`.")
  }
  if (is.null(x_train)) x_train <- checkpoint$x_train

  # Check that the provided vaeac model is trained on a dataset with the same feature names
  vaeac_check_x_train_names(feature_names_vaeac = checkpoint$feature_list$labels,
                            feature_names_new = names(x_train))

  # Specify the learning rate the function will use now
  lr_now = if (!is.null(lr_new)) lr_new else checkpoint$lr









  # EVERYTHING WITH SETUP X CAN BE A SEPARATE FUNCITON

  # Get the number of training observations
  n_train = nrow(x_train)

  # Preprocess x_train. Turn factor names into numerics 1,2,...,K, (vaeac only accepts numerics) and keep track
  # of the maping of names. Optionally log-transform the continuous features. Then, finally, normalize the data.
  x_train_preprocessed <- vaeac_preprocess_data(
    data = x_train,
    log_exp_cont_feat = checkpoint$log_exp_cont_feat,
    normalize = TRUE
  )

  # Extract the preprocessed and normalized x_train as a torch tensor, and the one-hot sizes
  x_train_torch <- x_train_preprocessed$data_normalized_torch
  one_hot_max_sizes <- x_train_preprocessed$one_hot_max_sizes

  # Splitting the input into a training and validation data sets
  if (!is.null(checkpoint$x_train) || n_train == checkpoint$n_train) {
    # Reuse the original validation and training indices
    val_indices <- checkpoint$val_indices
    train_indices <- checkpoint$train_indices
  } else {
    # Generate new validation and training indices
    val_size <- ceiling(n_train * validation_ratio) # Number of observations in the validation set
    val_indices <- sample(n_train, val_size, replace = FALSE) # Sample indices for the validation set
    train_indices <- seq(n_train)[-val_indices] # The remaining indices constitutes the training set
  }
  val_dataset <- vaeac_dataset(x_train_torch[val_indices], one_hot_max_sizes) # Create a torch::dataset() for vaeac
  train_dataset <- vaeac_dataset(x_train_torch[train_indices], one_hot_max_sizes) # Create a torch::dataset() for vaeac

  # Ensure a valid batch size
  if (checkpoint$batch_size > length(train_indices)) {
    message(paste0(
      "Decrease `batch_size` (", batch_size, ") to largest allowed value (", length(train_indices), "), ",
      "i.e., the number of training observations."
    ))
    checkpoint$batch_size <- length(train_indices)
  }

  # Create the Data Loader objects which iterate over the data in the Data Set objects
  train_dataloader <- torch::dataloader(
    dataset = train_dataset,
    batch_size = checkpoint$batch_size,
    shuffle = if (checkpoint$paired_sampling) FALSE else TRUE, # Must be `FALSE` when `sampler` is specified
    sampler = if (checkpoint$paired_sampling) paired_sampler(train_dataset, shuffle = TRUE) else NULL
  )

  val_dataloader <- torch::dataloader(
    dataset = val_dataset,
    batch_size = checkpoint$batch_size,
    shuffle = FALSE,
    sampler = if (checkpoint$paired_sampling) paired_sampler(val_dataset, shuffle = FALSE) else NULL
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
      x_train_size = format(object.size(x_train), units = "auto"))
  }


  # TODO: NEED TO REMEBER TO update the vaeac_save_file_names. The simplest idea is just to call
  # the function again but this time with a higher/the new number of epochs.
  checkpoint$vaeac_save_file_names = vaeac_get_save_file_names(
    model_description = checkpoint$model_description,
    n_features = checkpoint$n_features,
    n_train = checkpoint$n_train,
    depth = checkpoint$depth,
    width = checkpoint$width,
    latent_dim = checkpoint$latent_dim,
    lr = checkpoint$lr,
    epochs = checkpoint$epochs + epochs_new,
    save_every_nth_epoch = checkpoint$save_every_nth_epoch,
    folder_to_save_model = checkpoint$folder_to_save_model
  )

  # Add the new state list as a list to the checkpoint
  n_times_continued_trained <- sum(grepl("state_list_new", names(checkpoint)))
  state_list_new_name <- paste("state_list_new", n_times_continued_trained + 1, sep = "_")
  state_list <- checkpoint
  state_list[[state_list_new_name]] <- state_list_new










  # Set up the vaeac model in training mode and based on the parameters stored in the checkpoint
  vaeac_model <- vaeac_get_model_from_checkp(checkpoint = checkpoint, cuda = checkpoint$cuda, mode_train = TRUE)

  # Create the an adam optimizer and insert the state
  optimizer <- vaeac_get_optimizer(vaeac_model = vaeac_model, lr = lr_now, optimizer_name = "adam")
  optimizer$load_state_dict(checkpoint$optimizer_state_dict)




  # Compute the new number of epochs
  epochs_old <- checkpoint$epochs
  epochs <- epochs_old + epochs_new
  state_list$epochs <- epochs



  # Create a `progressr::progressor()` to keep track of the new training
  progressr_bar <- progressr::progressor(steps = epochs_new)


  return_list = vaeac_train_model_auxiliary(
    vaeac_model = vaeac_model,
    optimizer = optimizer,
    train_dataloader = train_dataloader,
    val_dataloader = val_dataloader,
    validation_iwae_n_samples = checkpoint$validation_iwae_n_samples,
    running_avg_n_values = checkpoint$running_avg_n_values,
    verbose = verbose,
    cuda = checkpoint$cuda,
    progressr_bar = progressr_bar,
    epochs = epochs_total,
    epochs_start = epochs_old + 1,
    epochs_early_stopping = checkpoint$epochs_early_stopping,
    save_every_nth_epoch = checkpoint$save_every_nth_epoch,
    vaeac_save_file_names = checkpoint$vaeac_save_file_names, # Provide the save names for the models
    state_list = state_list, # Need to provide the state list as it will be saved together with the models
    initialization_idx = NULL, # Do not need to specify it as we are not doing the initialization now
    n_vaeacs_initialize = NULL, # Do not need to specify it as we are not doing the initialization now
    train_vlb = checkpoint$train_vlb,
    validation_iwae = checkpoint$validation_iwae,
    validation_iwae_running = checkpoint$validation_iwae_running
  )


  # Return the paths where the models are saved and the training/validation errors.
  return(return_list)
}


# Compute Imputations ==================================================================================================
#' Impute Missing Values Using Vaeac
#'
#' @details  Function that imputes the missing values in 2D matrix where each row constitute an individual.
#' The values are sampled from the conditional distribution estimated by a vaeac model.
#'
#' @param x_explain_with_NaNs A 2D matrix, where the missing entries to impute are represented by `NaN`.
#' @param n_samples Integer. The number of imputed versions we create for each row in `x_explain_with_NaNs`.
#' @param sample_random Boolean. If we are to generate random samples from the inferred generative distributions,
#' or if we are to sample the most likely values (mean for continuous and class with highest prob for categorical).
#' @param batch_size Integer. The number of samples in each batch.
#' If `NULL`, then use the same value saved in the vaeac object.
#' We recommend a large number because of a lot of overhead for small batch sizes, that is,
#' `batch_size` should be much larger than the batch size used during the training. checkpoint$batch_size
#' @param verbose Boolean. If we are to print the progress to the user.
#' @param seed Integer. Seed used before generating the MC samples.
#' @param index_features Optional integer vector. Used internally in shapr package to index the coalitions.
#' @param n_explain Positive integer. The number of explicands.
#' @param vaeac_model ADD TEXT
#' @param checkpoint List containing the
#' @param sampler ADD TEXT
#'
#' @return A 2D or 3D array or 2D data.table where the missing values (`NaN`) in `x_explain_with_NaNs`
#' have been imputed `n_samples` times. The dimension of the returned object depends on the
#' boolean parameters `convert_to_2D` and `return_as_postprocessed_dt`.
#'
#' @export
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

  if (verbose == 2) message("Concatenating the Monte Carlo samples.")

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

  if (verbose == 2) message("Postprocessing the Monte Carlo samples.")

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


# Plot functions =======================================================================================================


#' Plot the training VLB and validation IWAE for `vaeac` models
#'
#' @description
#' This function make figures ([ggplot2::ggplot()]) of the training VLB and the validation IWAE for a list
#' of [explain()] objects with `approach = "vaeac"`. See [setup_approach()] for more
#' information about the `vaeac` approach. Two figures are made, where in the first one each object in
#' `explanation_list` gets its own facet, while in the second figure, we plot the criteria in each facet.
#'
#' @details
#' See \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)} or the
#' \href{https://borea17.github.io/paper_summaries/iwae/}{blog post} for a summary of the VLB and IWAE.
#'
#' @param explanation_list A list of [explain()] objects applied to the same data, model, and
#' `vaeac` must be the used approach. If the entries in the list is named, then the function use
#' these names. Otherwise, it defaults to the approach names (with integer suffix for duplicates)
#' for the explanation objects in `explanation_list`.
#' @param plot_from_nth_epoch Integer. If we are only plot the results form the nth epoch and so forth.
#' The first epochs can be large in absolute value and make the rest of the plot difficult to interpret.
#' @param plot_every_nth_epoch Integer. If we are only to plot every nth epoch. Usefully to illustrate
#' the overall trend, as there can be a lot of fluctuation and oscillation in the values between each epoch.
#' @param facet_wrap_scales String. Should the scales be fixed ("`fixed`", the default),
#' free ("`free`"), or free in one dimension ("`free_x`", "`free_y`").
#' @param facet_wrap_ncol Integer. Number of columns in the facet wrap.
#' @param criteria Character vector. The possible options are "VLB", "IWAE", "IWAE_running". Default is the first two.
#' @param plot_type Character vector. The possible options are "method" and "criterion". Default is to plot both.
#'
#' @return Either a single [ggplot2::ggplot()] object or a list of [ggplot2::ggplot()] objects based on the
#' `plot_type` parameter.
#'
#' @examples
#' \dontrun{
#' library(xgboost)
#' data("airquality")
#' data <- data.table::as.data.table(airquality)
#' data <- data[complete.cases(data), ]
#'
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' ind_x_explain <- 1:6
#' x_train <- data[-ind_x_explain, ..x_var]
#' y_train <- data[-ind_x_explain, get(y_var)]
#' x_explain <- data[ind_x_explain, ..x_var]
#'
#' # Fitting a basic xgboost model to the training data
#' model <- xgboost(
#'   data = as.matrix(x_train),
#'   label = y_train,
#'   nround = 100,
#'   verbose = FALSE
#' )
#'
#' # Specifying the phi_0, i.e. the expected prediction without any features
#' p0 <- mean(y_train)
#'
#' # Train several different NN
#' explanation_paired_sampling_TRUE <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = approach,
#'   prediction_zero = p0,
#'   n_batches = 2,
#'   n_samples = 1, #' As we are only interested in the training of the vaeac
#'   vaeac.epochs = 25, #' Should be higher in applications.
#'   vaeac.n_vaeacs_initialize = 5,
#'   vaeac.extra_parameters = list(
#'     vaeac.paired_sampling = TRUE,
#'     vaeac.verbose = TRUE
#'   )
#' )
#'
#' explanation_paired_sampling_FALSE <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = approach,
#'   prediction_zero = p0,
#'   n_batches = 2,
#'   n_samples = 1, #' As we are only interested in the training of the vaeac
#'   vaeac.epochs = 25, #' Should be higher in applications.
#'   vaeac.n_vaeacs_initialize = 5,
#'   vaeac.extra_parameters = list(
#'     vaeac.paired_sampling = FALSE,
#'     vaeac.verbose = TRUE
#'   )
#' )
#'
#' # Other networks have 4.76 times more parameters.
#' explanation_paired_sampling_FALSE_small <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = approach,
#'   prediction_zero = p0,
#'   n_batches = 2,
#'   n_samples = 1, #' As we are only interested in the training of the vaeac
#'   vaeac.epochs = 25, #' Should be higher in applications.
#'   vaeac.width = 16, #' Default is 32
#'   vaeac.depth = 2, #' Default is 3
#'   vaeac.latent_dim = 4, #' Default is 8
#'   vaeac.n_vaeacs_initialize = 5,
#'   vaeac.extra_parameters = list(
#'     vaeac.paired_sampling = FALSE,
#'     vaeac.verbose = TRUE
#'   )
#' )
#'
#' explanation_paired_sampling_TRUE_small <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = approach,
#'   prediction_zero = p0,
#'   n_batches = 2,
#'   n_samples = 1, #' As we are only interested in the training of the vaeac
#'   vaeac.epochs = 25, #' Should be higher in applications.
#'   vaeac.width = 16, #' Default is 32
#'   vaeac.depth = 2, #' Default is 3
#'   vaeac.latent_dim = 4, #' Default is 8
#'   vaeac.n_vaeacs_initialize = 5,
#'   vaeac.extra_parameters = list(
#'     vaeac.paired_sampling = TRUE,
#'     vaeac.verbose = TRUE
#'   )
#' )
#'
#' # Collect the explanation objects in an unnamed list
#' explanation_list_unnamed <- list(
#'   explanation_paired_sampling_FALSE,
#'   explanation_paired_sampling_FALSE_small,
#'   explanation_paired_sampling_TRUE,
#'   explanation_paired_sampling_TRUE_small
#' )
#'
#' # Collect the explanation objects in an named list
#' explanation_list_named <- list(
#'   "Regular samp. & large NN" = explanation_paired_sampling_FALSE,
#'   "Regular samp. & small NN" = explanation_paired_sampling_FALSE_small,
#'   "Paired samp. & large NN" = explanation_paired_sampling_TRUE,
#'   "Paired samp. & small NN" = explanation_paired_sampling_TRUE_small
#' )
#'
#' # Call the function with the unnamed list, will create names
#' vaeac_plot_evaluation_criterion(explanation_list = explanation_list_unnamed)
#'
#' # Call the function with the named list, will use the provided names
#' # See that the paired samplign often produce more stable results
#' vaeac_plot_evaluation_criterion(explanation_list = explanation_list_named)
#'
#' # The function also works if we have only one method,
#' # but then one should only look at the method plot
#' vaeac_plot_evaluation_criterion(
#'   explanation_list = list("Paired samp. & large NN" = explanation_paired_sampling_TRUE),
#'   plot_type = "method"
#' )
#'
#' # Can alter the plot
#' vaeac_plot_evaluation_criterion(
#'   explanation_list = explanation_list_named,
#'   plot_from_nth_epoch = 5,
#'   plot_every_nth_epoch = 3,
#'   facet_wrap_scales = "free"
#' )
#'
#' # If we want only want the criterion version
#' tmp_fig_criterion <- vaeac_plot_evaluation_criterion(
#'   explanation_list = explanation_list_named,
#'   plot_type = "criterion"
#' )
#'
#' # We can add points
#' tmp_fig_criterion + ggplot2::geom_point(shape = "circle", size = 1, ggplot2::aes(col = Method))
#'
#' # If we rather want smooths with se bands
#' tmp_fig_criterion$layers[[1]] <- NULL
#' tmp_fig_criterion + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
#'   ggplot2::scale_color_brewer(palette = "Set1") +
#'   ggplot2::theme_minimal()
#'
#' # If we only want the VLB
#' vaeac_plot_evaluation_criterion(
#'   explanation_list = explanation_list_named,
#'   criteria = "VLB",
#'   plot_type = "criterion"
#' )
#' }
#'
#' @author Lars Henry Berge Olsen
#' @export
vaeac_plot_evaluation_criterion <- function(explanation_list,
                                            plot_from_nth_epoch = 1,
                                            plot_every_nth_epoch = 1,
                                            criteria = c("VLB", "IWAE"),
                                            plot_type = c("method", "criterion"),
                                            facet_wrap_scales = "fixed",
                                            facet_wrap_ncol = NULL) {
  ## Checks
  # Check that ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }

  # Check for valid criteria argument
  unknown_criteria <- criteria[!(criteria %in% c("VLB", "IWAE", "IWAE_running"))]
  if (length(unknown_criteria) > 0) {
    error(paste0(
      "The `criteria` must be one (or several) of 'VLB', 'IWAE', and 'IWAE_running'. ",
      "Do not recognise: '", paste(unknown_plot_type, collapse = "', '"), "'."
    ))
  }

  # Check for valid plot type argument
  unknown_plot_type <- plot_type[!(plot_type %in% c("method", "criterion"))]
  if (length(unknown_plot_type) > 0) {
    error(paste0(
      "The `plot_type` must be one (or several) of 'method' and 'criterion'. ",
      "Do not recognise: '", paste(unknown_plot_type, collapse = "', '"), "'."
    ))
  }

  # Ensure that even a single explanation object is in a list
  if ("shapr" %in% class(explanation_list)) explanation_list <- list(explanation_list)

  ## Create data.tables
  # Extract the VLB and IWAE
  vaeac_VLB_IWAE_dt <- vaeac_get_evaluation_criteria(explanation_list)

  # Get the relevant criteria
  keep_these_columns <- c("Method", "Epoch", criteria)
  vaeac_VLB_IWAE_dt <- vaeac_VLB_IWAE_dt[, keep_these_columns, with = FALSE]

  # Check for valid `plot_from_nth_epoch`
  max_epoch <- max(vaeac_VLB_IWAE_dt$Epoch)
  if (plot_from_nth_epoch > max_epoch) {
    stop(sprintf(
      "`plot_from_nth_epoch` (%d) is larger than the number of epochs (%d)",
      plot_from_nth_epoch, max_epoch
    ))
  }

  # Remove entries with too low epoch
  vaeac_VLB_IWAE_dt <- vaeac_VLB_IWAE_dt[Epoch >= plot_from_nth_epoch, ]

  # If we are only to plot every nth epoch
  vaeac_VLB_IWAE_dt <- vaeac_VLB_IWAE_dt[Epoch %% plot_every_nth_epoch == 0]

  # Convert it from wide to long
  vaeac_VLB_IWAE_dt_long <- data.table::melt(
    data = vaeac_VLB_IWAE_dt,
    id.vars = c("Method", "Epoch"),
    variable.name = "Criterion",
    variable.factor = TRUE,
    value.name = "Value"
  )

  ## Plot
  return_object <- list()

  # Make the figure where each explanation object has its own facet
  if ("method" %in% plot_type) {
    return_object$figure_each_method <-
      ggplot2::ggplot(vaeac_VLB_IWAE_dt_long, ggplot2::aes(x = Epoch, y = Value, col = Criterion)) +
      ggplot2::labs(title = "The evaluation criterions for different vaeac models") +
      ggplot2::geom_line(ggplot2::aes(group = Criterion, col = Criterion)) +
      ggplot2::facet_wrap(ggplot2::vars(Method), ncol = facet_wrap_ncol, scales = facet_wrap_scales)
  }

  # Make the figure where each criterion has its own facet
  if ("criterion" %in% plot_type) {
    return_object$figure_each_criterion <-
      ggplot2::ggplot(vaeac_VLB_IWAE_dt_long, ggplot2::aes(x = Epoch, y = Value, col = Method)) +
      ggplot2::labs(title = "The evaluation criterions for different vaeac models") +
      ggplot2::geom_line(ggplot2::aes(group = Method, col = Method)) +
      ggplot2::facet_wrap(ggplot2::vars(Criterion), ncol = facet_wrap_ncol, scales = facet_wrap_scales)
  }

  # If only made one figure, then we directly return that object and not a list
  if (length(return_object) == 1) return_object <- return_object[[1]]

  return(return_object)
}

#' Plot Pairwise Plots for Imputed and True Data
#'
#' @description A function that creates a matrix of plots ([GGally::ggpairs()]) from
#' generated imputations from the unconditioned distribution \eqn{p(\boldsymbol{x})} estimated by
#' a vaeac model, and then compares the imputed values with data from the true distribution (if provided).
#' See \href{https://www.blopig.com/blog/2019/06/a-brief-introduction-to-ggpairs/}{ggpairs} for an
#' introduction to [GGally::ggpairs()], and the corresponding
#' \href{https://ggobi.github.io/ggally/articles/ggally_plots.html}{vignette}.
#'
#' @param explanation Shapr list. The output list from the [shapr::explain()] function.
#' @param which_vaeac_model String. Indicating which `vaeac` model to use when generating the samples.
#' Possible options are always `'best'`, `'best_running'`, and `'last'`. All possible options can be obtained
#' by calling `names(explanation$internal$parameters$vaeac$models)`.
#' @param x_true Data.table containing the data from the distribution that the `vaeac` model is fitted to.
#' @param upper_cont String. Type of plot to use in upper triangle for continuous features, see [GGally::ggpairs()].
#' Possible options are: `'cor'` (default), `'points'`, `'smooth'`, `'smooth_loess'`, `'density'`, and `'blank'`.
#' @param upper_cat String. Type of plot to use in upper triangle for categorical features, see [GGally::ggpairs()].
#' Possible options are: `'count'` (default), `'cross'`, `'ratio'`, `'facetbar'`, and `'blank'`.
#' @param upper_mix String. Type of plot to use in upper triangle for mixed features, see [GGally::ggpairs()].
#' Possible options are: `'box'` (default), `'box_no_facet'`, `'dot'`, `'dot_no_facet'`, `'facethist'`, `'facetdensity'`, `'denstrip'`, and `'blank'`
#' @param lower_cont String. Type of plot to use in lower triangle for continuous features, see [GGally::ggpairs()].
#' Possible options are: `'points'` (default), `'smooth'`, `'smooth_loess'`, `'density'`, `'cor'`, and `'blank'`.
#' @param lower_cat String. Type of plot to use in lower triangle for categorical features, see [GGally::ggpairs()].
#' Possible options are: `'facetbar'` (default), `'ratio'`, `'count'`, `'cross'`, and `'blank'`.
#' @param lower_mix String. Type of plot to use in lower triangle for mixed features, see [GGally::ggpairs()].
#' Possible options are: `'facetdensity'` (default), `'box'`, `'box_no_facet'`, `'dot'`, `'dot_no_facet'`, `'facethist'`, `'denstrip'`, and `'blank'`.
#' @param diag_cont String. Type of plot to use on the diagonal for continuous features, see [GGally::ggpairs()].
#' Possible options are: `'densityDiag'` (default), `'barDiag'`, and `'blankDiag'`.
#' @param diag_cat String. Type of plot to use on the diagonal for categorical features, see [GGally::ggpairs()].
#' Possible options are: `'barDiag'` (default) and `'blankDiag'`.
#' @param cor_method String. Type of correlation measure, see [GGally::ggpairs()].
#' Possible options are: `'pearson'` (default), `'kendall'`, and `'spearman'`.
#' @param add_title Logical. If `TRUE`, then a title is added to the plot based on the internal description
#' of the `vaeac` model specified in `which_vaeac_model`.
#' @param alpha Numeric between `0` and `1` (default is `0.5`). The degree of color transparency.
#'
#' @return A [GGally::ggpairs()] figure.
#' @export
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' \dontrun{
#' library(xgboost)
#' library(data.table)
#' library(shapr)
#'
#' data("airquality")
#' data <- data.table::as.data.table(airquality)
#' data <- data[complete.cases(data), ]
#'
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' ind_x_explain <- 1:6
#' x_train <- data[-ind_x_explain, ..x_var]
#' y_train <- data[-ind_x_explain, get(y_var)]
#' x_explain <- data[ind_x_explain, ..x_var]
#'
#' # Fitting a basic xgboost model to the training data
#' model <- xgboost(
#'   data = as.matrix(x_train),
#'   label = y_train,
#'   nround = 100,
#'   verbose = FALSE
#' )
#'
#' explanation <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "vaeac",
#'   prediction_zero = mean(y_train),
#'   n_samples = 1,
#'   vaeac.epochs = 10,
#'   vaeac.n_vaeacs_initialize = 1
#' )
#'
#' # Plot the results
#' figure = vaeac_plot_imputed_ggpairs(explanation = explanation,
#'                                     which_vaeac_model = "best",
#'                                     x_true = x_train,
#'                                     add_title = TRUE)
#' figure
#'
#' # Note that this is an ggplot2 object which we can alter. E.g., we can change the colors
#' figure +
#'   ggplot2::scale_color_manual(values = c("#E69F00", "#999999")) +
#'   ggplot2::scale_fill_manual(values = c("#E69F00", "#999999"))
#' }
vaeac_plot_imputed_ggpairs <- function(
    explanation,
    which_vaeac_model = "best",
    x_true = NULL,
    add_title = TRUE,
    alpha = 0.5,
    upper_cont = c("cor", "points", "smooth", "smooth_loess", "density", "blank"),
    upper_cat = c("count", "cross", "ratio", "facetbar", "blank"),
    upper_mix = c("box", "box_no_facet", "dot", "dot_no_facet", "facethist", "facetdensity", "denstrip", "blank"),
    lower_cont = c("points", "smooth", "smooth_loess", "density", "cor", "blank"),
    lower_cat = c("facetbar", "ratio", "count", "cross", "blank"),
    lower_mix = c("facetdensity", "box", "box_no_facet", "dot", "dot_no_facet", "facethist", "denstrip", "blank"),
    diag_cont = c("densityDiag", "barDiag", "blankDiag"),
    diag_cat = c("barDiag", "blankDiag"),
    cor_method = c("pearson", "kendall", "spearman")) {

  # Check that ggplot2 and GGally are installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }
  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("GGally is not installed. Please run install.packages('GGally')")
  }

  # Check all input parameters except `which_vaeac_model`
  if (!"shapr" %in% class(explanation)) stop("`explanation` must be an object of type `shapr`.")
  if (!is.null(x_true) && !is.data.table(x_true)) stop("`x_true` must be an object of type `data.table`.")
  vaeac_check_logicals(list(add_title = add_title))
  vaeac_check_probabilities(list(alpha = alpha))
  upper_cont <- match.arg(upper_cont)
  upper_cat <- match.arg(upper_cat)
  upper_mix <- match.arg(upper_mix)
  lower_cont <- match.arg(lower_cont)
  lower_cat <- match.arg(lower_cat)
  lower_mix <- match.arg(lower_mix)
  diag_cont <- match.arg(diag_cont)
  diag_cat <- match.arg(diag_cat)
  cor_method <- match.arg(cor_method)

  # Check if the vaeac model is expected to give a reasonable figure.
  if (!explanation$internal$parameters$exact || explanation$internal$parameters$is_groupwise) {
    message("The vaeac model has not been trained on the empty colition, hence, the figure can be missleading. ",
            "The figure is only reasonable if 'n_combintations = NULL' and 'group = NULL' in the explanation call.")
  }

  # Extract the vaeac list from the explanation list
  vaeac_list <- explanation$internal$parameters$vaeac

  # Check that `which_vaeac_model` is a valid vaeac model name and then load the vaeac checkpoint
  if (!is.character(which_vaeac_model) || !which_vaeac_model %in% names(vaeac_list$models)) {
    stop(paste0("The parameter `which_vaeac_model` ('", which_vaeac_model ,"') must be one of the following: '",
                paste(names(vaeac_list$models), collapse = "', '"), "'."))
  }
  vaeac_model_path <- vaeac_list$models[[which_vaeac_model]]
  checkpoint <- torch::torch_load(vaeac_model_path)

  # Get the number of observations in the x_true and features
  n_samples <- if (is.null(x_true)) 500 else nrow(x_true)
  n_features = checkpoint$n_features

  # Checking for valid dimension
  if (!is.null(x_true) && ncol(x_true) != n_features) {
    stop(paste0(
      "Different number of columns in the vaeac model (", n_features ,") and `x_true` (", ncol(x_true), ")."))
  }

  # Set up the vaeac model
  vaeac_model = vaeac_get_model_from_checkp(checkpoint = checkpoint, cuda = FALSE, mode_train = FALSE)

  # Impute the missing entries using the vaeac approach. Here we generate x from p(x), so no conditioning.
  imputed_values = vaeac_impute_missing_entries(
    x_explain_with_NaNs = matrix(NaN, n_samples, checkpoint$n_features),
    n_samples = 1,
    vaeac_model = vaeac_model,
    checkpoint = checkpoint,
    sampler = explanation$internal$parameters$vaeac.sampler,
    batch_size = n_samples,
    verbose = explanation$internal$parameters$verbose,
    seed = explanation$internal$parameters$seed
  )

  # Combine the true (if there are any) adn imputed data and ensure that the categorical features are marked as factors.
  combined_data <- data.table(rbind(x_true, imputed_values))
  col_cat_names = checkpoint$col_cat_names
  if (length(col_cat_names) > 0) combined_data[,(col_cat_names) := lapply(.SD, as.factor), .SDcols = col_cat_names]

  # Add type variable representing if they are imputed samples or from `x_true`
  combined_data$type <-
    factor(rep(c("True", "Imputed"), times = c(ifelse(is.null(nrow(x_true)), 0, nrow(x_true)), n_samples)))

  # Create the ggpairs figure and potentially add title based on the description of the used vaeac model
  figure <- GGally::ggpairs(
    combined_data,
    columns = seq(n_features),
    mapping = ggplot2::aes(color = type),
    diag = list(continuous = GGally::wrap(diag_cont, alpha = alpha), discrete = diag_cat),
    upper = list(combo = upper_mix, discrete = upper_cat, continuous = GGally::wrap(upper_cont, method = cor_method)),
    lower = list(combo = lower_mix, discrete = lower_cat, continuous = GGally::wrap(lower_cont, alpha = alpha))
  )
  if (add_title) figure = figure + ggplot2::ggtitle(tools::file_path_sans_ext(basename(vaeac_model_path)))

  return(figure)
}

