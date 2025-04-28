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
#' @inheritParams default_doc_export
#'
#' @export
#' @author Lars Henry Berge Olsen
setup_approach.vaeac <- function(internal,
                                 vaeac.depth = 3,
                                 vaeac.width = 32,
                                 vaeac.latent_dim = 8,
                                 vaeac.activation_function = torch::nn_relu,
                                 vaeac.lr = 0.001,
                                 vaeac.n_vaeacs_initialize = 4,
                                 vaeac.epochs = 100,
                                 vaeac.extra_parameters = list(),
                                 ...) {
  verbose <- internal$parameters$verbose

  # Check that torch is installed
  if (!requireNamespace("torch", quietly = TRUE)) {
    cli::cli_abort("`torch` is not installed. Please run {.run install.packages('torch')}.")
  }
  if (!torch::torch_is_installed()) {
    cli::cli_abort("`torch` is not properly installed. Please run {.run torch::install_torch()}.")
  }

  # Extract the objects we will use later
  iter <- length(internal$iter_list)
  X <- internal$iter_list[[iter]]$X
  S <- internal$iter_list[[iter]]$S
  S_causal <- internal$iter_list[[iter]]$S_causal_steps_unique_S # NULL if not causal sampling
  causal_sampling <- internal$parameters$causal_sampling # NULL if not causal sampling
  parameters <- internal$parameters

  # Check if we are doing a combination of approaches
  combined_approaches <- length(parameters$approach) > 1

  # Ensure that `parameters$vaeac.extra_parameters` is a named list
  if (is.null(parameters$vaeac.extra_parameters)) parameters$vaeac.extra_parameters <- list()
  if (!is.list(parameters$vaeac.extra_parameters)) cli::cli_abort("`vaeac.extra_parameters` must be a list.")
  if (length(parameters$vaeac.extra_parameters) > 0) vaeac_check_extra_named_list(parameters$vaeac.extra_parameters)

  # Ensure that all vaeac parameters are in their right location
  parameters <- vaeac_update_para_locations(parameters = parameters)

  # Extract the default values defined for the vaeac parameters in this function
  vaeac_main_para_names <- methods::formalArgs(setup_approach.vaeac)
  vaeac_main_para_names <- vaeac_main_para_names[!vaeac_main_para_names %in% c("internal", "...")]
  vaeac_main_para <- mget(vaeac_main_para_names)

  # Add the default extra parameter values for the non-user specified extra parameters
  parameters$vaeac.extra_parameters <-
    utils::modifyList(vaeac_get_extra_para_default(), parameters$vaeac.extra_parameters, keep.null = TRUE)

  # Add the default main parameter values for the non-user specified main parameters
  parameters <- utils::modifyList(vaeac_main_para, parameters, keep.null = TRUE)

  # Reorder them such that the vaeac parameters are at the end of the parameters list
  parameters <- c(parameters[(length(vaeac_main_para) + 1):length(parameters)], parameters[seq_along(vaeac_main_para)])

  # Check if vaeac is to be applied on a subset of coalitions.
  if (isTRUE(causal_sampling)) {
    # We are doing causal Shapley values. Then we do not want to train on the full
    # coalitions, but rather the coalitions in the chain of sampling steps used
    # to generate the full MC sample. Casual Shapley does not support combined
    # approaches, so we do not have to check for that. All coalitions are
    # done by vaeac, and we give them equal importance. Skip the empty and grand coalitions.
    # Note that some steps occur more often (when features in Sbar are late in the causal ordering),
    # and one can potentially consider to give this more weight.
    nrow_S_causal <- nrow(S_causal)
    parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions <- S_causal[-c(1, nrow_S_causal), , drop = FALSE]
    parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions_prob <- rep(1, nrow_S_causal - 2) / (nrow_S_causal - 2)
  } else if (!parameters$exact || parameters$is_groupwise || combined_approaches) {
    # We have either:
    # 1) sampled `n_coalitions` different subsets of coalitions (i.e., not exact),
    # 2) using the coalitions which respects the groups in group Shapley values, and/or
    # 3) using a combination of approaches where vaeac is only used on a subset of the coalitions.
    # Here, objects$S contains the coalitions while objects$X contains the information about the approach.

    # Extract the the coalitions / masks which are estimated using vaeac as a matrix
    parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions <-
      S[X[approach == "vaeac"]$id_coalition, , drop = FALSE]

    # Extract the weights for the corresponding coalitions / masks.
    parameters$vaeac.extra_parameters$vaeac.mask_gen_coalitions_prob <-
      X$shapley_weight[X[approach == "vaeac"]$id_coalition]

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
    if ("vS_details" %in% verbose) {
      cli::cli_text(paste0(
        "Training the `vaeac` model with the provided parameters from scratch on ",
        ifelse(parameters$vaeac.extra_parameter$vaeac.cuda, "GPU", "CPU"), "."
      ))
    }

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
    if ("vS_details" %in% verbose) cli::cli_text("Loading the provided `vaeac` model.")

    # Boolean representing that a pre-trained vaeac model was provided
    parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model_provided <- TRUE

    # Check some aspects of the pre-trained vaeac model and add it to the parameters list if it passes the checks
    parameters <- vaeac_update_pretrained_model(parameters = parameters)

    # Small printout informing about the location of the model
    if ("vS_details" %in% verbose) {
      cli::cli_text(paste0(
        "The `vaeac` model runs/is trained on ", ifelse(parameters$vaeac$parameters$cuda, "GPU", "CPU"), "."
      ))
    }
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

  if ("vS_details" %in% verbose) {
    folder_to_save_model <- parameters$vaeac$parameters$folder_to_save_model
    vaeac_save_file_names <- parameters$vaeac$parameters$vaeac_save_file_names

    cli::cli_alert_info(c(
      "The trained `vaeac` models are saved to folder {.path {folder_to_save_model}} at\n",
      "{.path {vaeac_save_file_names[1]}}\n",
      "{.path {vaeac_save_file_names[2]}}\n",
      "{.path {vaeac_save_file_names[3]}}"
    ))
  }


  # Return the updated internal list.
  return(internal)
}

#' @inheritParams default_doc_export
#'
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.vaeac <- function(internal, index_features = NULL, ...) {
  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  S <- internal$iter_list[[iter]]$S

  # If not provided, then set `index_features` to all non trivial coalitions
  if (is.null(index_features)) index_features <- seq(2, n_coalitions - 1)

  # Extract objects we are going to need later
  seed <- internal$parameters$seed
  verbose <- internal$parameters$verbose
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_MC_samples <- internal$parameters$n_MC_samples
  vaeac.model <- internal$parameters$vaeac.model
  vaeac.sampler <- internal$parameters$vaeac.sampler
  vaeac.checkpoint <- internal$parameters$vaeac.checkpoint
  vaeac.batch_size_sampling <- internal$parameters$vaeac.extra_parameters$vaeac.batch_size_sampling

  # Apply all coalitions to all explicands to get a data table where `vaeac` will impute the `NaN` values
  x_explain_extended <- vaeac_get_x_explain_extended(x_explain = x_explain, S = S, index_features = index_features)

  # Set the number of observations do generate the MC samples for at the time.
  n_explain_extended <- nrow(x_explain_extended)
  batch_size <- if (is.null(vaeac.batch_size_sampling)) n_explain_extended else vaeac.batch_size_sampling
  if (batch_size > n_explain_extended) batch_size <- n_explain_extended

  # Impute the missing entries using the vaeac approach.
  x_explain_with_MC_samples_dt <- vaeac_impute_missing_entries(
    x_explain_with_NaNs = x_explain_extended,
    n_explain = n_explain,
    n_MC_samples = n_MC_samples,
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


# Train functions ======================================================================================================
#' Train the Vaeac Model
#'
#' @description Function that fits a vaeac model to the given dataset based on the provided parameters,
#' as described in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}. Note that
#' all default parameters specified below origin from [shapr::setup_approach.vaeac()] and
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
#' @param cuda Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will be trained using cuda/GPU.
#' If [torch::cuda_is_available()] is `FALSE`, the we fall back to use CPU. If `FALSE`, we use the CPU. Using a GPU
#' for smaller tabular dataset often do not improve the efficiency.
#' See \code{vignette("installation", package = "torch")} fo help to enable running on the GPU (only Linux and Windows).
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
#' layer, see [shapr::skip_connection()]. That is, we add the input \eqn{X} to the outcome of each hidden layer,
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
#' [shapr::mcar_mask_generator()] (MCAR = Missing Completely At Random). The MCAR masking scheme ensures that `vaeac`
#' model can do arbitrary conditioning as all coalitions will be trained. `masking_ratio` will be overruled if
#' `mask_gen_coalitions` is specified.
#' @param mask_gen_coalitions Matrix (default is `NULL`). Matrix containing the coalitions that the
#' `vaeac` model will be trained on, see [shapr::specified_masks_mask_generator()]. This parameter is used internally
#' in `shapr` when we only consider a subset of coalitions, i.e., when
#' `n_coalitions` \eqn{< 2^{n_{\text{features}}}}, and for group Shapley, i.e.,
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
#' the model. Useful if one are to continue to train the model later using [shapr::vaeac_train_model_continue()].
#' @param log_exp_cont_feat Logical (default is `FALSE`). If we are to \eqn{\log} transform all
#' continuous features before sending the data to [shapr::vaeac()]. The `vaeac` model creates unbounded Monte Carlo
#' sample values. Thus, if the continuous features are strictly positive (as for, e.g., the Burr distribution and
#' Abalone data set), it can be advantageous to \eqn{\log} transform the data to unbounded form before using `vaeac`.
#' If `TRUE`, then [shapr::vaeac_postprocess_data()] will take the \eqn{\exp} of the results to get back to strictly
#' positive values when using the `vaeac` model to impute missing values/generate the Monte Carlo samples.
#' @param seed Positive integer (default is `1`). Seed for reproducibility. Specifies the seed before any randomness
#' based code is being run.
#' @param which_vaeac_model String (default is `best`). The name of the `vaeac` model (snapshots from different
#' epochs) to use when generating the Monte Carlo samples. The standard choices are: `"best"` (epoch with lowest IWAE),
#' `"best_running"` (epoch with lowest running IWAE, see `vaeac.running_avg_n_values`), and `last` (the last epoch).
#' Note that additional choices are available if `vaeac.save_every_nth_epoch` is provided. For example, if
#' `vaeac.save_every_nth_epoch = 5`, then `vaeac.which_vaeac_model` can also take the values `"epoch_5"`, `"epoch_10"`,
#' `"epoch_15"`, and so on.
#' @inheritParams explain
#' @param ... List of extra parameters, currently not used.
#'
#' @return A list containing the training/validation errors and paths to where the vaeac models are saved on the disk.
#' @export
#' @author Lars Henry Berge Olsen
#' @references
#'   - \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{
#'   Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using Shapley values and variational autoencoders to
#'   explain predictive models with dependent mixed features. Journal of machine learning research, 23(213), 1-51}
#' @keywords internal
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
  # Set seed for reproducibility if provided by the user. Both in R and torch.
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

  # Set epochs_early_stopping to epochs to ensure that early stopping never occurs
  if (is.null(epochs_early_stopping)) epochs_early_stopping <- epochs

  # Check all the vaeac parameters
  do.call(vaeac_check_parameters, mget(methods::formalArgs(vaeac_train_model)))

  # Check if we can use cuda
  if (cuda) cuda <- vaeac_check_cuda(cuda, verbose)

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
  if (requireNamespace("progressr", quietly = TRUE)) {
    progressr_bar <- progressr::progressor(steps = epochs_initiation_phase * (n_vaeacs_initialize - 1) + epochs)
  } else {
    progressr_bar <- NULL
  }

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

    # Send the model to the GPU, if we have access to it and user wants to
    if (cuda) vaeac_model$cuda()

    # Add the number of trainable parameters in the vaeac model to the state list
    if (initialization_idx == 1) {
      state_list$n_trainable_parameters <- vaeac_model$n_train_param
      if ("vS_details" %in% verbose) {
        cli::cli_text(paste0("The vaeac model contains ", vaeac_model$n_train_param[1, 1], " trainable parameters."))
      }
    }

    # Print which initialization vaeac the function is working on
    if ("vS_details" %in% verbose) {
      cli::cli_text(paste0("Initializing vaeac model number ", initialization_idx, " of ", n_vaeacs_initialize, "."))
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

  # Check if we are printing detailed debug information
  # Small printout to the user stating which initiated vaeac model was the best.
  if ("vS_details" %in% verbose) {
    cli::cli_text(paste0(
      "Best vaeac inititalization was number ", vaeac_model_best_list$initialization_idx, " (of ", n_vaeacs_initialize,
      ") with a training VLB = ", round(as.numeric(vaeac_model_best_list$train_vlb[-1]$cpu()), 3),
      " after ", epochs_initiation_phase, " epochs. Continue to train this inititalization."
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

#' Function used to train a `vaeac` model
#'
#' @description
#' This function can be applied both in the initialization phase when, we train several initiated `vaeac` models, and
#' to keep training the best performing `vaeac` model for the remaining number of epochs. We are in the former setting
#' when `initialization_idx` is provided and the latter when it is `NULL`. When it is `NULL`, we save the `vaeac` models
#' with lowest VLB, IWAE, running IWAE, and the epochs according to `save_every_nth_epoch` to disk.
#'
#' @inheritParams vaeac_train_model
#' @param vaeac_model A [shapr::vaeac()] object. The `vaeac` model this function is to train.
#' @param optimizer A [torch::optimizer()] object. See [shapr::vaeac_get_optimizer()].
#' @param train_dataloader A [torch::dataloader()] containing the training data for the `vaeac` model.
#' @param val_dataloader A [torch::dataloader()] containing the validation data for the `vaeac` model.
#' @param train_vlb A [torch::torch_tensor()] (default is `NULL`)
#' of one dimension containing previous values for the training VLB.
#' @param val_iwae A [torch::torch_tensor()] (default is `NULL`)
#' of one dimension containing previous values for the validation IWAE.
#' @param val_iwae_running A [torch::torch_tensor()] (default is `NULL`)
#' of one dimension containing previous values for the running validation IWAE.
#' @param progressr_bar A [progressr::progressor()] object (default is `NULL`) to keep track of progress.
#' @param epochs_start Positive integer (default is `1`). At which epoch the training is starting at.
#' @param vaeac_save_file_names Array of strings containing the save file names for the `vaeac` model.
#' @param state_list Named list containing the objects returned from [shapr::vaeac_get_full_state_list()].
#' @param initialization_idx Positive integer (default is `NULL`). The index
#' of the current `vaeac` model in the initialization phase.
#'
#' @return Depending on if we are in the initialization phase or not. Then either the trained `vaeac` model, or
#' a list of where the `vaeac` models are stored on disk and the parameters of the model.
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_train_model_auxiliary <- function(vaeac_model,
                                        optimizer,
                                        train_dataloader,
                                        val_dataloader,
                                        val_iwae_n_samples,
                                        running_avg_n_values,
                                        verbose,
                                        cuda,
                                        epochs,
                                        save_every_nth_epoch,
                                        epochs_early_stopping,
                                        epochs_start = 1,
                                        progressr_bar = NULL,
                                        vaeac_save_file_names = NULL,
                                        state_list = NULL,
                                        initialization_idx = NULL,
                                        n_vaeacs_initialize = NULL,
                                        train_vlb = NULL,
                                        val_iwae = NULL,
                                        val_iwae_running = NULL) {
  # Check for valid input
  if (xor(is.null(initialization_idx), is.null(n_vaeacs_initialize))) {
    cli::cli_abort("Either none or both of `initialization_idx` and `n_vaeacs_initialize` must be given.")
  }

  if (is.null(state_list) && is.null(initialization_idx)) {
    cli::cli_abort("`state_list` must be provide when `initialization_idx = NULL` to properly save the `vaeac` model.")
  }

  if (is.null(vaeac_save_file_names) && is.null(initialization_idx)) {
    cli::cli_abort(paste0(
      "`vaeac_save_file_names` must be provide when `initialization_idx = NULL` ",
      "to know where to save the vaeac model."
    ))
  }

  if (!((is.null(train_vlb) && is.null(val_iwae) && is.null(val_iwae_running)) ||
    (!is.null(train_vlb) && !is.null(val_iwae) && !is.null(val_iwae_running)))) {
    cli::cli_abort("Either none or all of `train_vlb`, `val_iwae`, and `val_iwae_running` must be given.")
  }

  # Variable that we change to `TRUE` if early stopping is applied
  if (!is.null(state_list)) state_list$early_stopping_applied <- FALSE

  # Variable to store the epochs of the `vaeac` at the best epoch according to IWAE and IWAE_running
  if (is.null(initialization_idx)) best_epoch <- best_epoch_running <- NULL

  # Get the batch size
  batch_size <- train_dataloader$batch_size

  # Extract the mask generator and the variational lower bound scale factor from the vaeac model object.
  mask_generator <- vaeac_model$mask_generator
  vlb_scale_factor <- vaeac_model$vlb_scale_factor

  # Start the training loop
  for (epoch in seq(from = epochs_start, to = epochs)) {
    avg_vlb <- 0 # Set average variational lower bound to 0 for this epoch
    batch_index <- 1 # Index to keep track of which batch we are working on

    # Iterate over the training data
    coro::loop(for (batch in train_dataloader) {
      # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
      if (batch$shape[1] < batch_size) {
        batch <- vaeac_extend_batch(batch = batch, dataloader = train_dataloader, batch_size = batch_size)
      }

      # Generate mask and do an optimizer step over the mask and the batch
      mask <- mask_generator(batch)

      # Send the batch and mask to GPU if we have access to it and user wants to
      if (cuda) {
        batch <- batch$cuda()
        mask <- mask$cuda()
      }

      # Set all previous gradients to zero.
      optimizer$zero_grad()

      # Compute the variational lower bound for the batch given the mask
      vlb <- vaeac_model$batch_vlb(batch, mask)$mean()

      # Backpropagation: minimize the negative vlb.
      vlb_loss <- (-vlb / vlb_scale_factor)
      vlb_loss$backward()

      # Update the vaeac_model parameters by using the optimizer
      optimizer$step()

      # Update running variational lower bound average using the recursive average formula/update.
      # a + (new - a)/(i+1) = {(i+1)a + new - a}/(i+1) = { a(i) + new}/(i+1) = a *i/(i+1) + new/(i+1)
      avg_vlb <- avg_vlb + (vlb$to(dtype = torch::torch_float())$clone()$detach() - avg_vlb) / batch_index

      # Update the batch index.
      batch_index <- batch_index + 1
    }) # Done with one new epoch

    ## Time to evaluate the vaeac_model on the validation data, potentially save it, and check for early stopping.

    # Store the VLB
    train_vlb <- torch::torch_cat(c(train_vlb, avg_vlb), -1)

    # Compute the validation IWAE
    val_iwae_now <- vaeac_get_val_iwae(
      val_dataloader = val_dataloader,
      mask_generator = mask_generator,
      batch_size = batch_size,
      vaeac_model = vaeac_model,
      val_iwae_n_samples = val_iwae_n_samples
    )
    val_iwae <- torch::torch_cat(c(val_iwae, val_iwae_now), -1)

    # Compute the running validation IWAE
    val_iwae_running_now <-
      val_iwae[
        (-min(length(val_iwae), running_avg_n_values) +
          length(val_iwae) + 1):(-1 + length(val_iwae) + 1),
        drop = FALSE
      ]$mean()$view(1)
    val_iwae_running <- torch::torch_cat(c(val_iwae_running, val_iwae_running_now), -1)

    # Check if we are to save the models
    if (is.null(initialization_idx)) {
      # Save if current vaeac model has the lowest validation IWAE error
      if ((max(val_iwae) <= val_iwae_now)$item() || is.null(best_epoch)) {
        best_epoch <- epoch
        vaeac_save_state(state_list = state_list, file_name = vaeac_save_file_names[1])
      }

      # Save if current vaeac model has the lowest running validation IWAE error
      if ((max(val_iwae_running) <= val_iwae_running_now)$item() || is.null(best_epoch_running)) {
        best_epoch_running <- epoch
        vaeac_save_state(state_list = state_list, file_name = vaeac_save_file_names[2])
      }

      # Save if we are in an n'th epoch and are to save every n'th epoch
      if (is.numeric(save_every_nth_epoch) && epoch %% save_every_nth_epoch == 0) {
        vaeac_save_state(state_list = state_list, file_name = vaeac_save_file_names[3 + epoch %/% save_every_nth_epoch])
      }
    }

    # Handle the message to the progress bar based on if we are doing initialization or final training
    if (!is.null(progressr_bar)) {
      update_message <- if (!is.null(initialization_idx)) {
        paste0(
          "Training vaeac (init. ", initialization_idx, " of ", n_vaeacs_initialize, "): Epoch: ", epoch,
          " | VLB: ", vaeac_get_n_decimals(avg_vlb$item()), " | IWAE: ", vaeac_get_n_decimals(val_iwae_now$item()), " |"
        )
      } else {
        paste0(
          "Training vaeac (final model): Epoch: ", epoch, " | best epoch: ", best_epoch,
          " | VLB: ", vaeac_get_n_decimals(avg_vlb$item()), " | IWAE: ", vaeac_get_n_decimals(val_iwae_now$item()), " |"
        )
      }
      progressr_bar(message = update_message)
    }

    # Check if we are to apply early stopping, i.e., no improvement in the IWAE for `epochs_early_stopping` epochs.
    if (is.numeric(epochs_early_stopping)) {
      if (epoch - best_epoch >= epochs_early_stopping) {
        if ("vS_details" %in% verbose) {
          cli::cli_text(paste0(
            "No IWAE improvment in ", epochs_early_stopping, " epochs. Apply early stopping at epoch ",
            epoch, "."
          ))
        }
        if (!is.null(progressr_bar)) progressr_bar("Training vaeac (early stopping)", amount = epochs - epoch)
        state_list$early_stopping_applied <- TRUE # Add that we did early stopping to the state list
        state_list$epochs <- epoch # Update the number of used epochs.
        break # Stop the training loop
      }
    }
  } # Done with all epochs in training phase

  # Find out what to return
  if (!is.null(initialization_idx)) {
    # Here we return the models and the optimizer which we will train further if this was the best initialization
    return_list <- list(
      vaeac_model = vaeac_model,
      optimizer = optimizer,
      train_vlb = train_vlb,
      val_iwae = val_iwae,
      val_iwae_running = val_iwae_running,
      avg_vlb = avg_vlb,
      initialization_idx = initialization_idx,
      state_list = state_list
    )
  } else {
    # Save the vaeac model at the last epoch
    last_state <- vaeac_save_state(state_list = state_list, file_name = vaeac_save_file_names[3], return_state = TRUE)

    # Summary printout
    if ("vS_details" %in% verbose) vaeac_print_train_summary(best_epoch, best_epoch_running, last_state)

    # Create a return list
    return_list <- list(
      best = vaeac_save_file_names[1],
      best_running = vaeac_save_file_names[2],
      last = vaeac_save_file_names[3],
      train_vlb = as.array(train_vlb$cpu()),
      val_iwae = as.array(val_iwae$cpu()),
      val_iwae_running = as.array(val_iwae_running$cpu()),
      parameters = last_state
    )

    # Add the potentially additional save names
    if (!is.null(vaeac_save_file_names) && length(vaeac_save_file_names) > 3) {
      return_list <- append(
        return_list,
        setNames(
          as.list(vaeac_save_file_names[-(1:3)]),
          paste0("epoch_", save_every_nth_epoch * seq(length(vaeac_save_file_names) - 3))
        ),
        3
      )
    }

    # Update the class of the returned object
    attr(return_list, "class") <- c("vaeac", class(return_list))
  }
  return(return_list)
}

#' Continue to Train the vaeac Model
#'
#' @description Function that loads a previously trained vaeac model and continue the training, either
#' on new data or on the same dataset as it was trained on before. If we are given a new dataset, then
#' we assume that new dataset has the same distribution and one_hot_max_sizes as the original dataset.
#'
#' @inheritParams vaeac_train_model
#' @param explanation A [shapr::explain()] object and `vaeac` must be the used approach.
#' @param epochs_new Positive integer. The number of extra epochs to conduct.
#' @param lr_new Positive numeric. If we are to overwrite the old learning rate in the adam optimizer.
#'
#' @return A list containing the training/validation errors and paths to where the vaeac models are saved on the disk.
#' @export
#' @inherit vaeac_train_model references
#' @author Lars Henry Berge Olsen
vaeac_train_model_continue <- function(explanation,
                                       epochs_new,
                                       lr_new = NULL,
                                       x_train = NULL,
                                       save_data = FALSE,
                                       verbose = NULL,
                                       seed = 1) {
  # Check the input
  if (!"shapr" %in% class(explanation)) cli::cli_abort("`explanation` must be a list of class `shapr`.")
  if (!"vaeac" %in% explanation$internal$parameters$approach) {
    cli::cli_abort("`vaeac` is not an approach in `explanation`.")
  }
  if (!is.null(lr_new)) vaeac_check_positive_numerics(list(lr_new = lr_new))
  if (!is.null(x_train) && !data.table::is.data.table(x_train)) {
    cli::cli_abort("`x_train` must be a `data.table` object.")
  }
  check_verbose(verbose)
  vaeac_check_positive_integers(list(epochs_new = epochs_new, seed = seed))
  vaeac_check_logicals(list(save_data = save_data))

  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)


  # Extract the vaeac list and load the model at the last epoch or the best (default 'best' when path is provided)
  vaeac_model <- explanation$internal$parameters$vaeac
  vaeac_model_path <- if (!is.null(vaeac_model$models$last)) vaeac_model$models$last else vaeac_model$models$best
  checkpoint <- torch::torch_load(vaeac_model_path)

  # Get which device we are to continue to train the model
  device <- ifelse(checkpoint$cuda, "cuda", "cpu")

  # If we applied early stopping before and are calling this function, then we turn early stopping off
  if (isTRUE(checkpoint$early_stopping_applied)) checkpoint$epochs_early_stopping <- epochs_new

  # Check for access to a single training data set and use the data from the checkpoint if `x_train` is not provided
  if (is.null(checkpoint$normalized_data) && is.null(x_train)) {
    if ("basic" %in% verbose) {
      msg <- "The `vaeac` model did not include data (set `vaeac.save_data = TRUE in `explain()`) and `x_train = NULL`."
      cli::cli_abort(msg)
    }
  }
  if (!is.null(checkpoint$x_train) && !is.null(x_train)) {
    if ("basic" %in% verbose) {
      msg <- "The `vaeac` model includes data and `x_train` was provided to this function. We only use `x_train`."
      cli::cli_inform(c("i" = msg))
    }
  }
  if (is.null(x_train)) x_train <- checkpoint$x_train

  # Check that the provided vaeac model is trained on a dataset with the same feature names
  vaeac_check_x_colnames(feature_names_vaeac = checkpoint$feature_list$labels, feature_names_new = names(x_train))

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
      x_train_size = format(utils::object.size(x_train), units = "auto"),
      verbose = verbose
    )
  }

  # Add the new state list as a list to the checkpoint
  n_times_continued_trained <- sum(grepl("state_list_new", names(checkpoint)))
  state_list_new_name <- paste("state_list_new", n_times_continued_trained + 1, sep = "_")
  state_list <- checkpoint
  state_list[[state_list_new_name]] <- state_list_new

  # Set up the vaeac model in training mode and based on the parameters stored in the checkpoint
  vaeac_model <- vaeac_get_model_from_checkp(checkpoint = checkpoint, cuda = checkpoint$cuda, mode_train = TRUE)

  # Send the loaded optimizer parameters to GPU if necessary
  if (checkpoint$cuda) {
    checkpoint$optimizer_state_dict$state <- lapply(
      checkpoint$optimizer_state_dict$state,
      function(x) lapply(x, function(y) if ("torch_tensor" %in% class(y)) y$cuda() else y)
    )
  }

  # Specify the learning rate we will use, create the an adam optimizer, and insert the stored optimizer state.
  lr_now <- if (!is.null(lr_new)) lr_new else checkpoint$lr
  optimizer <- vaeac_get_optimizer(vaeac_model = vaeac_model, lr = lr_now, optimizer_name = "adam")
  optimizer$load_state_dict(checkpoint$optimizer_state_dict)

  # Compute the new number of epochs
  epochs_old <- checkpoint$epochs
  epochs <- epochs_old + epochs_new
  state_list$epochs <- epochs

  # Create a `progressr::progressor()` to keep track of the new training
  if (requireNamespace("progressr", quietly = TRUE)) {
    progressr_bar <- progressr::progressor(steps = epochs_new)
  } else {
    progressr_bar <- NULL
  }

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
    train_vlb = checkpoint$train_vlb$to(device = device), # Send to correct device such that we can append new values
    val_iwae = checkpoint$val_iwae$to(device = device),
    val_iwae_running = checkpoint$val_iwae_running$to(device = device)
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


# Imputation functions =================================================================================================
#' Impute Missing Values Using Vaeac
#'
#' @details  Function that imputes the missing values in 2D matrix where each row constitute an individual.
#' The values are sampled from the conditional distribution estimated by a vaeac model.
#'
#' @inheritParams vaeac_train_model
#' @param x_explain_with_NaNs A 2D matrix, where the missing entries to impute are represented by `NaN`.
#' @param n_MC_samples Integer. The number of imputed versions we create for each row in `x_explain_with_NaNs`.
#' @param index_features Optional integer vector. Used internally in shapr package to index the coalitions.
#' @param n_explain Positive integer. The number of explicands.
#' @param vaeac_model An initialized `vaeac` model that we are going to use to generate the MC samples.
#' @param checkpoint List containing the parameters of the `vaeac` model.
#' @param sampler A sampler object used to sample the MC samples.
#'
#' @return A data.table where the missing values (`NaN`) in `x_explain_with_NaNs` have been imputed `n_MC_samples`
#' times.
#' The data table will contain extra id columns if `index_features` and `n_explain` are provided.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_impute_missing_entries <- function(x_explain_with_NaNs,
                                         n_MC_samples,
                                         vaeac_model,
                                         checkpoint,
                                         sampler,
                                         batch_size,
                                         verbose = NULL,
                                         seed = NULL,
                                         n_explain = NULL,
                                         index_features = NULL) {
  # We only need `n_explain` when `index_features` is provided
  if (xor(is.null(index_features), is.null(n_explain))) {
    cli::cli_abort("Either none or both of `index_features` and `n_explain` must be given.")
  }

  # Set seed for reproducibility if provided by the user. Both in R and torch.
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

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

  # Create an auxiliary list of lists to store the imputed values combined with the original values. The structure is
  # [[i'th MC sample]][[b'th batch]], where the entries are tensors of dimension batch_size x n_features.
  results <- lapply(seq(n_MC_samples), function(k) list())

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
      # This is a tensor of shape [batch_size, n_MC_samples, n_generative_parameters]. Note that, for only continuous
      # features we have that n_generative_parameters = 2*n_features, but for categorical data the number depends
      # on the number of categories.
      samples_params <- vaeac_model$generate_samples_params(
        batch = batch_extended,
        mask = mask_extended,
        K = n_MC_samples
      )

      # Remove the parameters belonging to added instances in batch_extended.
      samples_params <- samples_params[1:batch$shape[1], , ]
    })

    # Make a deep copy of the batch with missing values set to zero.
    mask <- torch::torch_isnan(batch)
    batch_zeroed_nans <- batch$clone()$detach()
    batch_zeroed_nans[mask] <- 0

    # Iterate over the number of imputations and generate the imputed samples
    for (i in seq(n_MC_samples)) {
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
    } # End of iterating over the n_MC_samples
  }) # End of iterating over the batches. Done imputing.

  # Order the MC samples into a tensor of shape [nrow(x_explain_with_NaNs), n_MC_samples, n_features].
  # The lapply function
  # creates a list of tensors of shape [nrow(x_explain_with_NaNs), 1, n_features] by concatenating the batches for the
  # i'th MC sample to a tensor of shape [nrow(x_explain_with_NaNs), n_features] and then add unsqueeze to add a new
  # singleton dimension as the second dimension to get the shape [nrow(x_explain_with_NaNs), 1, n_features]. Then
  # outside of the lapply function, we concatenate the n_MC_samples torch elements to form a final torch result of shape
  # [nrow(x_explain_with_NaNs), n_MC_samples, n_features].
  result <- torch::torch_cat(lapply(
    seq(n_MC_samples),
    function(i) torch::torch_cat(results[[i]])$unsqueeze(2)
  ), dim = 2)

  # Get back to the original distribution by undoing the normalization by multiplying with the std and adding the mean
  result <- result * checkpoint$norm_std + checkpoint$norm_mean

  # Convert from a tensor of shape [nrow(x_explain_with_NaNs), n_MC_samples, n_features]
  # to a matrix of shape [(nrow(x_explain_with_NaNs) * n_MC_samples), n_features].
  result <- data.table::as.data.table(as.matrix(result$view(c(
    result$shape[1] * result$shape[2],
    result$shape[3]
  ))$detach()$cpu()))

  # Post-process the data such that categorical features have original level names and convert to a data table.
  result <- vaeac_postprocess_data(data = result, vaeac_model_state_list = checkpoint)

  # If user provide `index_features`, then we add columns needed for shapr computations
  if (!is.null(index_features)) {
    # Add id, id_coalition and weights (uniform for the `vaeac` approach) to the result.
    result[, c("id", "id_coalition", "w") := list(
      rep(x = seq(n_explain), each = length(index_features) * n_MC_samples),
      rep(x = index_features, each = n_MC_samples, times = n_explain),
      1 / n_MC_samples
    )]

    # Set the key in the data table
    data.table::setkeyv(result, c("id", "id_coalition"))
  }

  return(result)
}

# Check functions ======================================================================================================
#' Check vaeac.extra_parameters list
#'
#' @param vaeac.extra_parameters List containing the extra parameters to the `vaeac` approach
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_check_extra_named_list <- function(vaeac.extra_parameters) {
  names <- names(vaeac.extra_parameters)
  if (is.null(names)) cli::cli_abort("The parameter `vaeac.extra_parameters` is not a named list.")
  if (any(names == "")) cli::cli_abort("Not all parameters in the list `vaeac.extra_parameters` are named.")
}

#' Function that checks positive integers
#'
#' @param named_list_positive_integers List containing named entries. I.e., `list(a = 1, b = 2)`.
#'
#' @return The function does not return anything.
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_positive_integers <- function(named_list_positive_integers) {
  param_names <- names(named_list_positive_integers)
  for (idx in seq_len(length(named_list_positive_integers))) {
    param_name <- param_names[idx]
    value <- named_list_positive_integers[[param_name]]
    if (!is.numeric(value) || length(value) != 1 || value <= 0 || !is.finite(value) || value %% 1 != 0) {
      cli::cli_abort(paste0("'vaeac.", param_name, "' must be a positive integer."))
    }
  }
}

#' Function that checks positive numerics
#'
#' @param named_list_positive_numerics List containing named entries. I.e., `list(a = 0.2, b = 10^3)`.
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_positive_numerics <- function(named_list_positive_numerics) {
  param_names <- names(named_list_positive_numerics)
  for (idx in seq_len(length(named_list_positive_numerics))) {
    param_name <- param_names[idx]
    value <- named_list_positive_numerics[[param_name]]
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value <= 0) {
      cli::cli_abort(paste0("'vaeac.", param_name, "' must be a positive numeric."))
    }
  }
}

#' Function that checks probabilities
#'
#' @param named_list_probabilities List containing named entries. I.e., `list(a = 0.2, b = 0.9)`.
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_probabilities <- function(named_list_probabilities) {
  # Trick needed for entries that can be vectors (i.e., `vaeac.masking_ratio`)
  named_list_probabilities_tmp <- as.list(unlist(named_list_probabilities))
  param_names <- names(named_list_probabilities_tmp)
  for (idx in seq_len(length(named_list_probabilities_tmp))) {
    param_name <- param_names[idx]
    value <- named_list_probabilities_tmp[[param_name]]
    if (!is.numeric(value) || length(value) != 1 || !is.finite(value) || value < 0 || value > 1) {
      cli::cli_abort(paste0("'vaeac.", param_name, "' must be a valid probability (a number between 0 and 1)."))
    }
  }
}

#' Function that checks logicals
#'
#' @param named_list_logicals List containing named entries. I.e., `list(a = TRUE, b = FALSE)`.
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_logicals <- function(named_list_logicals) {
  param_names <- names(named_list_logicals)
  for (idx in seq_len(length(named_list_logicals))) {
    param_name <- param_names[idx]
    value <- named_list_logicals[[param_name]]
    if (!is.logical(value) || length(value) != 1) {
      cli::cli_abort(paste0("'vaeac.", param_name, "' must be a boolean (i.e., `TRUE` or `FALSE`)."))
    }
  }
}

#' Function that checks for valid `vaeac` model name
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_which_vaeac_model <- function(which_vaeac_model, epochs, save_every_nth_epoch = NULL) {
  valid_names <- c("best", "best_running", "last")
  if (!is.null(save_every_nth_epoch)) {
    valid_names <- c(
      valid_names,
      paste0("epoch_", seq(
        from = save_every_nth_epoch,
        by = save_every_nth_epoch,
        length.out = floor(epochs / save_every_nth_epoch)
      ))
    )
  }

  if (!is.null(which_vaeac_model) && !is.character(which_vaeac_model)) {
    cli::cli_abort("`vaeac.which_vaeac_model` must be a string.")
  }

  if (!which_vaeac_model %in% valid_names) {
    cli::cli_abort(paste0(
      "The provided `vaeac.which_vaeac_model` ('", which_vaeac_model, "') does not match any of the valid values: '",
      paste(valid_names, collapse = "', '"), "'."
    ))
  }
}

#' Function that checks provided epoch arguments
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_epoch_values <- function(epochs, epochs_initiation_phase, epochs_early_stopping, save_every_nth_epoch,
                                     verbose) {
  if (epochs_initiation_phase >= epochs) {
    cli::cli_abort(paste0(
      "'vaeac.epochs_initiation_phase' (", epochs_initiation_phase, ") must be strictly less than ",
      "'vaeac.epochs' (", epochs, ")."
    ))
  }

  if (epochs_early_stopping > epochs) {
    if ("basic" %in% verbose) {
      msg <- paste0(
        "No early stopping as `vaeac.epochs_early_stopping` (", epochs_early_stopping, ") is larger than ",
        "`vaeac.epochs` (", epochs, ")."
      )
      cli::cli_inform(c("i" = msg))
    }
  }

  # Ensure a valid value for save_every_nth_epoch.
  if (!is.null(save_every_nth_epoch) && save_every_nth_epoch > epochs) {
    cli::cli_abort(
      paste0("Number of 'epochs' (", epochs, ") is less than 'save_every_nth_epoch' (", save_every_nth_epoch, ").")
    )
  }
  # Ensure a valid value for save_every_nth_epoch.
  if (!is.null(save_every_nth_epoch) && save_every_nth_epoch <= epochs_initiation_phase) {
    cli::cli_abort(paste0(
      "Number of 'epochs_initiation_phase' (", epochs_initiation_phase, ") is less than ",
      "'save_every_nth_epoch' (", save_every_nth_epoch, ")."
    ))
  }
}

#' Function that checks the provided activation function
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_activation_func <- function(activation_function) {
  # In future, check that it is one of the activation functions and not just a nn_module
  # Check that activation function is an nn_module
  if (!any("nn_module" %in% class(activation_function))) {
    cli::cli_abort("`vaeac.activation_function` is not an `nn_module`.")
  }
}

#' Function that checks the specified masking scheme
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_mask_gen <- function(mask_gen_coalitions, mask_gen_coalitions_prob, x_train) {
  masks <- mask_gen_coalitions
  probs <- mask_gen_coalitions_prob

  if (!is.null(masks) || !is.null(probs)) {
    if (xor(is.null(masks), is.null(probs))) {
      cli::cli_abort(
        "Either both `vaeac.mask_gen_coalitions` and `vaeac.mask_gen_coalitions_prob` need to `NULL` ",
        "or both have to be specified."
      )
    }

    if (!is.matrix(masks)) cli::cli_abort("`vaeac.mask_gen_coalitions` must be a matrix.")
    if (!is.numeric(probs)) cli::cli_abort("`vaeac.mask_gen_coalitions_prob` must be an array.")

    if (nrow(masks) != length(probs)) {
      cli::cli_abort(
        "The number of rows in `vaeac.mask_gen_coalitions` must be equal to the length of ",
        "`vaeac.mask_gen_coalitions_prob`."
      )
    }

    if (ncol(masks) != ncol(x_train)) {
      cli::cli_abort(
        "The number of columns in `vaeac.mask_gen_coalitions` must be equal to the number of ",
        "columns in the `x_train`. That is, the number of features."
      )
    }
  }
}


#' Function that checks that the save folder exists and for a valid file name
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_save_names <- function(folder_to_save_model, model_description) {
  if (!is.character(folder_to_save_model)) cli::cli_abort("`vaeac.folder_to_save_model` must be a string.")
  if (!is.character(model_description)) cli::cli_abort("`vaeac.model_description` must be a string.")
  if (!dir.exists(folder_to_save_model)) {
    cli::cli_abort(paste0("the folder `vaeac.folder_to_save_model` ('", folder_to_save_model, "') does not exist."))
  }
  if (!grepl("^[A-Za-z0-9._-]+$", model_description)) {
    cli::cli_abort(paste0(
      "`vaeac.model_description` can only contain uppercase and lowercase letters, ",
      "digits, dots, underscores, and hyphens."
    ))
  }
}

#' Function that checks for access to CUDA
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_cuda <- function(cuda, verbose) {
  # Check if cuda/GPU is available on the current system
  cuda_available <- torch::cuda_is_available()

  # Give message to user if asked to run on cuda, but cuda is not available.
  if (isFALSE(cuda_available) && isTRUE(cuda)) {
    cuda <- FALSE
    if ("basic" %in% verbose) {
      msg <- "Cuda/GPU is not available ({.pkg shapr} uses CPU instead)."
      cli::cli_inform(c("i" = msg), immediate. = TRUE)
    }
  }

  return(cuda)
}

#' Function that checks that the masking ratio argument is valid
#'
#' @inheritParams vaeac_train_model
#' @param n_features The number of features, i.e., the number of columns in the training data.
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_masking_ratio <- function(masking_ratio, n_features) {
  if (length(masking_ratio) > 1 && length(masking_ratio) != n_features) {
    cli::cli_abort(paste0(
      "`masking_ratio` contains masking ratios for ',", length(masking_ratio), "' features, ",
      "but there are '", n_features, "' features in 'x_train'."
    ))
  }
}

#' Function that gives a warning about disk usage
#'
#' @param x_train_size The object size of the `x_train` object.
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_save_parameters <- function(save_data, epochs, save_every_nth_epoch, x_train_size, verbose) {
  if (save_data && !is.null(save_every_nth_epoch) && epochs / save_every_nth_epoch > 5) {
    if ("basic" %in% verbose) {
      msg <- paste0(
        "Having `save_data = TRUE` and `save_every_nth_epoch = ", save_every_nth_epoch, "` might require ",
        "a lot of disk storage if `x_train` (", x_train_size, ") is large."
      )
      cli::cli_inform(c("i" = msg))
    }
  }
}

#' Function that checks the feature names of data and `vaeac` model
#'
#' @param feature_names_vaeac Array of strings containing the feature names of the `vaeac` model.
#' @param feature_names_new Array of strings containing the feature names to compare with.
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_x_colnames <- function(feature_names_vaeac, feature_names_new) {
  n_features_vaeac <- length(feature_names_vaeac)
  n_features_new <- length(feature_names_new)

  # Check that the feature names of x_train matches the names of the training data used to train the vaeac model
  if (!isTRUE(all.equal(feature_names_vaeac, feature_names_new))) {
    cli::cli_abort(paste0(
      "The current feature names (`", paste(feature_names_new, collapse = "`, `"), "`) do not match the ",
      "feature names in the provided `vaeac` model (`", paste(feature_names_vaeac, collapse = "`, `"), ")."
    ))
  }

  # Check for equal number of features (this should never occur as test above indirectly checks this too)
  if (n_features_new != n_features_vaeac) {
    cli::cli_abort(paste0(
      "The provided `vaeac` model is trained on a ", n_features_vaeac, "-dimensional dataset, but the current ",
      "dataset is ", n_features_new, "-dimensional."
    ))
  }
}

#' Function that calls all vaeac parameters check functions
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_parameters <- function(x_train,
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
  # Check that the activation function is valid torch::nn_module object
  vaeac_check_activation_func(activation_function = activation_function)

  # Check that the save folder exists and for a valid file name
  vaeac_check_save_names(folder_to_save_model = folder_to_save_model, model_description = model_description)

  # Check the probability parameters
  vaeac_check_probabilities(list(val_ratio = val_ratio, masking_ratio = masking_ratio))

  # Check the masking ratio
  vaeac_check_masking_ratio(masking_ratio = masking_ratio, n_features = ncol(x_train))

  # Check the positive numeric parameters
  vaeac_check_positive_numerics(list(lr = lr, sigma_mu = sigma_mu, sigma_sigma = sigma_sigma))

  # Check the mask_gen_coalitions and mask_gen_coalitions_prob parameters
  vaeac_check_mask_gen(
    mask_gen_coalitions = mask_gen_coalitions,
    mask_gen_coalitions_prob = mask_gen_coalitions_prob,
    x_train = x_train
  )

  # Check the logical parameters
  vaeac_check_logicals(list(
    cuda = cuda,
    skip_conn_layer = skip_conn_layer,
    skip_conn_masked_enc_dec = skip_conn_masked_enc_dec,
    batch_normalization = batch_normalization,
    paired_sampling = paired_sampling,
    save_data = save_data,
    log_exp_cont_feat = log_exp_cont_feat
  ))

  # Check the positive integer parameters
  unchecked_positive_integers <- list(
    n_vaeacs_initialize = n_vaeacs_initialize,
    epochs = epochs,
    epochs_early_stopping = epochs_early_stopping,
    epochs_initiation_phase = epochs_initiation_phase,
    val_iwae_n_samples = val_iwae_n_samples,
    depth = depth,
    width = width,
    latent_dim = latent_dim,
    batch_size = batch_size,
    running_avg_n_values = running_avg_n_values
  )
  if (!is.null(save_every_nth_epoch)) unchecked_positive_integers$save_every_nth_epoch <- save_every_nth_epoch
  vaeac_check_positive_integers(unchecked_positive_integers)

  # Check the epoch values
  vaeac_check_epoch_values(
    epochs = epochs,
    epochs_initiation_phase = epochs_initiation_phase,
    epochs_early_stopping = epochs_early_stopping,
    save_every_nth_epoch = save_every_nth_epoch
  )

  # Check the save parameters
  vaeac_check_save_parameters(
    save_data = save_data,
    epochs = epochs,
    save_every_nth_epoch = save_every_nth_epoch,
    x_train_size = format(utils::object.size(x_train), units = "auto")
  )

  # Check that user want to use the vaeac model at a valid checkpoint
  vaeac_check_which_vaeac_model(
    which_vaeac_model = which_vaeac_model,
    epochs = epochs,
    save_every_nth_epoch = save_every_nth_epoch
  )

  # verbose and seed are already checked
}

# Get functions ========================================================================================================
#' Function to specify the extra parameters in the `vaeac` model
#'
#' @description In this function, we specify the default values for the extra parameters used in [shapr::explain()]
#' for `approach = "vaeac"`.
#'
#' @details
#' The `vaeac` model consists of three neural network (a full encoder, a masked encoder, and a decoder) based
#' on the provided `vaeac.depth` and `vaeac.width`. The encoders map the full and masked input
#' representations to latent representations, respectively, where the dimension is given by `vaeac.latent_dim`.
#' The latent representations are sent to the decoder to go back to the real feature space and
#' provide a samplable probabilistic representation, from which the Monte Carlo samples are generated.
#' We use the `vaeac` method at the epoch with the lowest validation error (IWAE) by default, but
#' other possibilities are available but setting the `vaeac.which_vaeac_model` parameter. See
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)} for more details.
#'
#' @param vaeac.model_description String (default is `make.names(Sys.time())`). String containing, e.g., the name of the
#' data distribution or additional parameter information. Used in the save name of the fitted model. If not provided,
#' then a name will be generated based on [base::Sys.time()] to ensure a unique name. We use [base::make.names()] to
#' ensure a valid file name for all operating systems.
#' @param vaeac.folder_to_save_model String (default is [base::tempdir()]). String specifying a path to a folder where
#' the function is to save the fitted vaeac model. Note that the path will be removed from the returned
#' [shapr::explain()] object if `vaeac.save_model = FALSE`. Furthermore, the model cannot be moved from its
#' original folder if we are to use the [shapr::vaeac_train_model_continue()] function to continue training the model.
#' @param vaeac.pretrained_vaeac_model List or String (default is `NULL`). 1) Either a list of class
#' `vaeac`, i.e., the list stored in `explanation$internal$parameters$vaeac` where `explanation` is the returned list
#' from an earlier call to the [shapr::explain()] function. 2) A string containing the path to where the `vaeac`
#' model is stored on disk, for example, `explanation$internal$parameters$vaeac$models$best`.
#' @param vaeac.cuda Logical (default is `FALSE`). If `TRUE`, then the `vaeac` model will be trained using cuda/GPU.
#' If [torch::cuda_is_available()] is `FALSE`, the we fall back to use CPU. If `FALSE`, we use the CPU. Using a GPU
#' for smaller tabular dataset often do not improve the efficiency.
#' See \code{vignette("installation", package = "torch")} fo help to enable running on the GPU (only Linux and Windows).
#' @param vaeac.epochs_initiation_phase Positive integer (default is `2`). The number of epochs to run each of the
#' `vaeac.n_vaeacs_initialize` `vaeac` models before continuing to train only the best performing model.
#' @param vaeac.epochs_early_stopping Positive integer (default is `NULL`). The training stops if there has been no
#' improvement in the validation IWAE for `vaeac.epochs_early_stopping` epochs. If the user wants the training process
#' to be solely based on this training criterion, then `vaeac.epochs` in [shapr::explain()] should be set to a large
#' number. If `NULL`, then `shapr` will internally set `vaeac.epochs_early_stopping = vaeac.epochs` such that early
#' stopping does not occur.
#' @param vaeac.save_every_nth_epoch Positive integer (default is `NULL`). If provided, then the vaeac model after
#' every `vaeac.save_every_nth_epoch`th epoch will be saved.
#' @param vaeac.val_ratio Numeric (default is `0.25`). Scalar between `0` and `1` indicating the ratio of
#' instances from the input data which will be used as validation data. That is, `vaeac.val_ratio = 0.25` means
#' that `75%` of the provided data is used as training data, while the remaining `25%` is used as validation data.
#' @param vaeac.val_iwae_n_samples Positive integer (default is `25`). The number of generated samples used
#' to compute the IWAE criterion when validating the vaeac model on the validation data.
#' @param vaeac.batch_size Positive integer (default is `64`). The number of samples to include in each batch
#' during the training of the vaeac model. Used in [torch::dataloader()].
#' @param vaeac.batch_size_sampling Positive integer (default is `NULL`) The number of samples to include in
#' each batch when generating the Monte Carlo samples. If `NULL`, then the function generates the Monte Carlo samples
#' for the provided coalitions and all explicands sent to [shapr::explain()] at the time.
#' The number of coalitions are determined by the `n_batches` used by [shapr::explain()]. We recommend to tweak
#' `extra_computation_args$max_batch_size` and `extra_computation_args$min_n_batches`
#' rather than `vaeac.batch_size_sampling`. Larger batch sizes are often much faster provided sufficient memory.
#' @param vaeac.running_avg_n_values Positive integer (default is `5`). The number of previous IWAE values to include
#' when we compute the running means of the IWAE criterion.
#' @param vaeac.skip_conn_layer Logical (default is `TRUE`). If `TRUE`, we apply identity skip connections in each
#' layer, see [shapr::skip_connection()]. That is, we add the input \eqn{X} to the outcome of each hidden layer,
#' so the output becomes \eqn{X + activation(WX + b)}.
#' @param vaeac.skip_conn_masked_enc_dec Logical (default is `TRUE`). If `TRUE`, we apply concatenate skip
#' connections between the layers in the masked encoder and decoder. The first layer of the masked encoder will be
#' linked to the last layer of the decoder. The second layer of the masked encoder will be
#' linked to the second to last layer of the decoder, and so on.
#' @param vaeac.batch_normalization Logical (default is `FALSE`). If `TRUE`, we apply batch normalization after the
#' activation function. Note that if `vaeac.skip_conn_layer = TRUE`, then the normalization is applied after the
#' inclusion of the skip connection. That is, we batch normalize the whole quantity \eqn{X + activation(WX + b)}.
#' @param vaeac.paired_sampling Logical (default is `TRUE`). If `TRUE`, we apply paired sampling to the training
#' batches. That is, the training observations in each batch will be duplicated, where the first instance will be masked
#' by \eqn{S} while the second instance will be masked by \eqn{\bar{S}}. This ensures that the training of the
#' `vaeac` model becomes more stable as the model has access to the full version of each training observation. However,
#' this will increase the training time due to more complex implementation and doubling the size of each batch. See
#' [shapr::paired_sampler()] for more information.
#' @param vaeac.masking_ratio Numeric (default is `0.5`). Probability of masking a feature in the
#' [shapr::mcar_mask_generator()] (MCAR = Missing Completely At Random). The MCAR masking scheme ensures that `vaeac`
#' model can do arbitrary conditioning as all coalitions will be trained. `vaeac.masking_ratio` will be overruled if
#' `vaeac.mask_gen_coalitions` is specified.
#' @param vaeac.mask_gen_coalitions Matrix (default is `NULL`). Matrix containing the coalitions that the
#' `vaeac` model will be trained on, see [shapr::specified_masks_mask_generator()]. This parameter is used internally
#' in `shapr` when we only consider a subset of coalitions, i.e., when
#' `n_coalitions` \eqn{< 2^{n_{\text{features}}}}, and for group Shapley, i.e.,
#' when `group` is specified in [shapr::explain()].
#' @param vaeac.mask_gen_coalitions_prob Numeric array (default is `NULL`). Array of length equal to the height
#' of `vaeac.mask_gen_coalitions` containing the probabilities of sampling the corresponding coalitions in
#' `vaeac.mask_gen_coalitions`.
#' @param vaeac.sigma_mu Numeric (default is `1e4`). One of two hyperparameter values in the normal-gamma prior
#' used in the masked encoder, see Section 3.3.1 in
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param vaeac.sigma_sigma Numeric (default is `1e-4`). One of two hyperparameter values in the normal-gamma prior
#' used in the masked encoder, see Section 3.3.1 in
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param vaeac.save_data Logical (default is `FALSE`). If `TRUE`, then the data is stored together with
#' the model. Useful if one are to continue to train the model later using [shapr::vaeac_train_model_continue()].
#' @param vaeac.log_exp_cont_feat Logical (default is `FALSE`). If we are to \eqn{\log} transform all
#' continuous features before sending the data to [shapr::vaeac()]. The `vaeac` model creates unbounded Monte Carlo
#' sample values. Thus, if the continuous features are strictly positive (as for, e.g., the Burr distribution and
#' Abalone data set), it can be advantageous to \eqn{\log} transform the data to unbounded form before using `vaeac`.
#' If `TRUE`, then [shapr::vaeac_postprocess_data()] will take the \eqn{\exp} of the results to get back to strictly
#' positive values when using the `vaeac` model to impute missing values/generate the Monte Carlo samples.
#' @param vaeac.sample_random Logical (default is `TRUE`). If `TRUE`, the function generates random Monte Carlo samples
#' from the inferred generative distributions. If `FALSE`, the function use the most likely values, i.e., the mean and
#' class with highest probability for continuous and categorical, respectively.
#' @param vaeac.which_vaeac_model String (default is `best`). The name of the `vaeac` model (snapshots from different
#' epochs) to use when generating the Monte Carlo samples. The standard choices are: `"best"` (epoch with lowest IWAE),
#' `"best_running"` (epoch with lowest running IWAE, see `vaeac.running_avg_n_values`), and `last` (the last epoch).
#' Note that additional choices are available if `vaeac.save_every_nth_epoch` is provided. For example, if
#' `vaeac.save_every_nth_epoch = 5`, then `vaeac.which_vaeac_model` can also take the values `"epoch_5"`, `"epoch_10"`,
#' `"epoch_15"`, and so on.
#' @param vaeac.save_model Boolean. If `TRUE` (default), the `vaeac` model will be saved either in a
#' [base::tempdir()] folder or in a user specified location in `vaeac.folder_to_save_model`. If `FALSE`, then
#' the paths to model and the model will will be deleted from the returned object from [shapr::explain()].
#'
#' @return Named list of the default values `vaeac` extra parameter arguments specified in this function call.
#' Note that both `vaeac.model_description` and `vaeac.folder_to_save_model` will change with time and R session.
#'
#' @export
#' @author Lars Henry Berge Olsen
#' @inherit vaeac_train_model references
vaeac_get_extra_para_default <- function(vaeac.model_description = make.names(Sys.time()),
                                         vaeac.folder_to_save_model = tempdir(),
                                         vaeac.pretrained_vaeac_model = NULL,
                                         vaeac.cuda = FALSE,
                                         vaeac.epochs_initiation_phase = 2,
                                         vaeac.epochs_early_stopping = NULL,
                                         vaeac.save_every_nth_epoch = NULL,
                                         vaeac.val_ratio = 0.25,
                                         vaeac.val_iwae_n_samples = 25,
                                         vaeac.batch_size = 64,
                                         vaeac.batch_size_sampling = NULL,
                                         vaeac.running_avg_n_values = 5,
                                         vaeac.skip_conn_layer = TRUE,
                                         vaeac.skip_conn_masked_enc_dec = TRUE,
                                         vaeac.batch_normalization = FALSE,
                                         vaeac.paired_sampling = TRUE,
                                         vaeac.masking_ratio = 0.5,
                                         vaeac.mask_gen_coalitions = NULL,
                                         vaeac.mask_gen_coalitions_prob = NULL,
                                         vaeac.sigma_mu = 1e4,
                                         vaeac.sigma_sigma = 1e-4,
                                         vaeac.sample_random = TRUE,
                                         vaeac.save_data = FALSE,
                                         vaeac.log_exp_cont_feat = FALSE,
                                         vaeac.which_vaeac_model = "best",
                                         vaeac.save_model = TRUE) {
  # Return a named list with the extra parameters to the vaeac model
  return(mget(methods::formalArgs(vaeac_get_extra_para_default)))
}

#' Function to load a `vaeac` model and set it in the right state and mode
#'
#' @inheritParams vaeac_train_model
#' @param checkpoint List. This must be a loaded `vaeac` save object. That is, `torch::torch_load('vaeac_save_path')`.
#' @param mode_train Logical. If `TRUE`, the returned `vaeac` model is set to be in training mode.
#' If `FALSE`, the returned `vaeac` model is set to be in evaluation mode.
#'
#' @return A `vaeac` model with the correct state (based on `checkpoint`), sent to the desired hardware (based on
#' `cuda`), and in the right mode (based on `mode_train`).
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_model_from_checkp <- function(checkpoint, cuda, mode_train) {
  # Check parameters
  vaeac_check_logicals(list(cuda = cuda, mode_train = mode_train))

  # Set up the model such that it is loaded before calling the `prepare_data.vaeac()` function.
  vaeac_model <- vaeac(
    one_hot_max_sizes = checkpoint$one_hot_max_sizes,
    width = checkpoint$width,
    depth = checkpoint$depth,
    latent_dim = checkpoint$latent_dim,
    activation_function = checkpoint$activation_function,
    skip_conn_layer = checkpoint$skip_conn_layer,
    skip_conn_masked_enc_dec = checkpoint$skip_conn_masked_enc_dec,
    batch_normalization = checkpoint$batch_normalization,
    paired_sampling = checkpoint$paired_sampling,
    mask_generator_name = checkpoint$mask_generator_name,
    masking_ratio = checkpoint$masking_ratio,
    mask_gen_coalitions = checkpoint$mask_gen_coalitions,
    mask_gen_coalitions_prob = checkpoint$mask_gen_coalitions_prob,
    sigma_mu = checkpoint$sigma_mu,
    sigma_sigma = checkpoint$sigma_sigma
  )

  # Set the state of the vaeac model (setting the weights and biases in the networks)
  vaeac_model$load_state_dict(checkpoint$model_state_dict)

  # Apply the mode. Evaluation mode effects certain modules by, e.g., deactivating dropout layers,
  # how batch norm is conducted, and so on...
  if (mode_train) vaeac_model$train() else vaeac_model$eval()

  # Send the model to the GPU, if we are supposed to. Otherwise use CPU
  if (cuda) vaeac_model$cuda() else vaeac_model$cpu()

  # Return the model
  return(vaeac_model)
}

#' Function that determines which mask generator to use
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_mask_generator_name <- function(mask_gen_coalitions,
                                          mask_gen_coalitions_prob,
                                          masking_ratio,
                                          verbose) {
  if (!is.null(mask_gen_coalitions) && !is.null(mask_gen_coalitions_prob)) {
    # User have provided mask_gen_coalitions (and mask_gen_coalitions_prob),
    # and we want to use specified_masks_mask_generator
    mask_generator_name <- "specified_masks_mask_generator"

    # Small printout
    if ("vS_details" %in% verbose) {
      cli::cli_text(paste0("Using 'specified_masks_mask_generator' with '", nrow(mask_gen_coalitions), "' coalitions."))
    }
  } else if (length(masking_ratio) == 1) {
    # We are going to use 'mcar_mask_generator' as masking_ratio is a singleton.
    # I.e., all feature values are equally likely to be masked based on masking_ratio.
    mask_generator_name <- "mcar_mask_generator"

    # Small printout
    if ("vS_details" %in% verbose) {
      cli::cli_text(paste0(
        "Using 'mcar_mask_generator' with 'masking_ratio = ",
        masking_ratio,
        "'."
      ))
    }
  } else if (length(masking_ratio) > 1) {
    # We are going to use 'specified_prob_mask_generator' as masking_ratio is a vector (of same length as ncol(x_train).
    # I.e., masking_ratio[5] specifies the probability of masking 5 features
    mask_generator_name <- "specified_prob_mask_generator"

    # We have an array of masking ratios. Then we are using the specified_prob_mask_generator.
    if ("vS_details" %in% verbose) {
      cli::cli_text(paste0(
        "Using 'specified_prob_mask_generator' mask generator with 'masking_ratio = [",
        paste(masking_ratio, collapse = ", "), "]'."
      ))
    }
  } else {
    cli::cli_abort("`vaeac` could not determine which masking scheme to use based on the givene parameter arguments.")
  }

  return(mask_generator_name)
}

#' Function that creates the save file names for the `vaeac` model
#'
#' @inheritParams vaeac_train_model
#'
#' @return Array of string containing the save files to use when training the `vaeac` model. The first three names
#' corresponds to the best, best_running, and last epochs, in that order.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_save_file_names <- function(model_description,
                                      n_features,
                                      n_train,
                                      depth,
                                      width,
                                      latent_dim,
                                      lr,
                                      epochs,
                                      save_every_nth_epoch,
                                      folder_to_save_model = NULL) {
  file_names <- c("best", "best_running", "last") # The standard epochs we save the vaeac model

  # Add the optional epochs to save the model
  if (!is.null(save_every_nth_epoch)) {
    file_names <- c(file_names, seq(
      from = save_every_nth_epoch,
      by = save_every_nth_epoch,
      length.out = floor(epochs / save_every_nth_epoch)
    ))
  }

  # Create the file names
  file_names <- paste0(
    make.names(model_description), "_n_features_", n_features, "_n_train_", n_train, "_depth_", depth,
    "_width_", width, "_latent_", latent_dim, "_lr_", lr, "_epoch_", file_names, ".pt"
  )

  # Add the (optional) path to the folder to the name
  if (!is.null(folder_to_save_model)) file_names <- file.path(folder_to_save_model, file_names)

  return(file_names)
}

#' Function to create the optimizer used to train `vaeac`
#'
#' @description
#' Only [torch::optim_adam()] is currently supported. But it is easy to add an additional option later.
#'
#' @inheritParams vaeac_train_model
#' @param vaeac_model A `vaeac` model created using [vaeac()].
#' @param optimizer_name String containing the name of the [torch::optimizer()] to use.
#'
#' @return A [torch::optim_adam()] optimizer connected to the parameters of the `vaeac_model`.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_optimizer <- function(vaeac_model, lr, optimizer_name = "adam") {
  if (optimizer_name == "adam") {
    # Create the adam optimizer with defualt parameters except from the provided learning rate
    optimizer <- torch::optim_adam(
      params = vaeac_model$parameters,
      lr = lr,
      betas = c(0.9, 0.999),
      eps = 1e-08,
      weight_decay = 0,
      amsgrad = FALSE
    )
  } else {
    cli::cli_abort("Only the `adam` optimizer has been implemented for the `vaeac` approach.")
  }

  return(optimizer)
}



#' Function that extracts additional objects from the environment to the state list
#'
#' @description
#' The function extract the objects that we are going to save together with the `vaeac` model to make it possible to
#' train the model further and to evaluate it.
#' The environment should be the local environment inside the [shapr::vaeac_train_model_auxiliary()] function.
#'
#' @inheritParams vaeac_get_full_state_list
#'
#' @return List containing the values of `epoch`, `train_vlb`, `val_iwae`, `val_iwae_running`,
#' and the `state_dict()` of the vaeac model and optimizer.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_current_save_state <- function(environment) {
  object_names <- c("epoch", "train_vlb", "val_iwae", "val_iwae_running")
  objects <- lapply(object_names, function(name) environment[[name]])
  names(objects) <- object_names
  objects$model_state_dict <- environment[["vaeac_model"]]$state_dict()
  objects$optimizer_state_dict <- environment[["optimizer"]]$state_dict()
  return(objects)
}

#' Function that extracts the state list objects from the environment
#'
#' #' @description
#' The function extract the objects that we are going to save together with the `vaeac` model to make it possible to
#' train the model further and to evaluate it.
#' The environment should be the local environment inside the [shapr::vaeac_train_model_auxiliary()] function.
#'
#' @param environment The [base::environment()] where the objects are stored.
#'
#' @return List containing the values of `norm_mean`, `norm_std`, `model_description`, `folder_to_save_model`,
#' `n_train`, `n_features`, `one_hot_max_sizes`, `epochs`, `epochs_specified`, `epochs_early_stopping`,
#' `early_stopping_applied`, `running_avg_n_values`, `paired_sampling`, `mask_generator_name`, `masking_ratio`,
#' `mask_gen_coalitions`, `mask_gen_coalitions_prob`, `val_ratio`, `val_iwae_n_samples`,
#' `n_vaeacs_initialize`, `epochs_initiation_phase`, `width`, `depth`, `latent_dim`, `activation_function`,
#' `lr`, `batch_size`, `skip_conn_layer`, `skip_conn_masked_enc_dec`, `batch_normalization`, `cuda`,
#' `train_indices`, `val_indices`, `save_every_nth_epoch`, `sigma_mu`,
#' `sigma_sigma`, `feature_list`, `col_cat_names`, `col_cont_names`, `col_cat`, `col_cont`, `cat_in_dataset`,
#' `map_new_to_original_names`, `map_original_to_new_names`, `log_exp_cont_feat`, `save_data`, `verbose`,
#' `seed`, and `vaeac_save_file_names`.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_full_state_list <- function(environment) {
  object_names <- c(
    "norm_mean", "norm_std", "model_description", "folder_to_save_model", "n_train", "n_features", "one_hot_max_sizes",
    "epochs", "epochs_specified", "epochs_early_stopping", "early_stopping_applied", "running_avg_n_values",
    "paired_sampling", "mask_generator_name", "masking_ratio", "mask_gen_coalitions",
    "mask_gen_coalitions_prob", "val_ratio", "val_iwae_n_samples", "n_vaeacs_initialize",
    "epochs_initiation_phase", "width", "depth", "latent_dim", "activation_function",
    "lr", "batch_size", "skip_conn_layer", "skip_conn_masked_enc_dec", "batch_normalization", "cuda",
    "train_indices", "val_indices", "save_every_nth_epoch", "sigma_mu", "sigma_sigma", "feature_list", "col_cat_names",
    "col_cont_names", "col_cat", "col_cont", "cat_in_dataset", "map_new_to_original_names", "map_original_to_new_names",
    "log_exp_cont_feat", "save_data", "verbose", "seed", "vaeac_save_file_names"
  )
  objects <- lapply(object_names, function(name) environment[[name]])
  names(objects) <- object_names
  objects <- utils::modifyList(objects, environment[["x_train_preprocessed"]], keep.null = TRUE) # Flatten this list
  return(objects)
}



#' Function to extend the explicands and apply all relevant masks/coalitions
#'
#' @inheritParams explain
#' @inheritParams default_doc_export
#' @param S The `internal$objects$S` matrix containing the possible coalitions.
#'
#' @return The extended version of `x_explain` where the masks from `S` with indices `index_features` have been applied.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_x_explain_extended <- function(x_explain, S, index_features) {
  n_coalitions <- length(index_features) # Get the number of active coalitions
  n_explain <- nrow(x_explain) # Get the number of explicands
  mask <- S[index_features, , drop = FALSE] # Get the masks/coalitions we are to generate MC samples for
  mask[mask == 0] <- NaN # Set zeros to `NaN` to indicate that they are missing and to be imputed by `vaeac`
  x_explain_extended <-
    x_explain[rep(seq_len(nrow(x_explain)), each = n_coalitions), ] # Extend the explicands `n_coalitions` times
  mask_extended <- mask[rep(seq(n_coalitions), times = n_explain), ] # Extend the masks `n_expliand` times
  x_explain_extended[is.na(mask_extended)] <- NaN # Apply the mask. The NaNs are features outside coalition S.
  return(x_explain_extended)
}

#' Extract the Training VLB and Validation IWAE from a list of explanations objects using the vaeac approach
#'
#' @param explanation_list A list of [explain()] objects applied to the same data, model, and
#' `vaeac` must be the used approach. If the entries in the list is named, then the function use
#' these names. Otherwise, it defaults to the approach names (with integer suffix for duplicates)
#' for the explanation objects in `explanation_list`.
#'
#' @return A data.table containing the training VLB, validation IWAE, and running validation IWAE at each epoch for
#' each vaeac model.
#' @export
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_get_evaluation_criteria <- function(explanation_list) {
  # Check if user only provided a single explanation and did not put it in a list
  if ("shapr" %in% class(explanation_list)) explanation_list <- list(explanation_list)

  # Check that all explanation objects use the `vaeac` approach
  explanation_approaches <- sapply(explanation_list, function(explanation) explanation$internal$parameters$approach)
  if (any(explanation_approaches != "vaeac")) {
    cli::cli_abort(sprintf(
      "Explanation object number `%d` in the `explanation_list` does not use the `vaeac` approach.",
      seq_along(explanation_approaches)[explanation_approaches != "vaeac"][1]
    ))
  }

  # Name the elements in the explanation_list if no names have been provided
  if (is.null(names(explanation_list))) explanation_list <- MSEv_name_explanation_list(explanation_list)

  # Extract the evaluation criteria and put them into a data.table
  vaeac_VLB_IWAE_dt <- data.table::rbindlist(
    lapply(explanation_list, function(explanation) {
      data.table::data.table(do.call(cbind, explanation$internal$parameters$vaeac$results))[, Epoch := .I]
    }),
    use.names = TRUE,
    idcol = "Method",
  )
  names(vaeac_VLB_IWAE_dt)[2:4] <- c("VLB", "IWAE", "IWAE_running")
  vaeac_VLB_IWAE_dt$Method <- factor(vaeac_VLB_IWAE_dt$Method, levels = names(explanation_list))
  data.table::setkeyv(vaeac_VLB_IWAE_dt, c("Method", "Epoch"))
  data.table::setcolorder(vaeac_VLB_IWAE_dt, c("Method", "Epoch"))

  return(vaeac_VLB_IWAE_dt)
}

#' Function to set up data loaders and save file names
#'
#' @inheritParams vaeac_train_model
#' @param train_indices Numeric array (optional) containing the indices of the training observations.
#' There are conducted no checks to validate the indices.
#' @param val_indices Numeric array (optional) containing the indices of the validation observations.
#' #' There are conducted no checks to validate the indices.
#'
#' @return List of objects needed to train the `vaeac` model
#' @keywords internal
vaeac_get_data_objects <- function(x_train,
                                   log_exp_cont_feat,
                                   val_ratio,
                                   batch_size,
                                   paired_sampling,
                                   model_description,
                                   depth,
                                   width,
                                   latent_dim,
                                   lr,
                                   epochs,
                                   save_every_nth_epoch,
                                   folder_to_save_model,
                                   train_indices = NULL,
                                   val_indices = NULL) {
  if (xor(is.null(train_indices), is.null(val_indices))) {
    cli::cli_abort("Either none or both of `train_indices` and `val_indices` must be given.")
  }

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
  if (is.null(train_indices)) { # The val_indices will also be NULL due to the xor check above
    val_size <- ceiling(n_train * val_ratio) # Number of observations in the validation set
    val_indices <- sample(n_train, val_size, replace = FALSE) # Sample indices for the validation set
    train_indices <- seq(n_train)[-val_indices] # The remaining indices constitutes the training set
  } else {
    val_size <- NULL
  }
  val_dataset <- vaeac_dataset(x_train_torch[val_indices], one_hot_max_sizes) # Create a torch::dataset() for vaeac
  train_dataset <- vaeac_dataset(x_train_torch[train_indices], one_hot_max_sizes) # Create a torch::dataset() for vaeac

  # Ensure a valid batch size
  if (batch_size > length(train_indices)) {
    if ("vS_details" %in% verbose) {
      cli::cli_text(paste0(
        "Decrease `batch_size` (", batch_size, ") to largest allowed value (", length(train_indices), "), ",
        "i.e., the number of training observations."
      ))
    }
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

  return(list(
    n_train = n_train,
    n_features = n_features,
    x_train_preprocessed = x_train_preprocessed,
    x_train_torch = x_train_torch,
    one_hot_max_sizes = one_hot_max_sizes,
    val_size = val_size,
    val_indices = val_indices,
    train_indices = train_indices,
    batch_size = batch_size,
    train_dataloader = train_dataloader,
    val_dataloader = val_dataloader,
    vaeac_save_file_names = vaeac_save_file_names
  ))
}

#' Function to get string of values with specific number of decimals
#'
#' @param value The number to get `n_decimals` for.
#' @param n_decimals Positive integer. The number of decimals. Default is three.
#'
#' @return String of `value` with `n_decimals` decimals.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_n_decimals <- function(value, n_decimals = 3) trimws(format(round(value, n_decimals), nsmall = n_decimals))

# Update functions =====================================================================================================
#' Move `vaeac` parameters to correct location
#'
#' @description
#' This function ensures that the main and extra parameters for the `vaeac`
#' approach is located at their right locations.
#'
#' @param parameters List. The `internal$parameters` list created inside the [shapr::explain()] function.
#'
#' @return Updated version of `parameters` where all `vaeac` parameters are located at the correct location.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_update_para_locations <- function(parameters) {
  # Get the name of the main parameters for the `vaeac` approach
  vaeac.main_para_default_names <- methods::formalArgs(setup_approach.vaeac)
  vaeac.main_para_default_names <-
    vaeac.main_para_default_names[!vaeac.main_para_default_names %in% c("internal", "vaeac.extra_parameters", "...")]

  # Get the default values for vaeac's main parameters defined above into a named list
  vaeac.main_para_default <- as.list(formals(sys.function(sys.parent())))
  vaeac.main_para_default <- vaeac.main_para_default[vaeac.main_para_default %in% vaeac.main_para_default_names]

  # Get the names of the vaeac's main parameters provided by the user
  vaeac.main_para_user_names <- names(parameters)
  vaeac.main_para_user_names <- vaeac.main_para_user_names[grepl("vaeac.", vaeac.main_para_user_names)]
  vaeac.main_para_user_names <- vaeac.main_para_user_names[!vaeac.main_para_user_names %in% "vaeac.extra_parameters"]

  # Get the default values for vaeac's extra parameters into a named list
  vaeac.extra_para_default <- vaeac_get_extra_para_default()
  vaeac.extra_para_default_names <- names(vaeac.extra_para_default)

  # Get the names of the extra parameters provided by the user
  vaeac.extra_para_user_names <- names(parameters$vaeac.extra_parameters)

  # Get the names of all parameters and the user specified parameters
  vaeav.all_para_default_names <- c(vaeac.main_para_default_names, vaeac.extra_para_default_names)

  # Check if any of the main parameters with the "vaeac." prefix is unknown (i.e., not main or extra parameter)
  not_extra_para_in_main_para <-
    vaeac.main_para_user_names[!vaeac.main_para_user_names %in% vaeav.all_para_default_names]
  if (length(not_extra_para_in_main_para) > 0) {
    # Give a warning to the user about the unknown extra parameters
    msg1 <- "The following vaeac main parameters are not recognized (`shapr` removes them): "
    msg2 <- paste0(paste(strsplit(paste(paste0("`", not_extra_para_in_main_para, "`"), collapse = ", "),
      ",(?=[^,]+$)",
      perl = TRUE
    )[[1]], collapse = " and"), ".")
    cli::cli_warn(c("!" = msg1, " " = msg2), immediate. = TRUE)

    # Delete the unknown extra parameters
    parameters[not_extra_para_in_main_para] <- NULL
  }

  # Check if any of the extra parameters with the "vaeac." prefix is unknown (i.e., not main or extra parameter)
  not_main_para_in_extra_para <-
    vaeac.extra_para_user_names[!vaeac.extra_para_user_names %in% vaeav.all_para_default_names]
  if (length(not_main_para_in_extra_para) > 0) {
    # Give a warning to the user about the unknown extra parameters
    msg1 <- "The following vaeac extra parameters are not recognized (`shapr` removes them): "
    msg2 <- paste0(paste(strsplit(paste(paste0("`", not_main_para_in_extra_para, "`"), collapse = ", "),
      ",(?=[^,]+$)",
      perl = TRUE
    )[[1]], collapse = " and"), ".")
    cli::cli_warn(c("!" = msg1, " " = msg2), immediate. = TRUE)

    # Delete the unknown extra parameters
    parameters$vaeac.extra_parameters[not_main_para_in_extra_para] <- NULL
  }

  # Check for parameters that have been provided as both main and extra parameter
  both_main_and_extra_para <- vaeac.extra_para_user_names[vaeac.extra_para_user_names %in% vaeac.main_para_user_names]
  if (length(both_main_and_extra_para > 0)) {
    # Print a warning to the user and tell them that we use those in `vaeac.extra_parameters`.
    msg1 <- paste0(
      "The following vaeac parameters were given as both main and extra parameters (`shapr` uses the ",
      "values at the correct location): "
    )
    msg2 <- paste0(paste(strsplit(paste(paste0("`", both_main_and_extra_para, "`"), collapse = ", "),
      ",(?=[^,]+$)",
      perl = TRUE
    )[[1]], collapse = " and"), ".")
    cli::cli_warn(c("!" = msg1, " " = msg2), immediate. = TRUE)

    # Note that we do not move it here as the moving will be fixed in the next two if-clauses
  }

  # Check if any any extra parameters have been given as main parameters
  extra_para_in_main_para <- vaeac.main_para_user_names[vaeac.main_para_user_names %in% vaeac.extra_para_default_names]
  if (length(extra_para_in_main_para) > 0) {
    msg1 <- paste0(
      "The following vaeac parameters were given as main parameters but should have been extra ",
      "parameters (`shapr` fixes this): "
    )
    msg2 <- paste0(paste(strsplit(paste(paste0("`", extra_para_in_main_para, "`"), collapse = ", "),
      ",(?=[^,]+$)",
      perl = TRUE
    )[[1]], collapse = " and"), ".")
    cli::cli_warn(c("!" = msg1, " " = msg2), immediate. = TRUE)

    # Move extra parameter from the main parameters to extra_parameters list if they have NOT been specified already
    parameters$vaeac.extra_parameters[extra_para_in_main_para[!extra_para_in_main_para %in%
      vaeac.extra_para_user_names]] <-
      parameters[extra_para_in_main_para[!extra_para_in_main_para %in% vaeac.extra_para_user_names]]

    # Remove the extra parameter from the main parameters
    parameters[extra_para_in_main_para] <- NULL
  }

  # Check if any any main parameters have been given as extra parameters
  main_para_in_extra_para <- vaeac.extra_para_user_names[vaeac.extra_para_user_names %in% vaeac.main_para_default_names]
  if (length(main_para_in_extra_para) > 0) {
    # Give a warning to the user about the misplaced main parameters in the extra list
    msg1 <- paste0(
      "The following vaeac parameters were given as extra parameters but should have been main ",
      "parameters (`shapr` fixes this): "
    )
    msg2 <- paste0(paste(strsplit(paste(paste0("`", main_para_in_extra_para, "`"), collapse = ", "),
      ",(?=[^,]+$)",
      perl = TRUE
    )[[1]], collapse = " and"), ".")
    cli::cli_warn(c("!" = msg1, " " = msg2), immediate. = TRUE)

    # Move main parameters from the extra_parameters list to main parameters if they have NOT been specified already
    parameters[main_para_in_extra_para[!main_para_in_extra_para %in% vaeac.main_para_user_names]] <-
      parameters$vaeac.extra_parameters[main_para_in_extra_para[!main_para_in_extra_para
      %in% vaeac.main_para_user_names]]

    # Remove the main parameter from the extra list
    parameters$vaeac.extra_parameters[main_para_in_extra_para] <- NULL
  }

  # Return the fixed parameters list
  return(parameters)
}

#' Function that checks and adds a pre-trained `vaeac` model
#'
#' @param parameters List containing the parameters used within [shapr::explain()].
#'
#' @return This function adds a valid pre-trained vaeac model to the `parameter`.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_update_pretrained_model <- function(parameters) {
  # Extract the provided pre-trained vaeac model
  vaeac_object <- parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model

  # Check that it is either a list or string
  if (!(is.list(vaeac_object) || is.character(vaeac_object))) {
    cli::cli_abort(
      "The `vaeac.pretrained_vaeac_model` parameter must be either a list or a string. Read the documentation."
    )
  }

  # Check if we are given a list
  if (is.list(vaeac_object)) {
    # Check for list of type vaeac
    if (!("vaeac" %in% class(vaeac_object))) {
      cli::cli_abort("The `vaeac.pretrained_vaeac_model` list is not of type `vaeac`.")
    }
    vaeac_check_x_colnames(
      feature_names_vaeac = vaeac_object$parameters$feature_list$labels,
      feature_names_new = parameters$feature_names
    )

    # Add the pre-trained valid vaeac model to the parameters list
    parameters$vaeac <- parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model

    # Remove the pre-trained vaeac model as it has been approved as a vaeac model
    parameters$vaeac.extra_parameters$vaeac.pretrained_vaeac_model <- NULL
  }

  # Check if we are given a string
  if (is.character(vaeac_object)) {
    # Check that the file exists
    if (!file.exists(vaeac_object)) {
      cli::cli_abort(paste0("The `vaeac.pretrained_vaeac_model` file ('", vaeac_object, "') does not exist."))
    }

    # Read in the vaeac model from the disk
    vaeac_model <- torch::torch_load(vaeac_object)

    # Some very small check that we have read in a vaeac model
    if (is.null(vaeac_model$model_state_dict)) {
      cli::cli_abort("The provided file is not a vaeac model as it is missing, e.g., the `model_state_dict` entry.")
    }
    if (is.null(vaeac_model$optimizer_state_dict)) {
      cli::cli_abort("The provided file is not a vaeac model as it is missing, e.g., the `optimizer_state_dict` entry.")
    }

    # Check that the provided vaeac model is trained on a dataset with the same feature names
    vaeac_check_x_colnames(
      feature_names_vaeac = vaeac_model$feature_list$labels,
      feature_names_new = parameters$feature_names
    )

    # Extract the training/validation results
    evaluation_criterions <- c("train_vlb", "val_iwae", "val_iwae_running")
    vaeac_model_results <- lapply(vaeac_model[evaluation_criterions], as.array)

    # Save path to the vaeac approach to use to generate the MC samples.
    parameters$vaeac <- list(
      models = list(best = vaeac_object),
      results = vaeac_model_results,
      parameters = vaeac_model[!names(vaeac_model) %in% evaluation_criterions]
    )

    # Add `vaeac` as a class to the object. We use this to validate the input when
    # `vaeac.pretrained_vaeac_model` is given to the `shapr::explain()` function.
    class(parameters$vaeac) <- c(class(parameters$vaeac), "vaeac")
  }

  # Return the updated parameters list
  return(parameters)
}

# Save functions =======================================================================================================
#' Function that saves the state list and the current save state of the `vaeac` model
#'
#' @param state_list List containing all the parameters in the state.
#' @param file_name String containing the file path.
#' @param return_state Logical if we are to return the state list or not.
#'
#' @return This function does not return anything
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_save_state <- function(state_list, file_name, return_state = FALSE) {
  state <- modifyList(state_list, vaeac_get_current_save_state(parent.frame()), keep.null = TRUE)
  class(state) <- c(class(state), "vaeac")
  torch::torch_save(state, file_name)
  if (return_state) {
    return(state)
  }
}

# Print functions ======================================================================================================
#' Function to printout a training summary for the `vaeac` model
#'
#' @param best_epoch Positive integer. The epoch with the lowest validation error.
#' @param best_epoch_running Positive integer. The epoch with the lowest running validation error.
#' @param last_state The state list (i.e., the saved `vaeac` object)
#' of `vaeac` model at the epoch with the lowest IWAE.
#'
#' @return This function only prints out a message.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_print_train_summary <- function(best_epoch, best_epoch_running, last_state) {
  rlang::inform(sprintf(
    "\nResults of the `vaeac` training process:
Best epoch:             %d. \tVLB = %.3f \tIWAE = %.3f \tIWAE_running = %.3f
Best running avg epoch: %d. \tVLB = %.3f \tIWAE = %.3f \tIWAE_running = %.3f
Last epoch:             %d. \tVLB = %.3f \tIWAE = %.3f \tIWAE_running = %.3f\n",
    best_epoch,
    last_state$train_vlb[best_epoch]$cpu(),
    last_state$val_iwae[best_epoch]$cpu(),
    last_state$val_iwae_running[best_epoch]$cpu(),
    best_epoch_running,
    last_state$train_vlb[best_epoch_running]$cpu(),
    last_state$val_iwae[best_epoch_running]$cpu(),
    last_state$val_iwae_running[best_epoch_running]$cpu(),
    last_state$epoch,
    last_state$train_vlb[-1]$cpu(),
    last_state$val_iwae[-1]$cpu(),
    last_state$val_iwae_running[-1]$cpu()
  ))
}


# Plot functions =======================================================================================================
#' Plot the training VLB and validation IWAE for `vaeac` models
#'
#' @description This function makes ([ggplot2::ggplot()]) figures of the training VLB and the validation IWAE for a list
#' of [shapr::explain()] objects with `approach = "vaeac"`. See [setup_approach()] for more information about the
#' `vaeac` approach. Two figures are returned by the function. In the figure, each object in `explanation_list` gets
#' its own facet, while in the second figure, we plot the criteria in each facet for all objects.
#'
#' @details See \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)} or the
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
#' \donttest{
#'
#' if (requireNamespace("xgboost", quietly = TRUE) &&
#'   requireNamespace("torch", quietly = TRUE) &&
#'   torch::torch_is_installed()) {
#'   data("airquality")
#'   data <- data.table::as.data.table(airquality)
#'   data <- data[complete.cases(data), ]
#'
#'   x_var <- c("Solar.R", "Wind", "Temp", "Month")
#'   y_var <- "Ozone"
#'
#'   ind_x_explain <- 1:6
#'   x_train <- data[-ind_x_explain, ..x_var]
#'   y_train <- data[-ind_x_explain, get(y_var)]
#'   x_explain <- data[ind_x_explain, ..x_var]
#'
#'   # Fitting a basic xgboost model to the training data
#'   model <- xgboost::xgboost(
#'     data = as.matrix(x_train),
#'     label = y_train,
#'     nround = 100,
#'     verbose = FALSE
#'   )
#'
#'   # Specifying the phi_0, i.e. the expected prediction without any features
#'   p0 <- mean(y_train)
#'
#'   # Train vaeac with and without paired sampling
#'   explanation_paired <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "vaeac",
#'     phi0 = p0,
#'     n_MC_samples = 1, # As we are only interested in the training of the vaeac
#'     vaeac.epochs = 10, # Should be higher in applications.
#'     vaeac.n_vaeacs_initialize = 1,
#'     vaeac.width = 16,
#'     vaeac.depth = 2,
#'     vaeac.extra_parameters = list(vaeac.paired_sampling = TRUE)
#'   )
#'
#'   explanation_regular <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "vaeac",
#'     phi0 = p0,
#'     n_MC_samples = 1, # As we are only interested in the training of the vaeac
#'     vaeac.epochs = 10, # Should be higher in applications.
#'     vaeac.width = 16,
#'     vaeac.depth = 2,
#'     vaeac.n_vaeacs_initialize = 1,
#'     vaeac.extra_parameters = list(vaeac.paired_sampling = FALSE)
#'   )
#'
#'   # Collect the explanation objects in an named list
#'   explanation_list <- list(
#'     "Regular sampling" = explanation_regular,
#'     "Paired sampling" = explanation_paired
#'   )
#'
#'   # Call the function with the named list, will use the provided names
#'   plot_vaeac_eval_crit(explanation_list = explanation_list)
#'
#'   # The function also works if we have only one method,
#'   # but then one should only look at the method plot.
#'   plot_vaeac_eval_crit(
#'     explanation_list = explanation_list[2],
#'     plot_type = "method"
#'   )
#'
#'   # Can alter the plot
#'   plot_vaeac_eval_crit(
#'     explanation_list = explanation_list,
#'     plot_from_nth_epoch = 2,
#'     plot_every_nth_epoch = 2,
#'     facet_wrap_scales = "free"
#'   )
#'
#'   # If we only want the VLB
#'   plot_vaeac_eval_crit(
#'     explanation_list = explanation_list,
#'     criteria = "VLB",
#'     plot_type = "criterion"
#'   )
#'
#'   # If we want only want the criterion version
#'   tmp_fig_criterion <-
#'     plot_vaeac_eval_crit(explanation_list = explanation_list, plot_type = "criterion")
#'
#'   # Since tmp_fig_criterion is a ggplot2 object, we can alter it
#'   # by, e.g,. adding points or smooths with se bands
#'   tmp_fig_criterion + ggplot2::geom_point(shape = "circle", size = 1, ggplot2::aes(col = Method))
#'   tmp_fig_criterion$layers[[1]] <- NULL
#'   tmp_fig_criterion + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
#'     ggplot2::scale_color_brewer(palette = "Set1") +
#'     ggplot2::theme_minimal()
#' }
#' }
#'
#' @author Lars Henry Berge Olsen
#' @export
#' @inherit vaeac_train_model references
plot_vaeac_eval_crit <- function(explanation_list,
                                 plot_from_nth_epoch = 1,
                                 plot_every_nth_epoch = 1,
                                 criteria = c("VLB", "IWAE"),
                                 plot_type = c("method", "criterion"),
                                 facet_wrap_scales = "fixed",
                                 facet_wrap_ncol = NULL) {
  ## Checks
  # Check that ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    cli::cli_abort("ggplot2 is not installed. Please run {.run install.packages('ggplot2')}.")
  }

  # Check for valid criteria argument
  unknown_criteria <- criteria[!(criteria %in% c("VLB", "IWAE", "IWAE_running"))]
  if (length(unknown_criteria) > 0) {
    cli::cli_abort(paste0(
      "The `criteria` must be one (or several) of 'VLB', 'IWAE', and 'IWAE_running'. ",
      "Do not recognise: '", paste(unknown_plot_type, collapse = "', '"), "'."
    ))
  }

  # Check for valid plot type argument
  unknown_plot_type <- plot_type[!(plot_type %in% c("method", "criterion"))]
  if (length(unknown_plot_type) > 0) {
    cli::cli_abort(paste0(
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
    cli::cli_abort(sprintf(
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
#' a `vaeac` model, and then compares the imputed values with data from the true distribution (if provided).
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
#' Possible options are: `'box'` (default), `'box_no_facet'`, `'dot'`, `'dot_no_facet'`, `'facethist'`,
#'  `'facetdensity'`, `'denstrip'`, and `'blank'`
#' @param lower_cont String. Type of plot to use in lower triangle for continuous features, see [GGally::ggpairs()].
#' Possible options are: `'points'` (default), `'smooth'`, `'smooth_loess'`, `'density'`, `'cor'`, and `'blank'`.
#' @param lower_cat String. Type of plot to use in lower triangle for categorical features, see [GGally::ggpairs()].
#' Possible options are: `'facetbar'` (default), `'ratio'`, `'count'`, `'cross'`, and `'blank'`.
#' @param lower_mix String. Type of plot to use in lower triangle for mixed features, see [GGally::ggpairs()].
#' Possible options are: `'facetdensity'` (default), `'box'`, `'box_no_facet'`, `'dot'`, `'dot_no_facet'`,
#'  `'facethist'`, `'denstrip'`, and `'blank'`.
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
#' @inherit vaeac_train_model references
#'
#' @examples
#' \donttest{
#'
#' if (requireNamespace("xgboost", quietly = TRUE) &&
#'   requireNamespace("ggplot2", quietly = TRUE) &&
#'   requireNamespace("torch", quietly = TRUE) &&
#'   torch::torch_is_installed()) {
#'   data("airquality")
#'   data <- data.table::as.data.table(airquality)
#'   data <- data[complete.cases(data), ]
#'
#'   x_var <- c("Solar.R", "Wind", "Temp", "Month")
#'   y_var <- "Ozone"
#'
#'   ind_x_explain <- 1:6
#'   x_train <- data[-ind_x_explain, ..x_var]
#'   y_train <- data[-ind_x_explain, get(y_var)]
#'   x_explain <- data[ind_x_explain, ..x_var]
#'
#'   # Fitting a basic xgboost model to the training data
#'   model <- xgboost::xgboost(
#'     data = as.matrix(x_train),
#'     label = y_train,
#'     nround = 100,
#'     verbose = FALSE
#'   )
#'
#'   explanation <- shapr::explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "vaeac",
#'     phi0 = mean(y_train),
#'     n_MC_samples = 1,
#'     vaeac.epochs = 10,
#'     vaeac.n_vaeacs_initialize = 1
#'   )
#'
#'   # Plot the results
#'   figure <- shapr::plot_vaeac_imputed_ggpairs(
#'     explanation = explanation,
#'     which_vaeac_model = "best",
#'     x_true = x_train,
#'     add_title = TRUE
#'   )
#'   figure
#'
#'   # Note that this is an ggplot2 object which we can alter, e.g., we can change the colors.
#'   figure +
#'     ggplot2::scale_color_manual(values = c("#E69F00", "#999999")) +
#'     ggplot2::scale_fill_manual(values = c("#E69F00", "#999999"))
#' }
#' }
plot_vaeac_imputed_ggpairs <- function(
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
    cli::cli_abort("ggplot2 is not installed. Please run {.run install.packages('ggplot2')}.")
  }
  if (!requireNamespace("GGally", quietly = TRUE)) {
    cli::cli_abort("GGally is not installed. Please run {.run install.packages('GGally')}.")
  }

  # Check all input parameters except `which_vaeac_model`
  if (!"shapr" %in% class(explanation)) cli::cli_abort("`explanation` must be an object of type `shapr`.")
  if (!is.null(x_true) && !is.data.table(x_true)) cli::cli_abort("`x_true` must be an object of type `data.table`.")
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
    msg1 <- "The vaeac model has not been trained on the empty colition, hence, the figure can be missleading."
    msg2 <- "The figure is only reasonable if `n_combintations = NULL` and `group = NULL` in the explanation call."
    cli::cli_inform(c("i" = msg1, " " = msg2))
  }

  # Extract the vaeac list from the explanation list
  vaeac_list <- explanation$internal$parameters$vaeac

  # Check that `which_vaeac_model` is a valid vaeac model name and then load the vaeac checkpoint
  if (!is.character(which_vaeac_model) || !which_vaeac_model %in% names(vaeac_list$models)) {
    cli::cli_abort(paste0(
      "The parameter `which_vaeac_model` ('", which_vaeac_model, "') must be one of the following: '",
      paste(names(vaeac_list$models), collapse = "', '"), "'."
    ))
  }
  vaeac_model_path <- vaeac_list$models[[which_vaeac_model]]
  checkpoint <- torch::torch_load(vaeac_model_path)

  # Get the number of observations in the x_true and features
  n_MC_samples <- if (is.null(x_true)) 500 else nrow(x_true)
  n_features <- checkpoint$n_features

  # Checking for valid dimension
  if (!is.null(x_true) && ncol(x_true) != n_features) {
    cli::cli_abort(paste0(
      "Different number of columns in the vaeac model (", n_features, ") and `x_true` (", ncol(x_true), ")."
    ))
  }

  # Set up the vaeac model
  vaeac_model <- vaeac_get_model_from_checkp(checkpoint = checkpoint, cuda = checkpoint$cuda, mode_train = FALSE)

  # Impute the missing entries using the vaeac approach. Here we generate x from p(x), so no conditioning.
  imputed_values <- vaeac_impute_missing_entries(
    x_explain_with_NaNs = matrix(NaN, n_MC_samples, checkpoint$n_features),
    n_MC_samples = 1,
    vaeac_model = vaeac_model,
    checkpoint = checkpoint,
    sampler = explanation$internal$parameters$vaeac.sampler,
    batch_size = n_MC_samples,
    verbose = explanation$internal$parameters$verbose,
    seed = explanation$internal$parameters$seed
  )

  # Combine the true (if there are any) and imputed data and ensure that the categorical features are marked as factors.
  combined_data <- data.table(rbind(x_true, imputed_values))
  col_cat_names <- checkpoint$col_cat_names
  if (length(col_cat_names) > 0) combined_data[, (col_cat_names) := lapply(.SD, as.factor), .SDcols = col_cat_names]

  # Add type variable representing if they are imputed samples or from `x_true`
  combined_data$type <-
    factor(rep(c("True", "Imputed"), times = c(ifelse(is.null(nrow(x_true)), 0, nrow(x_true)), n_MC_samples)))

  # Create the ggpairs figure and potentially add title based on the description of the used vaeac model
  figure <- GGally::ggpairs(
    combined_data,
    columns = seq(n_features),
    mapping = ggplot2::aes(color = type),
    diag = list(continuous = GGally::wrap(diag_cont, alpha = alpha), discrete = diag_cat),
    upper = list(combo = upper_mix, discrete = upper_cat, continuous = GGally::wrap(upper_cont, method = cor_method)),
    lower = list(combo = lower_mix, discrete = lower_cat, continuous = GGally::wrap(lower_cont, alpha = alpha))
  )
  if (add_title) figure <- figure + ggplot2::ggtitle(tools::file_path_sans_ext(basename(vaeac_model_path)))

  return(figure)
}
