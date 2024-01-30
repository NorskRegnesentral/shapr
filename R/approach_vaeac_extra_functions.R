# Check functions ======================================================================================================
#' Check vaeac.extra_parameters list
#'
#' @param vaeac.extra_parameters List containing the extra parameters to the `vaeac` approach
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_check_extra_named_list <- function(vaeac.extra_parameters) {
  names <- names(vaeac.extra_parameters)
  if (is.null(names)) stop("The parameter `vaeac.extra_parameters` is not a named list.")
  if (any(names == "")) stop("Not all parameters in the list `vaeac.extra_parameters` are named.")
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
      stop(paste0("'vaeac.", param_name, "' must be a positive integer."))
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
      stop(paste0("'vaeac.", param_name, "' must be a positive numeric."))
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
      stop(paste0("'vaeac.", param_name, "' must be a valid probability (a number between 0 and 1)."))
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
      stop(paste0("'vaeac.", param_name, "' must be a boolean (i.e., `TRUE` or `FALSE`)."))
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

  if (!is.null(vaeac.which_vaeac_model) && !is.character(vaeac.which_vaeac_model)) {
    stop("`vaeac.which_vaeac_model` must be a string.")
  }

  if (!which_vaeac_model %in% valid_names) {
    stop(paste0(
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
vaeac_check_epoch_values <- function(epochs, epochs_initiation_phase, epochs_early_stopping, save_every_nth_epoch) {
  if (epochs_initiation_phase >= epochs) {
    stop(paste0(
      "'vaeac.epochs_initiation_phase' (", epochs_initiation_phase, ") must be strictly less than ",
      "'vaeac.epochs' (", epochs, ")."
    ))
  }

  if (epochs_early_stopping > epochs) {
    message(paste0(
      "No early stopping as `vaeac.epochs_early_stopping` (", epochs_early_stopping, ") is larger than ",
      "`vaeac.epochs` (", epochs, ")."
    ))
  }

  # Ensure a valid value for save_every_nth_epoch.
  if (!is.null(save_every_nth_epoch) && save_every_nth_epoch > epochs) {
    stop(paste0("Number of 'epochs' (", epochs, ") is less than 'save_every_nth_epoch' (", save_every_nth_epoch, ")."))
  }
  # Ensure a valid value for save_every_nth_epoch.
  if (!is.null(save_every_nth_epoch) && save_every_nth_epoch <= epochs_initiation_phase) {
    stop(paste0(
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
  # TODO: In future, check that it is one of the activation functions and not just a nn_module
  # Check that activation function is an nn_module
  if (!any("nn_module" %in% class(activation_function))) stop("`vaeac.activation_function` is not an `nn_module`.")
}

#' Function that checks the specified masking scheme
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_mask_gen <- function(mask_gen_these_coalitions, mask_gen_these_coalitions_prob, x_train) {
  masks <- mask_gen_these_coalitions
  probs <- mask_gen_these_coalitions_prob

  if (!is.null(masks) || !is.null(probs)) {
    if (xor(is.null(masks), is.null(probs))) {
      stop(
        "either both `vaeac.mask_gen_these_coalitions` and `vaeac.mask_gen_these_coalitions_prob` need to `NULL` ",
        "or both have to be specified."
      )
    }

    if (!is.matrix(masks)) stop("`vaeac.mask_gen_these_coalitions` must be a matrix.")
    if (!is.numeric(probs)) stop("`vaeac.mask_gen_these_coalitions_prob` must be an array.")

    if (nrow(masks) != length(probs)) {
      stop(
        "the number of rows in `vaeac.mask_gen_these_coalitions` must be equal to the length of ",
        "`vaeac.mask_gen_these_coalitions_prob`."
      )
    }

    if (ncol(masks) != ncol(x_train)) {
      stop(
        "the number of columns in `vaeac.mask_gen_these_coalitions` must be equal to the number of ",
        "columns in the `x_train`. That is, the number of features."
      )
    }
  }
}

#' Function the checks the verbose parameter
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_check_verbose <- function(verbose) {
  if (!is.numeric(verbose) || !(verbose %in% c(0, 1, 2))) {
    stop("`vaeac.verbose` must be either `0` (no verbosity), `1` (low verbosity), or `2` (high verbosity).")
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
  if (!is.character(folder_to_save_model)) stop("`vaeac.folder_to_save_model` must be a string.")
  if (!is.character(model_description)) stop("`vaeac.model_description` must be a string.")
  if (!dir.exists(folder_to_save_model)) {
    stop(paste0("the folder `vaeac.folder_to_save_model` ('", folder_to_save_model, "') does not exist."))
  }
  if (!grepl("^[A-Za-z0-9._-]+$", model_description)) {
    stop(paste0(
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
vaeac_check_use_cuda <- function(use_cuda) {
  # Check if cuda/GPU is available on the current system
  cuda_available <- torch::cuda_is_available()

  # Give message to user if asked to run on cuda, but cuda is not available.
  if (isFALSE(cuda_available) && isTRUE(use_cuda)) {
    use_cuda <- FALSE
    message("Cuda/GPU is not available (`shapr` uses CPU instead).", immediate. = TRUE)
  }

  return(use_cuda)
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
  if (length(masking_ratio) > 1 && length(masking_ratio) != ncol(x_train)) {
    stop(paste0(
      "'Masking_ratio' contains masking ratios for ',", length(masking_ratio), "' features, ",
      "but there are '", ncol(x_train), "' features in 'x_train'."
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
vaeac_check_save_parameters <- function(save_data, save_every_nth_epoch, x_train_size) {
  if (save_data && !is.null(save_every_nth_epoch) && epochs / save_every_nth_epoch > 5) {
    message(paste0(
      "Having `save_data = TRUE` and `save_every_nth_epoch = ", save_every_nth_epoch, "` might requirer ",
      "a lot of disk storage if `x_train` (", x_train_size, ") is large."
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
                                   use_cuda,
                                   num_vaeacs_initiate,
                                   epochs_initiation_phase,
                                   epochs,
                                   epochs_early_stopping,
                                   save_every_nth_epoch,
                                   validation_ratio,
                                   validation_iwae_num_samples,
                                   depth,
                                   width,
                                   latent_dim,
                                   lr,
                                   batch_size,
                                   running_avg_num_values,
                                   activation_function,
                                   use_skip_connections,
                                   skip_connection_masked_enc_dec,
                                   use_batch_normalization,
                                   paired_sampling,
                                   masking_ratio,
                                   mask_gen_these_coalitions,
                                   mask_gen_these_coalitions_prob,
                                   sigma_mu,
                                   sigma_sigma,
                                   save_data,
                                   transform_all_cont_features,
                                   which_vaeac_model,
                                   verbose,
                                   seed,
                                   ...) {
  # Check verbose parameter
  vaeac_check_verbose(verbose = verbose)

  # Check that the activation function is valid torch::nn_module object
  vaeac_check_activation_func(activation_function = activation_function)

  # Check that the save folder exists and for a valid file name
  vaeac_check_save_names(folder_to_save_model = folder_to_save_model, model_description = model_description)

  # Check the probability parameters
  vaeac_check_probabilities(list(validation_ratio = validation_ratio, masking_ratio = masking_ratio))

  # Check the masking ratio
  vaeac_check_masking_ratio(masking_ratio = masking_ratio, n_features = ncol(x_train))

  # Check the positive numeric parameters
  vaeac_check_positive_numerics(list(lr = lr, sigma_mu = sigma_mu, sigma_sigma = sigma_sigma))

  # Check the mask_gen_these_coalitions and mask_gen_these_coalitions_prob parameters
  vaeac_check_mask_gen(
    mask_gen_these_coalitions = mask_gen_these_coalitions,
    mask_gen_these_coalitions_prob = mask_gen_these_coalitions_prob,
    x_train = x_train
  )

  # Check the logical parameters
  vaeac_check_logicals(list(
    use_cuda = use_cuda,
    use_skip_connections = use_skip_connections,
    skip_connection_masked_enc_dec = skip_connection_masked_enc_dec,
    use_batch_normalization = use_batch_normalization,
    paired_sampling = paired_sampling,
    save_data = save_data,
    transform_all_cont_features = transform_all_cont_features
  ))

  # Check the positive integer parameters
  unchecked_positive_integers <- list(
    num_vaeacs_initiate = num_vaeacs_initiate,
    epochs = epochs,
    epochs_early_stopping = epochs_early_stopping,
    epochs_initiation_phase = epochs_initiation_phase,
    validation_iwae_num_samples = validation_iwae_num_samples,
    depth = depth,
    width = width,
    latent_dim = latent_dim,
    batch_size = batch_size,
    running_avg_num_values = running_avg_num_values,
    seed = seed
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
    save_every_nth_epoch = save_every_nth_epoch,
    x_train_size = format(object.size(x_train), units = "auto")
  )

  # Check that user want to use the vaeac model at a valid checkpoint
  vaeac_check_which_vaeac_model(
    which_vaeac_model = which_vaeac_model,
    epochs = epochs,
    save_every_nth_epoch = save_every_nth_epoch)
}

# Get functions ========================================================================================================
#' Function that determines which mask generator to use
#'
#' @inheritParams vaeac_train_model
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_mask_generator_name <- function(mask_gen_these_coalitions, mask_gen_these_coalitions_prob, masking_ratio) {
  if (!is.null(mask_gen_these_coalitions) && !is.null(mask_gen_these_coalitions_prob)) {
    # User have provided mask_gen_these_coalitions (and mask_gen_these_coalitions_prob),
    # and we want to use Specified_masks_mask_generator
    mask_generator_name <- "Specified_masks_mask_generator"

    # Small printout
    if (verbose == 2) {
      message(paste0("Using 'Specified_masks_mask_generator' with '", nrow(mask_gen_these_coalitions), "' coalitions."))
    }
  } else if (length(masking_ratio) == 1) {
    # We are going to use 'MCAR_mask_generator' as masking_ratio is a singleton.
    # I.e., all feature values are equally likely to be masked based on masking_ratio.
    mask_generator_name <- "MCAR_mask_generator"

    # Small printout
    if (verbose == 2) message(paste0("Using 'MCAR_mask_generator' with 'masking_ratio = ", masking_ratio, "'."))
  } else if (length(masking_ratio) > 1) {
    # We are going to use 'Specified_prob_mask_generator' as masking_ratio is a vector (of same length as ncol(x_train).
    # I.e., masking_ratio[5] specifies the probability of masking 5 features
    mask_generator_name <- "Specified_prob_mask_generator"

    # We have an array of masking ratios. Then we are using the Specified_prob_mask_generator.
    if (verbose == 2) {
      message(paste0(
        "Using 'Specified_prob_mask_generator' mask generator with 'masking_ratio = [",
        paste(masking_ratio, collapse = ", "), "]'."
      ))
    }
  } else {
    stop("`vaeac` could not determine which masking scheme to use based on the givene parameter arguments.")
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
vaeac_get_save_file_names <- function(epochs, save_every_nth_epoch, folder_to_save_model = NULL) {
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
#' @param vaeac_model A `vaeac` model created using [vaeac()].
#' @param optimizer_name String containing the name of the [torch::optimizer()] to use.
#'
#' @return Array of string containing the save files to use when training the `vaeac` model. The first three names
#' corresponds to the best, best_running, and last epochs, in that order.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_optimizer <- function(vaeac_model, optimizer_name = "adam") {
  if (optimizer_name == "adam") {
    # Create the adam optimizer
    optimizer <- torch::optim_adam(
      params = vaeac_model$parameters,
      lr = lr,
      betas = c(0.9, 0.999),
      eps = 1e-08,
      weight_decay = 0,
      amsgrad = FALSE
    )
  } else {
    stop("Only the `adam` optimizer has been implemented for the `vaeac` approach.")
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
#' @return List containing the values of `epoch`, `train_vlb`, `validation_iwae`, `validation_iwae_running`,
#' and the `state_dict()` of the vaeac model and optimizer.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_current_save_state <- function(environment) {
  object_names <- c("epoch", "train_vlb", "validation_iwae", "validation_iwae_running")
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
#' `early_stopping_applied`, `running_avg_num_values`, `paired_sampling`, `mask_generator_name`, `masking_ratio`,
#' `mask_gen_these_coalitions`, `mask_gen_these_coalitions_prob`, `validation_ratio`, `validation_iwae_num_samples`,
#' `num_vaeacs_initiate`, `epochs_initiation_phase`, `width`, `depth`, `latent_dim`, `activation_function`,
#' `lr`, `batch_size`, `use_skip_connections`, `skip_connection_masked_enc_dec`, `use_batch_normalization`, `use_cuda`,
#' `train_indices`, `val_indices`, `save_every_nth_epoch`, `sigma_mu`,
#' `sigma_sigma`, `feature_list`, `col_cat_names`, `col_cont_names`, `col_cat`, `col_cont`, `cat_in_dataset`,
#' `map_new_to_original_names`, `map_original_to_new_names`, `transform_all_cont_features`, `save_data`, `verbose`,
#' `seed`, and `vaeac_save_file_names`.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_full_state_list <- function(environment) {
  object_names <- c(
    "norm_mean", "norm_std", "model_description", "folder_to_save_model", "n_train", "n_features", "one_hot_max_sizes",
    "epochs", "epochs_specified", "epochs_early_stopping", "early_stopping_applied", "running_avg_num_values",
    "paired_sampling", "mask_generator_name", "masking_ratio", "mask_gen_these_coalitions",
    "mask_gen_these_coalitions_prob", "validation_ratio", "validation_iwae_num_samples", "num_vaeacs_initiate",
    "epochs_initiation_phase", "width", "depth", "latent_dim", "activation_function",
    "lr", "batch_size", "use_skip_connections", "skip_connection_masked_enc_dec", "use_batch_normalization", "use_cuda",
    "train_indices", "val_indices", "save_every_nth_epoch", "sigma_mu", "sigma_sigma", "feature_list", "col_cat_names",
    "col_cont_names", "col_cat", "col_cont", "cat_in_dataset", "map_new_to_original_names", "map_original_to_new_names",
    "transform_all_cont_features", "save_data", "verbose", "seed", "vaeac_save_file_names"
  )
  objects <- lapply(object_names, function(name) environment[[name]])
  names(objects) <- object_names
  objects <- utils::modifyList(objects, environment[["x_train_preprocessed"]], keep.null = TRUE) # Flatten this list
  return(objects)
}



#' Function to extend the explicands and apply all relevant masks/coalitions
#'
#' @inheritParams explain
#' @inheritParams default_doc_explain
#' @param S The `internal$objects$S` matrix containing the possible coalitions.
#'
#' @return The extended version of `x_explain` where the masks from `S` with indices `index_features` have been applied.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_get_x_explain_extended = function(x_explain, S, index_features) {
  n_coaltions <- length(index_features) # Get the number of active coalitions
  mask <- S[index_features, , drop = FALSE] # Get the masks/coalitions we are to generate MC samples for
  mask[mask == 0] <- NaN  # Set zeros to `NaN` to indicate that they are missing and to be imputed by `vaeac`
  x_explain_extended <-
    x_explain[rep(seq_len(nrow(x_explain)), each = n_coaltions), ] # Extend the explicands `n_coalitions` times
  mask_extended <- mask[rep(seq(n_coaltions), times = n_explain), ] # Extend the masks `n_expliand` times
  x_explain_extended[is.na(mask_extended)] <- NaN # Apply the mask. The NaNs are features outside coalition S.
  return(x_explain_extended)
}



# Train functions ======================================================================================================


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
#' @param validation_iwae A [torch::torch_tensor()] (default is `NULL`)
#' of one dimension containing previous values for the validation IWAE.
#' @param validation_iwae_running A [torch::torch_tensor()] (default is `NULL`)
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
                                        validation_iwae_num_samples,
                                        running_avg_num_values,
                                        verbose,
                                        epochs,
                                        save_every_nth_epoch,
                                        epochs_early_stopping,
                                        epochs_start = 1,
                                        progressr_bar = NULL,
                                        vaeac_save_file_names = NULL,
                                        state_list = NULL,
                                        initialization_idx = NULL,
                                        num_vaeacs_initiate = NULL,
                                        train_vlb = NULL,
                                        validation_iwae = NULL,
                                        validation_iwae_running = NULL) {
  # Check for valid input
  if (xor(is.null(initialization_idx), is.null(num_vaeacs_initiate))) {
    stop("Either none or both of `initialization_idx` and `num_vaeacs_initiate` must be given.")
  }

  if (is.null(state_list) && is.null(initialization_idx)) {
    stop("`state_list` must be provide when `initialization_idx = NULL` to properly save the `vaeac` model.")
  }

  if (is.null(vaeac_save_file_names) && is.null(initialization_idx)) {
    stop(paste0(
      "`vaeac_save_file_names` must be provide when `initialization_idx = NULL` ",
      "to know where to save the vaeac model."
    ))
  }

  if (!((is.null(train_vlb) && is.null(validation_iwae) && is.null(validation_iwae_running)) ||
    (!is.null(train_vlb) && !is.null(validation_iwae) && !is.null(validation_iwae_running)))) {
    stop("Either none or all of `train_vlb`, `validation_iwae`, and `validation_iwae_running` must be given.")
  }

  # Variable that we change to `TRUE` if early stopping is applied
  if (!is.null(state_list)) state_list$early_stopping_applied <- FALSE

  # Variables to stores the state of the `vaeac` at the best epoch according to IWAE and IWAE_running
  if (is.null(initialization_idx)) best_state <- best_state_running <- NULL

  # Get the batch size
  batch_size <- train_dataloader$batch_size

  # Extract the mask generator and the variational lower bound scale factor from the vaeac model object.
  mask_generator <- vaeac_model$mask_generator
  vlb_scale_factor <- vaeac_model$vlb_scale_factor

  # Start the training loop
  epoch <- 1
  for (epoch in seq(from = epochs_start, to = epochs)) {
    # Set average variational lower bound to 0 for this epoch
    avg_vlb <- 0

    # Index to keep track of which batch we are working on.
    batch_index <- 1

    # batch <- train_dataloader$.iter()$.next()

    # Iterate over the training data
    coro::loop(for (batch in train_dataloader) {
      # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
      if (batch$shape[1] < batch_size) {
        batch <- extend_batch(batch = batch, dataloader = train_dataloader, batch_size = batch_size)
      }

      # Generate mask and do an optimizer step over the mask and the batch
      mask <- mask_generator(batch)

      # TODO: Send the batch and mask to Nvida GPU if we have. IS it here it should be?
      if (use_cuda) {
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
    validation_iwae_now <- vaeac_get_validation_iwae(
      val_dataloader = val_dataloader,
      mask_generator = mask_generator,
      batch_size = batch_size,
      vaeac_model = vaeac_model,
      validation_iwae_num_samples = validation_iwae_num_samples
    )
    validation_iwae <- torch::torch_cat(c(validation_iwae, validation_iwae_now), -1)

    # Compute the running validation IWAE
    validation_iwae_running_now <-
      validation_iwae[
        (-min(length(validation_iwae), running_avg_num_values) +
          length(validation_iwae) + 1):(-1 + length(validation_iwae) + 1),
        drop = FALSE
      ]$mean()$view(1)
    validation_iwae_running <- torch::torch_cat(c(validation_iwae_running, validation_iwae_running_now), -1)

    # Check if we are to save the models
    if (is.null(initialization_idx)) {
      # Save if current vaeac model has the lowest validation IWAE error
      if ((max(validation_iwae) <= validation_iwae_now)$item() || is.null(best_state)) {
        best_state <- c(vaeac_get_current_save_state(environment()), state_list)
        class(best_state) <- c(class(best_state), "vaeac")
        torch::torch_save(best_state, vaeac_save_file_names[1])
      }

      # Save if current vaeac model has the lowest running validation IWAE error
      if ((max(validation_iwae_running) <= validation_iwae_running_now)$item() || is.null(best_state_running)) {
        best_state_running <- c(vaeac_get_current_save_state(environment()), state_list)
        class(best_state_running) <- c(class(best_state_running), "vaeac")
        torch::torch_save(best_state_running, vaeac_save_file_names[2])
      }

      # Save if we are in an n'th epoch and are to save every n'th epoch
      if (is.integer(save_every_nth_epoch) && epoch %% save_every_nth_epoch == 0) {
        nth_state <- c(vaeac_get_current_save_state(environment()), state_list)
        class(nth_state) <- c(class(nth_state), "vaeac")
        torch::torch_save(nth_state, vaeac_save_file_names[3 + epoch %/% save_every_nth_epoch])
      }
    }

    # Handle the message to the progress bar based on if we are doing initialization or final training
    if (!is.null(progressr_bar)) {
      update_message <- if (!is.null(initialization_idx)) {
        paste0(
          "Training vaeac (init. ", initialization_idx, " of ", num_vaeacs_initiate, "): Epoch: ", epoch,
          " | VLB: ", round(avg_vlb$item(), 3), " | IWAE: ", round(validation_iwae_now$item(), 3)
        )
      } else {
        paste0(
          "Training vaeac (final model): Epoch: ", epoch, " | best epoch: ", best_state$epoch,
          " | VLB: ", round(avg_vlb$item(), 3), " | IWAE: ", round(validation_iwae_now$item(), 3)
        )
      }
      progressr_bar(message = update_message)
    }

    # Check if we are to apply early stopping, i.e., no improvement in the IWAE for `epochs_early_stopping` epochs.
    if (is.integer(epochs_early_stopping)) {
      if (epoch - best_state$epoch >= epochs_early_stopping) {
        if (verbose == 1) {
          message(paste0(
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
      validation_iwae = validation_iwae,
      validation_iwae_running = validation_iwae_running,
      avg_vlb = avg_vlb,
      initialization_idx = initialization_idx,
      state_list = state_list
    )
  } else {
    # Save the vaeac model at the last epoch
    last_state <- c(vaeac_get_current_save_state(environment()), state_list)
    class(last_state) <- c(class(last_state), "vaeac")
    torch::torch_save(last_state, vaeac_save_file_names[3])

    # Summary printout
    vaeac_train_print_summary(best_state, best_state_running, last_state)

    # Create a return list
    return_list <- list(
      best = vaeac_save_file_names[1],
      best_running = vaeac_save_file_names[2],
      last = vaeac_save_file_names[3],
      train_vlb = as.array(train_vlb),
      validation_iwae = as.array(validation_iwae),
      validation_iwae_running = as.array(validation_iwae_running),
      parameters = state_list
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
  }
  return(return_list)
}

#' Function to printout a training summary for the `vaeac` model
#'
#' @param best_state The state list (i.e., the saved `vaeac` object) of the `vaeac`
#' model at the epoch with the lowest IWAE.
#' @param best_state_running The state list of (i.e., the saved `vaeac` object)
#' the `vaeac` model at the epoch with the lowest running IWAE.
#' @param last_epoch The state list (i.e., the saved `vaeac` object)
#' of `vaeac` model at the epoch with the lowest IWAE.
#'
#' @return This function only prints out a message.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_train_print_summary <- function(best_state, best_state_running, last_state) {
  message(sprintf(
    "\nResults:
Best epoch:             %d. \tVLB = %.3f. \tIWAE = %.3f \tIWAE_running = %.3f.
Best running avg epoch: %d. \tVLB = %.3f. \tIWAE = %.3f \tIWAE_running = %.3f.
Last epoch:             %d. \tVLB = %.3f. \tIWAE = %.3f \tIWAE_running = %.3f.\n",
    best_state$epoch,
    best_state$train_vlb[-1],
    best_state$validation_iwae[-1],
    best_state$validation_iwae_running[-1],
    best_state_running$epoch,
    best_state_running$train_vlb[-1],
    best_state_running$validation_iwae[-1],
    best_state_running$validation_iwae_running[-1],
    last_state$epoch,
    last_state$train_vlb[-1],
    last_state$validation_iwae[-1],
    last_state$validation_iwae_running[-1]
  ))
}

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
  vaeac.main_para_default_names <- formalArgs(setup_approach.vaeac) # TODO: check if I need to add "shapr:::"
  vaeac.main_para_default_names <-
    vaeac.main_para_default_names[!vaeac.main_para_default_names %in% c("internal", "vaeac.extra_parameters", "...")]

  # Get the default values for vaeac's main parameters defined above into a named list
  vaeac.main_para_default <- as.list(formals(sys.function(sys.parent())))
  vaeac.main_para_default <- vaeac.main_para_default[vaeac.main_para_default %in% vaeac.main_para_default_names]
  # vaeac.main_para_default <- mget(vaeac.main_para_default_names)
  # defaults = vaeac.main_para_default

  # Get the names of the vaeac's main parameters provided by the user
  vaeac.main_para_user_names <- names(parameters)
  vaeac.main_para_user_names <- vaeac.main_para_user_names[grepl("vaeac.", vaeac.main_para_user_names)]
  vaeac.main_para_user_names <- vaeac.main_para_user_names[!vaeac.main_para_user_names %in% "vaeac.extra_parameters"]

  # Get the default values for vaeac's extra parameters into a named list
  vaeac.extra_para_default <- vaeac_extra_para_default()
  vaeac.extra_para_default_names <- names(vaeac.extra_para_default)

  # Get the names of the extra parameters provided by the user
  vaeac.extra_para_user_names <- names(parameters$vaeac.extra_parameters)

  # Get the names of all parameters and the user specified parameters
  vaeav.all_para_default_names <- c(vaeac.main_para_default_names, vaeac.extra_para_default_names)

  # Check if any of the main parameters with the "vaeac." prefix is unknown (i.e., not main or extra parameter)
  not_extra_para_in_main_para <-
    vaeac.main_para_user_names[!vaeac.main_para_user_names %in% vaeav.all_para_default_names]
  if (length(not_extra_para_in_main_para) > 0) {
    # Give a message to the user about the unknown extra parameters
    warning(paste0(
      "The following vaeac main parameters are not recognized (`shapr` removes them): ",
      paste(strsplit(paste(paste0("`", not_extra_para_in_main_para, "`"), collapse = ", "), ",(?=[^,]+$)", perl = TRUE)[[1]], collapse = " and"), ".\n"
    ))

    # Delete the unknown extra parameters
    parameters[not_extra_para_in_main_para] <- NULL
  }

  # Check if any of the extra parameters with the "vaeac." prefix is unknown (i.e., not main or extra parameter)
  not_main_para_in_extra_para <-
    vaeac.extra_para_user_names[!vaeac.extra_para_user_names %in% vaeav.all_para_default_names]
  if (length(not_main_para_in_extra_para) > 0) {
    # Give a message to the user about the unknown extra parameters
    warning(paste0(
      "The following vaeac extra parameters are not recognized (`shapr` removes them): ",
      paste(strsplit(paste(paste0("`", not_main_para_in_extra_para, "`"), collapse = ", "), ",(?=[^,]+$)", perl = TRUE)[[1]], collapse = " and"), ".\n"
    ))

    # Delete the unknown extra parameters
    parameters$vaeac.extra_parameters[not_main_para_in_extra_para] <- NULL
  }

  # Check for parameters that have been provided as both main and extra parameter
  both_main_and_extra_para <- vaeac.extra_para_user_names[vaeac.extra_para_user_names %in% vaeac.main_para_user_names]
  if (length(both_main_and_extra_para > 0)) {
    # Print a message to the user and tell them that we use those in `vaeac.extra_parameters`.
    warning(paste0(
      "The following vaeac parameters were given as both main and extra parameters (`shapr` uses the ",
      "values at the correct location ): ",
      paste(strsplit(paste(paste0("`", both_main_and_extra_para, "`"), collapse = ", "),
        ",(?=[^,]+$)",
        perl = TRUE
      )[[1]], collapse = " and"), ".\n"
    ))

    # Note that we do not move it here as the moving will be fixed in the next two if-clauses
  }

  # Check if any any extra parameters have been given as main parameters
  extra_para_in_main_para <- vaeac.main_para_user_names[vaeac.main_para_user_names %in% vaeac.extra_para_default_names]
  if (length(extra_para_in_main_para) > 0) {
    warning(paste0(
      "The following vaeac parameters were given as main parameters but should have been extra ",
      "parameters (`shapr` fixes this): ",
      paste(strsplit(paste(paste0("`", extra_para_in_main_para, "`"), collapse = ", "),
        ",(?=[^,]+$)",
        perl = TRUE
      )[[1]], collapse = " and"), ".\n"
    ))

    # Move extra parameter from the main parameters to extra_parameters list if they have NOT been specified already
    parameters$vaeac.extra_parameters[extra_para_in_main_para[!extra_para_in_main_para %in% vaeac.extra_para_user_names]] <-
      parameters[extra_para_in_main_para[!extra_para_in_main_para %in% vaeac.extra_para_user_names]]

    # Remove the extra parameter from the main parameters
    parameters[extra_para_in_main_para] <- NULL
  }

  # Check if any any main parameters have been given as extra parameters
  main_para_in_extra_para <- vaeac.extra_para_user_names[vaeac.extra_para_user_names %in% vaeac.main_para_default_names]
  if (length(main_para_in_extra_para) > 0) {
    # Give a message to the user about the misplaced main parameters in the extra list
    warning(paste0(
      "The following vaeac parameters were given as extra parameters but should have been main ",
      "parameters (`shapr` fixes this): ",
      paste(strsplit(paste(paste0("`", main_para_in_extra_para, "`"), collapse = ", "), ",(?=[^,]+$)", perl = TRUE)[[1]], collapse = " and"), ".\n"
    ))

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
#' @param vaeac_object List or String. 1) Either a list of class
#' `vaeac`, i.e., the list stored in `explanation$internal$parameters$vaeac` where `explanation` is the returned list
#' from an earlier call to the [shapr::explain()] function. 2) A string containing the path to where the `vaeac`
#' model is stored on disk, for example, `explanation$internal$parameters$vaeac$models$best`.
#'
#' @return This function adds a valid pre-trained vaeac model to the `parameter`
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_update_pretrained_model <- function(vaeac_object, parameters) {
  if (!(is.list(vaeac_object) || is.character(vaeac_object))) {
    stop("The `vaeac.pretrained_vaeac_model` parameter must be either a list or a string. Read documentation.")
  }

  # Check if we are given a list
  if (is.list(vaeac_object)) {
    # Check for list of type vaeac
    if (!("vaeac" %in% class(vaeac_object))) stop("The `vaeac.pretrained_vaeac_model` list is not of type `vaeac`.")

    if (parameters$n_features != vaeac_object$parameters$n_features) {
      stop(paste0(
        "The provided `vaeac` model is trainined on a ", vaeac_object$parameters$n_features,
        "-dimensional dataset, but the current dataset is ", parameters$n_features, "-dimensional."
      ))
    }

    # Check that the labels of x_train matches the labels of the training data used to train the vaeac model.
    if (!all.equal(parameters$feature_names, vaeac_object$parameters$feature_list$labels)) {
      stop(paste0(
        "The labels of the training data ('", paste(parameters$feature_names, collapse = "', '"), "') ",
        "do not match the labels of the `vaeac` model's training data (`",
        paste(vaeac_object$parameters$feature_list$labels, collapse = "', '"), "`)."
      ))
    }

    # Add the pre-trained valid vaeac model to the parameters list
    parameters$vaeac <- parameters$vaeac.pretrained_vaeac_model
  }


  # Check if we are given a string
  if (is.character(vaeac_object)) {
    # Check that the file exists
    if (!file.exists(vaeac_object)) {
      stop(paste0("The `vaeac.pretrained_vaeac_model` file ('", vaeac_object, "') does not exist."))
    }

    # Read in the vaeac model from the disk
    vaeac_model <- torch::torch_load(parameters$vaeac.pretrained_vaeac_model)

    # Some very small check that we have read in a vaeac model
    if (is.null(vaeac_model$model_state_dict)) {
      stop("The provided file is not a vaeac model as it is missing, e.g., the `model_state_dict` entry.")
    }
    if (is.null(vaeac_model$optimizer_state_dict)) {
      stop("The provided file is not a vaeac model as it is missing, e.g., the `optimizer_state_dict` entry.")
    }

    # Check that the provided vaeac model is trained on a dataset with the same number of features.
    if (parameters$n_features != vaeac_model$n_features) {
      stop(paste0(
        "The provided `vaeac` model is trainined on a ", vaeac_model$n_features, "-dimensional ",
        "dataset, but the current dataset is ", parameters$n_features, "-dimensional."
      ))
    }

    # Check that the labels of x_train matches the labels of the training data used to train the vaeac model.
    if (!all.equal(parameters$feature_names, vaeac_model$feature_list$labels)) {
      stop(paste0(
        "The labels of the training data ('", paste(parameters$feature_names, collapse = "', '"), "') ",
        "do not match the labels of the `vaeac` model's training data (`",
        paste(vaeac_model$feature_list$labels, collapse = "', '"), "`)."
      ))
    }

    # Extract the training/validation results
    evaluation_criterions <- c("train_vlb", "validation_iwae", "validation_iwae_running")
    vaeac_model_results <- lapply(vaeac_model[evaluation_criterions], as.array)

    # Save path to the vaeac approach to use to generate the MC samples.
    parameters$vaeac <- list(
      models = list(best = parameters$vaeac.pretrained_vaeac_model),
      results = vaeac_model_results,
      parameters = vaeac_model[-seq(2, 7)], # TODO: SJEKK TALLENE HER
    )

    # Add `vaeac` as a class to the object. We use this to validate the input when
    # `vaeac.pretrained_vaeac_model` is given to the `shapr::explain()` function.
    class(parameters$vaeac) <- c(class(parameters$vaeac), "vaeac")
  }

  # Return the updated parameters list
  return(parameters)
}
