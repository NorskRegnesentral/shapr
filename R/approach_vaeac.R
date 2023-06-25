# Laste ned shapr fra github: remotes::install_github("NorskRegnesentral/shapr", ref = "devel")
# pkgload::load_all()
# source("~/PhD/Paper2/VAEAC_in_R.R")

#  Load Required Libraries ==============================================================================================================================================================================================
#{
#  library(torch)
#  library(progress)
#  library(data.table)
#  library(ggplot2)
#  library(GGally)
#}

#' @rdname setup_approach
#'
#' @param vaeac.model_description String containing, e.g., the name of the data distribution or additional parameter information. Used in the save name of the fitted model.
#' @param vaeac.folder_to_save_model String specifying a path to a folder where the function is to save the fitted VAEAC model.
#' @param vaeac.use_cuda Boolean. If we are to use cuda (GPU) if available. NOT TESTED!
#' @param vaeac.num_different_vaeac_initiate Integer. The number of different VAEAC models to initiate in the start. Pick the best performing one after \code{epochs_initiation_phase } and continue training that one.
#' @param vaeac.epochs_initiation_phase Integer. The number of epochs to run each of the \code{num_different_vaeac_initiate} VAEAC models before only continuing training the best one.
#' @param vaeac.save_VAEAC_every_nth_epoch Integer. If we are to save the VAEAC model after every nth epoch.
#' @param vaeac.epochs Integer. The number of epochs to train the final VAEAC model. This includes \code{epochs_initiation_phase}.
#' @param vaeac.validation_ratio Scalar between 0 and 1 indicating the ratio of instances from data which will be used as validation data.
#' @param vaeac.validation_iwae_num_samples Integer. The number of samples used to compute the IWAE when validating the VAEAC model on the validation data.
#' @param vaeac.depth Integer. The number of hidden layers in the neural networks of the masked encoder, full encoder, and decoder.
#' @param vaeac.width Integer. The number of neurons in each hidden layer in the neural networks of the masked encoder, full encoder, and decoder.
#' @param vaeac.latent_dim Integer. The number of dimensions in the latent space.
#' @param vaeac.lr Numeric. The learning rate used in the ADAM optimizer.
#' @param vaeac.batch_size Integer. The number of samples to include in each batch.
#' @param vaeac.running_avg_num_values Integer. How many of the previous values to include when we compute the running means.
#' @param vaeac.activation_function An nn_module representing an activation function. E.g., nn_relu, nn_leaky_relu, nn_selu, nn_sigmoid.
#' @param vaeac.use_skip_connections Boolean. If we are to use skip connections in each layer. If true, then we add the input to the outcome of each hidden layer, so the output becomes X + activation(WX + b). I.e., identity skip connection.
#' @param vaeac.use_skip_connections_between_masked_encoder_and_decoder Boolean. If we are to apply concatenate skip connections between the layers in the masked encoder and decoder.
#' @param vaeac.use_batch_normalization Boolean. If we are to use batch normalization after the activation function. Note that if \code{use_skip_connections} is TRUE, then the normalization is
#' done after the adding from the skip connection. I.e, we batch normalize the whole quantity X + activation(WX + b).
#' @param vaeac.paired_sampling Boolean. If we are doing paired sampling. I.e., each batch contains two versions of the same training observation,
#' but where the first one is masked by S and the second one is masked by \bar{S}, the complement. See FastSHAP by Jethani et al (2022). Training becomes more stable, but slower due to non-optimal implementation.
#' @param vaeac.masking_ratio Probability of masking a feature in the MCAR mask generator. Default masking scheme which ensures that
#' VAEAC can do arbitrary conditioning. Is overruled if \code{mask_generator_only_these_coalitions} is specified.
#' @param vaeac.mask_generator_only_these_coalitions Matrix containing the different coalitions to learn.
#' @param vaeac.mask_generator_only_these_coalitions_probabilities Numerics containing the probabilities for sampling each mask in \code{mask_generator_only_these_coalitions}.
#'  Array containing the probabilities for sampling the coalitions in \code{mask_generator_only_these_coalitions}.
#' @param vaeac.sigma_mu Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder, see Section 3.3.1 in Olsen et al. (2022).
#' @param vaeac.sigma_sigma Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder, see Section 3.3.1 in Olsen et al. (2022).
#' @param vaeac.save_data Boolean. If we are to save the data together with the model. Useful if one are to continue to train the model later.
#' @param vaeac.transform_all_continuous_features Boolean. If we are to log transform all continuous features before sending the data to VAEAC.
#' VAEAC creates unbounded values, so if the continuous features are strictly positive, as for Burr and Abalone data, it can be advantageous
#' to log-transform the data to unbounded form before using VAEAC. If TRUE, then \code{VAEAC_postprocess_data} will take the exp of the results
#' to get back to strictly positive values when using the VAEAC model to impute missing values.
#' @param vaeac.verbose Boolean. If we are to print the progress of the initialization of different VAEAC models, the training of the final VAEAC model, and summary of the training progress.
#' @param vaeac.seed Integer. Seed for reproducibility.
#'
#' @inheritParams default_doc_explain
#'
#' @export
setup_approach.vaeac = function(internal, # add default values for vaeac here.
                                vaeac.model_description = NULL,
                                vaeac.folder_to_save_model = NULL,
                                vaeac.use_cuda = FALSE,
                                vaeac.num_different_vaeac_initiate = 10,
                                vaeac.epochs_initiation_phase = 2,
                                vaeac.epochs = 200,
                                vaeac.save_VAEAC_every_nth_epoch = NULL,
                                vaeac.validation_ratio = 0.25,
                                vaeac.validation_iwae_num_samples = 25,
                                vaeac.depth = 3,
                                vaeac.width = 32,
                                vaeac.latent_dim = 8,
                                vaeac.lr = 0.001,
                                vaeac.batch_size = 64,
                                vaeac.running_avg_num_values = 5,
                                vaeac.activation_function = nn_relu,
                                vaeac.use_skip_connections = TRUE,
                                vaeac.use_skip_connections_between_masked_encoder_and_decoder = TRUE,
                                vaeac.use_batch_normalization = FALSE,
                                vaeac.paired_sampling = TRUE,
                                vaeac.masking_ratio = 0.5,
                                vaeac.mask_generator_only_these_coalitions = NULL,
                                vaeac.mask_generator_only_these_coalitions_probabilities = NULL,
                                vaeac.sigma_mu = 1e4,
                                vaeac.sigma_sigma = 1e-4,
                                vaeac.save_data = FALSE,
                                vaeac.transform_all_continuous_features = FALSE,
                                vaeac.verbose = FALSE,
                                vaeac.seed = 1996,
                                ...) {
  # Lars: this function trains a vaeac model and adds it to the
  # internal$parameters list and returns it. The hyperparameters
  # for vaeac is contained in the internal$parameters list.
  # Thus, there should be no need to use the "..." variables.

  # We use the same seed for vaeac as the one specified for shapr::explain if not both has been provided.
  #print(internal$parameters)
  if (is.null(internal$parameters$vaeac.seed)) {
    internal$parameters$vaeac.seed = internal$parameters$seed
  }
  #print(2)
  #print(internal$parameters)

  # We use the same verbose for vaeac as the one specified for shapr::explain if not both has been provided.
  if (is.null(internal$parameters$vaeac.seed)) {
    internal$parameters$vaeac.verbose = internal$parameters$verbose
  }

  # The mget extract the default values defined for the variables to this function.
  defaults = mget(c("vaeac.model_description", "vaeac.folder_to_save_model", "vaeac.use_cuda", "vaeac.num_different_vaeac_initiate",
                    "vaeac.epochs_initiation_phase", "vaeac.epochs", "vaeac.save_VAEAC_every_nth_epoch", "vaeac.validation_ratio",
                    "vaeac.validation_iwae_num_samples", "vaeac.depth", "vaeac.width", "vaeac.latent_dim", "vaeac.lr",
                    "vaeac.batch_size", "vaeac.running_avg_num_values", "vaeac.activation_function", "vaeac.use_skip_connections",
                    "vaeac.use_skip_connections_between_masked_encoder_and_decoder", "vaeac.use_batch_normalization",
                    "vaeac.paired_sampling", "vaeac.masking_ratio", "vaeac.mask_generator_only_these_coalitions",
                    "vaeac.mask_generator_only_these_coalitions_probabilities", "vaeac.sigma_mu", "vaeac.sigma_sigma",
                    "vaeac.save_data", "vaeac.transform_all_continuous_features", "vaeac.verbose", "vaeac.seed"))

  # If a variable has not been specified by the user, then we use the default values extracted above.
  internal = insert_defaults(internal, defaults)

  # Extract the parameters list, which, e.g., contains user-provided
  # parameters for the vaeac approach. Also extract the objects list
  # which, e.g., contains information about the possible coalitions.
  parameters = internal$parameters
  objects = internal$objects

  # Check if we are considering all coalitions or a sampled subset of them.
  if (!parameters$exact | parameters$is_groupwise) {
    # We have sampled a subset of all possible coalitions.
    # Which means that we either do feature Shapley values, but only
    # consider 'parameters$n_combinations' of the coalitions.
    # Or, we are doing group Shapley values, where only the coalitions
    # which respects the group are used. I.e., either the whole group
    # is unconditional, or it is conditioned on.
    # The matrix objects$S contains the relevant coalitions, and will
    # always contain the empty and grand coalitions.

    # Extract the possible coalitions / masks.
    # Remove the empty and grand coalition, as we do not need to train
    # these as they are handled differently when computing Shapley values
    parameters$vaeac.mask_generator_only_these_coalitions =
      objects$S[-c(1, parameters$n_combinations),, drop = FALSE]

    # Extract the weights for the different coalitions / masks.
    # Remove the empty and grand coalition, as we do not need to train
    # these as they are handled differently when computing Shapley values
    parameters$vaeac.mask_generator_only_these_coalitions_probabilities =
      objects$X$shapley_weight[-c(1, parameters$n_combinations)]

    # Normalize the probabilities such that they sum to one.
    parameters$vaeac.mask_generator_only_these_coalitions_probabilities =
      parameters$vaeac.mask_generator_only_these_coalitions_probabilities /
      sum(parameters$vaeac.mask_generator_only_these_coalitions_probabilities)
  } else {
    # We are investigating all coalitions and we are going to use the MCAR(0.5) masking scheme.
    # Then our vaeac model will support arbitrary conditioning as every coalition will be trained
    # with the same probability, also for the empty and grand coalition.
    # PS. this else bracket is technically not needed as these entries are set to NULL by default
    parameters$vaeac.mask_generator_only_these_coalitions = NULL
    parameters$vaeac.mask_generator_only_these_coalitions_probabilities = NULL
  }

  # Check if user provided a pre-trained vaeac model, otherwise, we train one from scratch.
  if (is.null(parameters$vaeac.pretrained_VAEAC_model)) {
    # User did not provide pre-trained model.
    # We train a VAEAC model based on the given hyper-parameters, or use
    # the default hyper-parameter values if nothing has been provided.

    # Boolean representing that a pre-trained VAEAC model was NOT provided.
    parameters$vaeac.user_provided_pretrained_VAEAC_model = FALSE

    # Extract the training data from the explainer object
    x_train = internal$data$x_train

    # Fit/train the VAEAC model with the provided model parameters
    VAEAC_training_time = system.time({
      parameters_temp = parameters[grepl("vaeac.", names(parameters))]
      #print(names(parameters_temp))

      # Remove "vaeac." from the names
      # TODO: This can be dangerous if there is another variable which matches the name.
      # I.e., if we have a variable called "verbose" and "vaeac.verbose", then these will
      # after running this code have the same name.
      updated_names = sapply(strsplit(names(parameters_temp), "\\."), '[', 2)
      names(parameters_temp)[!is.na(updated_names)] = updated_names[!is.na(updated_names)]

      #print(names(parameters_temp))
      VAEAC_model = do.call(train_VAEAC_model, c(parameters_temp, list(training_data = x_train)))
    })

    # Extract the paths to the trained VAEAC models.
    VAEAC_model_names = VAEAC_model[grepl("filename", names(VAEAC_model))]
    names(VAEAC_model_names) = sub("filename_", "", names(VAEAC_model_names))

    # Extract the training/validation results.
    VAEAC_model_results = VAEAC_model[grepl("vlb", names(VAEAC_model)) | grepl("iwae", names(VAEAC_model))]

    # Create a list of all the parameters used to train the VAEAC-model
    VAEAC_model_parameters = VAEAC_model$parameters

    # Add this to the explainer object
    parameters$VAEAC = list("models"        = VAEAC_model_names,
                            "results"       = VAEAC_model_results,
                            "parameters"    = VAEAC_model_parameters,
                            "training_time" = VAEAC_training_time)

    # Add that "parameters$VAEAC" is not just a list, but also of type "vaeac".
    # This is used to validate the input if the user sends "parameters$VAEAC"
    # as a "vaeac.pretrained_VAEAC_model" to the shapr::explain function.
    class(parameters$VAEAC) = c(class(parameters$VAEAC), "vaeac")

  } else {
    # User provided a pre-trained VAEAC model.

    # This can either be a list of type "vaeac", i.e., the list stored in
    # explanation$internal$parameters$VAEAC from an earlier call to the
    # shapr::explain() function.
    # Or it can be a string containing the path to where the "vaeac" model
    # is stored on disk.
    # Minimal checking for valid VAEAC model is conducted.

    # Boolean representing that a pre-trained VAEAC model was provided
    parameters$vaeac.user_provided_pretrained_VAEAC_model = TRUE

    # Extract the relevant names based on what the input is.
    if (is.list(parameters$vaeac.pretrained_VAEAC_model)) {

      # Check that the list is of type vaeac.
      if (all(class(parameters$vaeac.pretrained_VAEAC_model) != "vaeac")) {
        stop("The provided 'vaeac.pretrained_VAEAC_model' is not a list of type 'vaeac'.")
      }

      # Check that the provided VAEAC model is trained on a dataset with the same number of features.
      if (parameters$n_features != parameters$vaeac.pretrained_VAEAC_model$parameters$p) {
        stop(sprintf("The current dataset is %d-dimensional, but the provided VAEAC model was trained on a %d-dimensional dataset.\n",
                     parameters$n_features,
                     parameters$vaeac.pretrained_VAEAC_model$parameters$p))
      }

      # Check that the labels of x_train matches the labels of the training data used to train the VAEAC model.
      if (!all.equal(parameters$feature_names, parameters$vaeac.pretrained_VAEAC_model$parameters$feature_list$labels)) {
        stop(sprintf("The labels of the training data (%s) do not match the labels of the data used to train the provided VAEAC model (%s).\n",
                     paste(parameters$feature_names, collapse = ", "),
                     paste(parameters$vaeac.pretrained_VAEAC_model$parameters$feature_list$labels, collapse = ", ")))
      }

      # The pre-trained VAEAC model has passed the checks and we therefore
      # adds it to the parameters list as a valid VAEAC model.
      parameters$VAEAC = parameters$vaeac.pretrained_VAEAC_model

    } else if (is.character(parameters$vaeac.pretrained_VAEAC_model)) {
      # We are only provided with a string, which we assume it the path to a saved VAEAC object.

      # Check that the file exists
      if (!file.exists(parameters$vaeac.pretrained_VAEAC_model)) {
        stop(sprintf("The 'vaeac.pretrained_VAEAC_model' file with path '%s' does not exist.\n", parameters$vaeac.pretrained_VAEAC_model))
      }

      # Read in the VAEAC model from the disk
      VAEAC_model = torch_load(parameters$vaeac.pretrained_VAEAC_model)

      # Some very small check that we have read in a vaeac model
      if (is.null(VAEAC_model$model_state_dict)) {
        stop(sprintf("The provided file is not a VAEAC model as it is missing, e.g., the model_state_dict entry.\n"))
      }
      if (is.null(VAEAC_model$optimizer_state_dict)) {
        stop(sprintf("The provided file is not a VAEAC model as it is missing, e.g., the optimizer_state_dict entry.\n"))
      }

      # Check that the provided VAEAC model is trained on a dataset with the same number of features.
      if (parameters$n_features != VAEAC_model$p) {
        stop(sprintf("The current dataset is %d-dimensional, but the provided VAEAC model was trained on a %d-dimensional dataset.\n",
                     parameters$n_features,
                     VAEAC_model$p))
      }

      # Check that the labels of x_train matches the labels of the training data used to train the VAEAC model.
      if (!all.equal(parameters$feature_names, VAEAC_model$feature_list$labels)) {
        stop(sprintf("The labels of the training data (%s) do not match the labels of the data used to train the provided VAEAC model (%s).\n",
                     paste(parameters$feature_names, collapse = ", "),
                     paste(VAEAC_model$feature_list$labels, collapse = ", ")))
      }

      # Extract the training/validation results.
      VAEAC_model_results = lapply(VAEAC_model[c("train_vlb", "validation_iwae", "validation_iwae_running_avg")], as.array)
      VAEAC_model_results

      # Save path to the VAEAC approach to use to generate the MC samples.
      parameters$VAEAC = list("models"        = list("best" = parameters$vaeac.pretrained_VAEAC_model),
                              "results"       = VAEAC_model_results,
                              "parameters"    = VAEAC_model[-seq(2,7)],
                              "training_time" = system.time({NULL})*NA)
      # A hack as the training time is not known. Using this hack we can also continue to train this
      # vaeac object but the training time will just be NA for all entries.

      # Add that "parameters$VAEAC" is not just a list, but also of type "vaeac".
      # This is used to validate the input if the user sends "parameters$VAEAC"
      # as a "pretrained_VAEAC_model" to the shapr::explain function.
      class(parameters$VAEAC) = c(class(parameters$VAEAC), "vaeac")

    } else {
      stop(sprintf("The variable 'vaeac.pretrained_VAEAC_model' is not a list or a string. Read the documentation.\n"))
    }
  }

  # Update/overwrite the parameters list in the internal list.
  internal$parameters = parameters

  if (!is.null(parameters$verbose)) {
    if (parameters$verbose) cat(sprintf("Done with 'setup_approach.vaeac'.\n"))
  }

  # Return the updated internal list.
  return(internal)
}



#' @rdname prepare_data
#' @export
prepare_data.vaeac = function(internal, index_features = NULL, ...) {

  if (!is.null(internal$parameters$verbose)) {
    if (internal$parameters$verbose) cat(sprintf("Starting 'prepare_data.vaeac'.\n"))
  }

  # Extract the observations to explain. This will always be a data.table
  x_explain = internal$data$x_explain

  # Extract the number of observations to explain
  n_explain = internal$parameters$n_explain

  # Extract the number of features
  n_features = internal$parameters$n_features

  # Extract the number of MC samples to generate
  n_samples = internal$parameters$n_samples

  # Extract the trained VAEAC model
  VAEAC_list = internal$parameters$VAEAC

  # Figure out which of the stored VAEAC checkpoints we are going to use.
  if (is.null(internal$parameters$which_vaeac_model)) {
    which_vaeac_model = "best"
  } else {
    if (internal$parameters$which_vaeac_model %in% names(VAEAC_list$models)) {
      # User provided a string which matches one of the file names and we use it.
      which_vaeac_model = internal$parameters$which_vaeac_model
    } else {
      # User provided a string which is not one of the file names. Overwrite it.
      which_vaeac_model = "best"
      warning(sprintf("The provided string for 'which_vaeac_model' (%s) did not match any stored checkpoints (%s).\nWe set 'which_vaeac_model = best' and continue.\n",
                      internal$parameters$which_vaeac_model,
                      paste(names(VAEAC_list$models), collapse = ", ")))
    }
  }

  # Create a path entry with the path to the VAEAC model that we are to use.
  VAEAC_list$path = VAEAC_list$models[[which_vaeac_model]]

  # Extract the relevant masks.
  # Index_features is an array of numbers indicating witch of the coalitions to sample from.
  # If n_batches == 1, then index_features is an array of the indices to all (sampled) coalitions.
  # If n_batches >= 2, then index_features is a subset of all possible indices.
  # The matrix internal$objects$S contains all (sampled) coalitions, always including the empty
  # and grand coalition. We extract the relevant coalitions we are to generate MC samples from.
  if (is.null(index_features)) {
    # This clause will never be run as this function should only be called inside the
    # shapr package, and then index_features will always be provided.
    mask = internal$objects$S[-1,, drop = FALSE]
    mask_id = seq(2, internal$parameters$n_combinations)
  } else {
    # We will always be in this situation.

    # Extract the relevant coalitions
    mask = internal$objects$S[index_features,, drop = FALSE]

    # The ID for each mask is the same as the index_features
    mask_id = index_features
  }

  # Get the number of active coalitions.
  n_coaltions = length(index_features)

  # Set zeros to indicate that the corresponding feature value is missing.
  mask[mask == 0] = NaN

  # Create the extended version of x_explain where each observation is repeated n_coalitions times.
  x_explain_extended = x_explain[rep(seq(nrow(x_explain)), each = n_coaltions),]

  # Get the number of observations, after applying the mask, we are to generate MC samples for.
  n_explain_extended = nrow(x_explain_extended)

  # Extend the mask to replicating it n_explain times.
  mask_extended = mask[rep(seq(n_coaltions), times = n_explain),]

  # Apply the mask. The NaN entries indicate the unconditional features, which we
  # are going to create n_sampled conditional MC samples for.
  x_explain_extended[is.na(mask_extended)] = NaN

  # Extract/set the batch size. Larger batch sizes is often much faster provided sufficient memory.
  if (is.null(internal$parameters$batch_size_sampling)) {
    # If user has not specified a desired size, then we do the whole batch in one go.
    # This is also indirectly controlled by n_batches in explain.
    batch_size = n_explain_extended
  } else {
    # Use the user provided batch size
    batch_size = internal$parameters$batch_size_sampling

    # Check/set valid batch size
    if (batch_size > n_explain_extended) batch_size = n_explain_extended
  }

  # TODO: Get access to GPU to check if it works
  # If we are to use cuda
  use_cuda = VAEAC_list$parameters$use_cuda

  # Check if cuda/GPU is available on the current system
  cuda_available = torch::cuda_is_available()

  # Give warning to user if asked to run on cuda, but cuda is not available.
  if (isFALSE(cuda_available) & isTRUE(use_cuda)) {
    use_cuda = FALSE
    warning("Cuda is not available. Uses CPU instead.", immediate. = TRUE)
  }

  # Load the VAEAC model from provided disk location.
  checkpoint = torch_load(VAEAC_list$path)

  # Preprocess the data. This function turns factor names into numerics 1,2,...,K,
  # as VAEAC only accepts numerics, and keep track of the maping of names.
  # And optionally log-transform all continuous features. Usual for strictly positive
  # data set like Burr and Abalone, such that VAEAC does not impute negative values.
  # Extract the pre-processed data by calling "$x_train".
  # TODO: Maybe update the name, as x_train might be confusing. -----
  x_explain_extended = VAEAC_preprocess_data(x_explain_extended, checkpoint$transform_all_continuous_features)$x_train

  # Create the VAEAC model based on the parameters in the checkpoint.
  model = VAEAC(one_hot_max_sizes = checkpoint$one_hot_max_sizes,
                width = checkpoint$width,
                depth = checkpoint$depth,
                latent_dim = checkpoint$latent_dim,
                activation_function = checkpoint$activation_function,
                use_skip_connections = checkpoint$use_skip_connections,
                use_skip_connections_between_masked_encoder_and_decoder = checkpoint$use_skip_connections_between_masked_encoder_and_decoder,
                use_batch_normalization = checkpoint$use_batch_normalization,
                mask_generator_name = checkpoint$mask_generator_name,
                masking_ratio = checkpoint$masking_ratio,
                mask_generator_only_these_coalitions = checkpoint$mask_generator_only_these_coalitions,
                mask_generator_only_these_coalitions_probabilities = checkpoint$mask_generator_only_these_coalitions_probabilities,
                sigma_mu = checkpoint$sigma_mu,
                sigma_sigma = checkpoint$sigma_sigma)

  # Update the model's state dictionary to the one in the checkpoint.
  # This sets the weights and biases in the network.
  model$load_state_dict(checkpoint$model_state_dict)

  # Extract sampling method.
  # I.e., if we are to generate random samples from the inferred generative distributions,
  # or if we are to sample the most likely values (mean for cont, class with highest prob for cat).
  # We will always use random unless user specify otherwise.
  if (is.null(internal$parameters$sample_random)) {
    # Set it to true.
    sample_random = TRUE
  } else {
    # Set it to the user-provided entry.
    sample_random = internal$parameters$sample_random

    # Check that it is a boolean, otherwise we set it to true.
    if (!is.logical(sample_random)) {
      warning(sprintf("The user-provided entry for 'sample_random' is not logical. We set it to TRUE.\n"))
      sample_random = TRUE
    }
  }

  # Define the sampler object
  if (sample_random) {
    sampler = model$sampler_random
  } else {
    sampler = model$sampler_most_likely
  }

  # Set the model in evaluation status, which effects certain modules.
  # E.g., deactivates dropout layers, how batch norm is conducted, et. cetera.
  model$eval()

  # Send the model to the GPU, if we are supposed to.
  if (use_cuda) {
    model = model$cuda()
    device = "cuda"
  } else {
    device = "cpu"
  }

  # Normalize the data with the mean and std from the training data.
  # I.e., we assume that the new data follow the same distribution as the training data.
  # If this is NOT the case, then VAEAC will generate unreasonable imputations.
  x_explain_extended_normalized = (x_explain_extended - checkpoint$norm_mean) / checkpoint$norm_std

  # Create the data set object.
  dataset = VAEAC_dataset(x_explain_extended_normalized, checkpoint$one_hot_max_sizes)

  # Create a data loader that load/iterate over the data set in chronological order.
  dataloader = dataloader(dataset, batch_size = batch_size, shuffle = FALSE)

  # Create an auxiliary list of lists to store the imputed values combined with the original values. The the structure is
  # [[i'th imputation]][[b'th batch]], where the entries are tensors of dimension batch_size x n_features.
  results = lapply(seq(n_samples), function(k) list())

  # Create a progress bar
  pb = progress_bar$new(format = "(:spin) [:bar] :percent [Imputing | time: :elapsedfull | ETR: :eta | Batch: :batch_index | Imputation: :imputation_index]",
                        total = dataloader$.length()*n_samples,
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = TRUE,     # If TRUE, clears the bar when finish
                        width = 125)      # Width of the progress bar

  # Set seed for reproducibility if provided by the user. Both in R and torch.
  if (!is.null(internal$parameters$seed)) {
    set.seed(internal$parameters$seed)
    torch::torch_manual_seed(internal$parameters$seed)
  }

  # Variable to keep track of which batch we are working on. Only needed for progress bar.
  batch_index = 1

  #batch = dataloader$.iter()$.next()
  # Generate the conditional Monte Carlo samples for the observation x_explain_extended, one batch at the time.
  coro::loop(for (batch in dataloader) {

    # Make a deep copy of the batch and detach it from graph.
    batch_extended = batch$clone()$detach()

    # If size of current batch is less than batch_size, extend it with objects from the beginning of the dataset.
    if (batch_extended$shape[1] < batch_size) {
      batch_extended = extend_batch(batch = batch_extended,
                                    dataloader = dataloader,
                                    batch_size = batch_size)
    }

    # Send the original and extended batch to GPU if applicable.
    if (use_cuda) {
      batch = batch$cuda()
      batch_extended = batch_extended$cuda()
    }

    # Compute the imputation mask, i.e., which entries we are to impute.
    mask_extended = torch_isnan(batch_extended)$to(dtype = torch_float())

    # Do not need to keep track of the gradients, as we are not fitting the model.
    with_no_grad({
      # Compute the distributions parameters for the generative models inferred by
      # the masked encoder and decoder together. This will be a tensor of shape
      # [batch_size, n_samples, num_generative_parameters].
      # For only continuous features we have that num_generative_parameters = 2*n_features,
      # but for categorical data the number depends on the number of categories.
      samples_params = model$generate_samples_params(batch = batch_extended,
                                                     mask = mask_extended,
                                                     K = n_samples)

      # Remove the parameters belonging to added instances in batch_extended.
      samples_params = samples_params[1:batch$shape[1],,]
    })

    # Make a deep copy of the batch with missing values set to zero.
    batch_mask = torch_isnan(batch)
    batch_zeroed_nans = batch$clone()$detach()
    batch_zeroed_nans[batch_mask] = 0

    # Iterate over the number of imputations and generate the imputed samples
    for (i in seq(n_samples)) {
      # Extract the i'th inferred generative parameters for the whole batch.
      # sample_params is a tensor of shape [batch_size, num_generative_parameters].
      sample_params = samples_params[,i,]

      # Generate the imputations using the generative distributions inferred by the decoder.
      # Can either be the most likely values (mean for cont, class with highest prob for cat),
      # or we can randomly sample the imputed values. We do the latter.
      # sample = networks$sampler_most_prob_false(sample_params)
      # sample = networks$sampler_most_prob_true(sample_params)
      sample = sampler(sample_params)

      # Need only the imputed values for the missing data entries.
      # Zero out the imputations done for known feature values.
      sample[torch_logical_not(batch_mask)] = 0

      # Combine the imputations with the original data to fill in the missing values.
      # Sample is a tensor of shape [batch_size, n_features]
      sample = sample + batch_zeroed_nans

      # Make a deep copy and add it to correct location in the results list.
      results[[i]] = append(results[[i]], sample$clone()$detach()$cpu())

      # Update the current state of the progress bar.
      pb$tick(tokens = list(batch_index = batch_index, imputation_index = i))
    }

    # Update the batch number.
    batch_index = batch_index + 1
  }) # End of iterating over the batches. Done imputing.

  # Goal is to order the imputations into a tensor of shape
  # [n_explain_extended, n_samples, n_features]
  # Start iterating over the number of imputations
  for (i in seq(n_samples)) {
    # Concatenate the batches for the i'th imputation to create a tensor of shape
    # [n_explain_extended, n_features] and then add a new singelton dimension
    # as the second dimension to get the shape [n_explain_extended, 1, n_features].
    results[[i]] = torch_cat(results[[i]])$unsqueeze(2)
  }

  # Concatenate the list of tensor of shape [n_explain_extended, 1, n_features] at the
  # second dimension to form a [n_explain_extended, n_samples, n_features] tensor.
  result = torch_cat(results, 2)

  # Undo the normalization ([data - mu]/sigma) to get back to the original
  # distribution by multiplying the results by sigma and adding the mean.
  result = result * checkpoint$norm_std + checkpoint$norm_mean

  # Concatenate the result such that we go from
  # [n_explain_extended, n_samples, n_features] to
  # [(n_explain_extended * n_samples), n_features].
  # I.e., each instance is no longer a separate 2D tensor.
  result = result$view(c(result$shape[1] * result$shape[2], result$shape[3]))

  # Convert the results to either a 3D or 2D array.
  result = as.array(result$detach()$cpu())

  # Postprocess the data such that categorical features have original level names.
  # and result is now a data.table
  result = VAEAC_postprocess_data(result, checkpoint)

  # Add id, id_combination and weights to the result.
  # The sampling weights is 1/n_samples in the case of VAEAC as we can sample as many MC as we will.
  # It is 1 for empty and full coalition.
  result = data.table(id = rep(seq(n_explain), each = nrow(mask)*n_samples),
                      id_combination = rep(mask_id, each = n_samples, times = n_explain),
                      w = 1 / n_samples,
                      result)

  # Set the key in the data table which sorts them.
  setkey(result, id, id_combination)

  # Return the generated conditional Monte Carlo samples
  return(result)
}



# Dataset Utility Functions ==============================================================================================================================================================================================

#' Compute Featurewise Means and Standard Deviations
#'
#' @description Returns the means and standard deviations for all continuous features in the data set.
#' Categorical features get mean=0 and sd=1 by default.
#'
#' @param data A torch_tensor of dimension N x p containing the data.
#' @param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
#' The sizes for the continuous features can either be '0' or '1'.
#'
#' @return List containing the means and the standard deviations of the different features.
#' @export
compute_normalization = function(data,
                                 one_hot_max_sizes) {
  # Create vectors of zeros that will store the means and sd for each feature.
  norm_vector_mean = torch_zeros(length(one_hot_max_sizes))
  norm_vector_std = torch_ones(length(one_hot_max_sizes))

  # Iterate over the features
  for (variable_j in seq(length(one_hot_max_sizes))) {
    # Number of one hot encoded dummy features for the j'th variable
    size_j = one_hot_max_sizes[variable_j]

    # Check if categorical or continuous feature
    if (size_j >= 2) {
      # Categorical feature
      # Do not do anything when the feature is categorical
      next
    } else {
      # Continuous feature

      # Get the values of the i'th features
      variable_j_values = data[, variable_j]

      # Only keep the non-missing values
      variable_j_values = variable_j_values[variable_j_values$isnan()$logical_not()]

      # Compute the mean of the values
      variable_j_values_mean = variable_j_values$mean()

      # Compute the sd of the values
      variable_j_values_sd = variable_j_values$std()

      # Save the mean and sd in the right place of the vectors
      norm_vector_mean[variable_j] = variable_j_values_mean
      norm_vector_std[variable_j] = variable_j_values_sd
    }
  }

  # return the vectors of means and standards deviations
  return(list(norm_vector_mean = norm_vector_mean,
              norm_vector_std = norm_vector_std))
}



#' Preprocess Data for the VAEAC approach
#'
#' @description VAEAC only supports numerical values. This function converts categorical features
#' to numerics with class labels 1,2,...,K, and keeps track of the map between the original and
#' new class labels. It also computes the one_hot_max_sizes.
#'
#' @param data matrix/data.frame/data.table containing the training data. Only the features and
#' not the response.
#' @param transform_all_continuous_features Boolean. If we are to log transform all continuous
#' features before sending the data to VAEAC. VAEAC creates unbounded values, so if the continuous
#' features are strictly positive, as for Burr and Abalone data, it can be advantageous to log-transform
#' the data to unbounded form before using VAEAC. If TRUE, then \code{VAEAC_postprocess_data} will
#' take the exp of the results to get back to strictly positive values.
#'
#' @return list containing data which can be used in VAEAC, maps between original and new class
#' names for categorical features, one_hot_max_sizes, and list of information about the data.
#' @export
#'
#' @examples
#' preprocessed = VAEAC_preprocess_data(abalone[,-1])
#' abalone[,-1]
#' preprocessed$x_train
VAEAC_preprocess_data = function(data, transform_all_continuous_features = FALSE) {

  # Ensure that data is data.table object
  data = data.table::copy(data.table::as.data.table(data))

  # Create feature list which contains information about the features
  feature_list = list()
  feature_list$labels = colnames(data)
  feature_list$classes = sapply(data, class)
  feature_list$factor_levels = sapply(data, levels)
  feature_list

  # Create an return_list object to store information about the data
  return_list = list()
  return_list$feature_list = feature_list
  return_list$n_features = ncol(return_list$x_train)

  # Compute the one_hot_max_sizes for the features
  one_hot_max_sizes = unname(sapply(return_list$feature_list$factor_levels, length))
  one_hot_max_sizes[one_hot_max_sizes == 0] = 1
  return_list$one_hot_max_sizes = as.integer(one_hot_max_sizes)

  # col_cat = unname(return_list$feature_list$classes == "factor")
  col_cat  = sapply(data, is.factor)
  col_cont = sapply(data, is.numeric)
  cat_in_dataset = sum(col_cat) > 0

  # Extract the names of the categorical and continuous features
  col_cat_names = names(col_cat[col_cat])
  col_cont_names = names(col_cont[col_cont])

  if (cat_in_dataset) {
    # Data table contains one or several categorical features.
    # For VAEAC to work, these must have levels 1,2, ..., K,
    # so we transform the levels to the desired form.

    # # Get the indices of the columns that are categorical
    # col_cat_indices = seq(length(col_cat))[col_cat]
    # col_cont_indices = seq(length(col_cont))[col_cont]

    # Lists that will store maps between the original and new class names for categorical features
    map_original_to_new_names = list()
    map_new_to_original_names = list()

    # Iterate over the categorical features
    for (col_cat_name in col_cat_names) {

      # Create a map from the original class names to the new class names
      map_original_to_new_names[[col_cat_name]] = as.list(seq(length(levels(data[[col_cat_name]]))))
      names(map_original_to_new_names[[col_cat_name]]) = levels(data[[col_cat_name]])
      map_original_to_new_names[[col_cat_name]]

      # Create a map from the new class names to the original class names
      map_new_to_original_names[[col_cat_name]] = as.list(levels(data[[col_cat_name]]))
      names(map_new_to_original_names[[col_cat_name]]) = seq(length(levels(data[[col_cat_name]])))
      map_new_to_original_names[[col_cat_name]]

      # Update the names of the levels. Faster to use the method with as.numeric bellow.
      # levels(data[[col_cat_name]]) = unlist(map_original_to_new_names[[col_cat_name]])
    }

    # Convert the categorical features to numeric. Automatically gets class levels 1,2,...,K.
    data[, (col_cat_names):=lapply(.SD, as.numeric), .SDcols = col_cat_names]

    # Add the maps to the return_list object
    return_list$map_new_to_original_names = map_new_to_original_names
    return_list$map_original_to_new_names = map_original_to_new_names
  }

  # Check if we are to log transform all continuous features.
  if (transform_all_continuous_features) {
    # This is not the best way. We only give an error when all features are known, i.e., during training.
    # During imputations we do not wory, as we are going to impute the NA values.
    if (!is.na(suppressWarnings(any(data[,..col_cont_names] <= 0)))) {
      # Small check that all continues features are strictly positive
      if (suppressWarnings(any(data[,..col_cont_names] <= 0))) stop("The continuous features in data is not strictly positive. Cannot log-transform them.")

    }

    # Log-transform the continuous features.
    data[, (col_cont_names):= lapply(.SD, log), .SDcols = col_cont_names]
  }

  # Add the numerical data table to the return_list object, and some other variables.
  return_list$transform_all_continuous_features = transform_all_continuous_features
  return_list$x_train = as.matrix(data)
  return_list$col_cat = col_cat
  return_list$col_cat_names = col_cat_names
  return_list$col_cont = col_cont
  return_list$col_cont_names = col_cont_names
  return_list$cat_in_dataset = cat_in_dataset

  # return the return_list object
  return(return_list)
}

#' Postprocess Data Generated by a VAEAC Model
#'
#' @description VAEAC generates numerical values. This function converts categorical features
#' to from numerics with class labels 1,2,...,K, to factors with the original and class labels.
#'
#' @param data Matrix/data.frame/data.table containing the data generated by a VAEAC model
#' @param VAEAC_model_state_list List. The returned list from the \code{VAEAC_preprocess_data} function.
#'
#' @return data.table with the generated data from a VAEAC model where the categorical features
#' now have the original class names.
#' @export
#'
#' @examples
#' preprocessed = VAEAC_preprocess_data(abalone[,-1])
#' preprocessed$x_train
#' postprocessed = VAEAC_postprocess_data(preprocessed$x_train, preprocessed)
#' postprocessed
#' all.equal(preprocessed$x_train_org, postprocessed)
VAEAC_postprocess_data = function(data, VAEAC_model_state_list) {
  # Go from VAEAC type data back to data.table used in shapr
  data_dt = as.data.table(data)
  colnames(data_dt) = VAEAC_model_state_list$feature_list$labels

  # Extract the column names for the categorical and continuous features
  col_cat_names = VAEAC_model_state_list$col_cat_names
  col_cont_names = VAEAC_model_state_list$col_cont_names

  # Extract the map from
  map_new_to_original_names = VAEAC_model_state_list$map_new_to_original_names

  # Convert all categorical features (if there are any) from numeric back to factors
  if (length(col_cat_names) > 0) data_dt[, (col_cat_names):= lapply(.SD, factor), .SDcols = col_cat_names]

  # Iterate over the categorical features
  for (col_cat_name in col_cat_names) {
    # Map the class labels names back to the original names
    levels(data_dt[[col_cat_name]]) = unlist(map_new_to_original_names[[col_cat_name]])
  }

  # If we log transformed the continuous features in the pre-processing, we need to
  # undo the transformation by exp-transforming the features back to strictly positives.
  if (VAEAC_model_state_list$transform_all_continuous_features) {
    # Exp-transform the continuous features.
    data_dt[, (col_cont_names):= lapply(.SD, exp), .SDcols = col_cont_names]
  }

  # return the data table
  return(data_dt)
}



#' Transform Strictly Positive Features to Unbounded by Log Function
#'
#' @description Function that converts all continuous features by applying the log function.
#' This ensures that strictly positive values are unbounded after the transformation.
#'
#' @details Used for, e.g., Burr data and Abalone data.
#'
#' @param data Matrix/data.frame. Original data set where all continuous features are strictly positive.
#' @param one_hot_max_sizes An array of numeric of dimension p containing the one hot sizes of the p features.
#' The sizes for the continuous features can either be '0' or '1'.
#'
#' @return The input data where the continuous features have been log-transformed.
#' @export
#'
#' @examples
#' data = data.frame(matrix(rgamma(1000*3, 2), ncol = 3)) # Simulate positive data
#' data$X2 = factor(data$X2 >= 2) # Create a factor, larger than mean
#' one_hot_max_sizes = c(1,2,1)
#' print(data)
#' data_unbounded = log_transform_all_continuous_features_function(data, one_hot_max_sizes)
#' print(data_unbounded)
#' data = as.data.table(data) # convert to data table. Same functions works then too
#' print(data)
#' data_unbounded = log_transform_all_continuous_features_function(data, one_hot_max_sizes)
#' print(data_unbounded)
log_transform_all_continuous_features_function = function(data, one_hot_max_sizes) {

  # Check if data is data.table or matrix/data.frame.
  if (any(class(data) == "data.table")) {
    # Extract which columns that are continuous.
    cont_cols = seq(length(one_hot_max_sizes))[one_hot_max_sizes < 2]

    # # Small check that all continues features are strictly positive
    # if (suppressWarnings(any(data[,..cont_cols] <= 0))) stop("The continuous features in data is not strictly positive. Cannot log-transform them.")

    # Make a deep copy of the data table. Otherwise, we alter the input data outside this function.
    data = copy(data)

    # Log-transform the continuous features.
    data[, (cont_cols):= lapply(.SD, log), .SDcols = cont_cols]
  } else {
    # Extract which columns that are continuous.
    cont_cols = one_hot_max_sizes < 2

    # # Small check that all continues features are strictly positive
    # if (suppressWarnings(any(data[,cont_cols] <= 0))) stop("The continuous features in data is not strictly positive. Cannot log-transform them.")

    # Log-transform the continuous features.
    data[,cont_cols] = log(data[,cont_cols, drop = FALSE])
  }

  # Return the data
  return(data)
}


#' Transform Unbounded Features to Strictly Positive by Exp Function
#'
#' @description Function that converts all continuous features by applying the exp function.
#' This ensures that unbounded features are strictly positive after the transformation.
#'
#' @details Used for, e.g., Burr data and Abalone data.
#'
#' @param data Matrix/data.frame. Original data set where all continuous features are strictly positive.
#' @param one_hot_max_sizes An array of numeric of dimension p containing the one hot sizes of the p features.
#' The sizes for the continuous features can either be '0' or '1'.
#'
#' @return The input data where the continuous features have been exp-transformed.
#' @export
#'
#' @examples
#' data = data.frame(matrix(rgamma(1000*3, 2), ncol = 3)) # Simulate positive data
#' data$X2 = factor(data$X2 >= 2) # Create a factor, larger than mean
#' one_hot_max_sizes = c(1,2,1)
#' print(data)
#' data_unbounded = log_transform_all_continuous_features_function(data, one_hot_max_sizes)
#' print(data_unbounded)
#' data_bounded = exp_transform_all_continuous_features_function(data_unbounded, one_hot_max_sizes)
#' print(data_bounded)
#' all.equal(data, data_bounded)
#' data = as.data.table(data) # convert to data table. Same functions works then too
#' print(data)
#' data_unbounded = log_transform_all_continuous_features_function(data, one_hot_max_sizes)
#' print(data_unbounded)
#' data_bounded = exp_transform_all_continuous_features_function(data_unbounded, one_hot_max_sizes)
#' print(data_bounded)
#' all.equal(data, data_bounded)
exp_transform_all_continuous_features_function = function(data, one_hot_max_sizes) {

  # Check if data is data.table or matrix/data.frame.
  if (any(class(data) == "data.table")) {
    # Extract which columns that are continuous.
    cont_cols = seq(length(one_hot_max_sizes))[one_hot_max_sizes < 2]

    # Make a deep copy of the data table. Otherwise, we alter the input data outside this function.
    data = copy(data)

    # Exp-transform the continuous features.
    data[, (cont_cols):= lapply(.SD, exp), .SDcols = cont_cols]
  } else {
    # Extract which columns that are continuous.
    cont_cols = one_hot_max_sizes < 2

    # Exp-transform the continuous features.
    data[,cont_cols] = exp(data[,cont_cols, drop = FALSE])
  }

  # Return the data
  return(data)
}


## VAEAC_dataset ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' Dataset used to load data into the VAEAC model
#'
#' @details Must create a data set object that represent a map from keys to data samples. It is used by the dataloader()
#' to load data which should be used to extract the batches for all epochs in the training phase of the neural
#' network. Note that a dataset object is an R6 instance (https://r6.r-lib.org/articles/Introduction.html)
#' which is classical object-oriented programming, with self reference. I.e, 'VAEAC_sample_data' is  a class of type 'dataset'.
#'
#' @examples
#' p = 5
#' N = 14
#' batch_size = 10
#' one_hot_max_sizes = rep(1, p)
#' VAEAC_ds = VAEAC_dataset(torch_tensor(matrix(rnorm(p*N), ncol = p), dtype = torch_float()), one_hot_max_sizes)
#' VAEAC_ds
#'
#' VAEAC_dl = dataloader(VAEAC_ds, batch_size = batch_size, shuffle = TRUE, drop_last = FALSE)
#' VAEAC_dl$.length()
#' VAEAC_dl$.iter()
#'
#' VAEAC_iterator = VAEAC_dl$.iter()
#' VAEAC_iterator$.next() # batch1
#' VAEAC_iterator$.next() # batch2
#' VAEAC_iterator$.next() # Empty
VAEAC_dataset = torch::dataset(
  #' @field A dataset must specify a 'name' for the object
  name = "VAEAC_dataset",
  #' @description Create a new VAEAC_dataset object.
  #' @param X A torch_tensor contain the data
  #' @param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
  #' The sizes for the continuous features can either be '0' or '1'.
  initialize = function(X,
                        one_hot_max_sizes) {
    # Save the number of observations in X
    self$N = nrow(X)

    # Save the number of features in X
    self$p = ncol(X)

    # Save the number of one hot dummy features for each features
    self$one_hot_max_sizes = one_hot_max_sizes

    # Save the dataset
    self$X = X
  },
  #' @description How to fetch a data sample for a given key/index.
  .getitem = function(index) {
    X = self$X[index, ]
  },
  #' @description Return the size of the dataset
  .length = function() {
    nrow(self$X)
  }
)


## Paired Sampler  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# A sampler object that allows for paired sampling by always including
# each observation twice. See more on https://rdrr.io/cran/torch/src/R/utils-data-sampler.R.
# Samplers can be used with dataloader() when creating batches from a torch dataset(), see `?dataloader` and `?sampler`.
# Have not used batch iterators that might increase the speed.
# Example how to use it combined with mask generators with paired sampling activated
# batch_size = 4
# if (batch_size %% 2 == 1) batch_size = batch_size - 1 # Make sure that batch size is even
# num_featuers = 3
# num_observations = 5
# shuffle = TRUE
# data = torch_tensor(matrix(rep(seq(num_observations), each = num_featuers), ncol = num_featuers, byrow = TRUE))
# data
# dataset = VAEAC_dataset(data, rep(1, num_featuers))
# dataload = dataloader(dataset,
#                       batch_size = batch_size,
#                       sampler = paired_sampler(dataset,
#                                                shuffle = shuffle))
# dataload$.length() # Number of batches, same as ceiling((2 * num_observation) / batch_size)
# mask_generator = MCAR_mask_generator(paired = TRUE)
# coro::loop(for (batch in dataload) {
#   mask = mask_generator(batch)
#   obs = mask * batch
#   print(torch_cat(c(batch, mask, obs), -1))
# })
paired_sampler = torch::sampler(
  classname = "paired_sampler",
  initialize = function(data_source, shuffle = FALSE) {
    self$data_source = data_source
    self$shuffle = shuffle
  },
  .length = function() {
    # Multiply by two do to the sampling
    length(self$data_source) * 2
  },
  .iter = function() {
    # Get the number of observations in the data
    n = length(self$data_source)

    # Check if the indices are to be shuffled
    if (self$shuffle) {
      # Sample a random order for the indices
      indices = sample.int(n)
    } else {
      # Take the indices in increasing order
      indices = seq_len(n)
    }

    # Duplicate each index and return an iterator
    coro::as_iterator(rep(indices, each = 2))
  }
)




# Neural Network Utility Functions =======================================================================================================================================================================================

##  MemoryLayer ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' A torch::nn_module representing a Memory Layer
#'
#' @description The layer is used to make skip-connections inside nn_sequential network
#' or between several nn_sequential networks without unnecessary code complication.
#'
#' @details If \code{output = FALSE}, this layer stores its input in a static list
#' \code{storage} with the key \code{id} and then passes the input to the next layer.
#' I.e., when memory layer is used in the masked encoder.
#' If \code{output = TRUE}, this layer takes stored tensor from the storage.
#' I.e., when memory layer is used in the decoder.
#' If \code{add = TRUE}, it returns sum of the stored vector and an \code{input},
#' otherwise it returns their concatenation. If the tensor with specified \code{id}
#' is not in storage when the layer with \textit{output = TRUE} is called, it would cause an exception.
#'
#' @examples
#' net1 = nn_sequential(
#'   MemoryLayer('#1'),
#'   MemoryLayer('#0.1'),
#'   nn_linear(512, 256),
#'   nn_leaky_relu(),
#'   MemoryLayer('#0.1', output=TRUE, add=FALSE), # here add cannot be TRUE because the dimensions mismatch
#'   nn_linear(768, 256), # the dimension after the concatenation with skip-connection is 512 + 256 = 768
#'  )
#'  net2 = nn_equential(
#'    nn_linear(512, 512),
#'    MemoryLayer('#1', output=TRUE, add=TRUE),
#'    ...
#'  )
#'  b = net1(a)
#'  d = net2(c) # net2 must be called after net1, otherwise tensor '#1' will not be in storage.
MemoryLayer = torch::nn_module(
  classname = "MemoryLayer",
  shared_env = new.env(),

  #' @description Create a new MemoryLayer object.
  #' @param id A unique id to use as a key in the storage list.
  #' @param output Boolean variable indicating if the memory layer is to store input in storage or extract from storage.
  #' @param add Boolean variable indicating if the extracted value are to be added or concatenated to the input. Only applicable when \code{output = TRUE}.
  #' @param verbose Boolean variable indicating if we want to give printouts to the user.
  initialize = function(id,
                        output = FALSE,
                        add = FALSE,
                        verbose = FALSE) {
    self$id = id
    self$output = output
    self$add = add
    self$verbose = verbose
  },
  forward = function(input) {
    # Check if we are going to insert input into the storage or extract data from the storage.
    if (!self$output) {
      # We are to insert input into the storage list.

      # Small printout to the user
      if (self$verbose) cat(sprintf("Inserting data to memory layer 'self$id = %s'.\n", self$id))

      # Save the input in the shared environment of the MemoryLayer class in the storage list.
      # Note that we do not check if self$id is unique.
      self$shared_env$storage[[self$id]] = input

      # Return/send the input to the next layer in the network.
      return(input)

    } else {
      # We are to extract data from the storage list.

      # Small printout to the user
      if (self$verbose) cat(sprintf("Extracting data from memory layer 'self$id = %s'. ", self$id))

      # Check that the memory layer has data is stored in it. If not, then thorw error.
      if (!self$id %in% names(self$shared_env$storage)) {
        stop(sprintf("ValueError: Looking for memory layer 'self$id = %s', but only available memory layers are: %s.",
                     self$id, paste(names(self$shared_env$storage), collapse = ", ")))
      }

      # Extract the stored data for the given memory layer
      stored = self$shared_env$storage[[self$id]]

      # If we are concatenate the input to the extracted data or add it
      if (!self$add) {
        # We are to concatenate the tensors.

        # Small printout to the user
        if (self$verbose) cat(sprintf("Concatenates the tensors.\n"))

        # Concatenate the columns of the tensors.
        data = torch_cat(c(input, stored), -1)
      } else {
        # We are to add the tensors.

        # Small printout to the user
        if (self$verbose) cat(sprintf("Adds the tensors.\n"))

        # Add the tensors together.
        data = input + stored
      }

      # Return the data
      return(data)
    }
  }
)

## SkipConnection --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' A torch::nn_module representing a Skip Connection
#'
#' @description Skip-connection over the sequence of layers in the constructor. The module passes
#' input data sequentially through these layers and then adds original data to the result.
#' @param ... network modules such as nn_linear, activation_function(), and memory layers. See \code{get_imputation_networks}.
SkipConnection = torch::nn_module(
  classname = "SkipConnection",
  initialize = function(...) {
    self$inner_net = nn_sequential(...)
  },
  forward = function(input) {
    return(input + self$inner_net(input))
  }
)

# Training Utility Functions =============================================================================================================================================================================================
#' Extends Incomplete Batches by Sampling Extra Data from Dataloader
#'
#' @description If the height of the \code{batch} is less than \code{batch_size}, this function extends the \code{batch} with
#' data from the \code{dataloader} until the \code{batch} reaches the required size. Note that \code{batch} is a tensor.
#'
#' @param batch The batch we want to check if has the right size, and if not extend it until it has the right size.
#' @param dataloader A dataloader object from which we can create an iterator object and load data to extend the batch.
#' @param batch_size Integer. The number of samples to include in each batch.
#'
#' @return Returns the extended batch with the correct batch_size.
#' @export
extend_batch = function(batch,
                        dataloader,
                        batch_size) {
  # This function could be more streamlined, as I believe that
  # it is sufficient to only call next once. If that is the case,
  # we could have removed the while with and if else, and called
  # next but only taken the first batch_size-batch$shape[1] observations.

  # batch.shape[1] is always smaller or equal to batch_size
  # if smaller we want to add data to the batch from the
  # dataloader until the batch has the correct size.
  while (batch$shape[1] != batch_size) {
    # Not the correct size, so we create an iterator
    # that can iterate over the batches in the dataloader
    dataloader_iterator = dataloader$.iter()

    # Load the next batch
    extra_batch = dataloader_iterator$.next()

    # If the the number of instances in extra_batch + batch
    # is larger than batch_size, then we need to remove the
    # appropriate number of instances from the extra_batch.
    if (extra_batch$shape[1] + batch$shape[1] > batch_size) {
      # Keep only the first batch_size - batch.shape[0] instances.
      extra_batch = extra_batch[seq(batch_size - batch$shape[1]), ]
    }

    # Concatenate the original batch with the extra_batch in a rowbind manner.
    batch = torch_cat(c(batch, extra_batch), 1)
  }

  # The batch is now of the correct dimension/height
  return(batch)
}

#' Compute the Importance Sampling Estimator (Validation Error)
#'
#' @description Compute the Importance Sampling Estimator which the VAEAC model uses to evaluate its performance on the validation data.
#'
#' @details Compute mean IWAE log likelihood estimation of the validation set.
#' Takes validation data loader, mask generator, batch size, model (VAEAC)
#' and number of IWAE latent samples per object.
#' Returns one float - the estimation.
#' IWAE is an abbreviation for Importance Sampling Estimator
#' \eqn{log p_{theta, psi}(x|y) \approx
#' log {1/S * sum_{i=1}^S [p_theta(x|z_i, y) * p_psi(z_i|y) / q_phi(z_i|x,y)]}}
#' where z_i ~ q_phi(z|x,y)
#'
#' @param val_dataloader A torch dataloader which loads the validation data.
#' @param mask_generator A mask generator object that generates the masks.
#' @param batch_size Integer. The number of samples to include in each batch.
#' @param model The VAEAC model.
#' @param num_samples Number of samples to generate for computing the IWAE for each validation sample.
#' @param verbose Boolean. If we are to print results to the user.
#'
#' @return The average iwae over all instances in the validation dataset.
#' @export
get_validation_iwae = function(val_dataloader,
                               mask_generator,
                               batch_size,
                               model,
                               num_samples,
                               verbose = FALSE) {
  # Set variables to store the number of instances evaluated and avg_iwae
  cum_size = 0
  avg_iwae = 0

  # Iterate over all the batches in the validation set
  coro::loop(for (batch in val_dataloader)  {

    # Get the number of instances in the current batch
    init_size = batch$shape[1]

    # Extend the batch if init_size not equal to batch_size
    # returns the batch if appropriate size or adds instances
    # from validation dataloader
    batch = extend_batch(batch, val_dataloader, batch_size)

    # Create the mask for the current batch.
    # Mask consists of zeros (observed) and ones (missing or masked)
    mask = mask_generator(batch)

    # If the model.parameters are located on a Nivida GPU, then
    # we send batch and mask to GPU, as it is faster than CPU.
    if (model$parameters[[1]]$is_cuda) {
      batch = batch$cuda()
      mask = mask$cuda()
    }

    # use with torch::with_no_grad() as we in this with clause do not
    # want to compute the gradients, as they are not important
    # / not needed for doing backpropagation.
    torch::with_no_grad({

      # Get the iwae for each instance in the current batch
      # but save only the first init_size, as the other are
      # just arbitrary instances we "padded" the batch with
      # to get the appropriate shape.
      iwae = model$batch_iwae(batch, mask, num_samples)[1:init_size, drop = FALSE]

      # Update the average iwae over all batches (over all instances)
      # This is called recursive/online updating of the mean.
      # I have verified the method. Takes the
      # old average * cum_size to get old sum of iwae
      # adds the sum of newly computed iwae. Then divide the
      # total iwae by the number of instances: cum_size + iwae.shape[0])
      avg_iwae = (avg_iwae * (cum_size / (cum_size + iwae$shape[1])) +
                    iwae$sum() / (cum_size + iwae$shape[1]))

      # Update the number of instances evaluated
      cum_size = cum_size + iwae$shape[1]

    }) # End with_no_grad
  }) # End iterating over the validation samples


  # return the average iwae over all instances in the validation set.
  return(avg_iwae$to(dtype = torch_float()))
}

# Probability Utility Functions ==========================================================================================================================================================================================
#' Creates Normal Distributions
#'
#' @description Function that takes in the a tensor where the first half of the columns contains the means of the
#' normal distributions, while the latter half of the columns contains the standard deviations. The standard deviations
#' are clamped with 'min_sigma' to ensure stable results. If 'params' is of dimensions batch_size x 8, the function will
#' create 4 independent normal distributions for each of the observation ('batch_size' observations in total).
#'
#' @details Take a Tensor (e.g. neural network output) and return torch::distr_normal distribution. This Normal distribution
#' is component-wise independent, and its dimensionality depends on the input shape. First half of channels is mean of the
#' distribution, the softplus of the second half is std (sigma), so there is no restrictions on the input tensor.
#' \code{min_sigma} is the minimal value of sigma. I.e., if the above softplus is less than min_sigma, then sigma is clipped
#' from below with value \code{min_sigma}. This regularization is required for the numerical stability and may be considered
#' as a neural network architecture choice without any change to the probabilistic model.
#'
#' @param params Tensor containing the parameters for the normal distributions.
#' @param min_sigma The minimal variance allowed.
#'
#' @return torch::distr_noramal distributions with the provided means and standard deviations.
#' @export
normal_parse_params = function(params,
                               min_sigma = 0.001) {
  # Get the number of instances
  n = params$shape[1]

  # Then get the dimension of the parameters
  d = params$shape[2]

  # Use double dash to get integer. Do not need it as we by construction always have 2*num_dim_latent_space
  mu = params[, 1:(d%/%2)]  # Get the first halves which are the means

  # Get the second half which are transformed sigmas
  sigma_params = params[, (d%/%2 + 1):d]
  sigma = nnf_softplus(sigma_params)  # ln(1 + exp(sigma_params))
  sigma = sigma$clamp(min = min_sigma)  # Make sure that sigma >= min_sigma

  # Create the normal dist. Multivariate, but with independent dimensions. Correlation = 0. So just Normal
  distr = distr_normal(loc = mu, scale = sigma)

  # Return the distribution
  return(distr)
}

#' Creates Categorical Distributions
#'
#' @description Function that takes in a tensor containing the logits for each of the K classes. Each row corresponds to an observations.
#' Send each row through the softmax function to convert from logits to probabilities that sum 1 one.
#' The function also clamps the probabilities between a minimum and maximum probability. Note that we still normalize them afterward,
#' so the final probabilities can be marginally below or above the thresholds.
#'
#' @details Take a Tensor (e. g. a part of neural network output) and return torch::distr_categorical distribution.
#' The input tensor after applying softmax over the last axis contains a batch of the categorical probabilities.
#' So there are no restrictions on the input tensor. Technically, this function treats the last axis as the categorical
#' probabilities, but Categorical takes only 2D input where the first axis is the batch axis and the second one corresponds
#' to the probabilities, so practically the function requires 2D input with the batch of probabilities for one categorical feature.
#' min_prob is the minimal probability for each class. After clipping the probabilities from below and above they are renormalized
#' in order to be a valid distribution. This regularization is required for the numerical stability and may be considered
#' as a neural network architecture choice without any change to the probabilistic model.
#' Note that the softmax function is given by \eqn{Softmax(x_i) = (exp(x_i))/(\sum_{j} exp(x_j))}, where \eqn{x_i} are the logits and can
#' take on any value, negative and positive. The output \eqn{Softmax(x_i) \in [0,1]} and \eqn{\sum_{j} Softmax(x_i) = 1}.
#'
#' @param params Tensor of dimension batch_size x K containing the logits for each of the K classes and batch_size observations.
#' @param min_prob For stability it might be desirable that the minimal probability is not exactly zero.
#' @param max_prob For stability it might be desirable that the maximal probability is not exactly zero.
#'
#' @return torch::distr_categorical distributions with the provided probabilities for each class.
#' @export
categorical_parse_params_column = function(params, min_prob = 0, max_prob = 1) {
  # Send the parameters through the softmax to get normalized probabilities
  params = nnf_softmax(params, dim = -1)

  # Ensure that the probabilities are between the minimum and maximum allowed probabilities
  params = torch_clamp(params, min = min_prob, max = max_prob)

  # Make sure that parms sum to 1 after the clamping.
  params = params / torch_sum(params, dim = -1, keepdim = TRUE)

  # Then create a categorical distribution which will have len(params)
  # number of categories and the probability for each of them is given in params.
  distr = distr_categorical(probs = params)

  # # Could have directly used that 'dist_categorical' supports logits. But then
  # # we would not be able to clamp the probabilities. This version is 30% faster.
  # distr = distr_categorical(logits = params)

  # Return the distribution
  return(distr)
}

#' Compute the KL divergence between Gaussian distributions.
#'
#' @description Computes the KL divergence between univariate normal distributions using the analytical formula,
#' see \url{https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence#Multivariate_normal_distributions}.
#'
#' @param p a Gaussian distribution,
#' @param q a Gaussian distribution.
#'
#' @return The KL divergence
#' @export
kl_normal_normal = function(p, q) {
  var_ratio = (p$scale / q$scale)$pow(2)
  t1 = ((p$loc - q$loc) / q$scale)$pow(2)
  return(0.5*(var_ratio + t1 - 1 - var_ratio$log()))
}


# Neural Network Modules =================================================================================================================================================================================================
## GaussianCategoricalSampler --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Generates a sample from the generative distribution defined by
# the output of the neural network.
#
# one_hot_max_sizes[i] is the one-hot max size of i-th feature,
# if i-th feature is categorical, and 0 or 1 if i-th feature is real-valued.
#
# The distribution parameters format, min_sigma and min_prob are described
# in docs for GaussianCategoricalLoss.
#
# If sample_most_probable is True, then the layer will return
# mean for Gaussians and the most probable class for categorical features.
# Otherwise the fair sampling procedure from Gaussian and categorical
# distributions is performed.
GaussianCategoricalSampler = torch::nn_module(
  classname = "GaussianCategoricalSampler",
  initialize = function(one_hot_max_sizes,
                        sample_most_probable = FALSE,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {

    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes = one_hot_max_sizes

    # We set this to TRUE when we use this class.
    # So just return the mean for Gaussian.
    # This is not what we want in case of SHAPLEY.
    # SO WE SHOULD USE FALSE
    self$sample_most_probable = sample_most_probable

    # We use the default values, both 1e-4.
    self$min_sigma = min_sigma
    self$min_prob = min_prob
  },
  forward = function(distr_params) {
    # dist_params is a matrix of form batch_size x (mu_1, sigma_1, ..., mu_n, sigma_n)
    # A bit different for categorical features as we
    # there do not have mu and sigma for generative normal at the end,
    # but rather logits for the categorical distribution.

    # A counter to keep track of which
    cur_distr_col = 1

    # List to store all the samples sampled from the
    # normal distribution with parameters from distr_params.
    sample = list()

    # Iterate over the features
    for (i in seq(length(self$one_hot_max_sizes))) {
      size = self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params = distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col = cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr = normal_parse_params(params, self$min_sigma)

        # If we are going to sample the mean or do a random sampling
        if (self$sample_most_probable) {
          col_sample = distr$mean
        } else {
          # Use rsample() here as it seems that sample() does not respect manual set seeds. Only for normal.
          col_sample = distr$rsample()
        }

      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params = distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col = cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_column' function.
        # distr is a "torch::distr_categorical" distribution.
        distr = categorical_parse_params_column(params, self$min_prob)

        # If we are to return the class with highest probability or sample a
        # class from the distribution based on each class' probabilities.
        if (self$sample_most_probable) {
          # By doing [, NULL], we add an extra dimension such that the tensor is a column vector.
          col_sample = torch_max(distr$probs, -1)[[2]][, NULL]$to(dtype = torch_float())
        } else {
          # Here we can use $sample() as it respects manual set seeds.
          col_sample = distr$sample()[, NULL]$to(dtype = torch_float())
        }
      }

      # Add the vector of sampled values for the i´th
      # feature to the sample list.
      sample = append(sample, col_sample)
    }

    # Create a matrix by column binding the vectors in the list
    return(torch_cat(sample, -1))
  }
)

## GaussianCategoricalSamplerMostLikely --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Generates the most likely samples from the generative distribution defined by
# the output of the neural network. I.e., the layer will return
# mean for Gaussians and the most probable class for categorical features.
#
# one_hot_max_sizes[i] is the one-hot max size of i-th feature,
# if i-th feature is categorical, and 0 or 1 if i-th feature is real-valued.
#
# The distribution parameters format, min_sigma and min_prob are described
# in docs for GaussianCategoricalLoss.
GaussianCategoricalSamplerMostLikely = torch::nn_module(
  classname = "GaussianCategoricalSamplerMostLikely",
  initialize = function(one_hot_max_sizes,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {

    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes = one_hot_max_sizes

    # We use the default values, both 1e-4.
    self$min_sigma = min_sigma
    self$min_prob = min_prob
  },
  forward = function(distr_params) {
    # dist_params is a matrix of form batch_size x (mu_1, sigma_1, ..., mu_n, sigma_n)
    # A bit different for categorical features as we
    # there do not have mu and sigma for generative normal at the end,
    # but rather logits for the categorical distribution.

    # A counter to keep track of which
    cur_distr_col = 1

    # List to store all the samples sampled from the
    # normal distribution with parameters from distr_params.
    sample = list()

    # Iterate over the features
    for (i in seq(length(self$one_hot_max_sizes))) {
      size = self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params = distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col = cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr = normal_parse_params(params, self$min_sigma)

        # We sample the mean (most likely value)
        col_sample = distr$mean
      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params = distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col = cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_column' function.
        # distr is a "torch::distr_categorical" distribution.
        distr = categorical_parse_params_column(params, self$min_prob)

        # Return the class with highest probability
        # By doing [, NULL], we add an extra dimension such that the tensor is a column vector.
        col_sample = torch_max(distr$probs, -1)[[2]][, NULL]$to(dtype = torch_float())
      }

      # Add the vector of sampled values for the i´th
      # feature to the sample list.
      sample = append(sample, col_sample)
    }

    # Create a matrix by column binding the vectors in the list
    return(torch_cat(sample, -1))
  }
)

## GaussianCategoricalSamplerRandom ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Generates a random sample from the generative distribution defined by
# the output of the neural network. The random sample is generated by
# sampling from the inferred Gaussian and categorical distributions.
#
# one_hot_max_sizes[i] is the one-hot max size of i-th feature,
# if i-th feature is categorical, and 0 or 1 if i-th feature is real-valued.
#
# The distribution parameters format, min_sigma and min_prob are described
# in docs for GaussianCategoricalLoss.
GaussianCategoricalSamplerRandom = torch::nn_module(
  classname = "GaussianCategoricalSamplerRandom",
  initialize = function(one_hot_max_sizes,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {

    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes = one_hot_max_sizes

    # We use the default values, both 1e-4.
    self$min_sigma = min_sigma
    self$min_prob = min_prob
  },
  forward = function(distr_params) {
    # dist_params is a matrix of form batch_size x (mu_1, sigma_1, ..., mu_n, sigma_n)
    # A bit different for categorical features as we
    # there do not have mu and sigma for generative normal at the end,
    # but rather logits for the categorical distribution.

    # A counter to keep track of which
    cur_distr_col = 1

    # List to store all the samples sampled from the
    # normal distribution with parameters from distr_params.
    sample = list()

    # Iterate over the features
    for (i in seq(length(self$one_hot_max_sizes))) {
      size = self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params = distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col = cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr = normal_parse_params(params, self$min_sigma)

        # Sample from the inferred Gaussian distributions
        # Use rsample() here as it seems that sample() does not respect manual set seeds. Only for normal.
        col_sample = distr$rsample()

      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params = distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col = cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_column' function.
        # distr is a "torch::distr_categorical" distribution.
        distr = categorical_parse_params_column(params, self$min_prob)

        # Sample a class from the distribution based on each class' probabilities.
        # By doing [, NULL], we add an extra dimension such that the tensor is a column vector.
        # Here we can use $sample() as it respects manual set seeds.
        col_sample = distr$sample()[, NULL]$to(dtype = torch_float())
      }

      # Add the vector of sampled values for the i´th
      # feature to the sample list.
      sample = append(sample, col_sample)
    }

    # Create a matrix by column binding the vectors in the list
    return(torch_cat(sample, -1))
  }
)




## GaussianCategoricalParameters --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Extract the parameters from the generative distribution defined by
# the output of the neural network.
#
# one_hot_max_sizes[i] is the one-hot max size of i-th feature,
# if i-th feature is categorical, and 0 or 1 if i-th feature is real-valued.
#
# The distribution parameters format, min_sigma and min_prob are described
# in docs for GaussianCategoricalLoss.
GaussianCategoricalParameters = torch::nn_module(
  classname = "GaussianCategoricalParameters",
  initialize = function(one_hot_max_sizes,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {

    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes = one_hot_max_sizes

    # We use the default values, both 1e-4.
    self$min_sigma = min_sigma
    self$min_prob = min_prob
  },
  forward = function(distr_params) {
    # dist_params is a matrix of form batch_size x (mu_1, sigma_1, ..., mu_n, sigma_n)
    # A bit different for categorical features as we
    # there do not have mu and sigma for generative normal at the end,
    # but rather logits for the categorical distribution.

    # A counter to keep track of which
    cur_distr_col = 1

    # List to store all the generative parameters from the normal and categorical distributions
    parameters = list()

    # Iterate over the features
    for (i in seq(length(self$one_hot_max_sizes))) {
      size = self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params = distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col = cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr = normal_parse_params(params, self$min_sigma)

        # Combine the current parameters
        current_parameters = torch_cat(c(distr$mean, distr$scale), -1)

      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params = distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col = cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_column' function.
        # distr is a "torch::distr_categorical" distribution.
        distr = categorical_parse_params_column(params, self$min_prob)

        # Extract the current probabailities for each classs
        current_parameters = distr$probs
      }

      # Add the tensor of current parameters for the i´th feature to the parameters list
      parameters = append(parameters, current_parameters)
    }

    # Create a torch_tensor by column binding the tensors in the list
    return(torch_cat(parameters, -1))
  }
)



## GaussianCategoricalLoss -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# This layer computes log probability of groundtruth for each object
# given the mask and the distribution parameters.
# This layer works for the cases when the dataset contains both
# real-valued and categorical features.
#
# one_hot_max_sizes[i] is the one-hot max size of i-th feature,
# if i-th feature is categorical, and 0 or 1 if i-th feature is real-valued.
# In the first case the distribution over feature is categorical,
# in the second case it is Gaussian.
#
# For example, if one_hot_max_sizes is [4, 1, 1, 2], then the distribution
# parameters for one object is the vector
# [p_00, p_01, p_02, p_03, mu_1, sigma_1, mu_2, sigma_2, p_30, p_31],
# where Softmax([p_00, p_01, p_02, p_03]) and Softmax([p_30, p_31])
# are probabilities of the first and the fourth feature categories
# respectively in the model generative distribution, and
# Gaussian(mu_1, sigma_1 ^ 2) and Gaussian(mu_2, sigma_2 ^ 2) are
# the model generative distributions on the second and the third features.
#
# For the definitions of min_sigma and min_prob see normal_parse_params
# and categorical_parse_params docs.
#
# This layer works correctly with missing values in groundtruth
# which are represented by NaNs.
#
# This layer works with 2D inputs only.
GaussianCategoricalLoss = torch::nn_module(
  classname = "GaussianCategoricalLoss",
  initialize = function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4){
    self$one_hot_max_sizes = one_hot_max_sizes
    self$min_sigma = min_sigma
    self$min_prob = min_prob
  },
  forward = function(groundtruth, distr_params, mask) {
    # DELETE THIS IN THE END. JUST USED IT TO MAKED IT SIMPLER TO DEBUG THE CODE
    # groundtruth = batch
    # distr_params = reconstruction_params
    # mask = mask
    # self$min_sigma = 1e-4
    # self$one_hot_max_sizes = one_hot_max_sizes

    # Which column in distr_params we now consider.
    # Either increases with 2 in cont case (mu and sigma)
    # or in increases with one-hot encoding size in cat case.
    cur_distr_col = 1

    # List to store the log probabilities.
    log_prob = list()

    # Iterate over the features
    i = 3
    for (i in seq(length(self$one_hot_max_sizes))) {
      size = self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous feature
        # Gaussian distribution

        # select groundtruth, mask and distr_params for i-th feature
        groundtruth_col = groundtruth[, i, drop = FALSE] # Look at the ith column of the truth
        mask_col = mask[, i, drop = FALSE]  # Get the ith column of the mask

        # These are the mean and sigma for the ith feature,
        # so dimensions batch_size x 2
        params = distr_params[ ,cur_distr_col:(cur_distr_col + 1), drop = FALSE]
        cur_distr_col = cur_distr_col + 2

        # generative model distribution for the feature
        distr = normal_parse_params(params, self$min_sigma)
        # distr$mean
        # distr$scale
        # log(1 + exp(params[,2]))

        # copy ground truth column, so that zeroing nans will not affect the original data
        gt_col_nansafe = groundtruth_col$clone()$detach()

        # If groundtruth don't have any nans then this line does not change anything
        nan_mask = torch_isnan(groundtruth_col)
        gt_col_nansafe[nan_mask] = 0
        # Everything that was nan is now 0.

        # Mask_col masks both the nan/missing values
        # and the artificially masked values.
        # We want to compute the the log prob
        # only over the artificially missing
        # features, so we omit the missing values.
        # Bottom of page 5.

        # So we remove the masking of the missing values
        # So those ones in mask_col which are there due
        # to missing values are now turned in to zeros.
        mask_col = mask_col * (torch_logical_not(nan_mask))$to(dtype = torch_float())

        # Get the log-likelihood, but only of the masked values
        # i.e., the ones hat are masked by the masking filter MCARGenerator
        # So this one is batch_size x 1.
        # So this is the log-lik of observing the ground truth
        # given the current parameters, for only the
        # artificially masked features.
        col_log_prob = distr$log_prob(gt_col_nansafe) * mask_col

      } else {
        # Categorical feature

        # categorical distribution

        # Extract the ground truth and mask
        groundtruth_col = groundtruth[, i, drop = FALSE] # Look at the ith column of the truth
        mask_col = mask[, i, drop = FALSE]  # Get the ith column of the mask

        # Extract the probabilities for each of the K-classes for the ith feature,
        # The dimension is batch_size x size.
        params = distr_params[ ,cur_distr_col:(cur_distr_col + size - 1), drop = FALSE]
        cur_distr_col = cur_distr_col + size

        # Create a categorical distrbution based on the extracted parameters.
        # Returns a "torch::distr_categorical" distribution.
        # Ensure that the probabbility for each class is at least self$min_prob.
        distr = categorical_parse_params_column(params, self$min_prob)

        # copy ground truth column, so that zeroing nans will not affect the original data
        gt_col_nansafe = groundtruth_col$clone()$detach()

        # If groundtruth don't have any nans then this line does not change anything
        nan_mask = torch_isnan(groundtruth_col)
        gt_col_nansafe[nan_mask] = 0

        # compute the mask of the values
        # which we consider in the log probability
        # So we remove the masking of the missing values
        # So those ones in mask_col which are there due
        # to missing values are now turned in to zeros.
        mask_col = mask_col * (torch_logical_not(nan_mask))$to(dtype = torch_float())
        col_log_prob = distr$log_prob(gt_col_nansafe$squeeze())[, NULL] * mask_col
        #col_log_prob = distr$log_prob(gt_col_nansafe) * mask_col
      }

      # append the column of log probabilities for the i-th feature
      # (for those instances that are masked) into log_prob list
      # log_prob.append(col_log_prob)
      log_prob = append(log_prob, col_log_prob)
      # log_prob is now a list of length num_features, where each
      # element is a tensor batch_size x 1 containing the log-lik
      # of the parameters of masked values.
    }

    # concatenate the list so we get a tensor of dim batch x features
    # Then we sum along the the rows. i.e., for each observation in the
    # batch. So a tensor of length batch size.
    return(torch_cat(log_prob, 2)$sum(-1))
  }
)


## CategoricalToOneHotLayer ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# This layer expands categorical features into one-hot vectors, because
# multi-layer perceptrons are known to work better with this data
# representation. It also replaces NaNs with zeros in order so that
# further layers may work correctly.
#
# one_hot_max_sizes[i] is the one-hot max size of i-th feature,
# if i-th feature is categorical, and 0 or 1 if i-th feature is real-valued.
#
# add_nan_maps_for_columns is an optional list which contains
# indices of columns which isnan masks are to be appended
# to the result tensor. This option is necessary for proposal
# network to distinguish whether value is to be reconstructed or not.
CategoricalToOneHotLayer = torch::nn_module(
  classname = "CategoricalToOneHotLayer",
  initialize = function(one_hot_max_sizes, add_nans_map_for_columns=NULL) {
    # Here one_hot_max_sizes includes zeros at the end of the list
    # one_hot_max_sizes + [0] * len(one_hot_max_sizes)
    # So if we have that features have this many categories [1, 2, 3, 1],
    # then we get that one_hot_max_sizes = [1, 2, 3, 1, 0, 0, 0, 0]
    self$one_hot_max_sizes = one_hot_max_sizes

    # Is always an empty column for the Masked Encoder network
    # while it is a list [0, 1, ..., length(one_hot_max_sizes)-1)
    # for the Full Encoder network.
    # So for the Full Encoder  network we apply the nan masks to each column/feature
    self$add_nans_map_for_columns = add_nans_map_for_columns
  },
  forward = function(input) {
    # input = torch_cat(c(batch, mask), -1)
    # Input is torch_cat(c(batch, mask), -1), so a matrix of
    # dimension batch_size x 2*sum(one_hot_max_sizes)
    # At least for continuous data where one_hot_max_sizes
    # only consists of ones.
    # ONE_HOT_MAX_SIZES ARE PADDED WITH ZEROS AT THE END!

    # Get the number of instances in the input batch.
    n = input$shape[1]

    # variable to store the outcolumns.
    # that is the input columns / one hot encoding
    # + is nan.mask.
    out_cols = NULL

    # We iterate over the features and get the number
    # of categories for each feature.
    # so i goes from 0 to 2*num_features-1
    # For i in [num_features, 2*num_features-1] will have size <= 1,
    # even for categorical features.
    i = 1
    for (i in seq(length(self$one_hot_max_sizes))) {
      size = self$one_hot_max_sizes[i]

      # Distinguish between continuous and categorical features
      if (size <= 1) {
        # If size <= 1, then the feature is continuous
        # just copy it and replace NaNs with zeros
        # OR, the last half of self.one_hot_max_sizes

        # Take the ith column of the input
        # NOTE THAT THIS IS NOT A DEEP COPY, so changing out_col changes input
        out_col = input[,i:i] #maybe add '$clone()$detach()'?

        # check if any of the values are nan, i.e., missing
        nan_mask = torch_isnan(out_col)

        # set all the missing values to 0.
        # THIS CHANGES THE INPUT VARIABLE.
        out_col[nan_mask] = 0
      } else  {
        # Categorical feature

        # Get the categories for each instance for the ith feature
        # start to count at zero. So if we have 2 cat, then this
        # vector will contains zeros and ones.
        #input[3,3] = NaN
        cat_idx = input[, i:i]#$clone()$detach()

        # Check if any of the categories are nan / missing
        nan_mask = torch_isnan(cat_idx)

        # THIS DOES NOT WORK BECAUSE, WHEN WE APPLY A MASK WE SET UNOBSERVED CATEGORICAL
        # FEATEURS TO CLASS 0. (AS WE MULITPLY WITH 0), BUT THEN NNF_ONE_HOT DOES NOT WORK
        # AS IT ONLY TAKES CLASSES 1,2,...,K.
        # # Done it using two approaches. One using torch, which is faster, so use it.
        # # Then we do the following:
        # {
        #   # Set the nan values to category one.
        #   # This will be overwritten below, but needed for nnf_one_hot to work
        #   # as it does not support observations without a class.
        #   cat_idx[nan_mask] = 1
        #
        #   # Generate the one hot encoding
        #   out_col = nnf_one_hot(cat_idx$to(dtype = torch_long()), size)$to(dtype = torch_bool())$squeeze()
        #
        #   # Overwrite the entries which we said belonged to category one, but was actually missing.
        #   out_col[torch_repeat_interleave(nan_mask, as.integer(size), dim = -1)] = 0
        # }
        # or we can use the default R. THIS IS A BIT SLOWER. LOOK MORE INTO IT!
        {
          # Set the nan values to 0
          cat_idx[nan_mask] = 0

          # create a matrix, where the jth row is the one-hot
          # encoding of the ith feature of the jth instance.
          out_col = matrix(0, nrow = n, ncol = size)
          out_col[cbind(seq(n), as.matrix(cat_idx))] = 1
          out_col = torch_tensor(out_col, device = input$device)
        }
      }

      # append this feature column to the result
      # out_col is n x size =
      # batch_size x num_categories_for_this_feature
      out_cols = torch_cat(c(out_cols, out_col), dim = -1)

      # if necessary, append isnan mask of this feature to the result
      # which we always do for the proposal network.
      # This only happens for the first half of the i's,
      # so for i = 1, ..., num_features.
      if (i %in% self$add_nans_map_for_columns) {
        # so we add the columns of nan_mask
        out_cols = torch_cat(c(out_cols, nan_mask$to(dtype = torch_float())), dim = -1)
      }
    }

    # ONLY FOR CONTINUOUS FEATURES.
    # out_cols now is a list of num_features tensors of shape n x size
    # = n x 1 for continuous variables. So we concatenate them
    # to get a matrix of dim n x 2*num_features (in cont case) for
    # prior net, but for proposal net, it is n x 3*num_features
    # They take the form  [batch1, is.nan1, batch2, is.nan2, …,
    # batch12, is.nan12, mask1, mask2, …, mask12]
    return(out_cols)
  }
)




# Mask Generators ========================================================================================================================================================================================================
## MCAR_mask_generator -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
MCAR_mask_generator = torch::nn_module(
  #' MCAR_mask_generator
  #' @examples
  #' mask_gen = MCAR_mask_generator(0.5)
  #' mask_gen(torch_randn(c(5, 3)))
  name = "MCAR_mask_generator",

  #' @description Initialize a missing completely at random mask generator.
  #' @param masking_ratio The probability for an entry in the generated mask to be 1 (masked).
  #' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  #' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  #' the first half and second half of the rows are duplicates of each other. That is,
  #' batch = [row1, row1, row2, row2, row3, row3, ...].
  initialize = function(masking_ratio = 0.5,
                        paired_sampling = FALSE) {
    self$masking_ratio = masking_ratio
    self$paired_sampling = paired_sampling
  },

  #' @description Generates a MCAR mask by calling self$MCAR_mask_generator_function function.
  #' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  #' entries are missing. If any are missing, then the returned mask will ensure that
  #' these missing entries are masked.
  forward = function(batch) {
    self$MCAR_mask_generator_function(batch,
                                      prob = self$masking_ratio,
                                      paired_sampling = self$paired_sampling)
  },

  #' Missing Completely At Random Mask Generator
  #'
  #' @description A mask generator where the masking is determined by component-wise
  #' independent Bernoulli distribution.
  #'
  #' @details Function that takes in a batch of observations and the probability
  #' of masking each element based on a component-wise independent Bernoulli
  #' distribution. Default value is 0.5, so all masks are equally likely to be trained.
  #' Function returns the mask of same shape as batch.
  #' Note that the batch can contain missing values, indicated by the "NaN" token.
  #' The mask will always mask missing values.
  #'
  #' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  #' entries are missing. If any are missing, then the returned mask will ensure that
  #' these missing entries are masked.
  #' @param prob Numeric between 0 and 1. The probability that an entry will be masked.
  #' @param seed Integer. Used to set the seed for the sampling process such that we
  #' can reproduce the same masks.
  #' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  #' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  #' the first half and second half of the rows are duplicates of each other. That is,
  #' batch = [row1, row1, row2, row2, row3, row3, ...].
  #'
  #' @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
  #' observed feature value will be masked. '0' means that the entry is NOT masked,
  #' i.e., the feature value will be observed/given/available.
  #' @examples  MCAR_mask_generator_function(torch_rand(c(5, 3)))
  MCAR_mask_generator_function = function(batch,
                                          prob = 0.5,
                                          seed = NULL,
                                          paired_sampling = FALSE) {
    # If the user specify a seed for reproducibility
    if (!is.null(seed)) set.seed(seed)

    # Get the number of entries in the batch.
    size = prod(batch$shape)

    # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
    if (paired_sampling) size = size / 2

    # Check for missing values in the batch
    nan_mask = batch$isnan()$to(torch_float())

    # # Torch version, but marginally slower than r version when batch_size <= 128 and num_features <= 50
    # mask = torch_bernoulli(torch_full_like(batch, prob))

    # Create the Bernoulli mask where an element is masked (1) with probability 'prob'.
    mask = torch_tensor(matrix(sample(c(0, 1),
                                      size = size,
                                      replace = TRUE,
                                      prob = c(prob, 1-prob)),
                               ncol = ncol(batch)),
                        dtype = torch_float())

    # If paired sampling, then concatenate the inverse mask.
    if (paired_sampling) {
      # Create the new order to ensure correct order [m1, !m1, m2, !m2, ...].
      new_order = c(matrix(seq(nrow(batch)), nrow = 2, byrow = T))

      # Concatenate the inverse mask and reorder.
      mask = torch_cat(c(mask, !mask), 1L)[new_order,]
    }

    # Final mask all entries that is either missing or artificially masked
    # by the Bernoulli mask. A value of 1 means that the entry is masked.
    return(mask + nan_mask >= 1)
  }
)


## Specified_probability_mask_generator --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Specified_probability_mask_generator = torch::nn_module(
  #' Specified_probability_mask_generator
  #' @examples
  #' probs = c(1, 8, 6, 3, 2)
  #' mask_gen = Specified_probability_mask_generator(probs)
  #' masks = mask_gen(torch_randn(c(10000, length(probs))-1))
  #' empirical_prob = table(as.array(masks$sum(2)))
  #' empirical_prob / sum(empirical_prob)
  #' probs / sum(probs)
  name = "Specified_probability_mask_generator",

  #' @description Initialize a specified_probability mask generator.
  #' @details A class that takes in the probabilities of having d masked observations.  I.e., for M dimensional data,
  #' masking_probs is of length M+1, where the d'th entry is the probability of having d-1 masked values.
  #' @param masking_probs An M+1 numerics containing the probabilities masking 'd' (0,...M) entries for each observation.
  #' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  #' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  #' the first half and second half of the rows are duplicates of each other. That is,
  #' batch = [row1, row1, row2, row2, row3, row3, ...].
  initialize = function(masking_probs,
                        paired_sampling = FALSE) {
    self$masking_probs = masking_probs / sum(masking_probs)
    self$paired_sampling = paired_sampling
  },

  #' @description Generates a specified probability mask by calling the self$Specified_probability_mask_generator_function.
  #' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the entries are
  #' missing. If any are missing, then the returned mask will ensure that these missing entries are masked.
  forward = function(batch) {
    self$Specified_probability_mask_generator_function(batch,
                                                       masking_prob = self$masking_probs,
                                                       paired_sampling = self$paired_sampling)
  },

  #' Specified Probability Mask Generator
  #'
  #' @description A mask generator that first samples the number of entries 'd' to be masked in
  #' the 'M'-dimensional observation 'x' in the batch based on the given M+1 probabilities. The
  #' 'd' masked are uniformly sampled from the 'M' possible feature indices. The d'th entry
  #' of the probability of having d-1 masked values.
  #'
  #' @details Note that MCAR_mask_generator with p = 0.5 is the same as using Specified_probability_mask_generator
  #' with masking_ratio = choose(M, 0:M), where M is the number of features. This function was initially
  #' created to check if increasing the probability of having a masks with many masked features improved
  #' VAEAC's performance by focusing more on these situations during training.
  #'
  #' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  #' entries are missing. If any are missing, then the returned mask will ensure that
  #' these missing entries are masked.
  #' @param masking_probs An M+1 numerics containing the probabilities masking 'd' (0,...M) entries for each observation.
  #' @param seed Integer. Used to set the seed for the sampling process such that we
  #' can reproduce the same masks.
  #' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  #' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  #' the first half and second half of the rows are duplicates of each other. That is,
  #' batch = [row1, row1, row2, row2, row3, row3, ...].
  #'
  #' @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
  #' observed feature value will be masked. '0' means that the entry is NOT masked,
  #' i.e., the feature value will be observed/given/available.
  #' @examples  Specified_probability_mask_generator_function(torch_rand(c(5, 4)), masking_probs = c(2,7,5,3,3))
  Specified_probability_mask_generator_function = function(batch,
                                                           masking_probs,
                                                           seed = NULL,
                                                           paired_sampling = FALSE) {
    # # Check for valid input.
    # if (ncol(batch) != (length(masking_probs)-1)) {
    #   stop(sprintf("Number of probabilities should be one more than the number of features: %d != 1 + %d.\n", length(masking_probs), ncol(batch)))
    # }

    # If the user specify a seed for reproducibility
    if (!is.null(seed))  set.seed(seed)

    # Check if we are doing paired sampling.
    if (paired_sampling) {
      # Divide the size by two as we later concatenate with the inverse mask.
      size = size / 2
    }

    # Check for missing values in the batch
    nan_mask = batch$isnan()$to(torch_float())

    # Get the number of features
    n_features = ncol(batch)

    # Get the number of observations in the batch
    size = nrow(batch)

    # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
    if (paired_sampling) size = size / 2

    # Sample the number of masked features in each row.
    num_masked_each_row = sample(x = seq(0, n_features),
                                 size = size,
                                 replace = TRUE,
                                 prob = masking_probs)

    # Crate the mask matrix
    mask = torch_zeros_like(batch)
    for (i in seq(size)) {
      if (num_masked_each_row[i] != 0) {
        mask[i, sample(n_features, size = num_masked_each_row[i], replace = FALSE)] = 1
      }
    }

    # If paired sampling, then concatenate the inverse mask.
    if (paired_sampling) {
      # Create the new order to ensure correct order [m1, !m1, m2, !m2, ...].
      new_order = c(matrix(seq(nrow(batch)), nrow = 2, byrow = T))

      # Concatenate the inverse mask and reorder.
      mask = torch_cat(c(mask, !mask), 1L)[new_order,]
    }

    # Final mask masks all entries that is either missing or artificially masked
    # by the generated mask. A value of 1 means that the entry is going to be masked.
    return(mask + nan_mask >= 1)
  }
)

## Specified_masks_mask_generator ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Specified_masks_mask_generator = torch::nn_module(
  #' Specified_probability_mask_generator
  #' @description Used for Shapley value estimation when only a subset of coalitions are used to compute the Shapley values.
  #' @examples
  #' masks = torch_tensor(matrix(c(0,0,1,0, 1,0,1,0, 1,1,1,1), nrow = 3, ncol = 4, byrow = TRUE))
  #' masks_probs = c(3, 1, 6)
  #' mask_gen = Specified_masks_mask_generator(masks = masks, masks_probs = masks_probs)
  #' empirical_prob = table(as.array(mask_gen(torch_randn(c(10000, ncol(masks))))$sum(-1)))
  #' empirical_prob / sum(empirical_prob)
  #' masks_probs / sum(masks_probs)
  name = "Specified_masks_mask_generator",

  #' @description Initialize a specified masks mask generator.
  #' @param masks Matrix/Tensor of possible/allowed 'masks' which we sample from.
  #' @param masks_probs Array of 'probabilities' for each of the masks specified in 'masks'.
  #' Note that they do not need to be between 0 and 1 (e.g. sampling frequency).
  #' They are scaled, hence, they only need to be positive.
  #' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  #' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  #' the first half and second half of the rows are duplicates of each other. That is,
  #' batch = [row1, row1, row2, row2, row3, row3, ...].
  initialize = function(masks,
                        masks_probs,
                        paired_sampling = FALSE) {
    self$masks = masks
    self$masks_probs = masks_probs / sum(masks_probs)
    self$paired_sampling = paired_sampling
  },

  #' @description Generates a mask by calling self$Specified_masks_mask_generator_function function.
  #' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  #' entries are missing. If any are missing, then the returned mask will ensure that
  #' these missing entries are masked.
  forward = function(batch) {
    self$Specified_masks_mask_generator_function(batch,
                                                 masks = self$masks,
                                                 masks_probs = self$masks_probs,
                                                 paired_sampling = self$paired_sampling)
  },

  #' Sampling Masks from the Provided Masks with the Given Probabilities
  #'
  #' @description Functions that samples masks from the provided masks.
  #'
  #' @details Function that takes in a 'batch' of observations and matrix of possible/allowed
  #' 'masks' which we are going to sample from based on the provided probability in 'masks_probs'.
  #' Function returns a mask of same shape as batch. Note that the batch can contain missing values,
  #' indicated by the "NaN" token. The mask will always mask missing values.
  #'
  #' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  #' entries are missing. If any are missing, then the returned mask will ensure that
  #' these missing entries are masked.
  #' @param masks Matrix/Tensor of possible/allowed 'masks' which we sample from.
  #' @param masks_probs Array of 'probabilities' for each of the masks specified in 'masks'.
  #' Note that they do not need to be between 0 and 1. They are scaled, hence, they only need to be positive.
  #' @param seed Integer. Used to set the seed for the sampling process such that we
  #' can reproduce the same masks.
  #' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  #' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  #' the first half and second half of the rows are duplicates of each other. That is,
  #' batch = [row1, row1, row2, row2, row3, row3, ...].
  #'
  #' @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
  #' observed feature value will be masked. '0' means that the entry is NOT masked,
  #' i.e., the feature value will be observed/given/available.
  #' @export
  Specified_masks_mask_generator_function = function(batch,
                                                     masks,
                                                     masks_probs,
                                                     seed = NULL,
                                                     paired_sampling = FALSE) {
    # Hashed out as we checking takes extra time
    # # Some check for valid inputs.
    # if (ncol(batch) != ncol(masks)) {
    #   stop(sprintf("The number of features in the 'batch' and 'masks' are incompatible: %d != %d.",
    #                ncol(batch), ncol(masks)))
    # }
    #
    # if (nrow(masks) != length(masks_probs)) {
    #   stop(sprintf("The number of masks in 'masks' and the number of probabilities in 'masks_probs' are incompatible: %d != %d.",
    #                nrow(masks), length(masks_probs)))
    # }

    # Set seed if the user specifies a seed for reproducibility.
    if (!is.null(seed)) set.seed(seed)

    # Check for missing values in the batch
    nan_mask = batch$isnan()$to(torch_float())

    # Get the number of masks to choose from
    n_masks = nrow(masks)

    # Get the number of observations in the batch
    size = nrow(batch)

    # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
    if (paired_sampling) size = size / 2

    # Sample 'n_observation' masks from the possible masks by first sampling the row indices
    # based on the given mask probabilities and then use these indices to extract the masks.
    mask_rows_indices = sample.int(n = n_masks,
                                   size = size,
                                   replace = TRUE,
                                   prob = masks_probs)
    mask = torch_tensor(masks[mask_rows_indices, ], dtype = torch_float())

    # If paired sampling, then concatenate the inverse mask.
    if (paired_sampling) {
      # Create the new order to ensure correct order [m1, !m1, m2, !m2, ...].
      new_order = c(matrix(seq(nrow(batch)), nrow = 2, byrow = T))

      # Concatenate the inverse mask and reorder.
      mask = torch_cat(c(mask, !mask), 1L)[new_order,]
    }

    # Final mask masks all entries that is either missing or artificially masked
    # by the generated mask. A value of 1 means that the entry is going to be masked.
    return(mask + nan_mask >= 1)
  }
)

# VAEAC Model ============================================================================================================================================================================================================
## VAEAC -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
VAEAC = torch::nn_module(
  classname = "VAEAC",

  #' Initializing a VAEAC Model
  #'
  #' @description Class that represents a VAEAC model. Create the Neural Networks and Training Utilities used in VAEAC.
  #'
  #' @details This function builds neural networks (masked encoder, full encoder, decoder) given
  #' the list of one-hot max sizes of the features in the dataset we use to train the VAEAC model,
  #' and the provided parameters for the networks. It also create reconstruction log probability function,
  #' and method for sampling from the decoder output. And then use this to create the VAEAC model.
  #'
  #' @param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
  #' The sizes for the continuous features can either be '0' or '1'.
  #' @param width Integer. The number of neurons in each hidden layer in the neural networks of the masked encoder, full encoder, and decoder.
  #' @param depth Integer. The number of hidden layers in the neural networks of the masked encoder, full encoder, and decoder.
  #' @param latent_dim Integer. The number of dimensions in the latent space.
  #' @param activation_function An nn_module representing an activation function. E.g., nn_relu, nn_leaky_relu, nn_selu, nn_sigmoid.
  #' @param use_skip_connections Boolean. If we are to use skip connections in each layer. If true, then we add the input to the outcome of each hidden layer, so the output becomes X + activation(WX + b). I.e., identity skip connection.
  #' @param use_skip_connections_between_masked_encoder_and_decoder Boolean. If we are to apply concatenate skip connections between the layers in the masked encoder and decoder.
  #' @param use_batch_normalization Boolean. If we are to use batch normalization after the activation function. Note that if \code{use_skip_connections} is TRUE, then the normalization is
  #' done after the adding from the skip connection. I.e, we batch normalize the whole quantity X + activation(WX + b).
  #' @param paired_sampling TODO
  #' @param mask_generator_name String specifying the type of mask generator to use. Need to be one of "MCAR_mask_generator", "Specified_probability_mask_generator", and "Specified_masks_mask_generator".
  #' @param masking_ratio Scalar. The probability for an entry in the generated mask to be 1 (masked). Not used if \code{mask_generator_only_these_coalitions} is given.
  #' @param mask_generator_only_these_coalitions Matrix containing the different coalitions to learn. Must be given if \code{mask_generator_name} = "Specified_masks_mask_generator".
  #' @param mask_generator_only_these_coalitions_probabilities Numerics containing the probabilities for sampling each mask in \code{mask_generator_only_these_coalitions}.
  #' Array containing the probabilities for sampling the coalitions in \code{mask_generator_only_these_coalitions}.
  #' @param sigma_mu Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder, see Section 3.3.1 in Olsen et al. (2022).
  #' @param sigma_sigma Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder, see Section 3.3.1 in Olsen et al. (2022).
  #'
  #' @return Returns a list with the neural networks of the masked encoder, full encoder, and decoder together
  #' with reconstruction log probability function, optimizer constructor, sampler from the decoder output,
  #' mask generator, batch size, and scale factor for the stability of the variational lower bound optimization.
  #'
  #' @export
  initialize = function(one_hot_max_sizes,
                        width = 32,
                        depth = 3,
                        latent_dim = 8,
                        activation_function = nn_relu,
                        use_skip_connections = FALSE,
                        use_skip_connections_between_masked_encoder_and_decoder = FALSE,
                        use_batch_normalization = FALSE,
                        paired_sampling = FALSE,
                        mask_generator_name = c("MCAR_mask_generator", "Specified_probability_mask_generator", "Specified_masks_mask_generator"),
                        masking_ratio = 0.5,
                        mask_generator_only_these_coalitions = NULL,
                        mask_generator_only_these_coalitions_probabilities = NULL,
                        sigma_mu = 1e4,
                        sigma_sigma = 1e-4) {

    # Check that a valid mask_generator was provided.
    mask_generator_name = match.arg(mask_generator_name)

    # Get the number of features
    num_features = length(one_hot_max_sizes)

    # Extra strings to add to names of layers depending on if we use memory layers and/or batch normalization.
    # If FALSE, they are just an empty string and do not effect the names.
    name_extra_memory_layer = ifelse(use_skip_connections_between_masked_encoder_and_decoder, "_and_memory", "")
    name_extra_batch_normalize = ifelse(use_batch_normalization, "_and_batch_norm", "")

    # Save some of the initializing hyperparameters to the VAEAC object. Others are saved later.
    self$one_hot_max_sizes = one_hot_max_sizes
    self$depth = depth
    self$width = width
    self$latent_dim = latent_dim
    self$activation_function = activation_function
    self$use_skip_connections = use_skip_connections
    self$use_skip_connections_between_masked_encoder_and_decoder = use_skip_connections_between_masked_encoder_and_decoder
    self$use_batch_normalization = use_batch_normalization
    self$sigma_mu = sigma_mu
    self$sigma_sigma = sigma_sigma
    self$paired_sampling = paired_sampling

    # Save the how to compute the loss and how to sample from the VAEAC model.
    self$reconstruction_log_prob = GaussianCategoricalLoss(one_hot_max_sizes)
    self$sampler_most_likely = GaussianCategoricalSamplerMostLikely(one_hot_max_sizes)
    self$sampler_random = GaussianCategoricalSamplerRandom(one_hot_max_sizes)
    self$generative_parameters = GaussianCategoricalParameters(one_hot_max_sizes)
    self$num_features = num_features
    self$vlb_scale_factor = 1 / num_features

    ##### Generate the mask generator
    if (mask_generator_name == "MCAR_mask_generator") {
      # Create a MCAR_mask_generator and attach it to the VAEAC object. Note that masking_ratio is a singleton here.
      self$mask_generator = MCAR_mask_generator(masking_ratio = masking_ratio,
                                                paired_sampling = paired_sampling)

      # Attach the masking ratio to the VAEAC object.
      self$masking_ratio = masking_ratio

    } else if (mask_generator_name == "Specified_probability_mask_generator") {
      # Create a Specified_probability_mask_generator and attach it to the VAEAC object. Note that masking_ratio is an array here.
      self$mask_generator = Specified_probability_mask_generator(masking_probs = masking_ratio,
                                                                 paired_sampling = paired_sampling)

      # Attach the masking probabilities to the VAEAC object.
      self$masking_probs = masking_ratio

    } else if (mask_generator_name == "Specified_masks_mask_generator") {
      # Small check that they have been provided.
      if (is.null(mask_generator_only_these_coalitions) | is.null(mask_generator_only_these_coalitions_probabilities)) {
        stop("Both 'mask_generator_only_these_coalitions' and 'mask_generator_only_these_coalitions_probabilities' must be provided when using 'Specified_masks_mask_generator'.\n")
      }

      # Create a Specified_masks_mask_generator and attach it to the VAEAC object.
      self$mask_generator = Specified_masks_mask_generator(masks = mask_generator_only_these_coalitions,
                                                           masks_probs = mask_generator_only_these_coalitions_probabilities,
                                                           paired_sampling = paired_sampling)

      # Save the possible masks and corresponding probabilities to the VAEAC object.
      self$masks = mask_generator_only_these_coalitions
      self$masks_probs = mask_generator_only_these_coalitions_probabilities

    } else {
      # Print error to user.
      stop(sprintf("Maske geneartor '%s' is not supported.
Chose one of 'MCAR_mask_generator', 'Specified_probability_mask_generator', and 'Specified_masks_mask_generator'.\n", mask_generator))
    }

    ##### Full Encoder
    full_encoder_network = nn_sequential()

    # Full Encoder: Input layer
    full_encoder_network$add_module(
      module = CategoricalToOneHotLayer(c(one_hot_max_sizes, rep(0, num_features)), seq(num_features)),
      name = "input_layer_cat_to_one_hot")
    full_encoder_network$add_module(
      module = nn_linear(in_features = sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) + num_features*2, out_features = width),
      name = "input_layer_linear")
    full_encoder_network$add_module(
      module = activation_function(),
      name = "input_layer_layer_activation")
    if (use_batch_normalization) {
      full_encoder_network$add_module(
        module = nn_batch_norm1d(num_features = width),
        name = "input_layer_layer_batch_norm")
    }

    # Full Encoder: Hidden layers
    for (i in seq(depth)) {
      if (use_skip_connections) {
        # Add identity skip connection. Such that the input is added to the output of the linear layer and activation function: output = X + activation(WX + b).
        full_encoder_network$add_module(
          module = SkipConnection(
            nn_linear(width, width),
            activation_function(),
            if (use_batch_normalization) nn_batch_norm1d(num_features = width)),
          name = paste0("hidden_layer_", i, "_skip_connection_with_linear_and_activation", name_extra_batch_normalize)
        )
      } else {
        # Do not use skip connections and do not add the input to the output.
        full_encoder_network$add_module(
          module = nn_linear(width, width),
          name = paste0("hidden_layer_", i, "_linear"))
        full_encoder_network$add_module(
          module = activation_function(),
          name = paste0("hidden_layer_", i, "_activation"))
        if (use_batch_normalization ) {
          full_encoder_network$add_module(
            module = nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm"))
        }
      }
    }

    # Full Encoder: Go to latent space
    full_encoder_network$add_module(
      module = nn_linear(width, latent_dim * 2),
      name = "latent_space_layer_linear")

    ##### Masked Encoder
    masked_encoder_network = nn_sequential()

    # Masked Encoder: Input layer
    masked_encoder_network$add_module(
      module = CategoricalToOneHotLayer(c(one_hot_max_sizes, rep(0, num_features))),
      name = "input_layer_cat_to_one_hot")
    if (use_skip_connections_between_masked_encoder_and_decoder) {
      masked_encoder_network$add_module(
        module = MemoryLayer("#input"),
        name = "input_layer_memory")
    }
    masked_encoder_network$add_module(
      module = nn_linear(in_features = sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) + num_features, out_features = width),
      name = "input_layer_linear")
    masked_encoder_network$add_module(
      module = activation_function(),
      name = "input_layer_activation")
    if (use_batch_normalization) {
      masked_encoder_network$add_module(
        module = nn_batch_norm1d(num_features = width),
        name = "input_layer_batch_norm")
    }

    # Masked Encoder: Hidden layers
    for (i in seq(depth)) {
      if (use_skip_connections) {
        # Add identity skip connection. Such that the input is added to the output of the linear layer and activation function: output = X + activation(WX + b).
        # Also check inside SkipConnection if we are to use MemoryLayer. I.e., skip connection with concatenation from masked encoder to decoder.
        masked_encoder_network$add_module(
          module = SkipConnection(
            if(use_skip_connections_between_masked_encoder_and_decoder) MemoryLayer(paste0("#", i)),
            nn_linear(width, width),
            activation_function()),
          name = paste0("hidden_layer_", i, "_skip_connection_with_linear_and_activation", name_extra_memory_layer))
        if (use_batch_normalization) {
          masked_encoder_network$add_module(
            module = nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm"))
        }
      } else {
        # Do not use skip connections and do not add the input to the output.
        if (use_skip_connections_between_masked_encoder_and_decoder) {
          masked_encoder_network$add_module(
            module = MemoryLayer(paste0("#", i)),
            name = paste0("hidden_layer_", i, "_memory"))
        }
        masked_encoder_network$add_module(
          module = nn_linear(width, width),
          name = paste0("hidden_layer_", i, "_linear"))
        masked_encoder_network$add_module(
          module = activation_function(),
          name = paste0("hidden_layer_", i, "_activation"))
        if (use_batch_normalization) {
          masked_encoder_network$add_module(
            module = nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm"))
        }
      }
    }

    # Masked Encoder: Go to latent space
    if (use_skip_connections_between_masked_encoder_and_decoder) {
      masked_encoder_network$add_module(
        module = MemoryLayer(paste0("#", depth+1)),
        name = "latent_space_layer_memory")
    }
    masked_encoder_network$add_module(
      module = nn_linear(width, 2*latent_dim),
      name = "latent_space_layer_linear")

    ##### Decoder
    decoder_network = nn_sequential()

    # Decoder: Go from latent space
    decoder_network$add_module(
      module = nn_linear(latent_dim, width),
      name = "latent_space_layer_linear")
    decoder_network$add_module(
      module =  activation_function(),
      name = "latent_space_layer_activation")
    if (use_batch_normalization) {
      decoder_network$add_module(
        module = nn_batch_norm1d(num_features = width),
        name = "latent_space_layer_batch_norm")
    }

    # Get the width of the hidden layers in the decoder. Needs to be multiplied with two if
    # we use skip connections between masked encoder and decoder as we concatenate the tensors.
    width_decoder = ifelse(use_skip_connections_between_masked_encoder_and_decoder, 2*width, width)

    # Same for the input dimension to the last layer in decoder that yields the distribution params.
    extra_params_from_skip_connection_from_masked_encoder =
      ifelse(test = use_skip_connections_between_masked_encoder_and_decoder,
             yes = sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) + num_features,
             no = 0)

    # Will need an extra hidden layer if we use skip connection from masked encoder to decoder
    # as we send the full input layer of the masked encoder to the last layer in the decoder.
    depth_decoder = ifelse(use_skip_connections_between_masked_encoder_and_decoder, depth + 1, depth)

    # Decoder: Hidden layers
    for (i in seq(depth_decoder)) {
      if (use_skip_connections) {
        # Add identity skip connection. Such that the input is added to the output of the linear layer and activation function: output = X + activation(WX + b).
        # Also check inside SkipConnection if we are to use MemoryLayer. I.e., skip connection with concatenation from masked encoder to decoder.
        # If TRUE, then the memory layers extracts the corresponding input used in the masked encoder and concatenate them with the current input.
        # Note that we add the memory layers in the opposite direction from how they were created. So, we get a classical U-net with latent
        # space at the bottom and a connection between the layers on the same height of the U-shape.
        decoder_network$add_module(
          module = nn_sequential(
            SkipConnection(
              if (use_skip_connections_between_masked_encoder_and_decoder) MemoryLayer(paste0("#", depth-i+2), TRUE),
              nn_linear(width_decoder, width),
              activation_function())),
          name = paste0("hidden_layer_", i, "_skip_connection_with_linear_and_activation", name_extra_memory_layer))
        if (use_batch_normalization) {
          decoder_network$add_module(
            module = nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm"))
        }
      } else {
        # Do not use skip connections and do not add the input to the output.
        if (use_skip_connections_between_masked_encoder_and_decoder) {
          decoder_network$add_module(
            module = MemoryLayer(paste0("#", depth-i+2), TRUE),
            name = paste0("hidden_layer_", i, "_memory"))
        }
        decoder_network$add_module(
          module = nn_linear(width_decoder, width),
          name = paste0("hidden_layer_", i, "_linear"))
        decoder_network$add_module(
          module = activation_function(),
          name = paste0("hidden_layer_", i, "_activation"))
        if (use_batch_normalization) {
          decoder_network$add_module(
            module = nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm"))
        }
      }
    }

    # Decoder: Go the parameter space of the generative distributions
    # Concatenate the input to the first layer of the masked encoder to the last layer of the decoder network.
    if (use_skip_connections_between_masked_encoder_and_decoder) {
      decoder_network$add_module(
        module = MemoryLayer('#input', TRUE),
        name = "output_layer_memory")
    }
    # Linear layer to the parameters of the generative distributions Gaussian and Categorical.
    # Note that sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) is the number of
    # one hot variables to the masked encoder and num_features represents the binary variables if
    # the features was masked/missing or not when they entered the masked encoder.
    # The output dimension is 2 for the continuous features and K_i for categorical feature X_i,
    # where K_i is the number of classes the i'th categorical feature can take on.
    decoder_network$add_module(
      module = nn_linear(in_features = width + extra_params_from_skip_connection_from_masked_encoder,
                         out_features = sum(apply(rbind(one_hot_max_sizes, rep(2, num_features)), 2, max))),
      name = "output_layer_linear")

    # Save the networks to the VAEAC object
    self$full_encoder_network = full_encoder_network
    self$masked_encoder_network = masked_encoder_network
    self$decoder_network = decoder_network

    # Compute the number of trainable parameters in the different networks
    num_trainable_params_full_encoder   = sum(sapply(full_encoder_network$parameters, function(p) prod(p$size())))
    num_trainable_params_masked_encoder = sum(sapply(masked_encoder_network$parameters, function(p) prod(p$size())))
    num_trainable_params_decoder        = sum(sapply(decoder_network$parameters, function(p) prod(p$size())))
    num_trainable_params_total = num_trainable_params_full_encoder + num_trainable_params_masked_encoder + num_trainable_params_decoder
    num_trainable_params = rbind(num_trainable_params_total,
                                 num_trainable_params_full_encoder,
                                 num_trainable_params_masked_encoder,
                                 num_trainable_params_decoder)

    # Save the number of parameters to the VAEAC object
    self$num_trainable_params = num_trainable_params
  },

  #' @description Forward functions are required in nn_modules, but is it not needed in the way we have implemented VAEAC.
  #'
  #' @param ... Anything, as the function does not use it.
  forward = function(...) {
    warning("NO FORWARD FUNCTION IMPLEMENTED FOR VAEAC.")
    return("NO FORWARD FUNCTION IMPLEMENTED FOR VAEAC.")
  },

  #' Apply Mask to Batch to Create Observed Batch
  #'
  #' @description Clones the batch and applies the mask to set masked entries to 0 to create the observed batch.
  #'
  #' @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  #' @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as \code{batch}.
  make_observed = function(batch, mask) {
    # Clone and detach the batch from the graph (removes the gradient element for the tensor).
    observed = batch$clone()$detach()

    # Apply the mask by masking every entry in batch where 'mask' is 1.
    observed[mask == 1] = 0

    # Return the observed batch where masked entries are set to 0.
    return (observed)
  },

  #' Compute the Latent Distributions Inferred by the Encoders
  #'
  #' @description Compute the parameters for the latent normal distributions inferred by the encoders.
  #' If \code{only_masked_encoder = TRUE}, then we only compute the latent normal distributions inferred by the
  #' masked encoder. This is used in the deployment phase when we do not have access to the full observation.
  #'
  #' @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  #' @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as \code{batch}.
  #' @param only_masked_encoder Boolean. If we are only to compute the latent distributions for the masked encoder.
  #' Used in deployment phase when we do not have access to the full data. Always FALSE in the training phase.
  make_latent_distributions = function(batch, mask, only_masked_encoder = FALSE) {
    # Artificially mask the observations where mask == 1 to create the observed batch values.
    observed = self$make_observed(batch = batch, mask = mask)

    # Check if we are in training or deployment phase
    if (only_masked_encoder) {
      # In deployment phase and only use the masked encoder.
      full_encoder = NULL

    } else {
      # In the training phase where we need to use both masked and full encoder.

      # Column bind the batch and the mask to create the full information sent to the full encoder.
      full_info = torch_cat(c(batch, mask), dim = 2)

      # Send the full_information through the full encoder. It needs the full information to know if a
      # value is missing or just masked. The output tensor is of shape batch_size x (2 x latent_dim)
      # In each row, i.e., each observation in the batch, the first latent_dim entries are the means mu
      # while the last latent_dim entries are the softplus of the sigmas, so they can take on any
      # negative or positive value. Recall that softplus(x) = ln(1+e^{x}).
      full_encoder_params = self$full_encoder_network(full_info)

      # Takes the full_encoder_parameters and returns a normal distribution, which is component-wise
      # independent. If sigma (after softmax transform) is less than 1e-3, then we set sigma to 0.001.
      full_encoder = normal_parse_params(params = full_encoder_params, min_sigma = 1e-3)
    }

    # Column bind the batch and the mask to create the observed information sent to the masked encoder.
    observed_info = torch_cat(c(observed, mask), dim = -1)

    # Compute the latent normal dist parameters (mu, sigma) for the masked
    # encoder by sending the observed values and the mask to the masked encoder.
    masked_encoder_params = self$masked_encoder_network(observed_info)

    # Create the latent normal distributions based on the parameters (mu, sigma) from the masked encoder
    masked_encoder = normal_parse_params(params = masked_encoder_params, min_sigma = 1e-3)

    # Return the full and masked encoders
    return(list(full_encoder = full_encoder,
                masked_encoder = masked_encoder))
  },

  #' Compute the Regularizes for the Latent Distribution Inferred by the Masked Encoder.
  #'
  #' @description The masked encoder (prior) distribution regularization in the latent space.
  #' This is used to compute the extended variational lower bound used to train VAEAC, see
  #' Section 3.3.1 in Olsen et al. (2022).
  #' Though regularizing prevents the masked encoder distribution parameters from going to infinity,
  #' the model usually doesn't diverge even without this regularization. It almost doesn't affect
  #' learning process near zero with default regularization parameters which are recommended to be used.
  #'
  #' @param masked_encoder The torch_Normal object returned when calling the masked encoder.
  masked_encoder_regularization = function(masked_encoder) {
    # Extract the number of observations. Same as batch_size.
    num_observations = masked_encoder$mean$shape[1]

    # Extract the number of dimension in the latent space.
    num_latent_dimensions = masked_encoder$mean$shape[2]

    # Extract means and ensure correct shape (batch_size x latent_dim).
    mu = masked_encoder$mean$view(c(num_observations, num_latent_dimensions))

    # Extract the sigmas and ensure correct shape (batch_size x latent_dim).
    sigma = masked_encoder$scale$view(c(num_observations, num_latent_dimensions))

    # Note that sum(-1) indicates that we sum together the columns.
    # mu_regularizer is then a tensor of length num_observations
    mu_regularizer = - (mu^2)$sum(-1) / (2*self$sigma_mu^2)

    # sigma_regularizer is then also a tensor of length num_observations.
    sigma_regularizer = (sigma$log() - sigma)$sum(-1) * self$sigma_sigma

    # Add the regularization terms together and return them.
    return(mu_regularizer + sigma_regularizer)
  },

  #' Compute the Variational Lower Bound for the Observations in the Batch
  #'
  #' @description Compute differentiable lower bound for the given batch of objects and mask.
  #' Used as the (negative) loss function for training the VAEAC model.
  #'
  #' @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  #' @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as \code{batch}.
  batch_vlb = function(batch, mask) {
    # Compute the latent normal distributions obtained from the full and masked encoder
    encoders_list = self$make_latent_distributions(batch = batch, mask = mask)

    # Extract the masked and full encoders. These are torch_Normal objects.
    masked_encoder = encoders_list$masked_encoder
    full_encoder = encoders_list$full_encoder

    # Apply the regularization on the mus and sigmas of the normal dist obtained from the masked encoder
    # such that they don't blow up. Regularized according to their normal gamma prior, see Olsen et al. (2022).
    masked_encoder_regularization = self$masked_encoder_regularization(masked_encoder)

    # To use the reparameterization trick to train VAEAC, we need to use 'rsample'
    # and not 'sample', which allows backpropagation through the mean and standard deviation layers,
    # see https://pytorch.org/docs/stable/distributions.html#pathwise-derivative.
    # For each training instance in the batch we sample values for each of the latent variables,
    # i.e.,  we get a tensor of dimension batch_size x latent_dim.
    latent = full_encoder$rsample()

    # Send the latent samples through the decoder and get the batch_size x 2*num_features (in cont case)
    # where we for each row have a normal dist on each feature The form will be (mu_1, sigma_1, ..., mu_p, sigma_p)
    reconstruction_params = self$decoder_network(latent)

    # Compute the reconstruction loss, i.e., the log likelihood of only the masked values in
    # the batch (true values) given the current reconstruction parameters from the decoder.
    # We do not consider the log likelihood of observed or missing/nan values.
    reconstruction_loss = self$reconstruction_log_prob(batch, reconstruction_params, mask)

    # Compute the KL divergence between the two latent normal distributions obtained from the full encoder
    # and masked encoder. Since the networks create MVN with diagonal covariance matrices, that is, the same as
    # a product of individual Gaussian distributions, we can compute KL analytically very easily:
    # KL(p, q) = \int p(x) log(p(x)/q(x)) dx = 0.5 * { (sigma_p/sigma_q)^2 + (mu_q - mu_p)^2/sigma_q^2 - 1 + 2 ln (sigma_q/sigma_p)}
    # when both p and q are torch_Normal objects.
    kl = kl_normal_normal(full_encoder, masked_encoder)$view(c(batch$shape[1], -1))$sum(-1)

    # Return the variational lower bound with the prior regularization. See Section 3.3.1 in Olsen et al. (2022)
    return(reconstruction_loss - kl + masked_encoder_regularization)
  },

  #' Compute the Importance Sampling Estimator for the Observations in the Batch
  #'
  #' @description Compute IWAE log likelihood estimate with K samples per object.
  #'
  #' @details Technically, it is differentiable, but it is recommended to use it for
  #' evaluation purposes inside torch.no_grad in order to save memory. With torch::with_no_grad
  #' the method almost doesn't require extra memory for very large K. The method makes K independent
  #' passes through decoder network, so the batch size is the same as for training with batch_vlb.
  #' IWAE is an abbreviation for Importance Sampling Estimator
  #' log p_{theta, psi}(x|y) approx
  #' log {1/K * sum_{i=1}^K [p_theta(x|z_i, y) * p_psi(z_i|y) / q_phi(z_i|x,y)]} =
  #' log {sum_{i=1}^K exp(log[p_theta(x|z_i, y) * p_psi(z_i|y) / q_phi(z_i|x,y)])} - log(K) =
  #' log {sum_{i=1}^K exp(log[p_theta(x|z_i, y)] + log[p_psi(z_i|y)] - log[q_phi(z_i|x,y)])} - log(K) =
  #' logsumexp(log[p_theta(x|z_i, y)] + log[p_psi(z_i|y)] - log[q_phi(z_i|x,y)]) - log(K) =
  #' logsumexp(rec_loss + prior_log_prob - proposal_log_prob) - log(K),
  #' where z_i ~ q_phi(z|x,y).
  #'
  #' @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  #' @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as \code{batch}.
  #' @param K Integer. The number of samples generated to compute the IWAE for each observation in \code{batch}.
  batch_iwae = function(batch, mask, K) {
    # Compute the latent normal distributions obtained from the full and masked encoder
    encoders_list = self$make_latent_distributions(batch = batch, mask = mask)

    # Extract the masked and full encoders. These are torch_Normal objects.
    masked_encoder = encoders_list$masked_encoder
    full_encoder = encoders_list$full_encoder

    # List to store the estimates.
    estimates = list()

    # Iterate over the number of samples/passes through the decoder for each validation observation.
    for (i in seq(K)) {

      # See equation 18 on page 18 in Ivanov et al. (2019). Create samples from the
      # full encoder; z_i ~ q_phi(z|x,y). We get a tensor of dimension batch_size x latent_dim.
      latent = full_encoder$rsample()

      # Send the latent samples through the decoder and get the batch_size x 2*num_features (in cont case)
      # where we for each row have a normal dist on each feature The form will be (mu_1, sigma_1, ..., mu_p, sigma_p)
      reconstruction_params = self$decoder_network(latent)

      # Compute the reconstruction loss, i.e., the log likelihood of only the masked values in
      # the batch (true values) given the current reconstruction parameters from the decoder.
      # We do not consider the log likelihood of observed or missing/nan values.
      reconstruction_loss = self$reconstruction_log_prob(batch, reconstruction_params, mask)

      # Compute the log likelihood of observing the sampled latent representations from
      # the full_encoder when using the normal distribution estimated by the masked_encoder.
      masked_encoder_log_prob = masked_encoder$log_prob(latent)

      # Ensure dimensions batch$shape[1] x something.
      masked_encoder_log_prob = masked_encoder_log_prob$view(c(batch$shape[1], -1))

      # Sum over the rows (last dimension), i.e., add the log-likelihood for each instance.
      masked_encoder_log_prob = masked_encoder_log_prob$sum(-1)

      # Same explanations here as above, but now for the full_encoder.
      full_encoder_log_prob = full_encoder$log_prob(latent)
      full_encoder_log_prob = full_encoder_log_prob$view(c(batch$shape[1], -1))
      full_encoder_log_prob = full_encoder_log_prob$sum(-1)

      # Combine the estimated loss based on the formula from equation 18 on page 18 in Ivanov et al. (2019).
      # Consists of batch.shape[0] number of values
      estimate = reconstruction_loss + masked_encoder_log_prob - full_encoder_log_prob

      # Make sure that the results are a column vector of height batch_size.
      estimate = estimate$unsqueeze(-1)

      # Add the results to the estimates list
      estimates = append(estimates, estimate)
    }

    # Convert from list of tensors to a single tensor using colum bind
    estimates = torch_cat(estimates, -1)

    # Use the stabilizing trick logsumexp.
    # We have worked on log-scale above, hence plus and minus and not multiplication and division,
    # while Eq. 18 in Ivanov et al. (2019) work on regular scale with multiplication and division.
    # We take the exp of the values to get back to original scale, then sum it and convert back to
    # log scale. Note that we add -log(K) instead of dividing each term by K.
    # Take the log sum exp along the rows (validation samples) then subtract log(K).
    return(torch_logsumexp(estimates, -1) - log(K))
  },

  #' Generate the Parameters of the Generative Distributions
  #'
  #' @description Generate the parameters of the generative distributions for samples from the batch.
  #'
  #' @details The function makes K latent representation for each object from the batch, send these
  #' latent representations through the decoder to obtain the parameters for the generative distributions.
  #' I.e., means and variances for the normal distributions (continuous features) and probabilities
  #' for the categorical distribution (categorical features).
  #' The second axis is used to index samples for an object, i.e. if the batch shape is [n x D1 x D2], then
  #' the result shape is [n x K x D1 x D2]. It is better to use it inside torch::with_no_grad in order to save
  #' memory. With torch::with_no_grad the method doesn't require extra memory except the memory for the result.
  #'
  #' @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  #' @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as \code{batch}.
  #' @param K Integer. The number of imputations to be done for each observation in batch.
  generate_samples_params = function(batch, mask, K = 1) {
    # Compute the latent normal distributions obtained from only the masked encoder.
    encoders_list = self$make_latent_distributions(batch = batch, mask = mask, only_masked_encoder = TRUE)

    # Only extract the masked encoder (torch_Normal object) as we are in the deployment phase.
    masked_encoder = encoders_list$masked_encoder

    # Create a list to keep the sampled parameters.
    samples_params = list()

    # Iterate over the number of imputations for each observation in the batch.
    for (i in seq(K)) {

      # Generate latent representations by using the masked encoder.
      latent = masked_encoder$rsample()

      # Send the latent representations through the decoder.
      sample_params = self$decoder_network(latent)

      # Collect the parameters of the induced Gaussian distributions.
      samples_params = append(samples_params, sample_params$unsqueeze(2))
    }

    # Concatenate the list to a 3d-tensor. 2nd dimensions is the imputations.
    return(torch_cat(samples_params, 2))
  }
)


# Train VAEAC Model =================================================================================================================================================================================================

#' Train the VAEAC Model
#'
#' @description Function that fits a VAEAC model to the given dataset based on the provided parameters.
#'
#' @details One_hot_max_sizes is one for continuous variables and the number of categories for a categorical variable.
#' First fit several VAEAC models in the initialization phase to reduce the effect of potentially starting with poor value
#'
#' @param training_data A matrix or data.frame containing the data. Categorical data must have class names 1,2,...,K.
#' @param model_description String containing, e.g., the name of the data distribution or additional parameter information. Used in the save name of the fitted model.
#' @param folder_to_save_model String specifying a path to a folder where the function is to save the fitted VAEAC model.
#' @param use_cuda Boolean. If we are to use cuda (GPU) if available.
#' @param num_different_vaeac_initiate Integer. The number of different VAEAC models to initiate in the start. Pick the best performing one after \code{epochs_initiation_phase } and continue training that one.
#' @param epochs_initiation_phase Integer. The number of epochs to run each of the \code{num_different_vaeac_initiate} VAEAC models before only continuing training the best one.
#' @param epochs Integer. The number of epochs to train the final VAEAC model. This includes \code{epochs_initiation_phase}.
#' @param validation_ratio Scalar between 0 and 1 indicating the ratio of instances from data which will be used as validation data.
#' @param validation_iwae_num_samples Integer. The number of samples used to compute the IWAE when validating the VAEAC model on the validation data.
#' @param depth Integer. The number of hidden layers in the neural networks of the masked encoder, full encoder, and decoder.
#' @param width Integer. The number of neurons in each hidden layer in the neural networks of the masked encoder, full encoder, and decoder.
#' @param latent_dim Integer. The number of dimensions in the latent space.
#' @param lr Numeric. The learning rate used in the ADAM optimizer.
#' @param batch_size Integer. The number of samples to include in each batch.
#' @param running_avg_num_values Integer. How many of the previous values to include when we compute the running means.
#' @param activation_function An nn_module representing an activation function. E.g., nn_relu, nn_leaky_relu, nn_selu, nn_sigmoid.
#' @param use_skip_connections Boolean. If we are to use skip connections in each layer. If true, then we add the input to the outcome of each hidden layer, so the output becomes X + activation(WX + b). I.e., identity skip connection.
#' @param use_skip_connections_between_masked_encoder_and_decoder Boolean. If we are to apply concatenate skip connections between the layers in the masked encoder and decoder.
#' @param use_batch_normalization Boolean. If we are to use batch normalization after the activation function. Note that if \code{use_skip_connections} is TRUE, then the normalization is
#' done after the adding from the skip connection. I.e, we batch normalize the whole quantity X + activation(WX + b).
#' @param paired_sampling Boolean. If we are doing paired sampling. I.e., each batch contains two versions of the same training observation,
#' but where the first one is masked by S and the second one is masked by \bar{S}, the complement. See FastSHAP by Jethani et al (2022).
#' @param masking_ratio Probability of masking a feature in the MCAR mask generator. Default masking scheme which ensures that
#' VAEAC can do arbitrary conditioning. Is overruled if \code{mask_generator_only_these_coalitions} is specified.
#' @param mask_generator_only_these_coalitions Matrix containing the different coalitions to learn.
#' @param mask_generator_only_these_coalitions_probabilities Numerics containing the probabilities for sampling each mask in \code{mask_generator_only_these_coalitions}.
#'  Array containing the probabilities for sampling the coalitions in \code{mask_generator_only_these_coalitions}.
#' @param sigma_mu Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder, see Section 3.3.1 in Olsen et al. (2022).
#' @param sigma_sigma Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder, see Section 3.3.1 in Olsen et al. (2022).
#' @param save_data Boolean. If we are to save the data together with the model. Useful if one are to continue to train the model later.
#' @param transform_all_continuous_features Boolean. If we are to log transform all continuous features before sending the data to VAEAC.
#' VAEAC creates unbounded values, so if the continuous features are strictly positive, as for Burr and Abalone data, it can be advantageous
#' to log-transform the data to unbounded form before using VAEAC. If TRUE, then \code{VAEAC_postprocess_data} will take the exp of the results
#' to get back to strictly positive values when using the VAEAC model to impute missing values.
#' @param verbose Boolean. If we are to print the progress of the initialization of different VAEAC models, the training of the final VAEAC model, and summary of the training progress.
#' @param save_VAEAC_every_nth_epoch Integer. If we are to save the VAEAC model after every nth epoch.
#' @param seed Integer. Seed for reproducibility.
#' @param ... List of extra parameters, currently not used.
#'
#' @return A list containing the training/validation errors and paths to where the VAEAC models are saved on the disk.
#' @export
#'
#'
train_VAEAC_model = function(training_data,
                             model_description = NULL,
                             folder_to_save_model = NULL,
                             use_cuda = FALSE,
                             num_different_vaeac_initiate = 10,
                             epochs_initiation_phase = 2,
                             epochs = 200,
                             save_VAEAC_every_nth_epoch = NULL,
                             validation_ratio = 0.25,
                             validation_iwae_num_samples = 25,
                             depth = 3,
                             width = 32,
                             latent_dim = 8,
                             lr = 0.001,
                             batch_size = 64,
                             running_avg_num_values = 5,
                             activation_function = nn_relu,
                             use_skip_connections = TRUE,
                             use_skip_connections_between_masked_encoder_and_decoder = TRUE,
                             use_batch_normalization = FALSE,
                             paired_sampling = TRUE,
                             masking_ratio = 0.5,
                             mask_generator_only_these_coalitions = NULL,
                             mask_generator_only_these_coalitions_probabilities = NULL,
                             sigma_mu = 1e4,
                             sigma_sigma = 1e-4,
                             save_data = FALSE,
                             transform_all_continuous_features = FALSE,
                             verbose = FALSE,
                             seed = NULL,
                             ...) {

  # Some checks. Not an exhaustive list.
  if (!is.numeric(num_different_vaeac_initiate)) {
    stop(sprintf("The 'num_different_vaeac_initiate' parameter must be of type numeric, and not of type %s.\n", paste(class(num_different_vaeac_initiate), collapse = ", ")))
  } else if (num_different_vaeac_initiate < 1) {
    warning(sprintf("The 'num_different_vaeac_initiate' parameter must be a positive integer, not %g. We set it to 1.\n", num_different_vaeac_initiate))
    num_different_vaeac_initiate = 1
  }

  if (epochs_initiation_phase >= epochs) {
    warning(sprintf("The 'epochs_initiation_phase' (%g) parameter must be strictly lower than 'epochs' (%g). We set epochs = %g.\n",
                    epochs_initiation_phase,
                    epochs,
                    epochs_initiation_phase + 1))
    epochs = epochs_initiation_phase + 1
  }


  # If no folder has been provided, we save the model in a temporary directory
  # which will be deleted when the R session is closed.
  if (is.null(folder_to_save_model)) {
    #folder_to_save_model = getwd()
    folder_to_save_model = tempdir()
    used_tempdir = TRUE
  } else {
    used_tempdir = FALSE
  }

  # If no model_description has been provided, then we use the current time.
  if (is.null(model_description)) {
    options(digits.secs = 3)
    model_description = gsub("\\.", "_", gsub(" ", "_", Sys.time()))
    options(digits.secs = 0)
  }

  # Preprocess the data. This function turns factor names into numerics 1,2,...,K,
  # as VAEAC only accepts numerics, and keep track of the maping of names.
  # And optionally log-transform all continuous features. Usual for strictly positive
  # data set like Burr and Abalone, such that VAEAC does not impute negative values.
  preprocessed_data = VAEAC_preprocess_data(as.data.table(training_data), transform_all_continuous_features)

  # Extract the training data where all the
  training_data = preprocessed_data$x_train

  # A torch tensor of dimension p containing the one hot sizes of the p features.
  # The sizes for the continuous features can either be '0' or '1'.
  one_hot_max_sizes = preprocessed_data$one_hot_max_sizes

  ##### Do some checks for invalid input parameter values
  # Do not check that integers are integers and that strings are strings.
  # Take for granted that the user reads the documentation.

  # Check for a valid file path
  if (!dir.exists(folder_to_save_model)) {
    stop(sprintf("Directory '%s' does not exist. Create it before calling 'train_VAEAC_model(...)' again, e.g., by using 'dir.create(...).\n", folder_to_save_model))
  }

  # Remove trailing slash on path name if there are any.
  if (endsWith(folder_to_save_model, "/")) folder_to_save_model = substr(folder_to_save_model, 1, nchar(folder_to_save_model)-1)

  # Check that we initiate at least one VAEAC model in the
  if (num_different_vaeac_initiate < 1) {
    stop(sprintf("User gave 'num_different_vaeac_initiate = %d'. But the number of VAEAC models to initiate must be equal or larger than 1.", num_different_vaeac_initiate))
  }

  # Check if cuda/GPU is available on the current system
  cuda_available = torch::cuda_is_available()

  # Give warning to user if asked to run on cuda, but cuda is not available.
  if (isFALSE(cuda_available) && isTRUE(use_cuda)) {
    use_cuda = FALSE
    warning("Cuda/GPU is not available. Uses CPU instead.", immediate. = TRUE)
  }

  # Check for coinciding number of features in data and the number of features specified in one_hot_max_sizes.
  if (ncol(training_data) != length(one_hot_max_sizes)) stop(sprintf("The number of columns/features in training_data must match the length of one_hot_max_sizes: %d != %s.\n", ncol(training_data), length(one_hot_max_sizes)))

  # Check if
  if (xor(!is.null(mask_generator_only_these_coalitions), !is.null(mask_generator_only_these_coalitions_probabilities))) {
    stop("User need to provided both 'mask_generator_only_these_coalitions' and 'mask_generator_only_these_coalitions_probabilities' for specified masking to function.")
  }

  ##### Figure out what kind of mask generator we are going to use.
  if (!is.null(mask_generator_only_these_coalitions) & !is.null(mask_generator_only_these_coalitions_probabilities)) {
    # Both are provided and we want to use Specified_masks_mask_generator

    # Check that the possible masks that are provided is given as a matrix
    if (!any(class(mask_generator_only_these_coalitions) == "matrix")) {
      stop(sprintf("The 'mask_generator_only_these_coalitions' must be of class 'matrix', not %s.\n", paste(class(mask_generator_only_these_coalitions), collapse = ", ")))
    }

    # Check that the number of masks and corresponding number of probabilities match.
    if (nrow(mask_generator_only_these_coalitions) != length(mask_generator_only_these_coalitions_probabilities)) {

      # print("Hei")
      # print(mask_generator_only_these_coalitions)
      # print("Hei2")
      # print(mask_generator_only_these_coalitions_probabilities)

      stop(sprintf("The number of coalitions ('%d') does not match with the number of provided probabilites ('%d').\n",
                   nrow(mask_generator_only_these_coalitions), length(mask_generator_only_these_coalitions_probabilities)))
    }

    # We are given possible coalitions and corresponding probabilities. Then we are using the Specified_masks_mask_generator.
    if (verbose) cat(sprintf("Use 'Specified_masks_mask_generator' mask generator with '%d' different possible coalitions.\n", nrow(mask_generator_only_these_coalitions)))
    mask_generator_name = "Specified_masks_mask_generator"

  } else {
    # We are NOT going to use 'Specified_masks_mask_generator'. Figure out if we are using
    # 'MCAR_mask_generator' or 'Specified_probability_mask_generator' and check for valid input.

    # Check that masking_ratio is numeric.
    if (all(class(masking_ratio) != "numeric")) stop(sprintf("class of 'masking_ratio' must be numeric, not %s.\n", class(masking_ratio)))

    # Masking ration is then either a scalar or array of scalar.
    if (length(masking_ratio) == 1) {
      # Only one masking ration, so we are going to use MCAR_mask_generator where each feature value
      # is going to be masked with this probability independently of if another features is masked.
      if (verbose) cat(sprintf("Use 'MCAR_mask_generator' with 'masking_ratio = %g'.\n", masking_ratio))
      mask_generator_name = "MCAR_mask_generator"
    } else {
      # Check that we have received a masking ratio for each feature
      if (length(masking_ratio) == ncol(training_data)) {
        # We have an array of masking ratios. Then we are using the Specified_probability_mask_generator.
        if (verbose) cat(sprintf("Use 'Specified_probability_mask_generator' mask generator with 'masking_ratios = {%s}'.\n", paste(masking_ratio, collapse = ", ")))
        mask_generator_name = "Specified_probability_mask_generator"
      } else {
        stop(sprintf("'Masking_ratio' contains masking ratios for '%d' features, but there are '%d' features in 'training_data'.\n", length(masking_ratio), ncol(training_data)))
      }
    }
  }

  #### Normalize training_data
  # Get the dimensions of the training_data
  n = nrow(training_data)
  p = ncol(training_data)

  # Convert X to tensor
  data_torch = torch_tensor(as.matrix(training_data))

  # Compute the mean and std for each continuous feature in the data
  # The categorical features will have mean zero and std 1.
  mean_and_sd = compute_normalization(data_torch, one_hot_max_sizes)
  norm_mean = mean_and_sd$norm_vector_mean
  norm_std  = mean_and_sd$norm_vector_std

  # Make sure that the standard deviation is not too low, in that case clip it.
  norm_std = norm_std$max(other = torch_tensor(1e-9))

  # normalize the data to have mean = 0 and std = 1.
  data = (data_torch - norm_mean) / norm_std

  #### Split Training & Validation Data
  # Splitting the input data into training and validation sets
  # Find the number of instances in the validation set
  val_size = ceiling(n * validation_ratio)

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

  # randomly sample indices for the validation set
  val_indices = sample(n, val_size, replace = FALSE)

  # Get the indices that are not in the validation set.
  train_indices = seq(n)[-val_indices]

  # Split the data into a training and validation set
  train_data = data[train_indices]
  val_data = data[val_indices]

  ##### Datasets and Dataloaders
  if (length(train_indices) <= batch_size) {
    if (length(train_indices) %% 2 != 0) {
      batch_size_new = (length(train_indices) + 1)/2
    } else {
      batch_size_new = length(train_indices) / 2
    }
    warning(sprintf("Provided batch_size (%d) is larger than the number of training observations (%d). Set batch_size = %d.\n",
                    batch_size, length(train_indices), batch_size_new), immediate. = TRUE)
    batch_size = batch_size_new
  }

  # Create the Data Set objects
  train_dataset = VAEAC_dataset(train_data, one_hot_max_sizes)
  val_dataset = VAEAC_dataset(val_data, one_hot_max_sizes)

  # Create the Data Loader object which can iterate over the data in the Data Set object
  # See more parameters here '?dataloader', but these are the most important.
  if (paired_sampling) {
    # Use paired sampling
    train_dataloader = dataloader(train_dataset,
                                  batch_size = batch_size,
                                  sampler = paired_sampler(train_dataset, shuffle = TRUE))
    val_dataloader = dataloader(val_dataset,
                                batch_size = batch_size,
                                sampler = paired_sampler(val_dataset, shuffle = FALSE))

  } else {
    # Usual approach
    train_dataloader = dataloader(train_dataset, batch_size = batch_size, shuffle = TRUE)
    val_dataloader = dataloader(val_dataset, batch_size = batch_size, shuffle = FALSE)
  }


  ##### List that stores needed information for save and load the model
  # List to values saved to disk together with the VAEAC models below.
  state_list = list("norm_mean" = norm_mean,
                    "norm_std" = norm_std,
                    "model_description" = model_description,
                    "folder_to_save_model" = folder_to_save_model,
                    "used_tempdir" = used_tempdir,
                    "n" = n,
                    "p" = p,
                    "one_hot_max_sizes" = one_hot_max_sizes,
                    "epochs" = epochs,
                    "running_avg_num_values" = running_avg_num_values,
                    "paired_sampling" = paired_sampling,
                    "mask_generator_name" = mask_generator_name,
                    "masking_ratio" = masking_ratio,
                    "mask_generator_only_these_coalitions" = mask_generator_only_these_coalitions,
                    "mask_generator_only_these_coalitions_probabilities" = mask_generator_only_these_coalitions_probabilities,
                    "validation_ratio" = validation_ratio,
                    "validation_iwae_num_samples" = validation_iwae_num_samples,
                    "num_different_vaeac_initiate" = num_different_vaeac_initiate,
                    "epochs_initiation_phase" = epochs_initiation_phase,
                    "width" = width,
                    "depth" = depth,
                    "latent_dim" = latent_dim,
                    "activation_function" = activation_function,
                    "activation_function_string" = activation_function$classname,
                    "lr" = lr,
                    "batch_size" = batch_size,
                    "use_skip_connections" = use_skip_connections,
                    "use_skip_connections_between_masked_encoder_and_decoder" = use_skip_connections_between_masked_encoder_and_decoder,
                    "use_batch_normalization" = use_batch_normalization,
                    "use_cuda" = use_cuda,
                    "train_indices" = train_indices,
                    "val_indices" = val_indices,
                    "save_VAEAC_every_nth_epoch" = save_VAEAC_every_nth_epoch,
                    "sigma_mu" = sigma_mu,
                    "sigma_sigma" = sigma_sigma,
                    "feature_list" = preprocessed_data$feature_list,
                    "col_cat_names" = preprocessed_data$col_cat_names,
                    "col_cont_names" = preprocessed_data$col_cont_names,
                    "col_cat" = preprocessed_data$col_cat,
                    "col_cont" = preprocessed_data$col_cont,
                    "cat_in_dataset" = preprocessed_data$cat_in_dataset,
                    "map_new_to_original_names" = preprocessed_data$map_new_to_original_names,
                    "map_original_to_new_names" = preprocessed_data$map_original_to_new_names,
                    "transform_all_continuous_features" = preprocessed_data$transform_all_continuous_features,
                    "save_data" = save_data,
                    "verbose" = verbose,
                    "verbose" = verbose,
                    "verbose" = verbose,
                    "seed" = seed)



  # If we are also to save the data to state_list.
  if (save_data) {
    state_list = c(state_list, list("training_data" = training_data,
                                    "normalized_data" = data))

    # Just a small warning regarding large disk usage
    if (!is.null(save_VAEAC_every_nth_epoch)) {
      warning(sprintf("Both having 'save_data = TRUE' and saving the VAEAC model every '%d' epoch might require a lot of disk storage if data is large.\n",
                      save_VAEAC_every_nth_epoch), immediate. = TRUE)
    }
  }

  # Check if we are to save VAEAC model every n'th epoch.
  if (!is.null(save_VAEAC_every_nth_epoch)) {
    # List of file names for VAEAC models after every n'th epoch (save_VAEAC_every_nth_epoch).
    filename_nth_list = list()

    # Check that save_VAEAC_every_nth_epoch is positive.
    if (save_VAEAC_every_nth_epoch <= 0) {
      stop(sprintf("The value 'save_VAEAC_every_nth_epoch' must be strictly positive, not '%d'.\n", save_VAEAC_every_nth_epoch))
    }

    # Ensure a valid value for save_VAEAC_every_nth_epoch.
    if (save_VAEAC_every_nth_epoch > epochs){
      stop(sprintf("Number of 'epochs' is less than 'save_VAEAC_every_nth_epoch': %d < %d.\n",
                   epochs, save_VAEAC_every_nth_epoch))
    }
  }

  ##### Initializing VAEAC models
  # Initialize several VAEAC models and keep the one with the best training variational lower bound
  # after a given number of epochs. Keep the version with highest vlb, denoted by "best_vlb"..
  best_vlb = -Inf

  # Iterate over the initializations.
  initialization = 1
  for (initialization in seq(num_different_vaeac_initiate)) {

    # OLD
    # # Get the neural networks used to create the VAEAC model
    # networks = get_imputation_networks(
    #   one_hot_max_sizes = one_hot_max_sizes,
    #   sample_most_probable = FALSE,
    #   width = width,
    #   depth = depth,
    #   latent_dim = latent_dim,
    #   lr = lr,
    #   batch_size = batch_size,
    #   mask_generator = mask_generator,
    #   masking_ratio = masking_ratio,
    #   mask_generator_only_these_coalitions = mask_generator_only_these_coalitions,
    #   mask_generator_only_these_coalitions_probabilities = mask_generator_only_these_coalitions_probabilities)
    #
    # # Create the VAEAC model
    # model = VAEAC(reconstruction_log_prob = networks$reconstruction_log_prob,
    #               full_encoder_network = networks$full_encoder_network,
    #               masked_encoder_network = networks$masked_encoder_network,
    #               decoder_network = networks$decoder_network)

    # Create the VAEAC model
    model = VAEAC(one_hot_max_sizes = one_hot_max_sizes,
                  width = width,
                  depth = depth,
                  latent_dim = latent_dim,
                  activation_function = activation_function,
                  use_skip_connections = use_skip_connections,
                  use_skip_connections_between_masked_encoder_and_decoder = use_skip_connections_between_masked_encoder_and_decoder,
                  use_batch_normalization = use_batch_normalization,
                  paired_sampling = paired_sampling,
                  mask_generator_name = mask_generator_name,
                  masking_ratio = masking_ratio,
                  mask_generator_only_these_coalitions = mask_generator_only_these_coalitions,
                  mask_generator_only_these_coalitions_probabilities = mask_generator_only_these_coalitions_probabilities,
                  sigma_mu = sigma_mu,
                  sigma_sigma = sigma_sigma)

    # Print the number of trainable parameters to the user
    if (initialization == 1 & verbose) cat(sprintf("The number of trainable parameters in the VAEAC model is '%d'.\n", model$num_trainable_params[1,1]))

    # Small printout to the user.
    if (verbose) cat(sprintf("Initializing VAEAC number %d...\n", initialization))

    # Extract the variational lower bound scale factor and mask generator from the VAEAC model object.
    vlb_scale_factor = model$vlb_scale_factor
    mask_generator = model$mask_generator

    # Create the ADAM optimizer
    optimizer = optim_adam(params = model$parameters,
                           lr = lr,
                           betas = c(0.9, 0.999),
                           eps = 1e-08,
                           weight_decay = 0,
                           amsgrad = FALSE)

    # An array to store the regular and running validation IWAE errors
    validation_iwae = c()
    validation_iwae_running_avg = c()

    # An array of running variational lower bounds on the train set
    train_vlb = c()

    # "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta || Epoch: :epoch || VLB: :vlb || IWAE: :iwae || IWAE Runnning: :iwae_running]"
    # Create a progress bar
    pb = progress_bar$new(format = "(:spin) [:bar] :percent [VAEAC: #:initialization | time: :elapsedfull | ETR: :eta | Epoch: :epoch | VLB: :vlb | IWAE: :iwae | IWAE_R: :runningiwae]",
                          total = epochs_initiation_phase,
                          complete = "=",        # Completion bar character
                          incomplete = "-",      # Incomplete bar character
                          current = ">",         # Current bar character
                          clear = !verbose, # If TRUE, clears the bar when finish
                          width = 125)           # Width of the progress bar

    epoch = 1
    # Start the training loop
    for (epoch in base::seq(epochs_initiation_phase)) {
      ## First we do one epoch of training before
      # Set average variational lower bound to 0 for this epoch
      avg_vlb = 0

      # Array to keep track of the training errors (i.e., VLB)
      training_error_batch = c()

      # Index to keep track of which batch we are working on. Only used for progress bar.
      batch_index = 1

      batch = train_dataloader$.iter()$.next()
      # Iterate over the training data
      coro::loop(for (batch in train_dataloader) {

        # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
        if (batch$shape[1] < batch_size) {
          batch = extend_batch(batch = batch,
                               dataloader = train_dataloader,
                               batch_size = batch_size)
        }

        # Generate mask and do an optimizer step over the mask and the batch
        mask = mask_generator(batch)

        # Set all previous gradients to zero.
        optimizer$zero_grad()

        # Compute the variational lower bound for the batch given the mask
        vlb = model$batch_vlb(batch, mask)$mean()

        # Backpropagation: minimize the negative vlb.
        vlb_loss = (-vlb / vlb_scale_factor)
        vlb_loss$backward()

        # Update the model parameters by using ADAM.
        optimizer$step()

        # Update running variational lower bound average
        avg_vlb = avg_vlb + (vlb$to(dtype = torch_float())$clone()$detach() - avg_vlb) / batch_index

        # Update the batch index.
        batch_index = batch_index + 1
      })

      ## Done one new epoch of training. Time to evaluate the model on the validation data.
      # Compute the validation iwae
      val_iwae = get_validation_iwae(val_dataloader,
                                     mask_generator,
                                     batch_size,
                                     model,
                                     validation_iwae_num_samples,
                                     verbose)

      # Add the current validation_iwae and train_vlb to the lists.
      validation_iwae = torch_cat(c(validation_iwae, val_iwae), -1)
      train_vlb = torch_cat(c(train_vlb, avg_vlb), -1)

      # Compute the running validation IWAE
      val_iwae_running = validation_iwae[(-min(length(validation_iwae), running_avg_num_values) + length(validation_iwae) + 1) : (-1 + length(validation_iwae) + 1), drop = FALSE]$mean()$view(1)
      validation_iwae_running_avg = torch_cat(c(validation_iwae_running_avg, val_iwae_running), -1)

      # Updates the current state of the progress bar
      pb$tick(tokens = list(initialization = initialization,
                            epoch = epoch,
                            vlb = round(avg_vlb$item(), 3),
                            iwae = round(val_iwae$item(), 3),
                            runningiwae = round(validation_iwae_running_avg[-1]$item(), 3)))
    } # Done with initial training of a single VAEAC model

    # Save the current VAEAC model, if it is the best initialized version so far.
    if ((best_vlb <= avg_vlb)$item()) {
      best_vlb = avg_vlb
      best_iteration = initialization
      best_model = model
      best_validation_iwae = validation_iwae
      best_validation_iwae_running_avg = validation_iwae_running_avg
      best_train_vlb = train_vlb
      best_optimizer = optimizer
      best_batch_size = batch_size
      best_mask_generator = mask_generator
      best_vlb_scale_factor = vlb_scale_factor
    }
  } # Done with initial training of all VAEAC models

  # Small printout to the user stating which initiated VAEAC model was the best.
  if (verbose) {
    cat(sprintf("VAEAC initiations: %d. Best: #%d. VLB: %.3f after %d epochs.\n",
                num_different_vaeac_initiate, best_iteration, best_train_vlb[-1], epochs_initiation_phase))
  }

  # Load the best initialized VAEAC model and continue training.
  # networks = best_networks
  model = best_model
  validation_iwae = best_validation_iwae
  validation_iwae_running_avg = best_validation_iwae_running_avg
  train_vlb = best_train_vlb
  optimizer = best_optimizer
  batch_size = best_batch_size
  mask_generator = best_mask_generator
  vlb_scale_factor = best_vlb_scale_factor

  # Include the number of trainable parameters in the state list.
  state_list = c(state_list, list("num_trainable_parameters" = model$num_trainable_params))

  # Send the model to the GPU, if we have access to it.
  if (use_cuda) model = model.cuda()

  # Create a progress bar. TODO: Maybe include width, depth, latent_dim, lr, if doing hyperparemeter tuning.
  pb = progress_bar$new(format = "(:spin) [:bar] :percent [time: :elapsedfull | ETR: :eta | Epoch: :epoch | VLB: :vlb | IWAE: :iwae | IWAE_R: :runningiwae]",
                        total = (epochs - epochs_initiation_phase), # * train_dataloader$.length(),
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = !verbose, # If TRUE, clears the bar when finish
                        width = 125)      # Width of the progress bar

  # Continue training the best VAEAC model
  for (epoch in seq(epochs_initiation_phase+1, epochs)) {
    # Set iterator to be the data loader which loads the training data.
    iterator = dataloader

    # Set average variational lower bound to 0 for this epoch
    avg_vlb = 0

    # index to keep track of which batch we are working on
    batch_index = 1

    # Iterate over the training data
    coro::loop(for (batch in train_dataloader) {

      # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
      if (batch$shape[1] < batch_size) {
        batch = extend_batch(batch = batch,
                             dataloader = train_dataloader,
                             batch_size = batch_size)
      }

      # Generate mask and do an optimizer step over the mask and the batch
      mask = mask_generator(batch)

      # Send the batch and mask to Nvida GPU if we have. Would be faster.
      if (use_cuda) {
        batch = batch$cuda()
        mask = mask$cuda()
      }

      # Set all previous gradients to zero.
      optimizer$zero_grad()

      # Compute the variational lower bound for the batch given the mask
      vlb = model$batch_vlb(batch, mask)$mean()

      # Backpropagation: minimize the negative vlb.
      vlb_loss = (-vlb / vlb_scale_factor)
      vlb_loss$backward()

      # Update the model parameters by using the optimizer.
      optimizer$step()

      # Update running variational lower bound average
      # a + (new - a)/(i+1) = {(i+1)a + new - a}/(i+1) = { a(i) + new}/(i+1) = a *i/(i+1) + new/(i+1)
      # recursive average formula/update.
      avg_vlb = avg_vlb + (vlb$to(dtype = torch_float())$clone()$detach() - avg_vlb) / batch_index

      # Update the batch index.
      batch_index = batch_index + 1
    })

    # Done with one new epoch of training. Time to use the model on the validation data.
    # Time to evaluate the model on the validation data. Compute the validation IWAE.
    val_iwae = get_validation_iwae(val_dataloader,
                                   mask_generator,
                                   batch_size,
                                   model,
                                   validation_iwae_num_samples,
                                   verbose)

    # Compute the running validation IWAE.
    val_iwae_running = validation_iwae[(-min(length(validation_iwae), running_avg_num_values) + length(validation_iwae) + 1) : (-1 + length(validation_iwae) + 1), drop = FALSE]$mean()$view(1)

    # Add the current validation_iwae and train_vlb to the lists.
    validation_iwae = torch_cat(c(validation_iwae, val_iwae), -1)
    train_vlb = torch_cat(c(train_vlb, avg_vlb), -1)
    validation_iwae_running_avg = torch_cat(c(validation_iwae_running_avg, val_iwae_running), -1)

    # Save if current VAEAC model has the lowest validation IWAE error
    if ((max(validation_iwae) <= val_iwae)$item()) {
      best_state = c(list('epoch' = epoch,
                          # 'model' = model,
                          # 'optimizer' = optimizer,
                          'model_state_dict' = model$state_dict(),
                          'optimizer_state_dict' = optimizer$state_dict(),
                          'validation_iwae' = validation_iwae,
                          'validation_iwae_running_avg' = validation_iwae_running_avg,
                          'running_avg_num_values' = running_avg_num_values,
                          'train_vlb' = train_vlb),
                     state_list)

      # Create the file name
      filename_best = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), 'p', p, 'n', n, 'depth',
                            depth, 'width', width, 'latent', latent_dim, 'lr', lr, 'best.pt', sep = "_")

      # Combine the file name with the folder path to form the final save file name.
      filename_best = file.path(folder_to_save_model, filename_best)
      class(best_state) = c(class(best_state), "R_VAEAC", "vaeac")
      torch_save(best_state, filename_best)
    }

    # Save if current VAEAC model has the lowest validation IWAE error
    if ((max(validation_iwae_running_avg) <= val_iwae_running)$item()) {
      best_state_running = c(list('epoch' = epoch,
                                  # 'model' = model,
                                  # 'optimizer' = optimizer,
                                  'model_state_dict' = model$state_dict(),
                                  'optimizer_state_dict' = optimizer$state_dict(),
                                  'validation_iwae' = validation_iwae,
                                  'validation_iwae_running_avg' = validation_iwae_running_avg,
                                  'running_avg_num_values' = running_avg_num_values,
                                  'train_vlb' = train_vlb),
                             state_list)

      # Create the file name
      filename_best_running = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), 'p', p, 'n', n, 'depth',
                                    depth, 'width', width, 'latent', latent_dim, 'lr', lr, 'best_running.pt', sep = "_")

      # Combine the file name with the folder path to form the final save file name.
      filename_best_running = file.path(folder_to_save_model, filename_best_running)
      class(best_state_running) = c(class(best_state_running), "R_VAEAC", "vaeac")
      torch_save(best_state_running, filename_best_running)
    }

    # If we are to save and we are in an n'th epoch, then we save the model.
    if (!is.null(save_VAEAC_every_nth_epoch)){
      if (epoch %% save_VAEAC_every_nth_epoch == 0) {
        nth_state = c(list('epoch' = epoch,
                           # 'model' = model,
                           # 'optimizer' = optimizer,
                           'model_state_dict' = model$state_dict(),
                           'optimizer_state_dict' = optimizer$state_dict(),
                           'validation_iwae' = validation_iwae,
                           'validation_iwae_running_avg' = validation_iwae_running_avg,
                           'running_avg_num_values' = running_avg_num_values,
                           'train_vlb' = train_vlb),
                      state_list)

        # Create the file name
        filename_nth = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), '_p_', p, '_n_', n, '_depth_',
                             depth, '_width_', width, '_latent_', latent_dim, '_lr_', lr, '_epoch_', epoch, '.pt', sep = "")

        # Combine the file name with the folder path to form the final save file name.
        filename_nth = file.path(folder_to_save_model, filename_nth)
        class(nth_state) = c(class(nth_state), "R_VAEAC", "vaeac")
        torch_save(nth_state, filename_nth)

        # Add file name to list over file names.
        tmp_list = list(filename_nth)
        names(tmp_list) = paste("filename_epoch_", epoch, sep = "")
        filename_nth_list = append(filename_nth_list, tmp_list)
      }
    }

    # Updates the current state
    pb$tick(tokens = list(epoch = epoch,
                          vlb = round(avg_vlb$item(), 3),
                          iwae = round(val_iwae$item(), 3),
                          runningiwae = round(validation_iwae_running_avg[-1]$item(), 3)))
  } # Done with training

  # Also save the model at the last epoch
  last_state = c(list('epoch' = epoch,
                      # 'model' = model,
                      # 'optimizer' = optimizer,
                      'model_state_dict' = model$state_dict(),
                      'optimizer_state_dict' = optimizer$state_dict(),
                      'validation_iwae' = validation_iwae,
                      'validation_iwae_running_avg' = validation_iwae_running_avg,
                      'running_avg_num_values' = running_avg_num_values,
                      'train_vlb' = train_vlb),
                 state_list)

  # Create the file name
  filename_last = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), 'p', p, 'n', n, 'depth',
                        depth, 'width', width, 'latent', latent_dim, 'lr', lr, 'last.pt', sep = "_")

  # Combine the file name with the folder path to form the final save file name.
  filename_last = file.path(folder_to_save_model, filename_last)
  class(last_state) = c(class(last_state), "R_VAEAC", "vaeac")
  torch_save(last_state, filename_last)

  # Printout to the user
  if (verbose) {
    cat(sprintf("
Best epoch:             %d. \tVLB = %.4f. \tIWAE = %.4f \tIWAE_running = %.4f.
Best running avg epoch: %d. \tVLB = %.4f. \tIWAE = %.4f \tIWAE_running = %.4f.
Last epoch:             %d. \tVLB = %.4f. \tIWAE = %.4f \tIWAE_running = %.4f.\n",
                best_state$epoch,
                best_state$train_vlb[-1],
                best_state$validation_iwae[-1],
                best_state$validation_iwae_running_avg[-1],
                best_state_running$epoch,
                best_state_running$train_vlb[-1],
                best_state_running$validation_iwae[-1],
                best_state_running$validation_iwae_running_avg[-1],
                last_state$epoch,
                last_state$train_vlb[-1],
                last_state$validation_iwae[-1],
                last_state$validation_iwae_running_avg[-1]))
  }

  # Create a return list
  return_list = list("filename_best" = filename_best,
                     "filename_best_running" = filename_best_running,
                     "filename_last" = filename_last,
                     "train_vlb" = as.array(train_vlb),
                     "validation_iwae" = as.array(validation_iwae),
                     "validation_iwae_running_avg" = as.array(validation_iwae_running_avg),
                     "parameters" = state_list)

  # If we are to add the 'filename_nth_list' list to the return list.
  if (!is.null(save_VAEAC_every_nth_epoch)) {
    return_list = append(return_list, filename_nth_list, 3)
  }

  # Update the class of the returned object
  attr(return_list, "class") = c("R_VAEAC", "vaeac", "list")

  # Return the paths where the models are saved and the training/validation errors.
  return(return_list)
}


#' Continue to Train the VAEAC model
#'
#' @description Function that loads a previously trained VAEAC model and continue the training, either
#' on new data or on the same dataset as it was trained on before. If we are given a new dataset, then
#' we assume that new dataset has the same distribution and one_hot_max_sizes as the original dataset.
#'
#' @param VAEAC_model The output revived from the 'train_VAEAC_model' function.
#' @param epochs_new Integer. The number of extra epochs to conduct.
#' @param lr_new Numeric. If we are to overwrite the old learning rate in the adam optimizer.
#' @param training_data Matrix/data.frame containing new training data. If not present, then we try to load training data from the VAEAC_model.
#' @param save_data Boolean. If we are to save the training data.
#' @param verbose Boolean. If we are to print out information to the user.
#'
#' @return A list containing the training/validation errors and paths to where the VAEAC models are saved on the disk.
#' @export
continue_train_VAEAC_model_shapr = function(explanation,
                                            epochs_new,
                                            lr_new = NULL,
                                            training_data = NULL,
                                            save_data = FALSE,
                                            verbose = TRUE,
                                            ...) {
  # Keep track of how much time we use training
  new_training_time = system.time({

    # Extract the VAEAC list
    VAEAC_model = explanation$internal$parameters$VAEAC

    # Load the VAEAC model from provided disk location.
    checkpoint = torch_load(VAEAC_model$models$last)

    # Check that we have access to training data
    if (is.null(checkpoint$normalized_data) & is.null(training_data)) {
      stop(sprintf("The save file did not include data (set 'save_data' = TRUE in 'train_VAEAC_model) and data was not provided to this function."))
    }

    # We have two training datasets
    if (!is.null(checkpoint$training_data) & !is.null(data)) {
      warning(sprintf("The save file includes data and data was not provided to this function. We only use the latter."))
    }

    # If training_data is not provided to this function, then we load the training_data from the save file.
    if (is.null(training_data)) {
      training_data = checkpoint$training_data
    }

    # Preprocess the data. This function turns factor names into numerics 1,2,...,K,
    # as VAEAC only accepts numerics, and keep track of the maping of names.
    # And optionally log-transform all continuous features. Usual for strictly positive
    # data set like Burr and Abalone, such that VAEAC does not impute negative values.
    preprocessed_data = VAEAC_preprocess_data(as.data.table(training_data), checkpoint$transform_all_continuous_features)

    # Extract the training data where all the
    training_data = preprocessed_data$x_train

    # Extract relevant information from the checkpoint
    batch_size = checkpoint$batch_size
    one_hot_max_sizes = checkpoint$one_hot_max_sizes
    save_VAEAC_every_nth_epoch = checkpoint$save_VAEAC_every_nth_epoch
    validation_iwae_num_samples = checkpoint$validation_iwae_num_samples
    running_avg_num_values = checkpoint$running_avg_num_values
    use_cuda = checkpoint$use_cuda
    model_description = checkpoint$model_description
    paired_sampling = checkpoint$paired_sampling
    depth = checkpoint$depth
    width = checkpoint$width
    latent_dim = checkpoint$latent_dim
    lr = checkpoint$lr
    folder_to_save_model = checkpoint$folder_to_save_model

    # Check if cuda/GPU is available on the current system
    cuda_available = torch::cuda_is_available()

    # Give warning to user if asked to run on cuda, but cuda is not available.
    if (isFALSE(cuda_available) && isTRUE(use_cuda)) {
      use_cuda = FALSE
      warning("Cuda/GPU is not available. Uses CPU instead.", immediate. = TRUE)
    }

    #### Normalize training_data
    # Get the dimensions of the training_data
    n = nrow(training_data)
    p = ncol(training_data)

    # Test for right number of features.
    if (p != checkpoint$p) stop(sprintf("The dimensions of current training data do not match the original dimension: %d != %d", p, checkpoint$p))

    # Convert X to tensor
    data_torch = torch_tensor(as.matrix(training_data))

    # Compute the mean and std for each continuous feature in the data
    # The categorical features will have mean zero and std 1.
    mean_and_sd = compute_normalization(data_torch, one_hot_max_sizes)
    norm_mean = mean_and_sd$norm_vector_mean
    norm_std  = mean_and_sd$norm_vector_std

    # Make sure that the standard deviation is not too low, in that case clip it.
    norm_std = norm_std$max(other = torch_tensor(1e-9))

    # normalize the data to have mean = 0 and std = 1.
    data = (data_torch - norm_mean) / norm_std

    #### Split Training & Validation Data
    if (!is.null(checkpoint$training_data) | n == checkpoint$n) {
      # We are using the data from the saved object, or the new
      # data has the same number of training observations.

      # Can then just extract the validation and training indices
      val_indices = checkpoint$val_indices
      train_indices = checkpoint$train_indices

    } else {
      # We have new training data with a different number of training observations.

      # Splitting the input data into training and validation sets
      # Find the number of instances in the validation set
      val_size = ceiling(n * checkpoint$validation_ratio)

      # randomly sample indices for the validation set
      val_indices = sample(n, val_size, replace = FALSE)

      # Get the indices that are not in the validation set.
      train_indices = seq(n)[-val_indices]
    }

    # Split the data into a training and validation set
    train_data = data[train_indices]
    val_data = data[val_indices]

    ##### Datasets and Dataloaders
    if (length(train_indices) <= batch_size) {
      warning(sprintf("Provided batch_size (%d) is larger than the number of training observations (%d). Set batch_size = %d.\n",
                      batch_size, length(train_indices), length(train_indices)), immediate. = TRUE)
      batch_size = length(train_indices)
    }

    # Create the Data Set objects
    train_dataset = VAEAC_dataset(train_data, one_hot_max_sizes)
    val_dataset = VAEAC_dataset(val_data, one_hot_max_sizes)

    # Create the Data Loader object which can iterate over the data in the Data Set object
    # See more parameters here '?dataloader', but these are the most important.
    if (paired_sampling) {
      # Use paired sampling
      train_dataloader = dataloader(train_dataset,
                                    batch_size = batch_size,
                                    sampler = paired_sampler(train_dataset, shuffle = TRUE))
      val_dataloader = dataloader(val_dataset,
                                  batch_size = batch_size,
                                  sampler = paired_sampler(val_dataset, shuffle = FALSE))

    } else {
      # Usual approach
      train_dataloader = dataloader(train_dataset, batch_size = batch_size, shuffle = TRUE)
      val_dataloader = dataloader(val_dataset, batch_size = batch_size, shuffle = FALSE)
    }

    ##### List that stores needed information for save and load the model
    # List to values saved to disk together with the VAEAC models below.
    state_list_new = list("norm_mean" = norm_mean,
                          "norm_std" = norm_std,
                          "n" = n,
                          "epochs_new" = epochs_new,
                          "train_indices" = train_indices,
                          "val_indices" = val_indices,
                          "lr_new" = lr_new)

    # If we are also to save the data to state_list.
    if (save_data) {
      state_list_new  = c(state_list_new, list("training_data" = training_data,
                                               "normalized_data" = data))

      # Just a small warning regarding large disk usage
      if (!is.null(save_VAEAC_every_nth_epoch)) {
        warning(sprintf("Both having 'save_data = TRUE' and saving the VAEAC model every '%d' epoch might require a lot of disk storage if data is large.\n",
                        save_VAEAC_every_nth_epoch), immediate. = TRUE)
      }
    }

    # Add the new state list as a list to the checkpoint
    num_times_continued_trained = sum(grepl("state_list_new", names(checkpoint)))
    state_list_new_name = paste("state_list_new", num_times_continued_trained + 1, sep = "_")
    state_list = checkpoint
    state_list[[state_list_new_name]] = state_list_new

    # Check if we are to save VAEAC model every n'th epoch.
    if (!is.null(save_VAEAC_every_nth_epoch)) {
      # List of file names for VAEAC models after every n'th epoch (save_VAEAC_every_nth_epoch).
      filename_nth_list = list()
    }

    # If batch size has not been provided, then we use the same as during training.
    if (is.null(batch_size)) {
      batch_size = checkpoint$batch_size
    }

    # Create a VAEAC model
    model = VAEAC(one_hot_max_sizes = checkpoint$one_hot_max_sizes,
                  width = checkpoint$width,
                  depth = checkpoint$depth,
                  latent_dim = checkpoint$latent_dim,
                  activation_function = checkpoint$activation_function,
                  use_skip_connections = checkpoint$use_skip_connections,
                  use_skip_connections_between_masked_encoder_and_decoder = checkpoint$use_skip_connections_between_masked_encoder_and_decoder,
                  use_batch_normalization = checkpoint$use_batch_normalization,
                  paired_sampling = checkpoint$paired_sampling,
                  mask_generator_name = checkpoint$mask_generator_name,
                  masking_ratio = checkpoint$masking_ratio,
                  mask_generator_only_these_coalitions = checkpoint$mask_generator_only_these_coalitions,
                  mask_generator_only_these_coalitions_probabilities = checkpoint$mask_generator_only_these_coalitions_probabilities,
                  sigma_mu = checkpoint$sigma_mu,
                  sigma_sigma = checkpoint$sigma_sigma)

    # Update the model's state dictionary to the one provided by the user.
    model$load_state_dict(checkpoint$model_state_dict)

    # Extract the variational lower bound scale factor and mask generator from the VAEAC model object.
    vlb_scale_factor = model$vlb_scale_factor
    mask_generator = model$mask_generator

    # Create the ADAM optimizer
    optimizer = optim_adam(params = model$parameters,
                           lr = checkpoint$lr,
                           betas = c(0.9, 0.999),
                           eps = 1e-08,
                           weight_decay = 0,
                           amsgrad = FALSE)

    # Insert the state dictionary
    optimizer$load_state_dict(checkpoint$optimizer_state_dict)

    # Override the earlier learning rate with the lr_new if provided.
    if (!is.null(lr_new)) {
      optimizer$param_groups[[1]]$lr = lr_new
      # for (param_group in optimizer$param_groups) {
      #   param_group['lr'] = lr_new
      # }
      lr = lr_new
    }

    # An array to store the regular and running validation IWAE errors
    validation_iwae = checkpoint$validation_iwae
    validation_iwae_running_avg = checkpoint$validation_iwae_running_avg

    # An array of running variational lower bounds on the train set
    train_vlb = checkpoint$train_vlb

    # Compute the total epochs
    epochs_old = checkpoint$epochs
    epochs_total = epochs_old + epochs_new
    state_list$epochs = epochs_total

    # Load the best states from the vaeac model
    filename_best = VAEAC_model$models$best
    best_state = torch_load(filename_best)
    filename_best_running = VAEAC_model$models$best_running
    best_state_running = torch_load(filename_best_running )

    # Create a progress bar.
    pb = progress_bar$new(format = "(:spin) [:bar] :percent [time: :elapsedfull | ETR: :eta | Epoch: :epoch | VLB: :vlb | IWAE: :iwae | IWAE_R: :runningiwae]",
                          total = epochs_new, # * train_dataloader$.length(),
                          complete = "=",     # Completion bar character
                          incomplete = "-",   # Incomplete bar character
                          current = ">",      # Current bar character
                          clear = !verbose,   # If TRUE, clears the bar when finish
                          width = 125)        # Width of the progress bar

    # Continue training the best VAEAC model
    for (epoch in seq(epochs_old+1, epochs_total)) {
      # Set iterator to be the data loader which loads the training data.
      iterator = dataloader

      # Set average variational lower bound to 0 for this epoch
      avg_vlb = 0

      # index to keep track of which batch we are working on
      batch_index = 1

      # Iterate over the training data
      coro::loop(for (batch in train_dataloader) {

        # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
        if (batch$shape[1] < batch_size) {
          batch = extend_batch(batch = batch,
                               dataloader = train_dataloader,
                               batch_size = batch_size)
        }

        # Generate mask and do an optimizer step over the mask and the batch
        mask = mask_generator(batch)

        # Send the batch and mask to Nvida GPU if we have. Would be faster.
        if (use_cuda) {
          batch = batch$cuda()
          mask = mask$cuda()
        }

        # Set all previous gradients to zero.
        optimizer$zero_grad()

        # Compute the variational lower bound for the batch given the mask
        vlb = model$batch_vlb(batch, mask)$mean()

        # Backpropagation: minimize the negative vlb.
        vlb_loss = (-vlb / vlb_scale_factor)
        vlb_loss$backward()

        # Update the model parameters by using the optimizer.
        optimizer$step()

        # Update running variational lower bound average
        # a + (new - a)/(i+1) = {(i+1)a + new - a}/(i+1) = { a(i) + new}/(i+1) = a *i/(i+1) + new/(i+1)
        # recursive average formula/update.
        avg_vlb = avg_vlb + (vlb$to(dtype = torch_float())$clone()$detach() - avg_vlb) / batch_index

        # Update the batch index.
        batch_index = batch_index + 1
      })

      # Done with one new epoch of training. Time to use the model on the validation data.
      # Time to evaluate the model on the validation data. Compute the validation IWAE.
      val_iwae = get_validation_iwae(val_dataloader,
                                     mask_generator,
                                     batch_size,
                                     model,
                                     validation_iwae_num_samples,
                                     verbose)

      # Compute the running validation IWAE.
      val_iwae_running = validation_iwae[(-min(length(validation_iwae), running_avg_num_values) + length(validation_iwae) + 1) : (-1 + length(validation_iwae) + 1), drop = FALSE]$mean()$view(1)

      # Add the current validation_iwae and train_vlb to the lists.
      validation_iwae = torch_cat(c(validation_iwae, val_iwae), -1)
      train_vlb = torch_cat(c(train_vlb, avg_vlb), -1)
      validation_iwae_running_avg = torch_cat(c(validation_iwae_running_avg, val_iwae_running), -1)

      # Save if current VAEAC model has the lowest validation IWAE error
      if ((max(validation_iwae) <= val_iwae)$item()) {
        best_state = c(list('epoch' = epoch,
                            # 'model' = model,
                            # 'optimizer' = optimizer,
                            'model_state_dict' = model$state_dict(),
                            'optimizer_state_dict' = optimizer$state_dict(),
                            'validation_iwae' = validation_iwae,
                            'validation_iwae_running_avg' = validation_iwae_running_avg,
                            'running_avg_num_values' = running_avg_num_values,
                            'train_vlb' = train_vlb),
                       state_list)

        # Create the file name
        filename_best = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), 'p', p, 'n', n, 'depth',
                              depth, 'width', width, 'latent', latent_dim, 'lr', lr, 'best.pt', sep = "_")

        # Combine the file name with the folder path to form the final save file name.
        filename_best = file.path(folder_to_save_model, filename_best)
        class(best_state) = c(class(best_state), "R_VAEAC", "vaeac")
        torch_save(best_state, filename_best)
      }

      # Save if current VAEAC model has the lowest validation IWAE error
      if ((max(validation_iwae_running_avg) <= val_iwae_running)$item()) {
        best_state_running = c(list('epoch' = epoch,
                                    # 'model' = model,
                                    # 'optimizer' = optimizer,
                                    'model_state_dict' = model$state_dict(),
                                    'optimizer_state_dict' = optimizer$state_dict(),
                                    'validation_iwae' = validation_iwae,
                                    'validation_iwae_running_avg' = validation_iwae_running_avg,
                                    'running_avg_num_values' = running_avg_num_values,
                                    'train_vlb' = train_vlb),
                               state_list)

        # Create the file name
        filename_best_running = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), 'p', p, 'n', n, 'depth',
                                      depth, 'width', width, 'latent', latent_dim, 'lr', lr, 'best_running.pt', sep = "_")

        # Combine the file name with the folder path to form the final save file name.
        filename_best_running = file.path(folder_to_save_model, filename_best_running)
        class(best_state_running) = c(class(best_state_running), "R_VAEAC", "vaeac")
        torch_save(best_state_running, filename_best_running)
      }

      # If we are to save and we are in an n'th epoch, then we save the model.
      if (!is.null(save_VAEAC_every_nth_epoch)) {
        if (epoch %% save_VAEAC_every_nth_epoch == 0) {
          nth_state = c(list('epoch' = epoch,
                             # 'model' = model,
                             # 'optimizer' = optimizer,
                             'model_state_dict' = model$state_dict(),
                             'optimizer_state_dict' = optimizer$state_dict(),
                             'validation_iwae' = validation_iwae,
                             'validation_iwae_running_avg' = validation_iwae_running_avg,
                             'running_avg_num_values' = running_avg_num_values,
                             'train_vlb' = train_vlb),
                        state_list)

          # Create the file name
          filename_nth = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), '_p_', p, '_n_', n, '_depth_',
                               depth, '_width_', width, '_latent_', latent_dim, '_lr_', lr, '_epoch_', epoch, '.pt', sep = "")

          # Combine the file name with the folder path to form the final save file name.
          filename_nth = file.path(folder_to_save_model, filename_nth)
          class(nth_state) = c(class(nth_state), "R_VAEAC", "vaeac")
          torch_save(nth_state, filename_nth)

          # Add file name to list over file names.
          tmp_list = list(filename_nth)
          names(tmp_list) = paste("epoch_", epoch, sep = "")
          filename_nth_list = append(filename_nth_list, tmp_list)
        }
      }

      # Updates the current state
      pb$tick(tokens = list(epoch = epoch,
                            vlb = round(avg_vlb$item(), 3),
                            iwae = round(val_iwae$item(), 3),
                            runningiwae = round(validation_iwae_running_avg[-1]$item(), 3)))
    } # Done with training

    # Also save the model at the last epoch
    last_state = c(list('epoch' = epoch,
                        # 'model' = model,
                        # 'optimizer' = optimizer,
                        'model_state_dict' = model$state_dict(),
                        'optimizer_state_dict' = optimizer$state_dict(),
                        'validation_iwae' = validation_iwae,
                        'validation_iwae_running_avg' = validation_iwae_running_avg,
                        'running_avg_num_values' = running_avg_num_values,
                        'train_vlb' = train_vlb),
                   state_list)

    # Create the file name
    filename_last = paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), 'p', p, 'n', n, 'depth',
                          depth, 'width', width, 'latent', latent_dim, 'lr', lr, 'last.pt', sep = "_")

    # Combine the file name with the folder path to form the final save file name.
    filename_last = file.path(folder_to_save_model, filename_last)
    class(last_state) = c(class(last_state), "R_VAEAC", "vaeac")
    torch_save(last_state, filename_last)

    # Printout to the user
    if (verbose) {
      cat(sprintf("
Best epoch:             %d. \tVLB = %.4f. \tIWAE = %.4f \tIWAE_running = %.4f.
Best running avg epoch: %d. \tVLB = %.4f. \tIWAE = %.4f \tIWAE_running = %.4f.
Last epoch:             %d. \tVLB = %.4f. \tIWAE = %.4f \tIWAE_running = %.4f.\n",
                  best_state$epoch,
                  best_state$train_vlb[-1],
                  best_state$validation_iwae[-1],
                  best_state$validation_iwae_running_avg[-1],
                  best_state_running$epoch,
                  best_state_running$train_vlb[-1],
                  best_state_running$validation_iwae[-1],
                  best_state_running$validation_iwae_running_avg[-1],
                  last_state$epoch,
                  last_state$train_vlb[-1],
                  last_state$validation_iwae[-1],
                  last_state$validation_iwae_running_avg[-1]))
    }

    # Create a return list
    return_models = list("best" = filename_best,
                         "best_running" = filename_best_running,
                         "last" = filename_last)
    return_results = list("train_vlb" = as.array(train_vlb),
                          "validation_iwae" = as.array(validation_iwae),
                          "validation_iwae_running_avg" = as.array(validation_iwae_running_avg))
    return_parameters = state_list[-seq(2:7)]

    # If we are to add the 'filename_nth_list' list to the return list.
    if (!is.null(save_VAEAC_every_nth_epoch)) {
      filename_nth_list = c(VAEAC_model$models[grepl("epoch", names(VAEAC_model$models))], filename_nth_list)
      return_models = append(return_models, filename_nth_list, 3)
    }

  })

  return_list = list("models" = return_models,
                     "results" =  return_results,
                     "parameters" = return_parameters,
                     "training_time" = VAEAC_model$training_time + new_training_time)

  # Update the class of the returned object
  attr(return_list, "class") = c("R_VAEAC", "vaeac", "list")

  # Return the paths where the models are saved and the training/validation errors.
  return(return_list)
}


# Plot Functions ==================================================================================================================================================================================================

#' Extract the Training VLB and Validation IWAE from the Provided VAEAC Model
#'
#' @param explanation List. The output list from the shapr::explanation function.
#' @param plot Boolean. If we are to plot the train VLB and validation IWAE, and add figure to return list.
#' @param plot_from_nth_epoch Integer. From which epoch to plot from. First epoch can be large in absolute value and make the rest of the plot difficult to interpret.
#'
#' @return A list containing the training VLB and validation IWAE at each epoch, and the total number of epochs.
#' @export
VAEAC_training_vlb_and_validation_iwae_shapr = function(explanation,
                                                        plot_figure = TRUE,
                                                        plot_from_nth_epoch = 1,
                                                        return_figure = FALSE,
                                                        return_training_validation_errors = FALSE) {

  # Extract the checkpoint for the last trained VAEAC model.
  VAEAC_model_path = explanation$internal$parameters$VAEAC$models$last

  # Load the VAEAC model at the provided path.
  checkpoint = torch_load(path = VAEAC_model_path)

  # Create an empty return list
  temp_list = list()

  # Only want to return epoch, training loss, and validation loss.
  include_keys = c("validation_iwae", "validation_iwae_running_avg", "train_vlb", "epoch")

  # Iterate over the keys.
  for (key in include_keys) {
    # Extract the entries from the checkpoint and convert from torch tensors to R arrays.
    temp_list[[key]] = as.array(checkpoint[[key]])
  }

  # Check if we are to return the errors
  if (return_training_validation_errors) {
    return_list = temp_list
  }

  # Check if we are to plot the training VLB and validation IWAE.
  if (plot_figure | return_figure) {
    # Combine the results into a data table.
    temp_data = data.table("VLB" = temp_list$train_vlb,
                           "IWAE" = temp_list$validation_iwae,
                           "IWAE_running" = temp_list$validation_iwae_running_avg,
                           "Epoch" = seq(temp_list$epoch))

    # Convert it from wide to long
    temp_data = melt(data = temp_data,
                     id.vars = "Epoch",
                     variable.name = "Type",
                     variable.factor = TRUE,
                     value.name = "Value")

    # Remove entries with too low epoch
    temp_data = temp_data[Epoch >= plot_from_nth_epoch,]

    # Create the figure
    fig = ggplot(temp_data, aes(x = Epoch, y = Value, group = Type)) +
      geom_line(aes(color = Type)) +
      geom_point(aes(color = Type)) +
      scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
      theme(legend.position = "right") +
      lims(x = c(0, temp_list$epoch))

    # Check if we are to plot the figure
    if (plot_figure) print(fig)

    # Check id we are to return the figure
    if (return_figure) {
      if (return_training_validation_errors) {
        # We add the figure to the return list
        return_list[["Figure"]] = fig
      } else {
        # We are only to return the figure, so we replace the list with only the figure
        return_list = fig
      }
    }
  }

  # Return the the results
  if (return_figure | return_training_validation_errors) return(return_list)
}


#' Plot Pairwise Plots for Imputed and True Data
#'
#' @description A function that either takes the output of the train_VAEAC_model or continue_train_VAEAC_model function
#' and iterate over the VAEAC models. It generates imputations from the unconditioned distribution p(x) estimated by
#' the VAEAC models and then compares them with data from the true distribution (if provided).
#'
#' @details See https://www.blopig.com/blog/2019/06/a-brief-introduction-to-ggpairs/ for introduction to ggpairs.
#' Each combination of variables are plotted according to whether they are:
#' continuous: e.g. "points" (lower default), "smooth", "smooth_loess", "density", "cor" (upper default), or "blank"
#' combo: e.g. "box", "box_no_facet" (upper default), "dot", "dot_no_facet", "facethist", "facetdensity", "denstrip", or "blank"
#' discrete: e.g. "ratio", "facetbar", or "blank"
#' Plots on the diagonal can either be:
#' continuous: "densityDiag", "barDiag", "blankDiag"
#' discrete: "barDiag", "blankDiag"
#' Also call vig_ggally("ggally_plots").
#'
#' @param explanation List. The output list from the shapr::explanation function.
#' @param which_vaeac_model String. Indicating which VAEAC model to use when generating the samples.
#' @param true_data Matrix/data.frame containing the data from the distribution that the VAEAC model is fitted to.
#' @param return_figures Boolean. If we are to return the figures in a list.
#' @param save_figures Boolean. If we are to save the figures as png files.
#' @param show_figures Boolean. If we are to show the figures while making them.
#' If TRUE, then we create the figures twice, so the run time will double.
#' @param upper_cont Type of plot to use in upper triangle for continuous features.
#' @param upper_cat Type of plot to use in upper triangle for categorical features.
#' @param upper_mix Type of plot to use in upper triangle for mixed features.
#' @param lower_cont Type of plot to use in lower triangle for continuous features.
#' @param lower_cat Type of plot to use in lower triangle for categorical features.
#' @param lower_mix Type of plot to use in lower triangle for mixed features.
#' @param diag_cont Type of plot to use on the diagonal for continuous features.
#' @param diag_cat Type of plot to use on the diagonal for categorical features.
#' @param cor_method Type of correlation measure.
#' @param ... Extra parameters sent to ggsave function.
#'
#' @return A list containing the figures if \code{return_figures} = TRUE.
#' @export
ggpairs_plot_imputed_and_true_data_shapr = function(explanation,
                                                    which_vaeac_model = "best",
                                                    true_data = NULL,
                                                    return_figures = TRUE,
                                                    save_figures = FALSE,
                                                    show_figures = FALSE,
                                                    upper_cont = c("cor", "points", "smooth", "smooth_loess", "density", "blank"),
                                                    upper_cat = c("count", "cross", "ratio", "facetbar", "blank"),
                                                    upper_mix = c("box", "box_no_facet", "dot", "dot_no_facet", "facethist", "facetdensity", "denstrip", "blank"),
                                                    lower_cont = c("points", "smooth", "smooth_loess", "density", "cor", "blank"),
                                                    lower_cat = c("facetbar", "ratio", "count", "cross", "blank"),
                                                    lower_mix = c("facetdensity", "box", "box_no_facet", "dot", "dot_no_facet", "facethist", "denstrip", "blank"),
                                                    diag_cont = c("densityDiag", "barDiag", "blankDiag"),
                                                    diag_cat = c("barDiag", "blankDiag"),
                                                    cor_method = c("pearson", "kendall", "spearman"),
                                                    ...) {

  # Check if the VAEAC model is expected to give a reasonable figure.
  if (!explanation$internal$parameters$exact | explanation$internal$parameters$is_groupwise) {
    warning(sprintf("The VAEAC model has not been trained on the empty colition, hence, the figure can be missleading.
The figure is only reasonable if 'n_combintations = NULL' and 'group = NULL' in the explanation call."))
  }

  # Extract the VAEAC list from the explanation list
  VAEAC_list = explanation$internal$parameters$VAEAC

  # Figure out which VAEAC model to use.
  if (which_vaeac_model %in% names(VAEAC_list$models)) {
    # User provided a string which matches one of the file names and we use it.
    VAEAC_model_path = VAEAC_list$models[[which_vaeac_model]]
  } else {
    # User provided a string which is not one of the file names. Overwrite it.
    VAEAC_model_path = VAEAC_list$models[["best"]]
    warning(sprintf("The provided string for 'which_vaeac_model' (%s) did not match any stored checkpoints (%s).\nWe set 'which_vaeac_model = best' and continue.\n",
                    which_vaeac_model,
                    paste(names(VAEAC_list$models), collapse = ", ")))
  }

  # Check for valid input.
  upper_cont = match.arg(upper_cont)
  upper_cat  = match.arg(upper_cat)
  upper_mix  = match.arg(upper_mix)
  lower_cont = match.arg(lower_cont)
  lower_cat  = match.arg(lower_cat)
  lower_mix  = match.arg(lower_mix)
  diag_cont  = match.arg(diag_cont)
  diag_cat   = match.arg(diag_cat)
  cor_method = match.arg(cor_method)

  # Get the number of observations in the true_data
  num_samples = ifelse(is.null(true_data), 500, nrow(true_data))

  # Some small checks
  VAEAC_model = torch_load(VAEAC_model_path)
  if (!is.null(true_data)) {
    if (ncol(true_data) != VAEAC_model$p) {
      stop(sprintf("Different number of columns in the VAEAC model and 'true data': %d != %d.\n", checkpoint$p, ncol(true_data)))
    }
  }

  # Create folder if we are to save the figures
  if (save_figures) {
    # Create a path and folder for where we arr to save the figures
    folder_to_save_models = file.path(dirname(VAEAC_model), "Plots_ggpairs")
    dir.create(folder_to_save_models, showWarnings = FALSE)
  }

  # Extract which features that are categorical.
  cat_featuers = VAEAC_model$col_cat

  # If we are to return the figures
  if (return_figures) {
    figure_list = list()
  }

  # Impute the values. Here we generate x from p(x), so no conditioning.
  imputed_values = VAEAC_impute_values(
    instances_to_impute = matrix(NaN, num_samples, VAEAC_model$p), #instances_to_impute,
    path_VAEAC_model = VAEAC_model_path,
    num_imputations = 1,
    use_cuda = FALSE,
    convert_to_2D = TRUE,
    return_as_postprocessed_data_table = TRUE,
    batch_size = num_samples,
    verbose = FALSE)

  # Combine the true data (if there are any) with the imputed values.
  combined_data = data.frame(rbind(true_data, imputed_values))

  # Ensure that the categorical features are marked as factors.
  combined_data[cat_featuers] = lapply(combined_data[cat_featuers], factor)

  # Add a variable indicating if the values are from the true distribution or if they have been sampled.
  combined_data$type = factor(rep(c("True", "Imputed"), times = c(ifelse(is.null(nrow(true_data)), 0, nrow(true_data)), num_samples)))

  # Extract what to include as title in the figure.
  figure_title = tools::file_path_sans_ext(basename(VAEAC_model_path))
  # strsplit(basename(filename), "\\.pt")[[1]][1]

  # Create the ggparis figure.
  figure = ggpairs(combined_data,
                   columns = seq(VAEAC_model$p),
                   mapping = aes(color = type),
                   diag = list(continuous = wrap(diag_cont, alpha = 0.5),
                               discrete = wrap(diag_cat, alpha = 1.0)),
                   upper = list(combo = wrap(upper_mix, alpha = 1.0),
                                discrete = wrap(upper_cat, alpha = 1.0),
                                continuous = wrap(upper_cont, method = cor_method, size = 3.65)),
                   lower = list(continuous = wrap(lower_cont, alpha = 0.25),
                                discrete = wrap(lower_cat, alpha = 1.0),
                                combo = wrap(lower_mix, alpha = 1)),
                   title = figure_title,
                   proportions = rep(1, VAEAC_model$p)) +
    # theme(plot.title = element_text(size=22),
    #       text = element_text(size = 16),
    #       strip.text = element_text(size = 13)) +
    scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9")) +
    scale_fill_manual(values = c("#E69F00", "#999999",  "#56B4E9")) +
    theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust = 0.5))

  # If we are to show the figure.
  if (show_figures) {
    print(figure)
  }

  # If we are to save the figure.
  if (save_figures) {
    figure_save_name = file.path(folder_to_save_models, paste("ggpairs_", figure_title, ".png", sep = ""))
    suppressMessages(ggsave(figure_save_name, plot = figure, ...))
  }

  # If we are to return the figure.
  if (return_figures) {
    return(figure)
  }
}



# Compute Imputations =================================================================================================================================================================================================



#' Impute Missing Values Using VAEAC
#'
#' @details  Function that imputes the missing values in 2D matrix where each row constitute an individual.
#' The values are sampled from the conditional distribution estimated by a VAEAC model. Note that this function
#' does not preprocess the data.
#'
#' @param instances_to_impute A 2^p (maybe 2^p-2) x p matrix. Should be 'NaN' where we want to impute the missing values.
#' @param path_VAEAC_model String containing the location of the saved VAEAC model
#' @param num_imputations the number of imputed versions we create for each row in 'instances_to_impute'
#' @param use_cuda Boolean. If we are to use cuda.
#' @param sample_random Boolean. If we are to generate random samples from the inferred generative distributions, or if we are to sample the most likely values (mean for cont, class with highest prob for cat).
#' @param convert_to_2D Boolean. If the returned results should be of shape [num_instances_to_impute, num_imputations, num_features] or
#' [(num_instances_to_impute * num_imputations), num_features].
#' @param return_as_postprocessed_data_table Boolean. If we are to postprocess the data, i.e., convert categorical features to factors
#'with correct level names (and transform continuous features back to original scale). The returned object will then be a data table.
#' @param batch_size Integer. The number of samples in each batch. We recommend a large number because of a lot of overhead for small
#' batch sizes, that is, much larger than the batch size used during training. If NULL, then use the same value saved in the VAEAC object.
#' @param verbose Boolean. If we are to print the progress to the user.
#'
#' @return An array where the missing values (NaN) in \code{instances_to_impute} have been imputed \code{num_imputations} times.
#' @export
VAEAC_impute_values = function(instances_to_impute,
                               path_VAEAC_model,
                               num_imputations,
                               use_cuda = FALSE,
                               sample_random = TRUE,
                               convert_to_2D = TRUE,
                               return_as_postprocessed_data_table = TRUE,
                               batch_size = NULL,  # 32000 / num_features
                               verbose = FALSE) {

  # If we are to return a data table we need to convert to 2D.
  if (return_as_postprocessed_data_table) convert_to_2D = TRUE

  # Check if cuda/GPU is available on the current system
  cuda_available = torch::cuda_is_available()

  # Give warning to user if asked to run on cuda, but cuda is not available.
  if (isFALSE(cuda_available) && isTRUE(use_cuda)) {
    use_cuda = FALSE
    warning("Cuda is not available. Uses CPU instead.", immediate. = TRUE)
  }

  # Small printout to the user.
  # if (verbose) cat(sprintf("Loading VAEAC model.\n"))

  # Load the VAEAC model from provided disk location.
  checkpoint = torch_load(path_VAEAC_model)

  # REMOVED:
  # Preprocess the data. This function turns factor names into numerics 1,2,...,K,
  # as VAEAC only accepts numerics, and keep track of the maping of names.
  # And optionally log-transform all continuous features. Usual for strictly positive
  # data set like Burr and Abalone, such that VAEAC does not impute negative values.
  preprocessed_data = VAEAC_preprocess_data(instances_to_impute, checkpoint$transform_all_continuous_features)

  # Extract the training data where all the
  instances_to_impute = preprocessed_data$x_train

  # Extract relevant properties from the VAEAC model
  one_hot_max_sizes = checkpoint$one_hot_max_sizes
  norm_mean = checkpoint$norm_mean
  norm_std = checkpoint$norm_std

  # If batch size has not been provided, then we use the same as during training.
  if (is.null(batch_size)) batch_size = checkpoint$batch_size

  # Check/set valid batch size
  if (batch_size > nrow(instances_to_impute)) batch_size = nrow(instances_to_impute)

  # Create a VAEAC model
  model = VAEAC(one_hot_max_sizes = checkpoint$one_hot_max_sizes,
                width = checkpoint$width,
                depth = checkpoint$depth,
                latent_dim = checkpoint$latent_dim,
                activation_function = checkpoint$activation_function,
                use_skip_connections = checkpoint$use_skip_connections,
                use_skip_connections_between_masked_encoder_and_decoder = checkpoint$use_skip_connections_between_masked_encoder_and_decoder,
                use_batch_normalization = checkpoint$use_batch_normalization,
                mask_generator_name = checkpoint$mask_generator_name,
                masking_ratio = checkpoint$masking_ratio,
                mask_generator_only_these_coalitions = checkpoint$mask_generator_only_these_coalitions,
                mask_generator_only_these_coalitions_probabilities = checkpoint$mask_generator_only_these_coalitions_probabilities,
                sigma_mu = checkpoint$sigma_mu,
                sigma_sigma = checkpoint$sigma_sigma)

  # Update the model's state dictionary to the one provided by the user.
  model$load_state_dict(checkpoint$model_state_dict)

  # Extract sampling method.
  # I.e., if we are to generate random samples from the inferred generative distributions,
  # or if we are to sample the most likely values (mean for cont, class with highest prob for cat).
  if (sample_random) {
    sampler = model$sampler_random
  } else {
    sampler = model$sampler_most_likely
  }

  # Set the model in evaluation status, which effects certain modules.
  # E.g., deactivates dropout layers, how batch norm is conducted.
  model$eval()

  # Send the model to the GPU, if we are supposed to.
  if (use_cuda) {
    model = model$cuda()
    device = "cuda"
  } else {
    device = "cpu"
  }

  # Small printout to the user
  # if (verbose) cat(sprintf("Start preparation work before imputations.\n"))

  # Normalize the data with the mean and std from the training data.
  # I.e., we assume that the new data follow the same distribution as the training data.
  # If this is NOT the case, then VAEAC will generate unreasonable imputations.
  data_nan = (instances_to_impute - norm_mean) / norm_std

  # Create the data set object.
  dataset = VAEAC_dataset(data_nan, one_hot_max_sizes)

  # Create a data loader that load/iterate over the data set in chronological order.
  dataloader = dataloader(dataset, batch_size = batch_size)

  # Create an auxiliary list of lists to store the imputed values combined with the original values. The the structure is
  # [[i'th imputation]][[b'th batch]], where the entries are tensors of dimension batch_size x num_features.
  results = lapply(seq(num_imputations), function(k) list())

  # Create a progress bar that shows the progress of imputations
  # if (verbose) cat(sprintf("Ready to start imputing the instances.\n"))

  # Create a progress bar
  pb = progress_bar$new(format = "(:spin) [:bar] :percent [Imputing | time: :elapsedfull | ETR: :eta | Batch: :batch_index | Imputation: :imputation_index]",
                        total = dataloader$.length()*num_imputations,
                        complete = "=",   # Completion bar character
                        incomplete = "-", # Incomplete bar character
                        current = ">",    # Current bar character
                        clear = !verbose, # If TRUE, clears the bar when finish
                        width = 125)      # Width of the progress bar

  # Variable to keep track of which batch we are working on. Only needed for progress bar.
  batch_index = 1

  # Small printout to the user
  # if (verbose) cat(sprintf("Start imputating missing values.\n"))

  # batch = dataloader$.iter()$.next()
  # Impute the missing values for the input data, one batch at the time.
  coro::loop(for (batch in dataloader) {

    # Make a deep copy of the batch and detach it from graph.
    batch_extended = batch$clone()$detach()

    # If batch size is less than batch_size, extend it with objects from the beginning of the dataset.
    if (batch_extended$shape[1] < batch_size) {
      batch_extended = extend_batch(batch = batch_extended,
                                    dataloader = dataloader,
                                    batch_size = batch_size)
    }

    # Send the original and extended batch to GPU if applicable.
    if (use_cuda) {
      batch = batch$cuda()
      batch_extended = batch_extended$cuda()
    }

    # Compute the imputation mask, i.e., which entries we are to impute.
    mask_extended = torch_isnan(batch_extended)$to(dtype = torch_float())

    # Do not need to keep track of the gradients, as we are not fitting the model.
    with_no_grad({
      # Compute the distributions parameters for the generative models inferred by
      # the masked encoder and decoder together. This will be a tensor of shape
      # [batch_size, num_imputations, num_generative_parameters].
      # For only continuous features we have that num_generative_parameters = 2*num_features,
      # but for categorical data the number depends on the number of categories.
      samples_params = model$generate_samples_params(batch = batch_extended,
                                                     mask = mask_extended,
                                                     K = num_imputations)

      # Remove the parameters belonging to added instances in batch_extended.
      samples_params = samples_params[1:batch$shape[1],,]
    })

    # Make a deep copy of the batch with missing values set to zero.
    mask = torch_isnan(batch)
    batch_zeroed_nans = batch$clone()$detach()
    batch_zeroed_nans[mask] = 0

    # Iterate over the number of imputations and generate the imputed samples
    for (i in seq(num_imputations)) {
      # Extract the i'th inferred generative parameters for the whole batch.
      # sample_params is a tensor of shape [batch_size, num_generative_parameters].
      sample_params = samples_params[,i,]

      # Generate the imputations using the generative distributions inferred by the decoder.
      # Can either be the most likely values (mean for cont, class with highest prob for cat),
      # or we can randomly sample the imputed values. We do the latter.
      # sample = networks$sampler_most_prob_false(sample_params)
      # sample = networks$sampler_most_prob_true(sample_params)
      sample = sampler(sample_params)

      # Need only the imputed values for the missing data entries.
      # Zero out the imputations done for known feature values.
      sample[torch_logical_not(mask)] = 0

      # Combine the imputations with the original data to fill in the missing values.
      # Sample is a tensor of shape [batch_size, num_features]
      sample = sample + batch_zeroed_nans

      # Make a deep copy and add it to correct location in the results list.
      results[[i]] = append(results[[i]], sample$clone()$detach()$cpu())

      # Update the current state of the progress bar.
      pb$tick(tokens = list(batch_index = batch_index, imputation_index = i))
    }

    # Update the batch number.
    batch_index = batch_index + 1
  }) # End of iterating over the batches. Done imputing.

  # Small printout to the user
  # if (verbose) cat(sprintf("Start concatenating the imputations.\n"))

  # Goal is to order the imputations into a tensor of shape
  # [num_instances_to_impute, num_imputations, num_features]
  # Start iterating over the number of imputations
  for (i in seq(num_imputations)) {
    # Concatenate the batches for the i'th imputation to create a tensor of shape
    # [num_instances_to_impute, num_features] and then add a new singelton dimension
    # as the second dimension to get the shape [num_instances_to_impute, 1, num_features].
    results[[i]] = torch_cat(results[[i]])$unsqueeze(2)
  }

  # Concatenate the list of tensor of shape [num_instances_to_impute, 1, num_features] at the
  # second dimension to form a [num_instances_to_impute, num_imputations, num_features] tensor.
  result = torch_cat(results, 2)

  # Undo the normalization ([data - mu]/sigma) to get back to the original
  # distribution by multiplying the results by sigma and adding the mean.
  result = result * norm_std + norm_mean

  # If we are to concatenate the result such that we go from
  # [num_instances_to_impute, num_imputations, num_features] to
  # [(num_instances_to_impute * num_imputations), num_features].
  # I.e., each instance is no longer a separate 2D tensor.
  if (convert_to_2D) {
    # Result is now of dimension
    result = result$view(c(result$shape[1] * result$shape[2], result$shape[3]))
  }

  # Convert the results to either a 3D or 2D array.
  result = as.array(result$detach()$cpu())

  # Check if we are to post process the data before returning it.
  if (return_as_postprocessed_data_table) {
    # Postprocess the data such that categorical features have original level names.
    # and result is now a data.table
    result = VAEAC_postprocess_data(result, checkpoint)
  }

  # Return the input data with missing values imputed by VAEAC
  return(result)
}



# Evaluate Approach MSE v(S) ------------------------------------------------------------------
# This file contains a single function which I (Lars) originally wrote for Paper2,
# but which I have now updated to the syntax in shapr-devel.

# To call this function the use must have set 'keep_samp_for_vS = TRUE' in the
# call to the explain function as this method relies on the generated MC samples and
# is called after the 'explain' function.
# Ideally, if we find the MSE_Frye measure interesting to compute, this function
# should be automatically called within the 'explain' functions.

# The function computes the MSE criterion proposed by Frye et al. and Covert et al.
# This evaluation measures does not rely on the true Shapley values and rather look at the v(s).
#' MSE of Frye and Covert.
#'
#' @description Function that computes the MSE's proposed in Frye and Covert.
#' So it does not need the true Shapley values.
#'
#' @param explainer object returned from the shapr() function in the shapr package.
#' @param explanation object returned from explain() function in the shapr package.
#'  Note that the 'prediction' function must have been updated to the version in this file.
#'  I.e., that explanation keeps the simulated data.
#' @param return_individual Boolean. If we are to return the error on individual basis.
#' @param return_df Boolean. If we are to return the data frame.
#' @param return_p_hat_avg Boolean. If we are to return the average prediction.
#' @param return_coalition Boolean. If we are to return the results for the different coalitions.
#'
#' @export
#'
evaluate_approach = function(explanation,
                             return_individual = TRUE,
                             return_df = FALSE,
                             return_p_hat_avg = TRUE,
                             return_coalition = TRUE) {

  if (!explanation$internal$parameters$keep_samp_for_vS) {
    stop("The argument 'keep_samp_for_vS' must be 'TRUE' in the 'explain()' function for this function to work.")
  }


  # Explanation: object returned from explain() function
  # If we are to return the MSE for each individual separate

  # We can be in two situations.
  # 1. We iterate over all possible coalitions
  # 2. We have sampled the coalitions based on the Shapley kernel weights with replacements.
  # This is reflected in the form of explainer$exact and influence explanation object to.
  # Hence we need to do an if-else later on to handle this situation.

  # Get the number of unique coalitions. (two of these are the empty and full set)
  # This is 2^M if exact or some value below 2^M if sampled version
  number_of_unique_coalitions = explanation$internal$parameters$n_combinations

  # Get the number of test observations.
  N_test = nrow(explanation$internal$data$x_explain)

  # Get the imputed samples
  # The id_combination column goes from 1 to number_of_unique_coalitions
  samples_dt = explanation$internal$output$dt_samp_for_vS

  # Get the Shapley kernel weights
  weights = explanation$internal$objects$X$shapley_weight
  # weights[c(1,number_of_unique_coalitions)] = 1

  # Compute the mean predicted value. I.e., \hat{v}(S) = E_{\hat{p}(X_sbar | X_s)} [f(X_sbar, X_s)]
  # We use the weights here such that the function can also be used with e.g. ctree,
  # for Gaussian or any method where we sample all the imputations, all imputations
  # will have the same weight and this is equivalent of taking the mean.
  dt_res = samples_dt[, .(p_hat_avg = sum((p_hat1 * w) / sum(w))), .(id, id_combination)]

  # Create an auxiliary data table containing the empty set. Which the new version of shapr
  # do not include.
  dt_res_aux = data.table(id = c(seq(N_test)), id_combination = rep(1, N_test), p_hat_avg = explanation$internal$parameters$prediction_zero)

  # Combine the two data tables
  dt_res = rbind(dt_res_aux, dt_res)

  # Sort just in case.
  data.table::setkeyv(dt_res, c("id", "id_combination"))

  # Add the actual predicted value for each test id.
  dt_res[,p_hat_true:=rep(explanation$pred_explain, each = number_of_unique_coalitions)]

  # Add new column with the squared difference between the
  # predicted value f(x) and the average predicted value
  # \hat{v}(S) = E_{\hat{p}(X_sbar | X_s)} [f(X_sbar, X_s)],
  # when only x_s is known.
  dt_res[, squared_error:=(p_hat_true - p_hat_avg)^2]

  # Look at the mean squared error for each test observation and also averaged over the test observations
  # dt_res[, mean(squared_error), by = id]
  # dt_res[, mean(squared_error)]

  # Add the Shapley kernel weights to the data table
  dt_res[, Shapley_weight:=rep(weights, N_test)]



  if (explanation$internal$parameters$exact) {
    # We are in setting number 1.

    # Can then take the average over all these squared errors
    mean_squared_error_naive = dt_res[, mean(squared_error)]

    # Above we include both S={} and S=\mathcal{M}, which
    # we should not do.
    # For the former, we use the provided value given by the
    # user, i.e., the predicted value is not influenced by the
    # approach.
    # While for the second, we get a squared error of 0, as
    # we can just use the original method.
    dt_res_removed_empty_and_full_coalitions = dt_res[!(id_combination %in% c(1, number_of_unique_coalitions))]
    mean_squared_error_removed_empty_and_full_S = mean(dt_res_removed_empty_and_full_coalitions$squared_error)

    # But now we do not take into consideration that certain coalitions
    # have a larger effect on the Shapley values.
    # So we make a last version that combines the squared error
    # with the Shapley kernel weight of Lundberg and Lee (2017).

    # Get the Shapley kernel weights and normalize them.
    shapley_weights = weights[-c(1, number_of_unique_coalitions)]
    shapley_weights = shapley_weights/sum(shapley_weights)

    # Add the Shapley kernel weights to the data table
    dt_res_removed_empty_and_full_coalitions[,shapley_weight:=rep(shapley_weights, N_test)]

    # Compute the weighted squared error
    dt_res_removed_empty_and_full_coalitions[,weigthed_squared_error:=squared_error*shapley_weight]

    # Compute the weighted mean squared error.
    # Note that we take the sum as we normalized the Shapley kernel weights,
    # hence, we do not need to divide by sum(shapley kernel weights) => 1.
    # However, we still need to divide by the number of test observations
    weighted_mean_squared_error_removed_empty_and_full_S = sum(dt_res_removed_empty_and_full_coalitions$weigthed_squared_error) / N_test

  } else {
    # We are in setting number 2.

    #TODO: 9/6/23 I have not updated this part of the code.
    stop("We have not updated the code to support 'explanation' objects with sampled combinations.")

    # Number of total coalitions sampled.
    # THIS DOES NOT INCLUDE EMPTY SET AND FULL SET.
    number_of_total_coalitions = explainer$n_combinations

    # Can then take the average over all these squared errors.
    # But need to multiply with Shapley kernel weights as
    # some of the coalitions are sampled multiple times
    # which we need to take into consideration,
    # and the weights are in this setting the number of times
    # each coalition has been sampled.
    mean_squared_error_naive = sum(dt_res$squared_error * dt_res$Shapley_weight)/((number_of_total_coalitions + 2)*N_test)

    # Above we include both S={} and S=\mathcal{M}, which
    # we should not do.
    # For the former, we use the provided value given by the
    # user, i.e., the predicted value is not influenced by the
    # approach.
    # While for the second, we get a squared error of 0, as
    # we can just use the original method.
    dt_res_removed_empty_and_full_coalitions = dt_res[!(id_combination %in% c(1, number_of_unique_coalitions))]
    mean_squared_error_removed_empty_and_full_S =
      sum(dt_res_removed_empty_and_full_coalitions$squared_error *
            dt_res_removed_empty_and_full_coalitions$Shapley_weight) /
      (number_of_total_coalitions*N_test)
    dt_res_removed_empty_and_full_coalitions[,Shapley_weight_normalized := Shapley_weight/number_of_total_coalitions]

    # In this setting, we have already taken into consideration
    # that certain coalitions are more important than others in the
    # Shapley value formula. This is reflected by that certain features are
    # sampled several times.
    # So it does not makes sense to weight them a second time.
    # Thus, we set the WMSE equal to NA. (Not added)
    weighted_mean_squared_error_removed_empty_and_full_S = NA
  }

  # # If we want to look at each individual separately
  # dt_weighted_mean_squared_error_test_observations = dt_res_removed_empty_and_full_coalitions[, .(weighted_mean_squared_error=sum(weigthed_squared_error)), by = id]
  # dt_weighted_mean_squared_error_test_observations
  # mean(dt_weighted_mean_squared_error_test_observations$weighted_mean_squared_error)

  return_list = list(MSE_naive = mean_squared_error_naive,
                     mse_frye  = mean_squared_error_removed_empty_and_full_S,
                     WMSE      = weighted_mean_squared_error_removed_empty_and_full_S)

  if (return_individual) {
    return_list[["mse_frye_individuals"]] = (dt_res_removed_empty_and_full_coalitions[,mean(squared_error),by = id])$V1
  }
  if (return_coalition) {
    return_list[["mse_frye_coalitions"]] =  (dt_res_removed_empty_and_full_coalitions[,mean(squared_error), by = id_combination])$V1
  }
  if (return_df) {
    if (explainer$exact) {
      return_list[["df"]] = dt_res_removed_empty_and_full_coalitions[,c(1,2,3,7)]
    } else {
      return_list[["df"]] = dt_res_removed_empty_and_full_coalitions[,c(1,2,3,7)]
    }
  }

  if (return_p_hat_avg) {
    return_list[["p_hat_avg"]] = dt_res_removed_empty_and_full_coalitions$p_hat_avg
  }

  return(return_list)
}
