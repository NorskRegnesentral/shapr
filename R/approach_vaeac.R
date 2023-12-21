# Laste ned shapr fra github: remotes::install_github("NorskRegnesentral/shapr", ref = "devel")
# pkgload::load_all()
# source("~/PhD/Paper2/vaeac_in_R.R")

#  Load Required Libraries ============================================================================================
# {
#  library(torch)
#  library(progress)
#  library(data.table)
#  library(ggplot2)
#  library(GGally)
# }

#' @rdname setup_approach
#'
#' @param vaeac.depth Integer. The number of hidden layers in the neural networks of the masked
#'  encoder, full encoder, and decoder.
#' @param vaeac.width Integer. The number of neurons in each hidden layer in the neural networks
#'  of the masked encoder, full encoder, and decoder.
#' @param vaeac.latent_dim Integer. The number of dimensions in the latent space.
#' @param vaeac.lr Numeric. The learning rate used in the \code{\link[torch]{optim_adam}} optimizer.
#' @param vaeac.activation_function An \code{\link[torch]{nn_module}} representing an activation
#'  function such as, e.g., \code{\link[torch]{nn_relu}}, \code{\link[torch]{nn_leaky_relu}},
#'  \code{\link[torch]{nn_selu}}, and \code{\link[torch]{nn_sigmoid}}.
#' @param vaeac.num_different_vaeac_initiate Integer. The number of different vaeac models to initiate
#'  in the start. Pick the best performing one after `vaeac.extra_parameters$epochs_initiation_phase`
#'  epochs (default is `2`) and continue training that one.
#' @param vaeac.epochs Integer. The number of epochs to train the final vaeac model. This includes
#' `vaeac.extra_parameters$epochs_initiation_phase`, where the default is `2`.
#' @param vaeac.extra_parameters Named list with extra parameters to the `vaeac` approach. See
#' section "The `vaeac` approach" in [setup_approach()] for description of possible additional
#' parameters.
#'
#' @section The vaeac approach:
#' The function creates three neural network (a full encoder, a masked encoder, and a decoder) based
#' on the provided `vaeac.depth` and `vaeac.width`. The encoders maps the full and masked input
#' representations to latent representations, where the dimension is given by `vaeac.latent_dim`.
#' The latent representations are sent to the decoder to go back to the real feature space and
#' provide a samplable probabilistic representation, from which the MC samples are generated.
#' We use the `vaeac` method at the epoch with the lowest validation error (IWAE) by default, but
#' other possibilities are available but setting the `vaeac.which_vaeac_model` parameter.
#'
#' Here we provide a list of the extra parameters that can be provided as a named list
#' `vaeac.extra_parameters` to the [explain()] function when using `approach = "vaeac"`.
#' \describe{
#'  \item{vaeac.model_description}{Default is `NULL`. String containing, e.g., the name of the data
#'   distribution or additional parameter information. Used in the save name of the fitted model.}
#'  \item{vaeac.folder_to_save_model}{Default is `NULL`. String specifying a path to a folder where
#'   the function is to save the fitted vaeac model.}
#'  \item{vaeac.pretrained_vaeac_model}{Default is `NULL`. This can either be a list of type
#'  `vaeac`, i.e., the list stored in `explanation$internal$parameters$vaeac` from an earlier call
#'  to the [explain()] function. Or it can be a string containing the path to where the `vaeac`
#'  model is stored on disk, for example, `explanation$internal$parameters$vaeac$models$best`.}
#'  \item{vaeac.use_cuda}{Default is `FALSE`. Boolean. If we are to use cuda (GPU) if available.
#'  NOTE: not supported in the current version of the `shapr` package.}
#'  \item{vaeac.epochs_initiation_phase}{Default is `2`. Integer. The number of epochs to run each
#'   of the `vaeac.num_different_vaeac_initiate` vaeac models before only continuing training the
#'   best one.}
#'  \item{vaeac.epochs_early_stopping}{Default is `NULL`. Integer. The training stops if there has
#'   been no improvement in the validation IWAE for `vaeac.epochs_early_stopping` epochs. If the user wants
#'  the training process to be solely based on this, then `vaeac.epochs` should be set to a large number.}
#'  \item{vaeac.save_vaeac_every_nth_epoch}{Default is `NULL`. Integer. If we are to save the vaeac
#'   model after every nth epoch.}
#'  \item{vaeac.batch_size}{Default is `64`. Integer. The number of samples to include in each batch
#'   during the training of the vaeac model.
#'   Larger batches results in the training process to take less time, but it can result in a less
#'   precise `vaeac` model.}
#'  \item{vaeac.batch_size_sampling}{Default is `NULL`. Integer. The number of samples to include in
#'   each batch when generating the MC samples. If `NULL` (default), then we generate MC samples for
#'   the provided coalitions and all explicands at the time. The number of coalitions are determined
#'   by `n_batches`. We recommend to rather tweak `n_batches` than `vaeac.batch_size_sampling`.}
#'  \item{vaeac.validation_ratio}{Default is `0.25`. Scalar between 0 and 1 indicating the ratio of
#'   instances from data which will be used as validation data.}
#'  \item{vaeac.validation_iwae_num_samples}{Default is `25`. Integer. The number of samples used
#'   to compute the IWAE when validating the vaeac model on the validation data.}
#'  \item{vaeac.running_avg_num_values}{Default is `5`. Integer. How many of the previous values/
#'   epochs to include when we compute the running means of the IWAE score.}
#'  \item{use_skip_connections}{Default is `TRUE`. Boolean. If we are to use skip connections in
#'   each layer. If true, then we add the input to the outcome of each hidden layer, so the output
#'   becomes X + activation(WX + b). I.e., identity skip connection.}
#'  \item{vaeac.skip_connection_masked_enc_dec}{Default is `TRUE`. Boolean.
#'   If we are to apply concatenate skip connections between the layers in the masked encoder
#'   and decoder.}
#'  \item{vaeac.use_batch_normalization}{Default is `FALSE`. Boolean. If we are to use batch
#'   normalization after the activation function. Note that if
#'   `vaeac.extra_parameters$use_skip_connections` is TRUE, then the normalization is done after
#'   the adding from the skip connection. I.e, we batch normalize the whole quantity
#'   X + activation(WX + b).}
#'  \item{vaeac.paired_sampling}{Default is `TRUE`. Boolean. If we are doing paired sampling. I.e.,
#'   each batch contains two versions of the same training observation,  but where the first one is
#'   masked by \eqn{S} and the second one is masked by \eqn{\bar{S}}, the complement, see
#'   \href{https://arxiv.org/pdf/2107.07436.pdf}{Jethani et al. (2022)}. Training becomes more
#'   stable, but slower due to more complex implementation.}
#'  \item{vaeac.masking_ratio}{Default is `0.5`. Probability of masking a feature in the MCAR mask
#'   generator. Default masking scheme which ensures that vaeac can do arbitrary conditioning.
#'   Is overruled if `vaeac.extra_parameters$mask_generator_only_these_coalitions` is specified.}
#'  \item{vaeac.mask_generator_only_these_coalitions}{Default is `NULL`. Matrix containing the
#'   the only coalitions which the `vaeac` model will be trained on. Used when `n_combinations`
#'   is less then \eqn{2^{n_\text{features}}} and for group Shapley.}
#'  \item{vaeac.mask_gen_these_coalitions_prob}{Default is `NULL`. Numerical
#'   array containing the probabilities for sampling the coalitions in
#'   `vaeac.extra_parameters$mask_generator_only_these_coalitions`.}
#'  \item{vaeac.sigma_mu}{Default is `1e4`. Numeric representing a hyperparameter in the
#'   normal-gamma prior used on the masked encoder, see Section 3.3.1 in
#'   \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.}
#'  \item{vaeac.sigma_sigma}{Default is `1e-4`. Numeric representing a hyperparameter in the
#'   normal-gamma prior used on the masked encoder, see Section 3.3.1 in
#'   \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.}
#'  \item{vaeac.save_data}{Default is `FALSE`. Boolean. If we are to save the data together with
#'   the model. Useful if one are to continue to train the model later.}
#'  \item{vaeac.transform_all_continuous_features}{Default is `FALSE`. Boolean. If we are to log
#'  transform all continuous features before sending the data to `vaeac`. The `vaeac` model creates
#'  unbounded MC samples values, so if the continuous features are strictly positive, as for Burr
#'  distributed data and the Abalone data set, it can be advantageous to log-transform the data to
#'  unbounded form before using `vaeac`. If `TRUE`, then \code{vaeac_postprocess_data} will take
#'  the exp of the results to get back to strictly positive values when using the vaeac model to
#'  impute missing values.}
#'  \item{vaeac.sample_random}{Default is `TRUE`. Boolean. If we are to generate random samples
#'   from the inferred generative distributions, or if we are to sample the most likely values.
#'   That is, the mean and class with highest probability for continuous and categorical,
#'   respectively.}
#'  \item{vaeac.verbose}{Default is `FALSE`. Boolean. If we are to print the progress of the
#'   initialization of different vaeac models, the training of the final vaeac model,
#'   and summary of the training progress. This works independently of the
#'   [progressr::progressr()] package, which is supported by `shapr`.}
#'  \item{vaeac.seed}{Default is `NULL`. Integer. Seed for reproducibility. If `vaeac.seed = NULL`,
#'   then use the same seed as in [explain()].}
#'  \item{vaeac.which_vaeac_model}{Default is `NULL`. String. Which of the returned `vaeac` models
#'  (snapshots from different epochs) to use when generating the MC samples. The choices are `best`,
#'  `best_running`, and `last`. Here, `best` reflects the epoch with the lowest IWAE score, and is
#'  the default choice. Note that additional choices are available if
#'  `vaeac.save_vaeac_every_nth_epoch` is provided. E.g., if `vaeac.save_vaeac_every_nth_epoch = 5`,
#'  then `vaeac.which_vaeac_model` can also take the values `epoch_5`, `epoch_10`, `epoch_15`, and
#'  so on.}
#' }
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
                                 vaeac.num_different_vaeac_initiate = 10,
                                 vaeac.epochs = 200,
                                 vaeac.extra_parameters = list(),
                                 ...) {
  # The names of the main parameters for the `vaeac` approach
  main_parameters <- c(
    "vaeac.depth",
    "vaeac.width",
    "vaeac.latent_dim",
    "vaeac.activation_function",
    "vaeac.lr",
    "vaeac.num_different_vaeac_initiate",
    "vaeac.epochs"
  )

  # List of the default values for the extra parameters in the vaeac approach
  vaeac.extra_parameters_default <- list(
    "vaeac.model_description" = NULL,
    "vaeac.folder_to_save_model" = NULL,
    "vaeac.pretrained_vaeac_model" = NULL,
    "vaeac.use_cuda" = FALSE,
    "vaeac.epochs_initiation_phase" = 2,
    "vaeac.epochs_early_stopping" = NULL,
    "vaeac.save_vaeac_every_nth_epoch" = NULL,
    "vaeac.validation_ratio" = 0.25,
    "vaeac.validation_iwae_num_samples" = 25,
    "vaeac.batch_size" = 64,
    "vaeac.batch_size_sampling" = NULL,
    "vaeac.running_avg_num_values" = 5,
    "vaeac.use_skip_connections" = TRUE,
    "vaeac.skip_connection_masked_enc_dec" = TRUE,
    "vaeac.use_batch_normalization" = FALSE,
    "vaeac.paired_sampling" = TRUE,
    "vaeac.masking_ratio" = 0.5,
    "vaeac.mask_generator_only_these_coalitions" = NULL,
    "vaeac.mask_gen_these_coalitions_prob" = NULL,
    "vaeac.sigma_mu" = 1e4,
    "vaeac.sigma_sigma" = 1e-4,
    "vaeac.save_data" = FALSE,
    "vaeac.transform_all_continuous_features" = FALSE,
    "vaeac.verbose" = FALSE,
    "vaeac.seed" = NULL,
    "vaeac.which_vaeac_model" = NULL
  )

  # User has provided a vaeac.extra_parameters object. Check that it is a named list.
  if (length(internal$parameters$vaeac.extra_parameters) > 0) {
    if (is.null(names(internal$parameters$vaeac.extra_parameters))) {
      stop("The parameter `vaeac.extra_parameters` is not a named list.")
    }
    if (any(names(internal$parameters$vaeac.extra_parameters) == "")) {
      stop("Not all parameters in the list `vaeac.extra_parameters` are named.")
    }
  }

  # Check if user has provided named objects in vaeac.extra_parameters which are not in the default list.
  unknown_parameters <-
    names(internal$parameters$vaeac.extra_parameters)[!(names(internal$parameters$vaeac.extra_parameters)
    %in% names(vaeac.extra_parameters_default))]

  # Divide the unknown parameters into whether they are main parameters or completely unknown.
  main_parameters_in_unknown_parameters <- unknown_parameters[unknown_parameters %in% main_parameters]
  not_main_parameters_in_unknown_parameters <- unknown_parameters[!unknown_parameters %in% main_parameters]

  # For those parameters that are completely unknown, we give a message to the user.
  if (length(not_main_parameters_in_unknown_parameters) > 0) {
    if (length(not_main_parameters_in_unknown_parameters) == 1) {
      message(sprintf(
        "The extra parameter `%s` is not recognized by the `vaeac` approach.\n",
        not_main_parameters_in_unknown_parameters
      ))
    } else {
      message(sprintf(
        "The extra parameters %s are not recognized by the `vaeac` approach.\n",
        paste(strsplit(paste(paste0("`", not_main_parameters_in_unknown_parameters, "`"),
          collapse = ", "
        ), ",(?=[^,]+$)", perl = TRUE)[[1]], collapse = " and")
      ))
    }
  }

  # Check if user specified some vaeac parameters to `explain` which should have been specified
  # in the vaeac.extra_parameters list. Note that in the end it really does not matter as the
  # vaeac.extra_parameters list is flatten to be individual elements in internal$parameters.
  misplaced_parameters <-
    names(internal$parameters)[names(internal$parameters) %in% names(vaeac.extra_parameters_default)]
  if (length(misplaced_parameters) > 0) {
    if (length(misplaced_parameters) == 1) {
      message(sprintf(
        "The vaeac extra parameter `%s` should have been placed in the
`vaeac.extra_parameters` list in the call to the `explain()` function. This is fixed internally.\n",
        misplaced_parameters
      ))
    } else {
      message(sprintf(
        "The vaeac extra parameters %s should have been placed in the
`vaeac.extra_parameters` list in the call to the `explain()` function. This is fixed internally.\n",
        paste(strsplit(paste(paste0("`", misplaced_parameters, "`"), collapse = ", "),
          ",(?=[^,]+$)",
          perl = TRUE
        )[[1]], collapse = " and")
      ))
    }
  }

  # Check if user has provided the same parameter twice, both as a main and extra parameter
  provided_as_both_main_and_extra_parameter <-
    names(internal$parameters$vaeac.extra_parameters)[names(internal$parameters$vaeac.extra_parameters)
    %in% names(internal$parameters)]

  # Print a message to the user and tell them that we use those in `vaeac.extra_parameters`.
  if (length(provided_as_both_main_and_extra_parameter) > 0) {
    if (length(provided_as_both_main_and_extra_parameter) == 1) {
      message(sprintf(
        "The parameter `%s` has been provided as both a separete parameter and in the
`vaeac.extra_parameters` list in the call to the `explain()` function.
The function proceeds using the value in `vaeac.extra_parameters`.\n",
        provided_as_both_main_and_extra_parameter
      ))
    } else {
      message(sprintf(
        "The parameters %s have been provided as both separete parameters and in the
`vaeac.extra_parameters` list in the call to the `explain()` function.
The function proceeds using the values in `vaeac.extra_parameters`.\n",
        paste(strsplit(paste(paste0("`", provided_as_both_main_and_extra_parameter, "`"),
          collapse = ", "
        ), ",(?=[^,]+$)", perl = TRUE)[[1]], collapse = " and")
      ))
    }
  }

  # For does parameter that actually are main parameters, we include them and give a message
  if (length(main_parameters_in_unknown_parameters) > 0) {
    # Insert the misplaced main parameters to the correct place.
    internal$parameters <- modifyList(
      internal$parameters,
      internal$parameters$vaeac.extra_parameters[main_parameters_in_unknown_parameters]
    )

    if (length(main_parameters_in_unknown_parameters) == 1) {
      message(sprintf(
        "The extra parameter `%s` is a main parameter to the `vaeac` approach and should not
be included in the `vaeac.extra_parameters` list, but rather as a separate parameter in the
call to the `explain()` function. This is fixed internally.\n",
        main_parameters_in_unknown_parameters
      ))
    } else {
      message(sprintf(
        "The extra parameters %s are main parameters to the `vaeac` approach and should not
be included in the `vaeac.extra_parameters` list, but rather as separate parameters in the
call to the `explain()` function. This is fixed internally.\n",
        paste(strsplit(paste(paste0("`", main_parameters_in_unknown_parameters, "`"), collapse = ", "),
          ",(?=[^,]+$)",
          perl = TRUE
        )[[1]], collapse = " and")
      ))
    }
  }

  # Ensure that internal$parameters$vaeac.extra_parameters is a list, as `modifyList` only works with lists
  if (is.null(internal$parameters$vaeac.extra_parameters)) internal$parameters$vaeac.extra_parameters <- list()

  # Combine the provided extra parameter values with the default values. Overwrite the default values.
  internal$parameters$vaeac.extra_parameters <-
    modifyList(vaeac.extra_parameters_default, internal$parameters$vaeac.extra_parameters, keep.null = TRUE)

  # The mget extract the default values defined for the variables to this function into a named list.
  defaults <- mget(main_parameters)

  # If a variable has not been specified by the user, then we use the default values extracted above.
  internal <- insert_defaults(internal, defaults)

  # Extract the parameters list which, e.g., contains user-provided  parameters for the vaeac approach.
  parameters <- internal$parameters

  # I extract the vaeac.extra_parameters list to not have to rewrite a lot of code to work with
  # some of the parameters being in the the internal$parameter list while other are in the
  # internal$parameter$vaeac.extra_parameters list.
  # Want to flatten the list, as `vaeac.extra_parameters` is now a list inside a list.
  # We set `keep.null = TRUE` as some of the entries are `NULL`, which would be dropped otherwise.
  # We overwrite the parameters in `parameters$vaeac.extra_parameters` with those in `parameters`,
  # which is needed if the user forgot to use vaeac.extra_parameters, otherwise, the values
  # of the user would be overwritten by the default values.
  # We then remove the vaeac.extra_parameters list.
  parameters <- modifyList(parameters$vaeac.extra_parameters, parameters, keep.null = TRUE)
  parameters$vaeac.extra_parameters <- NULL

  # Chaos in the list order, so reorder such that vaeac entries come at the end of the list
  shapr_indices <- seq_along(parameters)[!grepl("vaeac", names(parameters))]
  vaeac_indices <- seq_along(parameters)[grepl("vaeac", names(parameters))]
  parameters <- parameters[c(shapr_indices, vaeac_indices)]

  # We use the same seed for vaeac as the one specified for shapr::explain if not both has been provided.
  if (is.null(parameters$vaeac.seed)) parameters$vaeac.seed <- parameters$seed

  # Extract the objects list which, e.g., contains information about the possible coalitions.
  objects <- internal$objects

  # Variable to store if we are doing a combination of approaches
  combined_approaches <- any(objects$X$approach[-c(1, parameters$n_combinations)] != "vaeac")

  # Check if vaeac is to be applied on a subset of coalitions.
  if (!parameters$exact || parameters$is_groupwise || combined_approaches) {
    # We have either:
    # 1) sampled `n_combinations` different subsets of coalitions,
    # 2) using the coalitions which respects the groups in group Shapley values, and/or
    # 3) using a combination of approaches where vaeac is only used on a subset of the coalitions.
    # Here, objects$S contains the coalitions while objects$X contains the information about the approach.

    # Extract the the coalitions / masks which are estimated using vaeac,
    # and we use drop = FALSE to ensure that the output is a matrix.
    parameters$vaeac.mask_generator_only_these_coalitions <-
      objects$S[objects$X[approach == "vaeac"]$id_combination, , drop = FALSE]

    # Extract the weights for the corresponding coalitions / masks.
    parameters$vaeac.mask_gen_these_coalitions_prob <-
      objects$X$shapley_weight[objects$X[approach == "vaeac"]$id_combination]

    # Normalize the weights/probabilities such that they sum to one.
    parameters$vaeac.mask_gen_these_coalitions_prob <-
      parameters$vaeac.mask_gen_these_coalitions_prob /
        sum(parameters$vaeac.mask_gen_these_coalitions_prob)
  } else {
    # All 2^M coalitions are to be estimated using a vaeac model with a MCAR(0.5) masking scheme.
    # I.e., the corresponding vaeac model will support arbitrary conditioning as every coalition
    # will be trained with the same probability, also the empty and grand coalition.
    parameters$vaeac.mask_generator_only_these_coalitions <- NULL
    parameters$vaeac.mask_gen_these_coalitions_prob <- NULL
  }

  # Check if user provided a pre-trained vaeac model, otherwise, we train one from scratch.
  if (is.null(parameters$vaeac.pretrained_vaeac_model)) {
    # User did not provide pre-trained model.
    # We train a vaeac model based on the given hyper-parameters, or use
    # the default hyper-parameter values if nothing has been provided.

    # Boolean representing that a pre-trained vaeac model was NOT provided.
    parameters$vaeac.user_provided_pretrained_vaeac_model <- FALSE

    # Extract the training data from the explainer object
    x_train <- internal$data$x_train

    # Fit/train the vaeac model with the provided model parameters
    vaeac_training_time <- system.time({
      # Extract the parameters related to veaac.
      parameters_temp <- parameters[grepl("vaeac.", names(parameters))]

      # Remove "vaeac." from the names
      # Note that this can be dangerous if there is another variable which matches the name.
      # I.e., if we have a variable called "verbose" and "vaeac.verbose", then these will
      # after running this code have the same name.
      # But we only use the variables that had vaeac in their names, so not a problem.
      updated_names <- sapply(strsplit(names(parameters_temp), "\\."), "[", 2)
      names(parameters_temp)[!is.na(updated_names)] <- updated_names[!is.na(updated_names)]

      vaeac_model <- do.call(vaeac_train_model, c(parameters_temp, list(training_data = x_train)))
    })

    # Extract the paths to the trained vaeac models.
    vaeac_model_names <- vaeac_model[grepl("filename", names(vaeac_model))]
    names(vaeac_model_names) <- sub("filename_", "", names(vaeac_model_names))

    # Extract the training/validation results.
    # vaeac_model_results = vaeac_model[grepl("vlb", names(vaeac_model)) | grepl("iwae", names(vaeac_model))]
    vaeac_model_results <- vaeac_model[c("train_vlb", "validation_iwae", "validation_iwae_running_avg")]

    # Create a list of all the parameters used to train the vaeac-model
    vaeac_model_parameters <- vaeac_model$parameters

    # Add this to the explainer object
    parameters$vaeac <- list(
      "models" = vaeac_model_names,
      "results" = vaeac_model_results,
      "parameters" = vaeac_model_parameters,
      "training_time" = vaeac_training_time
    )

    # Add that "parameters$vaeac" is not just a list, but also of type "vaeac".
    # This is used to validate the input if the user sends "parameters$vaeac"
    # as a "vaeac.pretrained_vaeac_model" to the shapr::explain function.
    class(parameters$vaeac) <- c(class(parameters$vaeac), "vaeac")
  } else {
    # User provided a pre-trained vaeac model.

    # This can either be a list of type "vaeac", i.e., the list stored in
    # explanation$internal$parameters$vaeac from an earlier call to the
    # shapr::explain() function. Or it can be a string containing the path
    # to where the "vaeac" model is stored on disk.
    # Minimal checking for valid vaeac model is conducted.

    # Boolean representing that a pre-trained vaeac model was provided
    parameters$vaeac.user_provided_pretrained_vaeac_model <- TRUE

    # Extract the relevant names based on what the input is.
    if (is.list(parameters$vaeac.pretrained_vaeac_model)) {
      # Check that the list is of type vaeac.
      if (all(class(parameters$vaeac.pretrained_vaeac_model) != "vaeac")) {
        stop("The provided 'vaeac.pretrained_vaeac_model' is not a list of type 'vaeac'.")
      }

      # Check that the provided vaeac model is trained on a dataset with the same number of features.
      if (parameters$n_features != parameters$vaeac.pretrained_vaeac_model$parameters$p) {
        stop(sprintf(
          "The current dataset is %d-dimensional, but the provided
vaeac model was trained on a %d-dimensional dataset.\n",
          parameters$n_features,
          parameters$vaeac.pretrained_vaeac_model$parameters$p
        ))
      }

      # Check that the labels of x_train matches the labels of the training data used to train the vaeac model.
      if (!all.equal(
        parameters$feature_names,
        parameters$vaeac.pretrained_vaeac_model$parameters$feature_list$labels
      )) {
        stop(sprintf(
          "The labels of the training data (%s) do not match the labels
of the data used to train the provided vaeac model (%s).\n",
          paste(parameters$feature_names, collapse = ", "),
          paste(parameters$vaeac.pretrained_vaeac_model$parameters$feature_list$labels, collapse = ", ")
        ))
      }

      # The pre-trained vaeac model has passed the checks and we therefore
      # adds it to the parameters list as a valid vaeac model.
      parameters$vaeac <- parameters$vaeac.pretrained_vaeac_model
    } else if (is.character(parameters$vaeac.pretrained_vaeac_model)) {
      # We are only provided with a string, which we assume it the path to a saved vaeac object.

      # Check that the file exists
      if (!file.exists(parameters$vaeac.pretrained_vaeac_model)) {
        stop(sprintf(
          "The 'vaeac.pretrained_vaeac_model' file with path '%s' does not exist.\n",
          parameters$vaeac.pretrained_vaeac_model
        ))
      }

      # Read in the vaeac model from the disk
      vaeac_model <- torch::torch_load(parameters$vaeac.pretrained_vaeac_model)

      # Some very small check that we have read in a vaeac model
      if (is.null(vaeac_model$model_state_dict)) {
        stop(sprintf("The provided file is not a vaeac model as it is missing, e.g., the model_state_dict entry.\n"))
      }
      if (is.null(vaeac_model$optimizer_state_dict)) {
        stop(sprintf(
          "The provided file is not a vaeac model as it is missing, e.g., the optimizer_state_dict entry.\n"
        ))
      }

      # Check that the provided vaeac model is trained on a dataset with the same number of features.
      if (parameters$n_features != vaeac_model$p) {
        stop(sprintf(
          "The dataset is %d-dimensional, but the provided vaeac model was trained on a %d-dimensional dataset.\n",
          parameters$n_features,
          vaeac_model$p
        ))
      }

      # Check that the labels of x_train matches the labels of the training data used to train the vaeac model.
      if (!all.equal(parameters$feature_names, vaeac_model$feature_list$labels)) {
        stop(sprintf(
          "The labels in the training data (%s) and in the data used to train the vaeac model (%s) must match.\n",
          paste(parameters$feature_names, collapse = ", "),
          paste(vaeac_model$feature_list$labels, collapse = ", ")
        ))
      }

      # Extract the training/validation results.
      vaeac_model_results <- lapply(
        vaeac_model[c("train_vlb", "validation_iwae", "validation_iwae_running_avg")],
        as.array
      )
      vaeac_model_results

      # Save path to the vaeac approach to use to generate the MC samples.
      parameters$vaeac <- list(
        "models" = list("best" = parameters$vaeac.pretrained_vaeac_model),
        "results" = vaeac_model_results,
        "parameters" = vaeac_model[-seq(2, 7)],
        "training_time" = system.time({
          NULL
        }) * NA
      )
      # A hack as the training time is not known. Using this hack we can also continue to train this
      # vaeac object but the training time will just be NA for all entries.

      # Add that "parameters$vaeac" is not just a list, but also of type "vaeac".
      # This is used to validate the input if the user sends "parameters$vaeac"
      # as a "pretrained_vaeac_model" to the shapr::explain function.
      class(parameters$vaeac) <- c(class(parameters$vaeac), "vaeac")
    } else {
      stop(sprintf("The variable 'vaeac.pretrained_vaeac_model' is not a list or a string. Read the documentation.\n"))
    }
  }

  # If the user has specified that timing should be off, then we remove the time
  if (isFALSE(internal$parameters$timing)) {
    parameters$vaeac$training_time <- NULL
  }

  # Update/overwrite the parameters list in the internal list.
  internal$parameters <- parameters

  if (isTRUE(parameters$vaeac.verbose)) message(sprintf("Done with 'setup_approach.vaeac'.\n"))

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
  # A function that generates the Monte Carlo samples using the vaeac approach.

  # Extract the observations to explain. This will always be a data.table
  x_explain <- internal$data$x_explain

  # Extract the number of observations to explain
  n_explain <- internal$parameters$n_explain

  # Extract the number of features
  n_features <- internal$parameters$n_features

  # Extract the number of MC samples to generate
  n_samples <- internal$parameters$n_samples

  # Extract the trained vaeac model
  vaeac_list <- internal$parameters$vaeac

  # Figure out which of the stored vaeac checkpoints we are going to use.
  if (is.null(internal$parameters$vaeac.which_vaeac_model)) {
    which_vaeac_model <- "best"
  } else {
    if (internal$parameters$vaeac.which_vaeac_model %in% names(vaeac_list$models)) {
      # User provided a string which matches one of the file names and we use it.
      which_vaeac_model <- internal$parameters$vaeac.which_vaeac_model
    } else {
      # User provided a string which is not one of the file names. Overwrite it.
      which_vaeac_model <- "best"
      message(sprintf(
        "The provided string for 'which_vaeac_model' (%s) did not match any stored checkpoints (%s).\n
We set 'which_vaeac_model = best' and continue.\n",
        internal$parameters$vaeac.which_vaeac_model,
        paste(names(vaeac_list$models), collapse = ", ")
      ))
    }
  }

  # Create a path entry with the path to the vaeac model that we are to use.
  vaeac_list$path <- vaeac_list$models[[which_vaeac_model]]

  # This should clause should never occur as this function is only called inside the shapr package,
  # and then `index_features` will always be provided. However, I include this clause as
  # the other approaches test for this and I copy their setup and use all coalitions.
  if (is.null(index_features)) index_features <- seq(2, internal$parameters$n_combinations)

  # Get the index of the current batch
  current_batch_index <- internal$objects$X[id_combination == index_features[1]]$batch

  # Check if we are going to print out process to the user
  if (isTRUE(internal$parameters$vaeac.verbose)) {
    message(sprintf(
      "Starting 'prepare_data.vaeac' for batch %d of %d.",
      current_batch_index, internal$parameters$n_batches
    ))
  }

  # Extract the relevant masks.
  # Index_features is an array of numbers indicating which of the (sampled) coalitions we are considering.
  # If n_batches == 1, then index_features is an array of the indices to all (sampled) coalitions.
  # If n_batches >= 2, then index_features is a subset of all possible indices.
  # The matrix internal$objects$S contains all (sampled) coalitions, always including the empty
  # and grand coalition. We extract the relevant coalitions we are to generate MC samples from.
  mask <- internal$objects$S[index_features, , drop = FALSE]

  # Get the number of active coalitions.
  n_coaltions <- length(index_features)

  # Set zeros to indicate that the corresponding feature value is missing.
  mask[mask == 0] <- NaN

  # Create the extended version of x_explain where each observation is repeated n_coalitions times.
  x_explain_extended <- x_explain[rep(seq_len(nrow(x_explain)), each = n_coaltions), ]

  # Get the number of observations, after applying the mask, we are to generate MC samples for.
  n_explain_extended <- nrow(x_explain_extended)

  # Extend the mask to replicating it n_explain times.
  mask_extended <- mask[rep(seq(n_coaltions), times = n_explain), ]

  # Apply the mask. The NaN entries indicate the unconditional features, which we
  # are going to create n_sampled conditional MC samples for.
  x_explain_extended[is.na(mask_extended)] <- NaN

  # Extract/set the batch size. Larger batch sizes is often much faster provided sufficient memory.
  if (is.null(internal$parameters$vaeac.batch_size_sampling)) {
    # If user has not specified a desired size, then we do the whole batch in one go.
    # This is also indirectly controlled by n_batches in explain.
    batch_size <- n_explain_extended
  } else {
    # Use the user provided batch size
    batch_size <- internal$parameters$vaeac.batch_size_sampling

    # Check/set valid batch size
    if (batch_size > n_explain_extended) batch_size <- n_explain_extended
  }

  # Extract if we are doing random sampling when we are generating the MC samples
  # I.e., if we are to generate random samples from the inferred generative distributions,
  # or if we are to sample the most likely values (mean for cont, class with highest prob for cat).
  # We will always use random unless user specify otherwise.
  sample_random <-
    if (is.null(internal$parameters$vaeac.sample_random)) TRUE else internal$parameters$vaeac.sample_random

  # Check that `sample_random` is a boolean, otherwise we set it to true.
  if (!is.logical(sample_random)) {
    message(sprintf("The user-provided entry for 'sample_random' is not logical. We set it to TRUE.\n"))
    sample_random <- TRUE
  }

  # Impute the missing entries using the vaeac approach.
  x_explain_with_MC_samples_as_dt <- vaeac_impute_missing_entries(
    x_explain_with_NaNs = x_explain_extended,
    path_vaeac_model = vaeac_list$path,
    n_samples = n_samples,
    use_cuda = vaeac_list$parameters$use_cuda,
    sample_random = sample_random,
    convert_to_2D = TRUE,
    return_as_postprocessed_data_table = TRUE,
    batch_size = batch_size,
    verbose = internal$parameters$vaeac.verbose,
    seed = internal$parameters$seed,
    index_features = index_features
  )

  # Return the generated conditional Monte Carlo samples
  return(x_explain_with_MC_samples_as_dt)
}


# Train vaeac model ===================================================================================================
#' Train the Vaeac Model
#'
#' @description Function that fits a vaeac model to the given dataset based on the provided parameters,
#' as described in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#'
#' @details
#' The vaeac model consists of three neural networks, i.e., a masked encoder, a full encoder, and a decoder.
#' The networks have shared `depth`, `width`, and `activation_function`. The encoders maps the `training_data`
#' to a latent representation of dimension `latent_dim`, while the decoder maps the latent representations
#' back to the feature space. See \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}
#' for more details. The function first initiates `num_different_vaeac_initiate` vaeac models with different randomly
#' initiated network parameter values to remedy poorly initiated values. After `epochs_initiation_phase` epochs, the
#' `num_different_vaeac_initiate` vaeac models are compared and the function continues to only train the best performing
#' one for a total of `epochs` epochs. The networks are trained using the ADAM optimizer with the learning rate is `lr`.
#'
#' @param training_data A matrix or data.frame containing the data. Categorical data must have class names 1,2,...,K.
#' @param model_description String containing, e.g., the name of the data distribution or
#' additional parameter information. Used in the save name of the fitted model.
#' @param folder_to_save_model String specifying a path to a folder where
#' the function is to save the fitted vaeac model.
#' @param use_cuda Boolean. If we are to use cuda (GPU) if available. STILL IN DEVELOPMENT!
#' @param num_different_vaeac_initiate Integer. The number of different vaeac models to initiate in the start.
#' Pick the best performing one after \code{epochs_initiation_phase } and continue training that one.
#' @param epochs_initiation_phase Integer. The number of epochs to run each of the \code{num_different_vaeac_initiate}
#' vaeac models before only continuing training the best one.
#' @param epochs Integer. The number of epochs to train the final vaeac model.
#' This includes \code{epochs_initiation_phase}.
#' @param epochs_early_stopping Integer. The training stops if there has been no improvement in the validation IWAE
#' for `epochs_early_stopping` epochs. If the user wants the training process to be solely based on this, then `epochs`
#' should be set to a large number.
#' @param validation_ratio Scalar between 0 and 1 indicating the ratio of
#' instances from data which will be used as validation data.
#' @param validation_iwae_num_samples Integer. The number of samples used to compute the
#' IWAE when validating the vaeac model on the validation data.
#' @param depth Integer. The number of hidden layers in the neural
#' networks of the masked encoder, full encoder, and decoder.
#' @param width Integer. The number of neurons in each hidden layer in
#' the neural networks of the masked encoder, full encoder, and decoder.
#' @param latent_dim Integer. The number of dimensions in the latent space.
#' @param lr Numeric. The learning rate used in the \code{\link[torch]{optim_adam}} optimizer.
#' @param batch_size Integer. The number of samples to include in each batch.
#' @param running_avg_num_values Integer. How many of the previous values to include when we compute the running means.
#' @param activation_function An \code{\link[torch]{nn_module}} representing an activation function such as, e.g.,
#' \code{\link[torch]{nn_relu}}, \code{\link[torch]{nn_leaky_relu}}, \code{\link[torch]{nn_selu}}, and
#' \code{\link[torch]{nn_sigmoid}}.
#' @param use_skip_connections Boolean. If we are to use skip connections in each layer. If true, then we add the input
#' to the outcome of each hidden layer, so the output becomes X + activation(WX + b). I.e., identity skip connection.
#' @param skip_connection_masked_enc_dec Boolean. If we are to apply concatenate skip
#' connections between the layers in the masked encoder and decoder.
#' @param use_batch_normalization Boolean. If we are to use batch normalization after the activation function.
#' Note that if \code{use_skip_connections} is TRUE, then the normalization is
#' done after the adding from the skip connection. I.e, we batch normalize the whole quantity X + activation(WX + b).
#' @param paired_sampling Boolean. Default is `TRUE`. If we are doing paired sampling. I.e.,
#  each batch contains two versions of the same training observation, but where the first one is
#' masked by \eqn{S} and the second one is masked by \eqn{\bar{S}}, the complement, see
#' \href{https://arxiv.org/pdf/2107.07436.pdf}{Jethani et al. (2022)}. Training becomes more
#' stable, but slower due to more complex implementation.
#' @param masking_ratio Probability of masking a feature in the MCAR mask generator.
#' Default masking scheme which ensures that vaeac can do arbitrary conditioning.
#' This is overruled if \code{mask_generator_only_these_coalitions} is specified.
#' @param mask_generator_only_these_coalitions Matrix containing the different coalitions to learn.
#' @param mask_gen_these_coalitions_prob Numerics containing the probabilities for
#' sampling each mask in \code{mask_generator_only_these_coalitions}.
#' Array containing the probabilities for sampling the coalitions in \code{mask_generator_only_these_coalitions}.
#' @param sigma_mu Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder,
#' see Section 3.3.1 in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param sigma_sigma Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder,
#' see Section 3.3.1 in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param save_data Boolean. If we are to save the data together with the model. Useful if one are to continue
#' to train the model later.
#' @param transform_all_continuous_features Boolean. If we are to log transform all continuous features before
#' sending the data to vaeac. vaeac creates unbounded values, so if the continuous features are strictly positive,
#' as for Burr and Abalone data, it can be advantageous to log-transform the data to unbounded form before using vaeac.
#' If `TRUE`, then \code{vaeac_postprocess_data} will take the exp of the results
#' to get back to strictly positive values when using the vaeac model to impute missing values.
#' @param verbose Boolean. If we are to print the progress of the initialization of different vaeac models,
#' the training of the final vaeac model, and summary of the training progress.
#' @param save_vaeac_every_nth_epoch Integer. If we are to save the vaeac model after every nth epoch.
#' @param seed Integer. Seed for reproducibility.
#' @param ... List of extra parameters, currently not used.
#'
#' @return A list containing the training/validation errors and paths to where the vaeac models are saved on the disk.
#' @export
#' @author Lars Henry Berge Olsen
vaeac_train_model <- function(training_data,
                              model_description = NULL,
                              folder_to_save_model = NULL,
                              use_cuda = FALSE,
                              num_different_vaeac_initiate = 10,
                              epochs_initiation_phase = 2,
                              epochs = 200,
                              epochs_early_stopping = NULL,
                              save_vaeac_every_nth_epoch = NULL,
                              validation_ratio = 0.25,
                              validation_iwae_num_samples = 25,
                              depth = 3,
                              width = 32,
                              latent_dim = 8,
                              lr = 0.001,
                              batch_size = 64,
                              running_avg_num_values = 5,
                              activation_function = torch::nn_relu,
                              use_skip_connections = TRUE,
                              skip_connection_masked_enc_dec = TRUE,
                              use_batch_normalization = FALSE,
                              paired_sampling = TRUE,
                              masking_ratio = 0.5,
                              mask_generator_only_these_coalitions = NULL,
                              mask_gen_these_coalitions_prob = NULL,
                              sigma_mu = 1e4,
                              sigma_sigma = 1e-4,
                              save_data = FALSE,
                              transform_all_continuous_features = FALSE,
                              verbose = FALSE,
                              seed = NULL,
                              ...) {
  # Some checks. Not an exhaustive list of checks.
  if (is.null(epochs_early_stopping)) epochs_early_stopping <- epochs
  if (epochs_early_stopping > epochs) {
    message(sprintf(
      "No early stopping as `epochs_early_stopping` (%d) is larger than `epochs` (%d).",
      epochs_early_stopping, epochs
    ))
  }

  # Variable to store if early stopping was conducted
  early_stopping_applied <- NULL

  if (!is.numeric(num_different_vaeac_initiate)) {
    stop(sprintf(
      "The 'num_different_vaeac_initiate' parameter must be of type numeric, and not of type %s.\n",
      paste(class(num_different_vaeac_initiate), collapse = ", ")
    ))
  } else if (num_different_vaeac_initiate < 1) {
    message(sprintf(
      "The 'num_different_vaeac_initiate' (%g) parameter must be a positive integer. We set it to 1.\n",
      num_different_vaeac_initiate
    ))
    num_different_vaeac_initiate <- 1
  }

  if (epochs_initiation_phase >= epochs) {
    message(sprintf(
      "The 'epochs_initiation_phase' (%g) parameter must be strictly lower than 'epochs' (%g). We set epochs = %g.\n",
      epochs_initiation_phase,
      epochs,
      epochs_initiation_phase + 1
    ))
    epochs <- epochs_initiation_phase + 1
  }

  # If no folder has been provided, we save the model in a temporary directory
  # which will be deleted when the R session is closed.
  if (is.null(folder_to_save_model)) {
    folder_to_save_model <- tempdir()
    used_tempdir <- TRUE
  } else {
    used_tempdir <- FALSE
  }

  # If no model_description has been provided, then we use the current time.
  if (is.null(model_description)) {
    options(digits.secs = 3)
    model_description <- gsub("\\.", "_", gsub(" ", "_", Sys.time()))
    options(digits.secs = 0)
  }

  # Preprocess the data. This function turns factor names into numerics 1,2,...,K,
  # as vaeac only accepts numerics, and keep track of the maping of names.
  # And optionally log-transform all continuous features. Usual for strictly positive
  # data set like Burr and Abalone, such that vaeac does not impute negative values.
  preprocessed_data <- vaeac_preprocess_data(as.data.table(training_data), transform_all_continuous_features)

  # Extract the training data where all the
  training_data <- preprocessed_data$data_preprocessed

  # A torch tensor of dimension p containing the one hot sizes of the p features.
  # The sizes for the continuous features can either be '0' or '1'.
  one_hot_max_sizes <- preprocessed_data$one_hot_max_sizes

  ##### Do some checks for invalid input parameter values
  # Do not check that integers are integers and that strings are strings.
  # Take for granted that the user reads the documentation.

  # Check for a valid file path
  if (!dir.exists(folder_to_save_model)) {
    stop(sprintf("Directory '%s' in `folder_to_save_model` does not exist.\n", folder_to_save_model))
  }

  # Remove trailing slash on path name if there are any.
  if (endsWith(folder_to_save_model, "/")) {
    folder_to_save_model <- substr(folder_to_save_model, 1, nchar(folder_to_save_model) - 1)
  }

  # Check that we initiate at least one vaeac model in the
  if (num_different_vaeac_initiate < 1) {
    stop(sprintf(
      "The parameter 'num_different_vaeac_initiate' (%d) must be equal or larger than 1.",
      num_different_vaeac_initiate
    ))
  }

  # Check if cuda/GPU is available on the current system
  cuda_available <- torch::cuda_is_available()

  # Give message to user if asked to run on cuda, but cuda is not available.
  if (isFALSE(cuda_available) && isTRUE(use_cuda)) {
    use_cuda <- FALSE
    message("Cuda/GPU is not available. Uses CPU instead.", immediate. = TRUE)
  }

  # Check for coinciding number of features in data and the number of features specified in one_hot_max_sizes.
  if (ncol(training_data) != length(one_hot_max_sizes)) {
    stop(sprintf(
      "The number of features in training_data must match the length of `one_hot_max_sizes`: %d != %s.\n",
      ncol(training_data),
      length(one_hot_max_sizes)
    ))
  }

  # Check if
  if (xor(
    !is.null(mask_generator_only_these_coalitions),
    !is.null(mask_gen_these_coalitions_prob)
  )) {
    stop(paste0(
      "User need to provided both 'mask_generator_only_these_coalitions' and ",
      "'mask_gen_these_coalitions_prob' for specified masking to function."
    ))
  }

  ##### Figure out what kind of mask generator we are going to use.
  if (!is.null(mask_generator_only_these_coalitions) && !is.null(mask_gen_these_coalitions_prob)) {
    # Both are provided and we want to use Specified_masks_mask_generator

    # Check that the possible masks that are provided is given as a matrix
    if (!any(class(mask_generator_only_these_coalitions) == "matrix")) {
      stop(sprintf(
        "The 'mask_generator_only_these_coalitions' must be of class 'matrix', not %s.\n",
        paste(class(mask_generator_only_these_coalitions), collapse = ", ")
      ))
    }

    # Check that the number of masks and corresponding number of probabilities match.
    if (nrow(mask_generator_only_these_coalitions) != length(mask_gen_these_coalitions_prob)) {
      stop(sprintf(
        "The number of coalitions ('%d') does not match with the number of provided probabilites ('%d').\n",
        nrow(mask_generator_only_these_coalitions), length(mask_gen_these_coalitions_prob)
      ))
    }

    # We are given possible coalitions and corresponding probabilities.
    # Then we are using the Specified_masks_mask_generator.
    if (verbose) {
      message(sprintf(
        "Use 'Specified_masks_mask_generator' mask generator with '%d' different possible coalitions.\n",
        nrow(mask_generator_only_these_coalitions)
      ))
    }
    mask_generator_name <- "Specified_masks_mask_generator"
  } else {
    # We are NOT going to use 'Specified_masks_mask_generator'. Figure out if we are using
    # 'MCAR_mask_generator' or 'Specified_probability_mask_generator' and check for valid input.

    # Check that masking_ratio is numeric.
    if (all(class(masking_ratio) != "numeric")) {
      stop(sprintf("class of 'masking_ratio' must be numeric, not %s.\n", class(masking_ratio)))
    }

    # Masking ration is then either a scalar or array of scalar.
    if (length(masking_ratio) == 1) {
      # Only one masking ration, so we are going to use MCAR_mask_generator where each feature value
      # is going to be masked with this probability independently of if another features is masked.
      if (verbose) message(sprintf("Use 'MCAR_mask_generator' with 'masking_ratio = %g'.\n", masking_ratio))
      mask_generator_name <- "MCAR_mask_generator"
    } else {
      # Check that we have received a masking ratio for each feature
      if (length(masking_ratio) == ncol(training_data)) {
        # We have an array of masking ratios. Then we are using the Specified_probability_mask_generator.
        if (verbose) {
          message(sprintf(
            "Use 'Specified_probability_mask_generator' mask generator with 'masking_ratios = {%s}'.\n",
            paste(masking_ratio, collapse = ", ")
          ))
        }
        mask_generator_name <- "Specified_probability_mask_generator"
      } else {
        stop(sprintf(
          "'Masking_ratio' contains masking ratios for '%d' features, but there are '%d' features in 'training_data'.\n",
          length(masking_ratio), ncol(training_data)
        ))
      }
    }
  }

  #### Normalize training_data
  # Get the dimensions of the training_data
  n <- nrow(training_data)
  p <- ncol(training_data)

  # Convert X to tensor
  data_torch <- torch::torch_tensor(as.matrix(training_data))

  # Compute the mean and std for each continuous feature in the data
  # The categorical features will have mean zero and std 1.
  mean_and_sd <- compute_normalization(data_torch, one_hot_max_sizes)
  norm_mean <- mean_and_sd$norm_vector_mean
  norm_std <- mean_and_sd$norm_vector_std

  # Make sure that the standard deviation is not too low, in that case clip it.
  norm_std <- norm_std$max(other = torch::torch_tensor(1e-9))

  # normalize the data to have mean = 0 and std = 1.
  data <- (data_torch - norm_mean) / norm_std

  #### Split Training & Validation Data
  # Splitting the input data into training and validation sets
  # Find the number of instances in the validation set
  val_size <- ceiling(n * validation_ratio)

  # Set seed for reproducibility
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

  # randomly sample indices for the validation set
  val_indices <- sample(n, val_size, replace = FALSE)

  # Get the indices that are not in the validation set.
  train_indices <- seq(n)[-val_indices]

  # Split the data into a training and validation set
  train_data <- data[train_indices]
  val_data <- data[val_indices]

  ##### Datasets and Dataloaders
  if (length(train_indices) <= batch_size) {
    if (length(train_indices) %% 2 != 0) {
      batch_size_new <- (length(train_indices) + 1) / 2
    } else {
      batch_size_new <- length(train_indices) / 2
    }
    message(sprintf(
      "Provided batch_size (%d) is larger than the number of training observations (%d). Set batch_size = %d.\n",
      batch_size, length(train_indices), batch_size_new
    ), immediate. = TRUE)
    batch_size <- batch_size_new
  }

  # Create the Data Set objects
  train_dataset <- vaeac_dataset(train_data, one_hot_max_sizes)
  val_dataset <- vaeac_dataset(val_data, one_hot_max_sizes)

  # Create the Data Loader object which can iterate over the data in the Data Set object
  # See more parameters here '?dataloader', but these are the most important.
  if (paired_sampling) {
    # Use paired sampling
    train_dataloader <- torch::dataloader(train_dataset,
      batch_size = batch_size,
      sampler = paired_sampler(train_dataset, shuffle = TRUE)
    )
    val_dataloader <- torch::dataloader(val_dataset,
      batch_size = batch_size,
      sampler = paired_sampler(val_dataset, shuffle = FALSE)
    )
  } else {
    # Usual approach
    train_dataloader <- torch::dataloader(train_dataset, batch_size = batch_size, shuffle = TRUE)
    val_dataloader <- torch::dataloader(val_dataset, batch_size = batch_size, shuffle = FALSE)
  }


  ##### List that stores needed information for save and load the model
  # List to values saved to disk together with the vaeac models below.
  state_list <- list(
    "norm_mean" = norm_mean,
    "norm_std" = norm_std,
    "model_description" = model_description,
    "folder_to_save_model" = folder_to_save_model,
    "used_tempdir" = used_tempdir,
    "n" = n,
    "p" = p,
    "one_hot_max_sizes" = one_hot_max_sizes,
    "epochs" = epochs,
    "epochs_specified" = epochs,
    "epochs_early_stopping" = epochs_early_stopping,
    "running_avg_num_values" = running_avg_num_values,
    "paired_sampling" = paired_sampling,
    "mask_generator_name" = mask_generator_name,
    "masking_ratio" = masking_ratio,
    "mask_generator_only_these_coalitions" = mask_generator_only_these_coalitions,
    "mask_gen_these_coalitions_prob" = mask_gen_these_coalitions_prob,
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
    "skip_connection_masked_enc_dec" = skip_connection_masked_enc_dec,
    "use_batch_normalization" = use_batch_normalization,
    "use_cuda" = use_cuda,
    "train_indices" = train_indices,
    "val_indices" = val_indices,
    "save_vaeac_every_nth_epoch" = save_vaeac_every_nth_epoch,
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
    "seed" = seed
  )

  # If we are also to save the data to state_list.
  if (save_data) {
    state_list <- c(state_list, list(
      "training_data" = training_data,
      "normalized_data" = data
    ))

    # Just a small message regarding large disk usage
    if (!is.null(save_vaeac_every_nth_epoch)) {
      message(sprintf(
        "Both having 'save_data = TRUE' and saving the vaeac model every '%d'
epoch might require a lot of disk storage if data is large.\n",
        save_vaeac_every_nth_epoch
      ), immediate. = TRUE)
    }
  }

  # Check if we are to save vaeac model every n'th epoch.
  if (!is.null(save_vaeac_every_nth_epoch)) {
    # List of file names for vaeac models after every n'th epoch (save_vaeac_every_nth_epoch).
    filename_nth_list <- list()

    # Check that save_vaeac_every_nth_epoch is positive.
    if (save_vaeac_every_nth_epoch <= 0) {
      stop(sprintf(
        "The value 'save_vaeac_every_nth_epoch' must be strictly positive, not '%d'.\n",
        save_vaeac_every_nth_epoch
      ))
    }

    # Ensure a valid value for save_vaeac_every_nth_epoch.
    if (save_vaeac_every_nth_epoch > epochs) {
      stop(sprintf(
        "Number of 'epochs' is less than 'save_vaeac_every_nth_epoch': %d < %d.\n",
        epochs, save_vaeac_every_nth_epoch
      ))
    }
  }

  ##### Initializing vaeac models
  # Initialize several vaeac models and keep the one with the best training variational lower bound
  # after a given number of epochs. Keep the version with highest vlb, denoted by "best_vlb"..
  best_vlb <- -Inf

  # Variables to stores the state of the `vaeac` at the best epoch according to IWAE and IWAE_running
  best_state <- NULL
  best_state_running <- NULL

  # Create a `progressr::progressor` to keep track of the overall training time of the vaeac approach
  progressr_bar <- progressr::progressor(steps = epochs_initiation_phase * (num_different_vaeac_initiate - 1) + epochs)

  # Iterate over the initializations.
  initialization <- 1
  for (initialization in seq(num_different_vaeac_initiate)) {
    # Initialize a new vaeac model
    model <- vaeac(
      one_hot_max_sizes = one_hot_max_sizes,
      width = width,
      depth = depth,
      latent_dim = latent_dim,
      activation_function = activation_function,
      use_skip_connections = use_skip_connections,
      skip_connection_masked_enc_dec = skip_connection_masked_enc_dec,
      use_batch_normalization = use_batch_normalization,
      paired_sampling = paired_sampling,
      mask_generator_name = mask_generator_name,
      masking_ratio = masking_ratio,
      mask_generator_only_these_coalitions = mask_generator_only_these_coalitions,
      mask_gen_these_coalitions_prob = mask_gen_these_coalitions_prob,
      sigma_mu = sigma_mu,
      sigma_sigma = sigma_sigma
    )

    # Check if we are providing more output for easier debugging
    if (verbose) {
      # Print the number of trainable parameters to the user
      if (initialization == 1) {
        message(sprintf(
          "The number of trainable parameters in the vaeac model is '%d'.",
          model$num_trainable_params[1, 1]
        ))
      }

      # Print which initialization vaeac the function is working on
      message(sprintf("\nInitializing vaeac number %d...", initialization))

      # Create a progress bar for the individual initialization vaeac.
      # Note that we will not see this `progress::progress_bar` move/update if
      # the `progressr` library is used. Then this will just print out
      # the finished `progress::progress_bar`.
      pb <- progress::progress_bar$new(
        format = paste(
          "(:spin) [:bar] :percent [vaeac: #:initialization | time: :elapsedfull |",
          "ETR: :eta | Epoch: :epoch | VLB: :vlb | IWAE: :iwae | IWAE_R: :runningiwae]"
        ),
        total = epochs_initiation_phase,
        complete = "=", # Completion bar character
        incomplete = "-", # Incomplete bar character
        current = ">", # Current bar character
        clear = FALSE, # If TRUE, clears the bar when finish
        width = 125
      ) # Width of the progress bar
    }

    # Extract the variational lower bound scale factor and mask generator from the vaeac model object.
    vlb_scale_factor <- model$vlb_scale_factor
    mask_generator <- model$mask_generator

    # Create the ADAM optimizer
    optimizer <- torch::optim_adam(
      params = model$parameters,
      lr = lr,
      betas = c(0.9, 0.999),
      eps = 1e-08,
      weight_decay = 0,
      amsgrad = FALSE
    )

    # An array to store the regular and running validation IWAE errors
    validation_iwae <- c()
    validation_iwae_running_avg <- c()

    # An array of running variational lower bounds on the train set
    train_vlb <- c()

    epoch <- 1
    # Start the training loop
    for (epoch in seq(epochs_initiation_phase)) {
      ## First we do one epoch of training before
      # Set average variational lower bound to 0 for this epoch
      avg_vlb <- 0

      # Array to keep track of the training errors (i.e., VLB)
      training_error_batch <- c()

      # Index to keep track of which batch we are working on. Only used for progress bar.
      batch_index <- 1

      batch <- train_dataloader$.iter()$.next()
      # Iterate over the training data
      coro::loop(for (batch in train_dataloader) {
        # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
        if (batch$shape[1] < batch_size) {
          batch <- extend_batch(
            batch = batch,
            dataloader = train_dataloader,
            batch_size = batch_size
          )
        }

        # Generate mask and do an optimizer step over the mask and the batch
        mask <- mask_generator(batch)

        # Set all previous gradients to zero.
        optimizer$zero_grad()

        # Compute the variational lower bound for the batch given the mask
        vlb <- model$batch_vlb(batch, mask)$mean()

        # Backpropagation: minimize the negative vlb.
        vlb_loss <- (-vlb / vlb_scale_factor)
        vlb_loss$backward()

        # Update the model parameters by using ADAM.
        optimizer$step()

        # Update running variational lower bound average
        avg_vlb <- avg_vlb + (vlb$to(dtype = torch::torch_float())$clone()$detach() - avg_vlb) / batch_index

        # Update the batch index.
        batch_index <- batch_index + 1
      })

      ## Done one new epoch of training. Time to evaluate the model on the validation data.
      # Compute the validation iwae
      val_iwae <- get_validation_iwae(
        val_dataloader,
        mask_generator,
        batch_size,
        model,
        validation_iwae_num_samples,
        verbose
      )

      # Add the current validation_iwae and train_vlb to the lists.
      validation_iwae <- torch::torch_cat(c(validation_iwae, val_iwae), -1)
      train_vlb <- torch::torch_cat(c(train_vlb, avg_vlb), -1)

      # Compute the running validation IWAE
      val_iwae_running <- validation_iwae[
        (-min(length(validation_iwae), running_avg_num_values) +
          length(validation_iwae) + 1):(-1 + length(validation_iwae) + 1),
        drop = FALSE
      ]$mean()$view(1)
      validation_iwae_running_avg <- torch::torch_cat(c(validation_iwae_running_avg, val_iwae_running), -1)

      # If printing debug messages
      if (verbose) {
        # Updates the current state of the progress bar for the individual vaeac initialization model
        pb$tick(tokens = list(
          initialization = initialization,
          epoch = epoch,
          vlb = round(avg_vlb$item(), 3),
          iwae = round(val_iwae$item(), 3),
          runningiwae = round(validation_iwae_running_avg[-1]$item(), 3)
        ))
      }

      # Update the overall `progressr::progressor`.
      progressr_bar(message = sprintf("Training vaeac (init. %d of %d) ", initialization, num_different_vaeac_initiate))
    } # Done with initial training of a single vaeac model

    # Save the current vaeac model, if it is the best initialized version so far.
    if ((best_vlb <= avg_vlb)$item()) {
      best_vlb <- avg_vlb
      best_iteration <- initialization
      best_model <- model
      best_validation_iwae <- validation_iwae
      best_validation_iwae_running_avg <- validation_iwae_running_avg
      best_train_vlb <- train_vlb
      best_optimizer <- optimizer
      best_batch_size <- batch_size
      best_mask_generator <- mask_generator
      best_vlb_scale_factor <- vlb_scale_factor
    }
  } # Done with initial training of all vaeac models

  # Load the best initialized vaeac model and continue training.
  # networks = best_networks
  model <- best_model
  validation_iwae <- best_validation_iwae
  validation_iwae_running_avg <- best_validation_iwae_running_avg
  train_vlb <- best_train_vlb
  optimizer <- best_optimizer
  batch_size <- best_batch_size
  mask_generator <- best_mask_generator
  vlb_scale_factor <- best_vlb_scale_factor

  # Include the number of trainable parameters in the state list.
  state_list <- c(state_list, list("num_trainable_parameters" = model$num_trainable_params))

  # Send the model to the GPU, if we have access to it.
  if (use_cuda) model <- model.cuda()

  # Check if we are printing detailed debug information
  if (verbose) {
    # Small printout to the user stating which initiated vaeac model was the best.
    message(sprintf(
      "\nNumber of vaeac initiations: %d. Best: #%d. VLB: %.3f after %d epochs.\nContinue with training the best initiation.",
      num_different_vaeac_initiate, best_iteration, best_train_vlb[-1], epochs_initiation_phase
    ))

    # Create a progress bar for the remaining epochs for the final/used vaeac model.
    # Note that we will not see this `progress::progress_bar` move/update if
    # the `progressr` library is used. Then this will just print out
    # the finished `progress::progress_bar`.
    # Should maybe include width, depth, latent_dim, lr, if doing hyperparameter tuning.
    pb <- progress::progress_bar$new(
      format = paste(
        "(:spin) [:bar] :percent [time: :elapsedfull | ETR: :eta |",
        "Epoch: :epoch | Best epoch: :be | VLB: :vlb | IWAE: :iwae | IWAE_R: :runningiwae]"
      ),
      total = (epochs - epochs_initiation_phase),
      complete = "=", # Completion bar character
      incomplete = "-", # Incomplete bar character
      current = ">", # Current bar character
      clear = FALSE, # If TRUE, clears the bar when finish
      width = 125
    ) # Width of the progress bar
  }

  # Continue training the best vaeac model
  for (epoch in seq(epochs_initiation_phase + 1, epochs)) {
    # Set iterator to be the data loader which loads the training data.
    iterator <- torch::dataloader

    # Set average variational lower bound to 0 for this epoch
    avg_vlb <- 0

    # index to keep track of which batch we are working on
    batch_index <- 1

    # Iterate over the training data
    coro::loop(for (batch in train_dataloader) {
      # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
      if (batch$shape[1] < batch_size) {
        batch <- extend_batch(
          batch = batch,
          dataloader = train_dataloader,
          batch_size = batch_size
        )
      }

      # Generate mask and do an optimizer step over the mask and the batch
      mask <- mask_generator(batch)

      # Send the batch and mask to Nvida GPU if we have. Would be faster.
      if (use_cuda) {
        batch <- batch$cuda()
        mask <- mask$cuda()
      }

      # Set all previous gradients to zero.
      optimizer$zero_grad()

      # Compute the variational lower bound for the batch given the mask
      vlb <- model$batch_vlb(batch, mask)$mean()

      # Backpropagation: minimize the negative vlb.
      vlb_loss <- (-vlb / vlb_scale_factor)
      vlb_loss$backward()

      # Update the model parameters by using the optimizer.
      optimizer$step()

      # Update running variational lower bound average
      # a + (new - a)/(i+1) = {(i+1)a + new - a}/(i+1) = { a(i) + new}/(i+1) = a *i/(i+1) + new/(i+1)
      # recursive average formula/update.
      avg_vlb <- avg_vlb + (vlb$to(dtype = torch::torch_float())$clone()$detach() - avg_vlb) / batch_index

      # Update the batch index.
      batch_index <- batch_index + 1
    })

    # Done with one new epoch of training. Time to use the model on the validation data.
    # Time to evaluate the model on the validation data. Compute the validation IWAE.
    val_iwae <- get_validation_iwae(
      val_dataloader,
      mask_generator,
      batch_size,
      model,
      validation_iwae_num_samples,
      verbose
    )

    # Compute the running validation IWAE.
    val_iwae_running <- validation_iwae[
      (-min(length(validation_iwae), running_avg_num_values) +
        length(validation_iwae) + 1):(-1 + length(validation_iwae) + 1),
      drop = FALSE
    ]$mean()$view(1)

    # Add the current validation_iwae and train_vlb to the lists.
    validation_iwae <- torch::torch_cat(c(validation_iwae, val_iwae), -1)
    train_vlb <- torch::torch_cat(c(train_vlb, avg_vlb), -1)
    validation_iwae_running_avg <- torch::torch_cat(c(validation_iwae_running_avg, val_iwae_running), -1)

    # Save if current vaeac model has the lowest validation IWAE error
    if ((max(validation_iwae) <= val_iwae)$item() || is.null(best_state)) {
      best_state <- c(
        list(
          "epoch" = epoch,
          "model_state_dict" = model$state_dict(),
          "optimizer_state_dict" = optimizer$state_dict(),
          "validation_iwae" = validation_iwae,
          "validation_iwae_running_avg" = validation_iwae_running_avg,
          "running_avg_num_values" = running_avg_num_values,
          "train_vlb" = train_vlb
        ),
        state_list
      )

      # Create the file name
      filename_best <-
        paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), "p", p,
          "n", n, "depth", depth, "width", width, "latent", latent_dim, "lr", lr, "best.pt",
          sep = "_"
        )

      # Combine the file name with the folder path to form the final save file name.
      filename_best <- file.path(folder_to_save_model, filename_best)
      class(best_state) <- c(class(best_state), "R_vaeac", "vaeac")
      torch::torch_save(best_state, filename_best)
    }

    # Save if current vaeac model has the lowest validation IWAE error
    if ((max(validation_iwae_running_avg) <= val_iwae_running)$item() || is.null(best_state_running)) {
      best_state_running <- c(
        list(
          "epoch" = epoch,
          "model_state_dict" = model$state_dict(),
          "optimizer_state_dict" = optimizer$state_dict(),
          "validation_iwae" = validation_iwae,
          "validation_iwae_running_avg" = validation_iwae_running_avg,
          "running_avg_num_values" = running_avg_num_values,
          "train_vlb" = train_vlb
        ),
        state_list
      )

      # Create the file name
      filename_best_running <-
        paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), "p", p,
          "n", n, "depth", depth, "width", width, "latent", latent_dim, "lr", lr,
          "best_running.pt",
          sep = "_"
        )

      # Combine the file name with the folder path to form the final save file name.
      filename_best_running <- file.path(folder_to_save_model, filename_best_running)
      class(best_state_running) <- c(class(best_state_running), "R_vaeac", "vaeac")
      torch::torch_save(best_state_running, filename_best_running)
    }

    # If we are to save and we are in an n'th epoch, then we save the model.
    if (!is.null(save_vaeac_every_nth_epoch)) {
      if (epoch %% save_vaeac_every_nth_epoch == 0) {
        nth_state <- c(
          list(
            "epoch" = epoch,
            "model_state_dict" = model$state_dict(),
            "optimizer_state_dict" = optimizer$state_dict(),
            "validation_iwae" = validation_iwae,
            "validation_iwae_running_avg" = validation_iwae_running_avg,
            "running_avg_num_values" = running_avg_num_values,
            "train_vlb" = train_vlb
          ),
          state_list
        )

        # Create the file name
        filename_nth <- paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), "_p_", p,
          "_n_", n, "_depth_", depth, "_width_", width, "_latent_", latent_dim, "_lr_", lr,
          "_epoch_", epoch, ".pt",
          sep = ""
        )

        # Combine the file name with the folder path to form the final save file name.
        filename_nth <- file.path(folder_to_save_model, filename_nth)
        class(nth_state) <- c(class(nth_state), "R_vaeac", "vaeac")
        torch::torch_save(nth_state, filename_nth)

        # Add file name to list over file names.
        tmp_list <- list(filename_nth)
        names(tmp_list) <- paste("filename_epoch_", epoch, sep = "")
        filename_nth_list <- append(filename_nth_list, tmp_list)
      }
    }

    # If printing debug messages
    if (verbose) {
      # Updates the current state of the progress bar for the final/used vaeac initialization model
      pb$tick(tokens = list(
        epoch = epoch,
        be = best_state$epoch,
        vlb = round(avg_vlb$item(), 3),
        iwae = round(val_iwae$item(), 3),
        runningiwae = round(validation_iwae_running_avg[-1]$item(), 3)
      ))
    }

    # Update the overall `progressr::progressor`.
    progressr_bar(message = sprintf("Training vaeac (final version)"))

    # Early stopping
    if (epoch - best_state$epoch >= epochs_early_stopping) {
      if (verbose) {
        # Small printout to the user
        message(sprintf(
          "\n\nEarly stopping at epoch %d. No validation improvment has been made in %d epochs.",
          epoch, epochs_early_stopping
        ))
        # Terminate the progress bar
        pb$terminate()
      }
      # Change the message in the progressr bar
      progressr_bar(message = sprintf("Training vaeac (early stopping)"))

      # Add that we did early stopping
      early_stopping_applied <- TRUE

      # Stop the training loop
      break
    }
  } # Done with training

  # If it is still null, then early stopping was not applied
  if (is.null(early_stopping_applied)) early_stopping_applied <- FALSE

  # Include if early stopping was conducted in the state list.
  state_list <- c(state_list, list("early_stopping_applied" = early_stopping_applied))
  # Update the number of used epochs.
  state_list$epochs <- epoch

  # Save the model at the last epoch
  last_state <- c(
    list(
      "epoch" = epoch,
      "model_state_dict" = model$state_dict(),
      "optimizer_state_dict" = optimizer$state_dict(),
      "validation_iwae" = validation_iwae,
      "validation_iwae_running_avg" = validation_iwae_running_avg,
      "running_avg_num_values" = running_avg_num_values,
      "train_vlb" = train_vlb
    ),
    state_list
  )

  # Create the file name
  filename_last <- paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), "p", p,
    "n", n, "depth", depth, "width", width, "latent", latent_dim, "lr", lr, "last.pt",
    sep = "_"
  )

  # Combine the file name with the folder path to form the final save file name.
  filename_last <- file.path(folder_to_save_model, filename_last)
  class(last_state) <- c(class(last_state), "R_vaeac", "vaeac")
  torch::torch_save(last_state, filename_last)

  # Printout to the user
  if (verbose) {
    message(sprintf(
      "\nResults:
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
      last_state$validation_iwae_running_avg[-1]
    ))
  }

  # Create a return list
  return_list <- list(
    "filename_best" = filename_best,
    "filename_best_running" = filename_best_running,
    "filename_last" = filename_last,
    "train_vlb" = as.array(train_vlb),
    "validation_iwae" = as.array(validation_iwae),
    "validation_iwae_running_avg" = as.array(validation_iwae_running_avg),
    "parameters" = state_list
  )

  # If we are to add the 'filename_nth_list' list to the return list.
  if (!is.null(save_vaeac_every_nth_epoch)) {
    return_list <- append(return_list, filename_nth_list, 3)
  }

  # Update the class of the returned object
  attr(return_list, "class") <- c("R_vaeac", "vaeac", "list")

  # Return the paths where the models are saved and the training/validation errors.
  return(return_list)
}


#' Continue to Train the Vaeac model
#'
#' @description Function that loads a previously trained vaeac model and continue the training, either
#' on new data or on the same dataset as it was trained on before. If we are given a new dataset, then
#' we assume that new dataset has the same distribution and one_hot_max_sizes as the original dataset.
#'
#' @param vaeac_model The output revived from the 'vaeac_train_model' function.
#' @param epochs_new Integer. The number of extra epochs to conduct.
#' @param lr_new Numeric. If we are to overwrite the old learning rate in the adam optimizer.
#' @param training_data Matrix/data.frame containing new training data. If not present,
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
                                       training_data = NULL,
                                       save_data = FALSE,
                                       verbose = FALSE,
                                       ...) {
  # Keep track of how much time we use training
  new_training_time <- system.time({
    # Extract the vaeac list
    vaeac_model <- explanation$internal$parameters$vaeac

    # Load the vaeac model from provided disk location.
    checkpoint <- torch::torch_load(vaeac_model$models$last)

    # Check that we have access to training data
    if (is.null(checkpoint$normalized_data) & is.null(training_data)) {
      stop(paste(
        "The save file did not include data (set 'save_data' = TRUE in 'vaeac_train_model)",
        "and data was not provided to this function."
      ))
    }

    # We have two training datasets
    if (!is.null(checkpoint$training_data) & !is.null(data)) {
      message("The save file includes data and data was not provided to this function. We only use the latter.")
    }

    # If training_data is not provided to this function, then we load the training_data from the save file.
    if (is.null(training_data)) {
      training_data <- checkpoint$training_data
    }

    # Preprocess the data. This function turns factor names into numerics 1,2,...,K,
    # as vaeac only accepts numerics, and keep track of the maping of names.
    # And optionally log-transform all continuous features. Usual for strictly positive
    # data set like Burr and Abalone, such that vaeac does not impute negative values.
    preprocessed_data <- vaeac_preprocess_data(
      as.data.table(training_data),
      checkpoint$transform_all_continuous_features
    )

    # Extract the training data where all the
    training_data <- preprocessed_data$data_preprocessed

    # Extract relevant information from the checkpoint
    batch_size <- checkpoint$batch_size
    one_hot_max_sizes <- checkpoint$one_hot_max_sizes
    save_vaeac_every_nth_epoch <- checkpoint$save_vaeac_every_nth_epoch
    validation_iwae_num_samples <- checkpoint$validation_iwae_num_samples
    running_avg_num_values <- checkpoint$running_avg_num_values
    use_cuda <- checkpoint$use_cuda
    model_description <- checkpoint$model_description
    paired_sampling <- checkpoint$paired_sampling
    depth <- checkpoint$depth
    width <- checkpoint$width
    latent_dim <- checkpoint$latent_dim
    lr <- checkpoint$lr
    folder_to_save_model <- checkpoint$folder_to_save_model

    # Check if cuda/GPU is available on the current system
    cuda_available <- torch::cuda_is_available()

    # Give message to user if asked to run on cuda, but cuda is not available.
    if (isFALSE(cuda_available) && isTRUE(use_cuda)) {
      use_cuda <- FALSE
      message("Cuda/GPU is not available. Uses CPU instead.", immediate. = TRUE)
    }

    #### Normalize training_data
    # Get the dimensions of the training_data
    n <- nrow(training_data)
    p <- ncol(training_data)

    # Test for right number of features.
    if (p != checkpoint$p) {
      stop(sprintf(
        "The dimensions of current training data do not match the original dimension: %d != %d",
        p, checkpoint$p
      ))
    }

    # Convert X to tensor
    data_torch <- torch::torch_tensor(as.matrix(training_data))

    # Compute the mean and std for each continuous feature in the data
    # The categorical features will have mean zero and std 1.
    mean_and_sd <- compute_normalization(data_torch, one_hot_max_sizes)
    norm_mean <- mean_and_sd$norm_vector_mean
    norm_std <- mean_and_sd$norm_vector_std

    # Make sure that the standard deviation is not too low, in that case clip it.
    norm_std <- norm_std$max(other = torch::torch_tensor(1e-9))

    # normalize the data to have mean = 0 and std = 1.
    data <- (data_torch - norm_mean) / norm_std

    #### Split Training & Validation Data
    if (!is.null(checkpoint$training_data) | n == checkpoint$n) {
      # We are using the data from the saved object, or the new
      # data has the same number of training observations.

      # Can then just extract the validation and training indices
      val_indices <- checkpoint$val_indices
      train_indices <- checkpoint$train_indices
    } else {
      # We have new training data with a different number of training observations.

      # Splitting the input data into training and validation sets
      # Find the number of instances in the validation set
      val_size <- ceiling(n * checkpoint$validation_ratio)

      # randomly sample indices for the validation set
      val_indices <- sample(n, val_size, replace = FALSE)

      # Get the indices that are not in the validation set.
      train_indices <- seq(n)[-val_indices]
    }

    # Split the data into a training and validation set
    train_data <- data[train_indices]
    val_data <- data[val_indices]

    ##### Datasets and Dataloaders
    if (length(train_indices) <= batch_size) {
      message(sprintf(
        "Provided batch_size (%d) is larger than the number of training observations (%d). Set batch_size = %d.\n",
        batch_size, length(train_indices), length(train_indices)
      ), immediate. = TRUE)
      batch_size <- length(train_indices)
    }

    # Create the Data Set objects
    train_dataset <- vaeac_dataset(train_data, one_hot_max_sizes)
    val_dataset <- vaeac_dataset(val_data, one_hot_max_sizes)

    # Create the Data Loader object which can iterate over the data in the Data Set object
    # See more parameters here '?dataloader', but these are the most important.
    if (paired_sampling) {
      # Use paired sampling
      train_dataloader <- torch::dataloader(train_dataset,
        batch_size = batch_size,
        sampler = paired_sampler(train_dataset, shuffle = TRUE)
      )
      val_dataloader <- torch::dataloader(val_dataset,
        batch_size = batch_size,
        sampler = paired_sampler(val_dataset, shuffle = FALSE)
      )
    } else {
      # Usual approach
      train_dataloader <- torch::dataloader(train_dataset, batch_size = batch_size, shuffle = TRUE)
      val_dataloader <- torch::dataloader(val_dataset, batch_size = batch_size, shuffle = FALSE)
    }

    ##### List that stores needed information for save and load the model
    # List to values saved to disk together with the vaeac models below.
    state_list_new <- list(
      "norm_mean" = norm_mean,
      "norm_std" = norm_std,
      "n" = n,
      "epochs_new" = epochs_new,
      "train_indices" = train_indices,
      "val_indices" = val_indices,
      "lr_new" = lr_new
    )

    # If we are also to save the data to state_list.
    if (save_data) {
      state_list_new <- c(state_list_new, list(
        "training_data" = training_data,
        "normalized_data" = data
      ))

      # Just a small message regarding large disk usage
      if (!is.null(save_vaeac_every_nth_epoch)) {
        message(sprintf(
          "Both having 'save_data = TRUE' and saving the vaeac model every '%d'
epoch might require a lot of disk storage if data is large.\n",
          save_vaeac_every_nth_epoch
        ), immediate. = TRUE)
      }
    }

    # Add the new state list as a list to the checkpoint
    num_times_continued_trained <- sum(grepl("state_list_new", names(checkpoint)))
    state_list_new_name <- paste("state_list_new", num_times_continued_trained + 1, sep = "_")
    state_list <- checkpoint
    state_list[[state_list_new_name]] <- state_list_new

    # Check if we are to save vaeac model every n'th epoch.
    if (!is.null(save_vaeac_every_nth_epoch)) {
      # List of file names for vaeac models after every n'th epoch (save_vaeac_every_nth_epoch).
      filename_nth_list <- list()
    }

    # If batch size has not been provided, then we use the same as during training.
    if (is.null(batch_size)) {
      batch_size <- checkpoint$batch_size
    }

    # Create a vaeac model
    model <- vaeac(
      one_hot_max_sizes = checkpoint$one_hot_max_sizes,
      width = checkpoint$width,
      depth = checkpoint$depth,
      latent_dim = checkpoint$latent_dim,
      activation_function = checkpoint$activation_function,
      use_skip_connections = checkpoint$use_skip_connections,
      skip_connection_masked_enc_dec =
        checkpoint$skip_connection_masked_enc_dec,
      use_batch_normalization = checkpoint$use_batch_normalization,
      paired_sampling = checkpoint$paired_sampling,
      mask_generator_name = checkpoint$mask_generator_name,
      masking_ratio = checkpoint$masking_ratio,
      mask_generator_only_these_coalitions = checkpoint$mask_generator_only_these_coalitions,
      mask_gen_these_coalitions_prob =
        checkpoint$mask_gen_these_coalitions_prob,
      sigma_mu = checkpoint$sigma_mu,
      sigma_sigma = checkpoint$sigma_sigma
    )

    # Update the model's state dictionary to the one provided by the user.
    model$load_state_dict(checkpoint$model_state_dict)

    # Extract the variational lower bound scale factor and mask generator from the vaeac model object.
    vlb_scale_factor <- model$vlb_scale_factor
    mask_generator <- model$mask_generator

    # Create the ADAM optimizer
    optimizer <- torch::optim_adam(
      params = model$parameters,
      lr = checkpoint$lr,
      betas = c(0.9, 0.999),
      eps = 1e-08,
      weight_decay = 0,
      amsgrad = FALSE
    )

    # Insert the state dictionary
    optimizer$load_state_dict(checkpoint$optimizer_state_dict)

    # Override the earlier learning rate with the lr_new if provided.
    if (!is.null(lr_new)) {
      optimizer$param_groups[[1]]$lr <- lr_new
      lr <- lr_new
    }

    # An array to store the regular and running validation IWAE errors
    validation_iwae <- checkpoint$validation_iwae
    validation_iwae_running_avg <- checkpoint$validation_iwae_running_avg

    # An array of running variational lower bounds on the train set
    train_vlb <- checkpoint$train_vlb

    # Compute the total epochs
    epochs_old <- checkpoint$epochs
    epochs_total <- epochs_old + epochs_new
    state_list$epochs <- epochs_total

    # Load the best states from the vaeac model
    filename_best <- vaeac_model$models$best
    best_state <- torch::torch_load(filename_best)
    filename_best_running <- vaeac_model$models$best_running
    best_state_running <- torch::torch_load(filename_best_running)

    # If the user has put the function in verbose mode
    if (verbose) {
      # Create a progress bar for the extra epochs for the final/used vaeac model.
      # Note that we will not see this `progress::progress_bar` move/update if
      # the `progressr` library is used. Then this will just print out
      # the finished `progress::progress_bar`.
      pb <- progress::progress_bar$new(
        format = paste(
          "(:spin) [:bar] :percent [time: :elapsedfull | ETR: :eta |",
          "Epoch: :epoch | VLB: :vlb | IWAE: :iwae | IWAE_R: :runningiwae]"
        ),
        total = epochs_new, #
        complete = "=", # Completion bar character
        incomplete = "-", # Incomplete bar character
        current = ">", # Current bar character
        clear = FALSE, # If TRUE, clears the bar when finish
        width = 125
      ) # Width of the progress bar
    }

    # Create a `progressr::progressor` to keep track of the overall training time of the vaeac approach
    progressr_bar <- progressr::progressor(steps = epochs_new)

    # Continue training the best vaeac model
    for (epoch in seq(epochs_old + 1, epochs_total)) {
      # Set iterator to be the data loader which loads the training data.
      iterator <- torch::dataloader

      # Set average variational lower bound to 0 for this epoch
      avg_vlb <- 0

      # index to keep track of which batch we are working on
      batch_index <- 1

      # Iterate over the training data
      coro::loop(for (batch in train_dataloader) {
        # If batch size is less than batch_size, extend it with objects from the beginning of the dataset
        if (batch$shape[1] < batch_size) {
          batch <- extend_batch(
            batch = batch,
            dataloader = train_dataloader,
            batch_size = batch_size
          )
        }

        # Generate mask and do an optimizer step over the mask and the batch
        mask <- mask_generator(batch)

        # Send the batch and mask to Nvida GPU if we have. Would be faster.
        if (use_cuda) {
          batch <- batch$cuda()
          mask <- mask$cuda()
        }

        # Set all previous gradients to zero.
        optimizer$zero_grad()

        # Compute the variational lower bound for the batch given the mask
        vlb <- model$batch_vlb(batch, mask)$mean()

        # Backpropagation: minimize the negative vlb.
        vlb_loss <- (-vlb / vlb_scale_factor)
        vlb_loss$backward()

        # Update the model parameters by using the optimizer.
        optimizer$step()

        # Update running variational lower bound average
        # a + (new - a)/(i+1) = {(i+1)a + new - a}/(i+1) = { a(i) + new}/(i+1) = a *i/(i+1) + new/(i+1)
        # recursive average formula/update.
        avg_vlb <- avg_vlb + (vlb$to(dtype = torch::torch_float())$clone()$detach() - avg_vlb) / batch_index

        # Update the batch index.
        batch_index <- batch_index + 1
      })

      # Done with one new epoch of training. Time to use the model on the validation data.
      # Time to evaluate the model on the validation data. Compute the validation IWAE.
      val_iwae <- get_validation_iwae(
        val_dataloader,
        mask_generator,
        batch_size,
        model,
        validation_iwae_num_samples,
        verbose
      )

      # Compute the running validation IWAE.
      val_iwae_running <- validation_iwae[
        (-min(length(validation_iwae), running_avg_num_values) +
          length(validation_iwae) + 1):(-1 + length(validation_iwae) + 1),
        drop = FALSE
      ]$mean()$view(1)

      # Add the current validation_iwae and train_vlb to the lists.
      validation_iwae <- torch::torch_cat(c(validation_iwae, val_iwae), -1)
      train_vlb <- torch::torch_cat(c(train_vlb, avg_vlb), -1)
      validation_iwae_running_avg <- torch::torch_cat(c(validation_iwae_running_avg, val_iwae_running), -1)

      # Save if current vaeac model has the lowest validation IWAE error
      if ((max(validation_iwae) <= val_iwae)$item()) {
        best_state <- c(
          list(
            "epoch" = epoch,
            "model_state_dict" = model$state_dict(),
            "optimizer_state_dict" = optimizer$state_dict(),
            "validation_iwae" = validation_iwae,
            "validation_iwae_running_avg" = validation_iwae_running_avg,
            "running_avg_num_values" = running_avg_num_values,
            "train_vlb" = train_vlb
          ),
          state_list
        )

        # Create the file name
        filename_best <- paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), "p", p,
          "n", n, "depth", depth, "width", width, "latent", latent_dim, "lr", lr, "best.pt",
          sep = "_"
        )

        # Combine the file name with the folder path to form the final save file name.
        filename_best <- file.path(folder_to_save_model, filename_best)
        class(best_state) <- c(class(best_state), "R_vaeac", "vaeac")
        torch::torch_save(best_state, filename_best)
      }

      # Save if current vaeac model has the lowest validation IWAE error
      if ((max(validation_iwae_running_avg) <= val_iwae_running)$item()) {
        best_state_running <- c(
          list(
            "epoch" = epoch,
            "model_state_dict" = model$state_dict(),
            "optimizer_state_dict" = optimizer$state_dict(),
            "validation_iwae" = validation_iwae,
            "validation_iwae_running_avg" = validation_iwae_running_avg,
            "running_avg_num_values" = running_avg_num_values,
            "train_vlb" = train_vlb
          ),
          state_list
        )

        # Create the file name
        filename_best_running <- paste(
          gsub(
            pattern = " ",
            replacement = "_",
            x = tolower(model_description)
          ),
          "p", p,
          "n", n,
          "depth", depth,
          "width", width,
          "latent", latent_dim,
          "lr", lr,
          "best_running.pt",
          sep = "_"
        )

        # Combine the file name with the folder path to form the final save file name.
        filename_best_running <- file.path(folder_to_save_model, filename_best_running)
        class(best_state_running) <- c(class(best_state_running), "R_vaeac", "vaeac")
        torch::torch_save(best_state_running, filename_best_running)
      }

      # If we are to save and we are in an n'th epoch, then we save the model.
      if (!is.null(save_vaeac_every_nth_epoch)) {
        if (epoch %% save_vaeac_every_nth_epoch == 0) {
          nth_state <- c(
            list(
              "epoch" = epoch,
              "model_state_dict" = model$state_dict(),
              "optimizer_state_dict" = optimizer$state_dict(),
              "validation_iwae" = validation_iwae,
              "validation_iwae_running_avg" = validation_iwae_running_avg,
              "running_avg_num_values" = running_avg_num_values,
              "train_vlb" = train_vlb
            ),
            state_list
          )

          # Create the file name
          filename_nth <- paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), "_p_", p,
            "_n_", n, "_depth_", depth, "_width_", width, "_latent_", latent_dim, "_lr_", lr,
            "_epoch_", epoch, ".pt",
            sep = ""
          )

          # Combine the file name with the folder path to form the final save file name.
          filename_nth <- file.path(folder_to_save_model, filename_nth)
          class(nth_state) <- c(class(nth_state), "R_vaeac", "vaeac")
          torch::torch_save(nth_state, filename_nth)

          # Add file name to list over file names.
          tmp_list <- list(filename_nth)
          names(tmp_list) <- paste("epoch_", epoch, sep = "")
          filename_nth_list <- append(filename_nth_list, tmp_list)
        }
      }

      if (verbose) {
        # Updates the current state of the progress bar for the final/used vaeac initialization model
        pb$tick(tokens = list(
          epoch = epoch,
          vlb = round(avg_vlb$item(), 3),
          iwae = round(val_iwae$item(), 3),
          runningiwae = round(validation_iwae_running_avg[-1]$item(), 3)
        ))
      }

      # Update the overall `progressr::progressor`.
      progressr_bar(message = sprintf("Continue training vaeac"))
    } # Done with training

    # Also save the model at the last epoch
    last_state <- c(
      list(
        "epoch" = epoch,
        "model_state_dict" = model$state_dict(),
        "optimizer_state_dict" = optimizer$state_dict(),
        "validation_iwae" = validation_iwae,
        "validation_iwae_running_avg" = validation_iwae_running_avg,
        "running_avg_num_values" = running_avg_num_values,
        "train_vlb" = train_vlb
      ),
      state_list
    )

    # Create the file name
    filename_last <- paste(gsub(pattern = " ", replacement = "_", x = tolower(model_description)), "p", p,
      "n", n, "depth", depth, "width", width, "latent", latent_dim, "lr", lr, "last.pt",
      sep = "_"
    )

    # Combine the file name with the folder path to form the final save file name.
    filename_last <- file.path(folder_to_save_model, filename_last)
    class(last_state) <- c(class(last_state), "R_vaeac", "vaeac")
    torch::torch_save(last_state, filename_last)

    # Printout to the user
    if (verbose) {
      message(sprintf(
        "
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
        last_state$validation_iwae_running_avg[-1]
      ))
    }

    # Create a return list
    return_models <- list(
      "best" = filename_best,
      "best_running" = filename_best_running,
      "last" = filename_last
    )
    return_results <- list(
      "train_vlb" = as.array(train_vlb),
      "validation_iwae" = as.array(validation_iwae),
      "validation_iwae_running_avg" = as.array(validation_iwae_running_avg)
    )
    return_parameters <- state_list[-seq(2:7)]

    # If we are to add the 'filename_nth_list' list to the return list.
    if (!is.null(save_vaeac_every_nth_epoch)) {
      filename_nth_list <- c(vaeac_model$models[grepl("epoch", names(vaeac_model$models))], filename_nth_list)
      return_models <- append(return_models, filename_nth_list, 3)
    }
  })

  return_list <- list(
    "models" = return_models,
    "results" = return_results,
    "parameters" = return_parameters,
    "training_time" = vaeac_model$training_time + new_training_time
  )

  # Update the class of the returned object
  attr(return_list, "class") <- c("R_vaeac", "vaeac", "list")

  # Return the paths where the models are saved and the training/validation errors.
  return(return_list)
}

# Compute Imputations =================================================================================================
#' Impute Missing Values Using Vaeac
#'
#' @details  Function that imputes the missing values in 2D matrix where each row constitute an individual.
#' The values are sampled from the conditional distribution estimated by a vaeac model.
#'
#' @param x_explain_with_NaNs A 2D matrix, where the missing entries to impute are represented by `NaN`.
#' @param path_vaeac_model String. The location of the saved vaeac model.
#' @param n_samples Integer. The number of imputed versions we create for each row in `x_explain_with_NaNs`.
#' @param use_cuda Boolean. If we are to use cuda. NOTE PROPERLY TESTED IN CURRENT VERSION.
#' @param sample_random Boolean. If we are to generate random samples from the inferred generative distributions,
#' or if we are to sample the most likely values (mean for continuous and class with highest prob for categorical).
#' @param convert_to_2D Boolean. If the returned results should be of shape
#' \[`nrow(x_explain_with_NaNs)`, `n_samples`, `n_features`\] or
#' \[(`nrow(x_explain_with_NaNs)` \eqn{\times} `n_samples`), `n_features`\].
#' @param return_as_postprocessed_data_table Boolean. If we are to postprocess the data, i.e.,
#' convert categorical features to factors with correct level names (and transform continuous features
#' back to original scale). The returned object will then be a \code{\link[data.table]{data.table}}.
#' @param batch_size Integer. The number of samples in each batch.
#' If `NULL`, then use the same value saved in the vaeac object.
#' We recommend a large number because of a lot of overhead for small batch sizes, that is,
#' `batch_size` should be much larger than the batch size used during the training.
#' @param verbose Boolean. If we are to print the progress to the user.
#' @param seed Integer. Seed used before generating the MC samples.
#' @param index_features Optional integer vector. Used internally in shapr package to index the coalitions.
#'
#' @return A 2D or 3D array or 2D data.table where the missing values (`NaN`) in `x_explain_with_NaNs`
#' have been imputed `n_samples` times. The dimension of the returned object depends on the
#' boolean parameters `convert_to_2D` and `return_as_postprocessed_data_table`.
#'
#' @export
#' @author Lars Henry Berge Olsen
vaeac_impute_missing_entries <- function(x_explain_with_NaNs,
                                         path_vaeac_model,
                                         n_samples,
                                         use_cuda = FALSE,
                                         sample_random = TRUE,
                                         convert_to_2D = TRUE,
                                         return_as_postprocessed_data_table = TRUE,
                                         batch_size = NULL,
                                         verbose = FALSE,
                                         seed = NULL,
                                         index_features = NULL) {
  # If we are to return a data table we need to convert to 2D.
  if (return_as_postprocessed_data_table) {
    if (isFALSE(convert_to_2D)) {
      message("Override `convert_to_2D = FALSE` to `convert_to_2D = TRUE`
as user set `return_as_postprocessed_data_table = TRUE`.")
    }
    convert_to_2D <- TRUE
  }

  # Check if cuda/GPU is available on the current system, and give message
  # to the user if the user asked to run on cuda, but cuda is not available.
  if (isFALSE(torch::cuda_is_available()) && isTRUE(use_cuda)) {
    use_cuda <- FALSE
    message("Cuda is not available. Uses CPU instead.", immediate. = TRUE)
  }

  # If batch size has not been provided, then we use the same as during training.
  if (is.null(batch_size)) batch_size <- checkpoint$batch_size

  # Check/set valid batch size
  if (batch_size > nrow(x_explain_with_NaNs)) batch_size <- nrow(x_explain_with_NaNs)

  # Small printout to the user.
  # if (verbose) message(sprintf("Loading vaeac model.\n"))

  # Load the vaeac model from provided disk location.
  checkpoint <- torch::torch_load(path_vaeac_model)

  # Preprocess the data. This function turns factor names into numerics 1,2,...,K,
  # as vaeac only accepts numerics, and keep track of the maping of names.
  # And optionally log-transform all continuous features. Useful for strictly positive
  # data set like Burr and Abalone, such that vaeac does not impute negative values.
  # Extract the pre-processed data by calling "$data_preprocessed".
  x_explain_with_NaNs_preprocessed <-
    vaeac_preprocess_data(x_explain_with_NaNs, checkpoint$transform_all_continuous_features)$data_preprocessed

  # Create a vaeac model
  model <- vaeac(
    one_hot_max_sizes = checkpoint$one_hot_max_sizes,
    width = checkpoint$width,
    depth = checkpoint$depth,
    latent_dim = checkpoint$latent_dim,
    activation_function = checkpoint$activation_function,
    use_skip_connections = checkpoint$use_skip_connections,
    skip_connection_masked_enc_dec =
      checkpoint$skip_connection_masked_enc_dec,
    use_batch_normalization = checkpoint$use_batch_normalization,
    mask_generator_name = checkpoint$mask_generator_name,
    masking_ratio = checkpoint$masking_ratio,
    mask_generator_only_these_coalitions = checkpoint$mask_generator_only_these_coalitions,
    mask_gen_these_coalitions_prob = checkpoint$mask_gen_these_coalitions_prob,
    sigma_mu = checkpoint$sigma_mu,
    sigma_sigma = checkpoint$sigma_sigma
  )

  # Update the model's state dictionary to the one provided by the user.
  # This sets the weights and biases in the network.
  model$load_state_dict(checkpoint$model_state_dict)

  # Extract sampling method.
  # I.e., if we are to generate random samples from the inferred generative distributions,
  # or if we are to sample the most likely values (mean for cont, class with highest prob for cat).
  if (sample_random) {
    sampler <- model$sampler_random
  } else {
    sampler <- model$sampler_most_likely
  }

  # Set the model in evaluation status, which effects certain modules.
  # E.g., deactivates dropout layers, how batch norm is conducted, etc.
  model$eval()

  # Send the model to the GPU, if we are supposed to.
  if (use_cuda) {
    model <- model$cuda()
    device <- "cuda"
  } else {
    device <- "cpu"
  }

  # Small printout to the user
  # if (verbose) message(sprintf("Start preparation work before imputations.\n"))

  # Normalize the data with the mean and std from the training data.
  # I.e., we assume that the new data follow the same distribution as the training data.
  # If this is NOT the case, then vaeac will generate unreasonable imputations.
  # Note that mean is zero and sd is one for categorical data.
  x_explain_with_NaNs_preprocessed_normalized <-
    (x_explain_with_NaNs_preprocessed - checkpoint$norm_mean) / checkpoint$norm_std

  # Create the data set object.
  dataset <- vaeac_dataset(
    x_explain_with_NaNs_preprocessed_normalized,
    checkpoint$one_hot_max_sizes
  )

  # Create a data loader that load/iterate over the data set in chronological order.
  dataloader <- torch::dataloader(dataset, batch_size = batch_size)

  # Create an auxiliary list of lists to store the imputed values combined with the original values. The structure is
  # [[i'th MC sample]][[b'th batch]], where the entries are tensors of dimension batch_size x n_features.
  results <- lapply(seq(n_samples), function(k) list())

  # Create a progress bar that shows the progress of imputations
  # if (verbose) message(sprintf("Ready to start imputing the instances.\n"))

  # Check if we are printing out verbose messages and progress bars
  if (verbose) {
    # Note that we will not see this `progress::progress_bar` move/update if
    # the `progressr` library is used. Then this will just print out
    # the finished `progress::progress_bar`.
    pb <- progress::progress_bar$new(
      format =
        "(:spin) [:bar] :percent [Imputing | time: :elapsedfull | ETR: :eta | torch batch: :b_i | MC sample: :s_i]",
      total = dataloader$.length() * n_samples,
      complete = "=", # Completion bar character
      incomplete = "-", # Incomplete bar character
      current = ">", # Current bar character
      clear = !verbose, # If TRUE, clears the bar when finish
      width = 125
    ) # Width of the progress bar
  }

  # Small printout to the user
  # if (verbose) message(sprintf("Start imputing missing values.\n"))

  # Set seed for reproducibility if provided by the user. Both in R and torch.
  if (!is.null(seed)) {
    set.seed(seed)
    torch::torch_manual_seed(seed)
  }

  # Variable to keep track of which batch we are working on. Only needed for progress bar.
  batch_index <- 1

  # batch = dataloader$.iter()$.next()
  # Generate the conditional Monte Carlo samples for the observation
  # `x_explain_with_NaNs`, one batch at the time.
  coro::loop(for (batch in dataloader) {
    # Make a deep copy of the batch and detach it from graph.
    batch_extended <- batch$clone()$detach()

    # If batch size is less than batch_size, extend it with objects from the beginning of the dataset.
    if (batch_extended$shape[1] < batch_size) {
      batch_extended <- extend_batch(
        batch = batch_extended,
        dataloader = dataloader,
        batch_size = batch_size
      )
    }

    # Send the original and extended batch to GPU if applicable.
    if (use_cuda) {
      batch <- batch$cuda()
      batch_extended <- batch_extended$cuda()
    }

    # Compute the imputation mask, i.e., which entries we are to impute.
    mask_extended <- torch::torch_isnan(batch_extended)$to(dtype = torch::torch_float())

    # Do not need to keep track of the gradients, as we are not fitting the model.
    torch::with_no_grad({
      # Compute the distributions parameters for the generative models inferred by
      # the masked encoder and decoder together. This will be a tensor of shape
      # [batch_size, n_samples, num_generative_parameters].
      # For only continuous features we have that num_generative_parameters = 2*num_features,
      # but for categorical data the number depends on the number of categories.
      samples_params <- model$generate_samples_params(
        batch = batch_extended,
        mask = mask_extended,
        K = n_samples
      )

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
      # sample_params is a tensor of shape [batch_size, num_generative_parameters].
      sample_params <- samples_params[, i, ]

      # Generate the imputations using the generative distributions inferred by the decoder.
      # Can either be the most likely values (mean for cont, class with highest prob for cat),
      # or we can randomly sample the imputed values. We do the latter.
      sample <- sampler(sample_params)

      # Need only the imputed values for the missing data entries.
      # Zero out the imputations done for known feature values.
      sample[torch::torch_logical_not(mask)] <- 0

      # Combine the imputations with the original data to fill in the missing values.
      # Sample is a tensor of shape [batch_size, n_features]
      sample <- sample + batch_zeroed_nans

      # Make a deep copy and add it to correct location in the results list.
      results[[i]] <- append(results[[i]], sample$clone()$detach()$cpu())

      # If verboser, then update the current state of the progress bar.
      if (verbose) pb$tick(tokens = list(b_i = batch_index, s_i = i))
    }

    # Update the batch number.
    batch_index <- batch_index + 1
  }) # End of iterating over the batches. Done imputing.

  # Small printout to the user
  # if (verbose) message(sprintf("Start concatenating the imputations.\n"))

  # Order the the imputations/MC samples into a tensor of shape
  # [nrow(x_explain_with_NaNs), n_samples, n_features]. The lapply function creates a list
  # of tensors of shape [nrow(x_explain_with_NaNs), 1, n_features] by concatenating the batches
  # for the i'th imputation/sample to a tensor of shape [nrow(x_explain_with_NaNs), n_features]
  # and then add unsqueeze to add a new singleton dimension as the second dimension to get the
  # shape [nrow(x_explain_with_NaNs), 1, n_features]. Then outside the lapply we concatenate
  # the n_samples torch elements to form a final torch result of shape
  # [nrow(x_explain_with_NaNs), n_samples, n_features].
  result <- torch::torch_cat(
    lapply(seq(n_samples), function(i) torch::torch_cat(results[[i]])$unsqueeze(2)),
    dim = 2
  )

  # Undo the normalization ([x_explain_with_NaNs_preprocessed - mu]/sigma) to get back to
  # the original distribution by multiplying the results by sigma and adding the mean.
  result <- result * checkpoint$norm_std + checkpoint$norm_mean

  # Check if we are to concatenate the result such that we go from a
  # tensor of shape [nrow(x_explain_with_NaNs), n_samples, n_features] to
  # a tensor of shape [(nrow(x_explain_with_NaNs) * n_samples), n_features].
  if (convert_to_2D) result <- result$view(c(result$shape[1] * result$shape[2], result$shape[3]))

  # Convert the results from at torch_tensor to either a 3D or 2D array.
  result <- as.array(result$detach()$cpu())

  # Check if we are to post process the data such that categorical features have
  # original level names and convert the result to a data table.
  if (return_as_postprocessed_data_table) result <- vaeac_postprocess_data(result, checkpoint)

  # If user provide `index_features`, then we add columns needed for shapr computations
  if (!is.null(index_features)) {
    # Get the number of explicands. Recall that `n_explain_with_NaNs` contains
    # `length(index_features)` duplicates of each explicand.
    n_explain <- nrow(x_explain_with_NaNs) / length(index_features)

    # Add id, id_combination and weights to the result.
    # The sampling weights is 1/n_samples in the case of vaeac as we can sample as many MC as we will.
    # It is 1 for empty and full coalition.
    result <- data.table(
      id = rep(
        seq(n_explain),
        each = length(index_features) * n_samples
      ),
      id_combination = rep(
        index_features,
        each = n_samples,
        times = n_explain
      ),
      w = 1 / n_samples,
      result
    )

    # Set the key in the data table which sorts them.
    setkey(result, id, id_combination)
  }

  # Return the input data with missing values imputed by vaeac
  return(result)
}


# Plot functions ======================================================================================================

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
#' @param plot_figures Boolean. If `TRUE`, the plot the figure before the function potentially
#' return the figures, depending on the value of `return_figures`.
#' @param return_figures Boolean. If `TRUE`, then the function returns the figures in a list.
#' @param return_training_validation_errors Boolean. If `TRUE`, then the training and validation errors are returned.
#' @param plot_VLB Boolean. If `TRUE`, then the function includes the VLB in the figures.
#' @param plot_IWAE Boolean. If `TRUE`, then the function includes the IWAE in the figures.
#' @param plot_IWAE_running Boolean. If `TRUE`, then the function includes the IWAE_running in the figures.
#' @param ggplot_theme A [ggplot2::theme()] object to customize the non-data components of the plots:
#' i.e. titles, labels, fonts, background, gridlines, and legends. Themes can be used to give plots
#' a consistent customized look. Use the themes available in \code{\link[ggplot2:theme_bw]{ggplot2::ggtheme()}}.
#' if you would like to use a complete theme such as `theme_bw()`, `theme_minimal()`, and more.
#' @param brewer_palette String. Name of one of the color palettes from [RColorBrewer::RColorBrewer()].
#'  If `NULL`, then the function uses the default [ggplot2::ggplot()] color scheme.
#' The following palettes are available for use with these scales:
#' \describe{
#'    \item{Diverging}{BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral}
#'    \item{Qualitative}{Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3}
#'    \item{Sequential}{Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges,
#'      OrRd, PuBu, PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd}
#' }
#' @param brewer_direction Integer. Sets the order of colors in the scale. If 1, the default,
#' colors are as output by \code{\link[RColorBrewer:ColorBrewer]{RColorBrewer::brewer.pal()}}.
#' If -1, the order of colors is reversed.
#' @param title_text_size Positive numeric. The size of the title. If `0`, then the text is removed.
#' @param line_width Positive numeric. Width of the lines, and 0 is the same as `line_type` = `blank`.
#' @param line_type Legal values are the strings "blank", "solid", "dashed", "dotted", "dotdash",
#' "longdash", and "twodash".
#' Alternatively, the numbers 0 to 6 can be used (0 for "blank", 1 for "solid", ...).
#' Moreover, one can also use strings that define the line type with up to 8 hexadecimal digits
#' (each digit specifying the length of interleaved lines and gaps), see
#' \url{https://ggplot2.tidyverse.org/articles/ggplot2-specs.html}
#' and/or \url{http://sape.inf.usi.ch/quick-reference/ggplot2/linetype}.
#' @param point_size Positive numeric. The size of the points. Set `point_size = 0` to remove points.
#' @param point_shape Integer or string. Specify the shape of the points.
#' For a complete list of all possible shapes, see \url{https://ggplot2.tidyverse.org/articles/ggplot2-specs.html}.
#' @param facet_wrap_scales String. Should the scales be fixed ("`fixed`", the default),
#' free ("`free`"), or free in one dimension ("`free_x`", "`free_y`")?
#' @param facet_wrap_nrow Integer. Number of rows in the facet wrap.
#' @param facet_wrap_ncol Integer. Number of columns in the facet wrap.
#' @param plot_geom_smooth Boolean. If we are to include a [ggplot2::geom_smooth()] with
#' `method = "loess"` in the figures..
#' @param plot_geom_smooth_se Boolean. If we are to display 95% confidence interval around the smooth.
#' If `TRUE`, the `plot_geom_smooth_se` overrides `plot_geom_smooth`.
#' @param legend_position String or numeric vector `c(x,y)`. The allowed string values for the
#' argument `legend_position` are: `left`,`top`, `right`, `bottom`. Note that, the argument
#' `legend_position` can be also a numeric vector `c(x,y)`. In this case it is possible to position
#' the legend inside the plotting area. `x` and `y` are the coordinates of the legend box.
#' Their values should be between `0` and `1`, where `c(0,0)` corresponds to the "bottom left"
#' and `c(1,1)` corresponds to the "top right" position.
#' @param legend_ncol Integer. The number of columns in the legend.
#' @param legend_nrow Integer. The number of rows in the legend.
#'
#' @return Depending on `return_figures` and `return_training_validation_errors`, either a list of
#' figures, a list of figures and a data.table, a data.table, or nothing.
#' @export
#'
#' @author Lars Henry Berge Olsen
#'
#' @examples
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
#'   vaeac.num_different_vaeac_initiate = 5,
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
#'   vaeac.num_different_vaeac_initiate = 5,
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
#'   vaeac.num_different_vaeac_initiate = 5,
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
#'   vaeac.num_different_vaeac_initiate = 5,
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
#' vaeac_training_vlb_and_validation_iwae_shapr(explanation_list = explanation_list_unnamed)
#'
#' # Call the function with the named list, will use the provided names
#' # See that the paired samplign often produce more stable results
#' vaeac_training_vlb_and_validation_iwae_shapr(explanation_list = explanation_list_named)
#'
#' # Can alter the plot
#' vaeac_training_vlb_and_validation_iwae_shapr(
#'   explanation_list = explanation_list_named,
#'   point_size = 0,
#'   ggplot_theme = ggplot2::theme_minimal(),
#'   brewer_palette = "Set1",
#'   plot_from_nth_epoch = 5,
#'   plot_every_nth_epoch = 3,
#'   legend_position = "bottom",
#'   legend_nrow = 2,
#'   facet_wrap_scales = "free"
#' )
#'
#' # If we want to get smooth versions
#' vaeac_training_vlb_and_validation_iwae_shapr(
#'   explanation_list = explanation_list_named,
#'   point_size = 0,
#'   line_type = "blank",
#'   plot_from_nth_epoch = 1,
#'   legend_position = "bottom",
#'   plot_geom_smooth = TRUE,
#'   plot_geom_smooth_se = TRUE
#' )
#'
#' # If we just want to extract the training and validation errors
#' vaeac_training_vlb_and_validation_iwae_shapr(
#'   explanation_list = explanation_list_named,
#'   plot_figures = FALSE,
#'   return_training_validation_errors = TRUE
#' )
#'
#' # If we want to return the figures
#' figures <- vaeac_training_vlb_and_validation_iwae_shapr(
#'   explanation_list = explanation_list_named,
#'   plot_figures = FALSE,
#'   return_figures = TRUE
#' )
#'
#' # If we want to return the figures
#' figures_and_errors <- vaeac_training_vlb_and_validation_iwae_shapr(
#'   explanation_list = explanation_list_named,
#'   plot_figures = FALSE,
#'   return_figures = TRUE,
#'   return_training_validation_errors = TRUE
#' )
#'
#' @author Lars Henry Berge Olsen
make_vaeac_training_evaluation_plots <- function(explanation_list,
                                                 plot_from_nth_epoch = 1,
                                                 plot_every_nth_epoch = 1,
                                                 plot_figures = TRUE,
                                                 return_figures = FALSE,
                                                 return_training_validation_errors = FALSE,
                                                 plot_VLB = TRUE,
                                                 plot_IWAE = TRUE,
                                                 plot_IWAE_running = FALSE,
                                                 ggplot_theme = NULL,
                                                 brewer_palette = "Paired",
                                                 brewer_direction = 1,
                                                 title_text_size = 12,
                                                 line_type = "solid",
                                                 line_width = 0.4,
                                                 point_size = 1,
                                                 point_shape = "circle",
                                                 facet_wrap_scales = "fixed",
                                                 facet_wrap_nrow = NULL,
                                                 facet_wrap_ncol = NULL,
                                                 plot_geom_smooth = FALSE,
                                                 plot_geom_smooth_se = FALSE,
                                                 legend_position = NULL,
                                                 legend_nrow = NULL,
                                                 legend_ncol = NULL) {
  # Setup and checks ----------------------------------------------------------------------------
  # Check that ggplot2 is installed
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is not installed. Please run install.packages('ggplot2')")
  }

  # Check if user only provided a single explanation and did not put it in a list
  if ("shapr" %in% class(explanation_list)) explanation_list <- list(explanation_list)

  # Check that all explanation objects use the `vaeac` approach
  explanation_approaches <- sapply(explanation_list, function(explanation) explanation$internal$parameters$approach)
  if (any(explanation_approaches != "vaeac")) {
    stop(sprintf(
      "Explanation object number `%d` in the `explanation_list` does not use the `vaeac` approach.",
      seq_along(explanation_approaches)[explanation_approaches != "vaeac"][1]
    ))
  }

  # Check if the entries in the list is named
  if (is.null(names(explanation_list))) {
    # The entries in explanation_list are lacking names

    # Extract the names of the approaches used in the explanation objects in explanation_list
    # We paste in case an explanation object has used a combination of approaches.
    names <- sapply(
      explanation_list,
      function(explanation) paste(explanation$internal$parameters$approach, collapse = "_")
    )

    # Ensure that we have unique names
    names <- make.unique(names, sep = "_")

    # Name the entries in explanation_list
    names(explanation_list) <- names

    # Give a message to the user
    warning(paste0(
      "User provided an `explanation_list` without named explanation objects.\n",
      "Default to the approach names (with integer suffix for duplicates) for the explanation objects.\n"
    ))
  }

  # Create datasets -----------------------------------------------------------------------------
  results_dt <- data.table::rbindlist(
    lapply(explanation_list, function(explanation) {
      data.table::data.table(do.call(cbind, explanation$internal$parameters$vaeac$results))[, Epoch := .I]
    }),
    use.names = TRUE,
    idcol = "Method"
  )
  names(results_dt)[2:4] <- c("VLB", "IWAE", "IWAE_running")
  results_dt$Method <- factor(results_dt$Method, levels = names(explanation_list))
  results_dt

  # Set key and reorder the columns
  data.table::setkeyv(results_dt, c("Method", "Epoch"))
  data.table::setcolorder(results_dt, c("Method", "Epoch"))

  # Go from wide to tall
  results_dt_tall <- data.table::melt(
    data = results_dt,
    id.vars = c("Method", "Epoch"),
    variable.name = "Criterion",
    variable.factor = TRUE,
    value.name = "Value"
  )

  # Remove rows based on that we only want to plot epochs from the nth epoch
  results_dt_tall_truncated <- results_dt_tall[Epoch >= plot_from_nth_epoch]

  # If we are only to plot every nth epoch
  results_dt_tall_truncated <- results_dt_tall_truncated[Epoch %% plot_every_nth_epoch == 0]

  # Remove some of the criterion if specified by the user
  if (!plot_VLB) results_dt_tall_truncated <- results_dt_tall_truncated[Criterion != "VLB"]
  if (!plot_IWAE) results_dt_tall_truncated <- results_dt_tall_truncated[Criterion != "IWAE"]
  if (!plot_IWAE_running) results_dt_tall_truncated <- results_dt_tall_truncated[Criterion != "IWAE_running"]

  # Make figures --------------------------------------------------------------------------------
  # Make the figure where each explanation object has its own facet
  figure_facet_for_each_method <-
    ggplot2::ggplot(results_dt_tall_truncated, ggplot2::aes(x = Epoch, y = Value, col = Criterion)) +
    {
      if (plot_geom_smooth || plot_geom_smooth_se) {
        ggplot2::geom_smooth(
          method = "loess",
          formula = y ~ x,
          se = plot_geom_smooth_se
        )
      }
    } +
    ggplot2::geom_point(shape = point_shape, size = point_size, ggplot2::aes(col = Criterion)) +
    ggplot2::geom_line(linetype = line_type, linewidth = line_width, ggplot2::aes(
      group = Criterion,
      col = Criterion
    )) +
    ggplot2::facet_wrap(ggplot2::vars(Method),
      nrow = facet_wrap_nrow,
      ncol = facet_wrap_ncol,
      scales = facet_wrap_scales
    ) +
    {
      if (!is.null(brewer_palette)) {
        ggplot2::scale_color_brewer(
          palette = brewer_palette,
          direction = brewer_direction
        )
      }
    } +
    {
      if (!is.null(ggplot_theme)) ggplot_theme
    } +
    {
      if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)
    } +
    {
      if (!is.null(legend_ncol)) ggplot2::guides(col = ggplot2::guide_legend(ncol = legend_ncol))
    } +
    {
      if (!is.null(legend_nrow)) ggplot2::guides(col = ggplot2::guide_legend(nrow = legend_nrow))
    } +
    ggplot2::labs(title = "The evaluation criterions for different vaeac models") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_text_size))


  # Make the figure where each criterion has its own facet
  figure_facet_for_each_criterion <-
    ggplot2::ggplot(results_dt_tall_truncated, ggplot2::aes(x = Epoch, y = Value, col = Method)) +
    {
      if (plot_geom_smooth || plot_geom_smooth_se) {
        ggplot2::geom_smooth(
          method = "loess",
          formula = y ~ x,
          se = plot_geom_smooth_se
        )
      }
    } +
    ggplot2::geom_point(shape = point_shape, size = point_size, ggplot2::aes(col = Method)) +
    ggplot2::geom_line(linetype = line_type, linewidth = line_width, ggplot2::aes(group = Method, col = Method)) +
    ggplot2::facet_wrap(ggplot2::vars(Criterion),
      nrow = facet_wrap_nrow,
      ncol = facet_wrap_ncol,
      scales = facet_wrap_scales
    ) +
    {
      if (!is.null(brewer_palette)) ggplot2::scale_color_brewer(palette = brewer_palette, direction = brewer_direction)
    } +
    {
      if (!is.null(ggplot_theme)) ggplot_theme
    } +
    {
      if (!is.null(legend_position)) ggplot2::theme(legend.position = legend_position)
    } +
    {
      if (!is.null(legend_ncol)) ggplot2::guides(col = ggplot2::guide_legend(ncol = legend_ncol))
    } +
    {
      if (!is.null(legend_nrow)) ggplot2::guides(col = ggplot2::guide_legend(nrow = legend_nrow))
    } +
    ggplot2::labs(title = "The evaluation criterions for different vaeac models") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = title_text_size))

  # Plot the figures if specified by the user
  if (plot_figures) {
    print(figure_facet_for_each_method)
    print(figure_facet_for_each_criterion)
  }

  # Cleanup and return objects ------------------------------------------------------------------
  # Check if we are to return figures and/or the data.table with the results
  if (return_figures || return_training_validation_errors) {
    # Create empty list to store the results
    return_list <- list()

    # Check if we are to return the figures
    if (return_figures) {
      return_list[["figure_facet_for_each_method"]] <- figure_facet_for_each_method
      return_list[["figure_facet_for_each_criterion"]] <- figure_facet_for_each_criterion
    }

    # Check if we are to return the data.table with the training results
    if (return_training_validation_errors) {
      if (length(return_list) == 0) {
        # We are no to return the figures, so directly return the data.table
        return_list <- results_dt
      } else {
        # Add the data.table to the result list
        return_list[["training_and_validation_errors"]] <- results_dt
      }
    }

    # Return the return list
    return(return_list)
  }
}



#' Extract the Training VLB and Validation IWAE from the Provided vaeac Model
#'
#' @param explanation List. The output list from the [explain()] function.
#' @param plot_from_nth_epoch Integer. From which epoch to plot from. First epoch can be large in
#' absolute value and make the rest of the plot difficult to interpret.
#' @param plot_figure Boolean. If `TRUE`, the plot the figure before the function potentially
#' return the figures, depending on the value of `return_figure`.
#' @param return_figure Boolean. If `TRUE`, then the function returns the figure.
#' @param return_training_validation_errors Boolean. If `TRUE`, then the training and validation errors are returned.
#'
#' @return A list containing the training VLB and validation IWAE at each epoch, and the total number of epochs.
#' @export
#' @author Lars Henry Berge Olsen
vaeac_training_vlb_and_validation_iwae_shapr <- function(explanation,
                                                         plot_figure = TRUE,
                                                         plot_from_nth_epoch = 1,
                                                         return_figure = FALSE,
                                                         return_training_validation_errors = FALSE) {
  # Extract the checkpoint for the last trained vaeac model.
  vaeac_model_path <- explanation$internal$parameters$vaeac$models$last

  # Load the vaeac model at the provided path.
  checkpoint <- torch::torch_load(path = vaeac_model_path)

  # Create an empty return list
  temp_list <- list()

  # Only want to return epoch, training loss, and validation loss.
  include_keys <- c("validation_iwae", "validation_iwae_running_avg", "train_vlb", "epoch")

  # Iterate over the keys.
  for (key in include_keys) {
    # Extract the entries from the checkpoint and convert from torch tensors to R arrays.
    temp_list[[key]] <- as.array(checkpoint[[key]])
  }

  # Check if we are to return the errors
  if (return_training_validation_errors) {
    return_list <- temp_list
  }

  # Check if we are to plot the training VLB and validation IWAE.
  if (plot_figure || return_figure) {
    # Combine the results into a data table.
    temp_data <- data.table(
      "VLB" = temp_list$train_vlb,
      "IWAE" = temp_list$validation_iwae,
      "IWAE_running" = temp_list$validation_iwae_running_avg,
      "Epoch" = seq(temp_list$epoch)
    )

    # Convert it from wide to long
    temp_data <- melt(
      data = temp_data,
      id.vars = "Epoch",
      variable.name = "Type",
      variable.factor = TRUE,
      value.name = "Value"
    )

    # Remove entries with too low epoch
    temp_data <- temp_data[Epoch >= plot_from_nth_epoch, ]

    # Create the figure
    fig <- ggplot2::ggplot(temp_data, ggplot2::aes(x = Epoch, y = Value, group = Type)) +
      ggplot2::geom_line(ggplot2::aes(color = Type)) +
      ggplot2::geom_point(ggplot2::aes(color = Type)) +
      ggplot2::scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
      ggplot2::theme(legend.position = "right") +
      ggplot2::lims(x = c(0, temp_list$epoch))

    # Check if we are to plot the figure
    if (plot_figure) print(fig)

    # Check id we are to return the figure
    if (return_figure) {
      if (return_training_validation_errors) {
        # We add the figure to the return list
        return_list[["Figure"]] <- fig
      } else {
        # We are only to return the figure, so we replace the list with only the figure
        return_list <- fig
      }
    }
  }

  # Return the the results
  if (return_figure || return_training_validation_errors) {
    return(return_list)
  }
}


#' Plot Pairwise Plots for Imputed and True Data
#'
#' @description A function that creates a matrix of plots (\code{GGally::\link[GGally]{ggpairs}}) from
#' generated imputations from the unconditioned distribution \eqn{p(\boldsymbol{x})} estimated by
#' a vaeac model, and then compares the imputed values with data from the true distribution (if provided).
#'
#' @details
#' See \href{https://www.blopig.com/blog/2019/06/a-brief-introduction-to-ggpairs/}{ggpairs} for an
#' introduction to \code{GGally::\link[GGally]{ggpairs}}, and the corresponding
#' \href{https://ggobi.github.io/ggally/articles/ggally_plots.html}{vignette}.
#'
#' Each combination of variables are plotted according to whether they are:
#' \describe{
#' \item{continuous}{exactly one of ('points' (lower default), 'smooth', 'smooth_loess',
#' 'density', 'cor' (upper default), or 'blank').}
#' \item{combo}{exactly one of ('box', 'box_no_facet' (upper default), 'dot', 'dot_no_facet',
#' 'facethist', 'facetdensity', 'denstrip', or 'blank').}
#' \item{discrete}{exactly one of ('ratio', 'facetbar', or 'blank')}
#' }
#'
#'
#' Plots on the diagonal can either be:
#' \describe{
#'  \item{continuous}{exactly one of ('densityDiag', 'barDiag', 'blankDiag').
#'  This option is used for continuous X data.}
#'  \item{discrete}{exactly one of ('barDiag', 'blankDiag'). This option is used for categorical X and Y data.}
#'  \item{na}{exactly one of ('naDiag', 'blankDiag').  This option is used when all X data is \code{NA}.}
#' }
#'
#'
#' @param explanation List. The output list from the \code{\link{explain}} function.
#' @param which_vaeac_model String. Indicating which vaeac model to use when generating the samples.
#' @param true_data Matrix/data.frame containing the data from the distribution that the vaeac model is fitted to.
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
#' @return A list containing the figures if `return_figures` = `TRUE`.
#' @export
#' @author Lars Henry Berge Olsen
vaeac_ggpairs_plot_imputed_and_true_data_shapr <-
  function(explanation,
           which_vaeac_model = "best",
           true_data = NULL,
           return_figures = TRUE,
           save_figures = FALSE,
           show_figures = FALSE,
           upper_cont = c("cor", "points", "smooth", "smooth_loess", "density", "blank"),
           upper_cat = c("count", "cross", "ratio", "facetbar", "blank"),
           upper_mix = c(
             "box", "box_no_facet", "dot", "dot_no_facet",
             "facethist", "facetdensity", "denstrip", "blank"
           ),
           lower_cont = c("points", "smooth", "smooth_loess", "density", "cor", "blank"),
           lower_cat = c("facetbar", "ratio", "count", "cross", "blank"),
           lower_mix = c(
             "facetdensity", "box", "box_no_facet", "dot",
             "dot_no_facet", "facethist", "denstrip", "blank"
           ),
           diag_cont = c("densityDiag", "barDiag", "blankDiag"),
           diag_cat = c("barDiag", "blankDiag"),
           cor_method = c("pearson", "kendall", "spearman"),
           ...) {
    # Check if the vaeac model is expected to give a reasonable figure.
    if (!explanation$internal$parameters$exact || explanation$internal$parameters$is_groupwise) {
      message(sprintf("The vaeac model has not been trained on the empty colition, hence, the figure can be missleading.
The figure is only reasonable if 'n_combintations = NULL' and 'group = NULL' in the explanation call."))
    }

    # Extract the vaeac list from the explanation list
    vaeac_list <- explanation$internal$parameters$vaeac

    # Figure out which vaeac model to use.
    if (which_vaeac_model %in% names(vaeac_list$models)) {
      # User provided a string which matches one of the file names and we use it.
      vaeac_model_path <- vaeac_list$models[[which_vaeac_model]]
    } else {
      # User provided a string which is not one of the file names. Overwrite it.
      vaeac_model_path <- vaeac_list$models[["best"]]
      message(sprintf(
        "The provided string for 'which_vaeac_model' (%s) did not match any stored checkpoints (%s).
We set 'which_vaeac_model = best' and continue.\n",
        which_vaeac_model,
        paste(names(vaeac_list$models), collapse = ", ")
      ))
    }

    # Check for valid input.
    upper_cont <- match.arg(upper_cont)
    upper_cat <- match.arg(upper_cat)
    upper_mix <- match.arg(upper_mix)
    lower_cont <- match.arg(lower_cont)
    lower_cat <- match.arg(lower_cat)
    lower_mix <- match.arg(lower_mix)
    diag_cont <- match.arg(diag_cont)
    diag_cat <- match.arg(diag_cat)
    cor_method <- match.arg(cor_method)

    # Get the number of observations in the true_data
    num_samples <- ifelse(is.null(true_data), 500, nrow(true_data))

    # Some small checks
    vaeac_model <- torch::torch_load(vaeac_model_path)
    if (!is.null(true_data)) {
      if (ncol(true_data) != vaeac_model$p) {
        stop(sprintf(
          "Different number of columns in the vaeac model and 'true data': %d != %d.\n",
          checkpoint$p, ncol(true_data)
        ))
      }
    }

    # Create folder if we are to save the figures
    if (save_figures) {
      # Create a path and folder for where we arr to save the figures
      folder_to_save_models <- file.path(dirname(vaeac_model), "Plots_ggpairs")
      dir.create(folder_to_save_models, showWarnings = FALSE)
    }

    # Extract which features that are categorical.
    cat_featuers <- vaeac_model$col_cat

    # If we are to return the figures
    if (return_figures) {
      figure_list <- list()
    }

    # Impute the values. Here we generate x from p(x), so no conditioning.
    imputed_values <- vaeac_impute_missing_entries(
      x_explain_with_NaNs = matrix(NaN, num_samples, vaeac_model$p), # instances_to_impute,
      path_vaeac_model = vaeac_model_path,
      n_samples = 1,
      use_cuda = FALSE,
      convert_to_2D = TRUE,
      return_as_postprocessed_data_table = TRUE,
      batch_size = num_samples,
      verbose = FALSE
    )

    # Combine the true data (if there are any) with the imputed values.
    combined_data <- data.frame(rbind(true_data, imputed_values))

    # Ensure that the categorical features are marked as factors.
    combined_data[cat_featuers] <- lapply(combined_data[cat_featuers], factor)

    # Add a variable indicating if the values are from the true distribution or if they have been sampled.
    combined_data$type <- factor(rep(c("True", "Imputed"),
      times = c(ifelse(is.null(nrow(true_data)), 0, nrow(true_data)), num_samples)
    ))

    # Extract what to include as title in the figure.
    figure_title <- tools::file_path_sans_ext(basename(vaeac_model_path))

    # Create the ggparis figure.
    figure <- GGally::ggpairs(combined_data,
      columns = seq(vaeac_model$p),
      mapping = ggplot2::aes(color = type),
      diag = list(
        continuous = GGally::wrap(diag_cont, alpha = 0.5),
        discrete = GGally::wrap(diag_cat, alpha = 1.0)
      ),
      upper = list(
        combo = GGally::wrap(upper_mix, alpha = 1.0),
        discrete = GGally::wrap(upper_cat, alpha = 1.0),
        continuous = GGally::wrap(upper_cont, method = cor_method, size = 3.65)
      ),
      lower = list(
        continuous = GGally::wrap(lower_cont, alpha = 0.25),
        discrete = GGally::wrap(lower_cat, alpha = 1.0),
        combo = GGally::wrap(lower_mix, alpha = 1)
      ),
      title = figure_title,
      proportions = rep(1, vaeac_model$p)
    ) +
      # theme(plot.title = element_text(size=22),
      #       text = element_text(size = 16),
      #       strip.text = element_text(size = 13)) +
      ggplot2::scale_color_manual(values = c("#E69F00", "#999999", "#56B4E9")) +
      ggplot2::scale_fill_manual(values = c("#E69F00", "#999999", "#56B4E9")) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -90, hjust = 0, vjust = 0.5))

    # If we are to show the figure.
    if (show_figures) {
      print(figure)
    }

    # If we are to save the figure.
    if (save_figures) {
      figure_save_name <- file.path(folder_to_save_models, paste("ggpairs_", figure_title, ".png", sep = ""))
      suppressMessages(ggplot2::ggsave(figure_save_name, plot = figure, ...))
    }

    # If we are to return the figure.
    if (return_figures) {
      return(figure)
    }
  }
