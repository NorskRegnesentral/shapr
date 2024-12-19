#' check_setup
#' @inheritParams explain
#' @inheritParams explain_forecast
#' @inheritParams default_doc_internal
#' @param type Character.
#' Either "regular" or "forecast" corresponding to function `setup()` is called from,
#' correspondingly the type of explanation that should be generated.
#'
#' @param feature_specs List. The output from [get_model_specs()] or [get_data_specs()].
#' Contains the 3 elements:
#' \describe{
#'   \item{labels}{Character vector with the names of each feature.}
#'   \item{classes}{Character vector with the classes of each features.}
#'   \item{factor_levels}{Character vector with the levels for any categorical features.}
#'   }
#' @param is_python Logical.
#' Indicates whether the function is called from the Python wrapper.
#' Default is FALSE which is never changed when calling the function via `explain()` in R.
#' The parameter is later used to disallow running the AICc-versions of the empirical method
#' as that requires data based optimization, which is not supported in `shaprpy`.
#' @param testing Logical.
#' Only use to remove random components like timing from the object output when comparing output with testthat.
#' Defaults to `FALSE`.
#' @param init_time POSIXct object.
#' The time when the `explain()` function was called, as outputted by `Sys.time()`.
#' Used to calculate the time it took to run the full `explain` call.
#' @export
#' @keywords internal
setup <- function(x_train,
                  x_explain,
                  approach,
                  phi0,
                  output_size = 1,
                  max_n_coalitions,
                  group,
                  n_MC_samples,
                  seed,
                  feature_specs,
                  type = "regular",
                  horizon = NULL,
                  y = NULL,
                  xreg = NULL,
                  train_idx = NULL,
                  explain_idx = NULL,
                  explain_y_lags = NULL,
                  explain_xreg_lags = NULL,
                  group_lags = NULL,
                  verbose,
                  iterative = NULL,
                  iterative_args = list(),
                  is_python = FALSE,
                  testing = FALSE,
                  init_time = NULL,
                  prev_shapr_object = NULL,
                  asymmetric = FALSE,
                  causal_ordering = NULL,
                  confounding = NULL,
                  output_args = list(),
                  extra_computation_args = list(),
                  ...) {
  internal <- list()

  # Using parameters and iter_list from a previouys  to continue estimation from on previous shapr objects
  if (is.null(prev_shapr_object)) {
    prev_iter_list <- NULL
  } else {
    # Overwrite the input arguments set in explain() with those from in prev_shapr_object
    # except model, x_explain, x_train, max_n_coalitions, iterative_args, seed
    prev_internal <- get_prev_internal(prev_shapr_object)

    prev_iter_list <- prev_internal$iter_list

    list2env(prev_internal$parameters)
  }


  internal$parameters <- get_parameters(
    approach = approach,
    phi0 = phi0,
    output_size = output_size,
    max_n_coalitions = max_n_coalitions,
    group = group,
    n_MC_samples = n_MC_samples,
    seed = seed,
    type = type,
    horizon = horizon,
    train_idx = train_idx,
    explain_idx = explain_idx,
    explain_y_lags = explain_y_lags,
    explain_xreg_lags = explain_xreg_lags,
    group_lags = group_lags,
    verbose = verbose,
    iterative = iterative,
    iterative_args = iterative_args,
    is_python = is_python,
    testing = testing,
    asymmetric = asymmetric,
    causal_ordering = causal_ordering,
    confounding = confounding,
    output_args = output_args,
    extra_computation_args = extra_computation_args,
    ...
  )

  # Sets up and organizes data
  if (type == "forecast") {
    internal$data <- get_data_forecast(y, xreg, train_idx, explain_idx, explain_y_lags, explain_xreg_lags, horizon)
  } else {
    internal$data <- get_data(x_train, x_explain)
  }

  internal$objects <- list(feature_specs = feature_specs)

  check_data(internal)

  internal <- get_extra_parameters(internal, type) # This includes both extra parameters and other objects

  internal <- check_and_set_parameters(internal, type)

  internal <- set_iterative_parameters(internal, prev_iter_list)

  internal$timing_list <- list(
    init_time = init_time,
    setup = Sys.time()
  )

  return(internal)
}

#' @keywords internal
get_prev_internal <- function(prev_shapr_object,
                              exclude_parameters = c("max_n_coalitions", "iterative_args", "seed")) {
  cl <- class(prev_shapr_object)[1]

  if (cl == "character") {
    internal <- readRDS(file = prev_shapr_object) # Already contains only "parameters" and "iter_list"
  } else if (cl == "shapr") {
    internal <- prev_shapr_object$internal[c("parameters", "iter_list")]
  } else {
    stop("Invalid `shapr_object` passed to explain(). See ?explain for details.")
  }

  if (length(exclude_parameters) > 0) {
    internal$parameters[exclude_parameters] <- NULL
  }

  iter <- length(internal$iter_list)
  internal$iter_list[[iter]]$converged <- FALSE # Hard resetting of the convergence parameter

  return(internal)
}


#' @keywords internal
get_parameters <- function(approach,
                           phi0,
                           output_size = 1,
                           max_n_coalitions,
                           group,
                           n_MC_samples,
                           seed,
                           type,
                           horizon,
                           train_idx,
                           explain_idx,
                           explain_y_lags,
                           explain_xreg_lags,
                           group_lags = NULL,
                           verbose = "basic",
                           iterative = FALSE,
                           iterative_args = list(),
                           asymmetric,
                           causal_ordering,
                           confounding,
                           is_python,
                           output_args = list(),
                           extra_computation_args = list(),
                           testing = FALSE,
                           ...) {
  # Check input type for approach

  # approach is checked more comprehensively later
  if (!is.logical(iterative) && length(iterative) == 1) {
    stop("`iterative` must be a single logical.")
  }
  if (!is.list(iterative_args)) {
    stop("`iterative_args` must be a list.")
  }
  if (!is.list(output_args)) {
    stop("`output_args` must be a list.")
  }
  if (!is.list(extra_computation_args)) {
    stop("`extra_computation_args` must be a list.")
  }



  # max_n_coalitions
  if (!is.null(max_n_coalitions) &&
    !(is.wholenumber(max_n_coalitions) &&
      length(max_n_coalitions) == 1 &&
      !is.na(max_n_coalitions) &&
      max_n_coalitions > 0)) {
    stop("`max_n_coalitions` must be NULL or a single positive integer.")
  }

  # group (checked more thoroughly later)
  if (!is.null(group) &&
    !is.list(group)) {
    stop("`group` must be NULL or a list")
  }

  # n_MC_samples
  if (!(is.wholenumber(n_MC_samples) &&
    length(n_MC_samples) == 1 &&
    !is.na(n_MC_samples) &&
    n_MC_samples > 0)) {
    stop("`n_MC_samples` must be a single positive integer.")
  }


  # type
  if (!(type %in% c("regular", "forecast"))) {
    stop("`type` must be either `regular` or `forecast`.\n")
  }

  # verbose
  check_verbose(verbose)
  if (!is.null(verbose) &&
    (!is.character(verbose) || !(all(verbose %in% c("basic", "progress", "convergence", "shapley", "vS_details"))))
  ) {
    stop(
      paste0(
        "`verbose` must be NULL or a string (vector) containing one or more of the strings ",
        "`basic`, `progress`, `convergence`, `shapley`, `vS_details`.\n"
      )
    )
  }

  # parameters only used for type "forecast"
  if (type == "forecast") {
    if (!(is.wholenumber(horizon) && all(horizon > 0))) {
      stop("`horizon` must be a vector (or scalar) of positive integers.\n")
    }

    if (any(horizon != output_size)) {
      stop(paste0("`horizon` must match the output size of the model (", paste0(output_size, collapse = ", "), ").\n"))
    }

    if (!(length(train_idx) > 1 && is.wholenumber(train_idx) && all(train_idx > 0) && all(is.finite(train_idx)))) {
      stop("`train_idx` must be a vector of positive finite integers and length > 1.\n")
    }

    if (!(is.wholenumber(explain_idx) && all(explain_idx > 0) && all(is.finite(explain_idx)))) {
      stop("`explain_idx` must be a vector of positive finite integers.\n")
    }

    if (!(is.wholenumber(explain_y_lags) && all(explain_y_lags >= 0) && all(is.finite(explain_y_lags)))) {
      stop("`explain_y_lags` must be a vector of positive finite integers.\n")
    }

    if (!(is.wholenumber(explain_xreg_lags) && all(explain_xreg_lags >= 0) && all(is.finite(explain_xreg_lags)))) {
      stop("`explain_xreg_lags` must be a vector of positive finite integers.\n")
    }

    if (!(is.logical(group_lags) && length(group_lags) == 1)) {
      stop("`group_lags` must be a single logical.\n")
    }
  }

  # Parameter used in asymmetric and causal Shapley values (more in-depth checks later)
  if (!is.logical(asymmetric) || length(asymmetric) != 1) stop("`asymmetric` must be a single logical.\n")
  if (!is.null(confounding) && !is.logical(confounding)) stop("`confounding` must be a logical (vector).\n")
  if (!is.null(causal_ordering) && !is.list(causal_ordering)) stop("`causal_ordering` must be a list.\n")

  #### Tests combining more than one parameter ####
  # phi0 vs output_size
  if (!all((is.numeric(phi0)) &&
    all(length(phi0) == output_size) &&
    all(!is.na(phi0)))) {
    stop(paste0(
      "`phi0` (", paste0(phi0, collapse = ", "),
      ") must be numeric and match the output size of the model (",
      paste0(output_size, collapse = ", "), ")."
    ))
  }




  # Getting basic input parameters
  parameters <- list(
    approach = approach,
    phi0 = phi0,
    max_n_coalitions = max_n_coalitions,
    group = group,
    n_MC_samples = n_MC_samples,
    seed = seed,
    is_python = is_python,
    output_size = output_size,
    type = type,
    verbose = verbose,
    iterative = iterative,
    iterative_args = iterative_args,
    output_args = output_args,
    extra_computation_args = extra_computation_args,
    asymmetric = asymmetric,
    causal_ordering = causal_ordering,
    confounding = confounding,
    testing = testing
  )

  # Additional forecast-specific arguments, only added for type="forecast"
  if (type == "forecast") {
    output_labels <-
      cbind(rep(explain_idx, horizon), rep(seq_len(horizon), each = length(explain_idx)))
    colnames(output_labels) <- c("explain_idx", "horizon")

    explain_lags <- list(y = explain_y_lags, xreg = explain_xreg_lags)

    parameters$horizon <- horizon
    parameters$train_idx <- train_idx
    parameters$explain_idx <- explain_idx
    parameters$group_lags <- group_lags
    parameters$output_labels <- output_labels
    parameters$explain_lags <- explain_lags
  }

  # Getting additional parameters from ...
  parameters <- append(parameters, list(...))

  # Set boolean to represent if a regression approach is used (any in case of several approaches)
  parameters$regression <- any(grepl("regression", parameters$approach))

  return(parameters)
}

#' Function that checks the verbose parameter
#'
#' @inheritParams explain
#'
#' @return The function does not return anything.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen, Martin Jullum
check_verbose <- function(verbose) {
  if (!is.null(verbose) &&
    (!is.character(verbose) || !(all(verbose %in% c("basic", "progress", "convergence", "shapley", "vS_details"))))
  ) {
    stop(
      paste0(
        "`verbose` must be NULL or a string (vector) containing one or more of the strings ",
        "`basic`, `progress`, `convergence`, `shapley`, `vS_details`.\n"
      )
    )
  }
}

#' @keywords internal
get_data <- function(x_train, x_explain) {
  # Check data object type
  stop_message <- ""
  if (!is.matrix(x_train) && !is.data.frame(x_train)) {
    stop_message <- paste0(stop_message, "x_train should be a matrix or a data.frame/data.table.\n")
  }
  if (!is.matrix(x_explain) && !is.data.frame(x_explain)) {
    stop_message <- paste0(stop_message, "x_explain should be a matrix or a data.frame/data.table.\n")
  }
  if (stop_message != "") {
    stop(stop_message)
  }

  # Check column names
  if (all(is.null(colnames(x_train)))) {
    stop_message <- paste0(stop_message, "x_train misses column names.\n")
  }
  if (all(is.null(colnames(x_explain)))) {
    stop_message <- paste0(stop_message, "x_explain misses column names.\n")
  }
  if (stop_message != "") {
    stop(stop_message)
  }


  data <- list(
    x_train = data.table::as.data.table(x_train),
    x_explain = data.table::as.data.table(x_explain)
  )
}


#' @keywords internal
check_data <- function(internal) {
  # Check model and data compatability
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  model_feature_specs <- internal$objects$feature_specs

  x_train_feature_specs <- get_data_specs(x_train)
  x_explain_feature_specs <- get_data_specs(x_explain)

  factors_exists <- any(model_feature_specs$classes == "factor")

  NA_labels <- any(is.na(model_feature_specs$labels))
  NA_classes <- any(is.na(model_feature_specs$classes))
  NA_factor_levels <- any(is.na(model_feature_specs$factor_levels))

  if (is.null(model_feature_specs)) {
    message(
      "Note: You passed a model to explain() which is not natively supported, and did not supply a ",
      "'get_model_specs' function to explain().\n",
      "Consistency checks between model and data is therefore disabled.\n"
    )

    model_feature_specs <- x_train_feature_specs
  } else if (NA_labels) {
    message(
      "Note: Feature names extracted from the model contains NA.\n",
      "Consistency checks between model and data is therefore disabled.\n"
    )

    model_feature_specs <- x_train_feature_specs
  } else if (NA_classes) {
    message(
      "Note: Feature classes extracted from the model contains NA.\n",
      "Assuming feature classes from the data are correct.\n"
    )

    model_feature_specs$classes <- x_train_feature_specs$classes
    model_feature_specs$factor_levels <- x_train_feature_specs$factor_levels
  } else if (factors_exists && NA_factor_levels) {
    message(
      "Note: Feature factor levels extracted from the model contains NA.\n",
      "Assuming feature factor levels from the data are correct.\n"
    )

    model_feature_specs$factor_levels <- x_train_feature_specs$factor_levels
  }

  # Check model vs x_train (allowing different label ordering in specs from model)
  compare_feature_specs(model_feature_specs, x_train_feature_specs, "model", "x_train", sort_labels = TRUE)

  # Then x_train vs x_explain (requiring exact same order)
  compare_feature_specs(x_train_feature_specs, x_explain_feature_specs, "x_train", "x_explain")
}

#' @keywords internal
compare_feature_specs <- function(spec1, spec2, name1 = "model", name2 = "x_train", sort_labels = FALSE) {
  if (sort_labels) {
    compare_vecs(sort(spec1$labels), sort(spec2$labels), "names", name1, name2)
    compare_vecs(
      spec1$classes[sort(names(spec1$classes))],
      spec2$classes[sort(names(spec2$classes))], "classes", name1, name2
    )
  } else {
    compare_vecs(spec1$labels, spec2$labels, "names", name1, name2)
    compare_vecs(spec1$classes, spec2$classes, "classes", name1, name2)
  }

  factor_classes <- which(spec1$classes == "factor")
  if (length(factor_classes) > 0) {
    for (fact in names(factor_classes)) {
      vec_type <- paste0("factor levels for feature '", fact, "'")
      compare_vecs(spec1$factor_levels[[fact]], spec2$factor_levels[[fact]], vec_type, name1, name2)
    }
  }
}

#' This includes both extra parameters and other objects
#' @keywords internal
get_extra_parameters <- function(internal, type) {
  if (type == "forecast") {
    if (internal$parameters$group_lags) {
      internal$parameters$group <- internal$data$group
    }
    internal$parameters$horizon_features <- lapply(
      internal$data$horizon_group,
      function(x) as.character(unlist(internal$data$group[x]))
    )
  }

  # get number of features and observations to explain
  internal$parameters$n_features <- ncol(internal$data$x_explain)
  internal$parameters$n_explain <- nrow(internal$data$x_explain)
  internal$parameters$n_train <- nrow(internal$data$x_train)

  # Names of features (already checked to be OK)
  internal$parameters$feature_names <- names(internal$data$x_explain)

  # Update feature_specss (in case model based spec included NAs)
  internal$objects$feature_specs <- get_data_specs(internal$data$x_explain)

  internal$parameters$is_groupwise <- !is.null(internal$parameters$group)

  # Processes groups if specified. Otherwise do nothing
  if (internal$parameters$is_groupwise) {
    group <- internal$parameters$group

    # Make group names if not existing
    if (is.null(names(group))) {
      message(
        "\nSuccess with message:\n
      Group names not provided. Assigning them the default names 'group1', 'group2', 'group3' etc."
      )
      names(group) <- paste0("group", seq_along(group))
    }

    # Make group list with numeric feature indicators
    internal$objects$coal_feature_list <- lapply(group, FUN = function(x) {
      match(x, internal$parameters$feature_names)
    })

    internal$parameters$n_groups <- length(group)
    internal$parameters$group_names <- names(group)
    internal$parameters$group <- group

    if (type == "forecast") {
      if (internal$parameters$group_lags) {
        internal$parameters$horizon_group <- internal$data$horizon_group
        internal$parameters$shap_names <- internal$data$shap_names
      } else {
        internal$parameters$shap_names <- internal$parameters$group_names
      }
    } else {
      # For regular explain
      internal$parameters$shap_names <- internal$parameters$group_names
    }
  } else {
    internal$objects$coal_feature_list <- as.list(seq_len(internal$parameters$n_features))

    internal$parameters$n_groups <- NULL
    internal$parameters$group_names <- NULL
    internal$parameters$shap_names <- internal$parameters$feature_names
  }
  internal$parameters$n_shapley_values <- length(internal$parameters$shap_names)


  # Get the number of unique approaches
  internal$parameters$n_approaches <- length(internal$parameters$approach)
  internal$parameters$n_unique_approaches <- length(unique(internal$parameters$approach))

  return(internal)
}

#' Fetches feature information from a given data set
#'
#' @param x data.frame or data.table.
#' The data to extract feature information from.
#'
#' @details This function is used to extract the feature information to be checked against the corresponding
#' information extracted from the model and other data sets.
#' The function is only called internally
#'
#' @return A list with the following elements:
#' \describe{
#'   \item{labels}{character vector with the feature names to compute Shapley values for}
#'   \item{classes}{a named character vector with the labels as names and the class types as elements}
#'   \item{factor_levels}{a named list with the labels as names and character vectors with the factor levels as elements
#'   (NULL if the feature is not a factor)}
#' }
#' @author Martin Jullum
#' @keywords internal
#'
#' @examples
#' # Load example data
#' \dontrun{
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' # Split data into test- and training data
#' x_train <- head(airquality, -3)
#' x_explain <- tail(airquality, 3)
#' # Split data into test- and training data
#' x_train <- data.table::as.data.table(head(airquality))
#' x_train[, Temp := as.factor(Temp)]
#' shapr:::get_data_specs(x_train)
#' }
get_data_specs <- function(x) {
  feature_specs <- list()
  feature_specs$labels <- names(x)
  feature_specs$classes <- unlist(lapply(x, class))
  feature_specs$factor_levels <- lapply(x, levels)

  # Defining all integer values as numeric
  feature_specs$classes[feature_specs$classes == "integer"] <- "numeric"

  return(feature_specs)
}

#' @keywords internal
check_and_set_parameters <- function(internal, type) {
  feature_names <- internal$parameters$feature_names
  confounding <- internal$parameters$confounding
  asymmetric <- internal$parameters$asymmetric
  regression <- internal$parameters$regression

  if (type == "forecast") {
    horizon <- internal$parameters$horizon
    horizon_group <- internal$parameters$horizon_group
    group <- internal$parameters$group[horizon_group[horizon][[1]]]
  } else {
    group <- internal$parameters$group
  }

  # Check group
  if (!is.null(group)) check_groups(feature_names, group)

  # Check approach
  check_approach(internal)

  # Check the arguments related to asymmetric and causal Shapley
  # Check the causal_ordering, which must happen before checking the causal sampling
  if (type == "regular") internal <- check_and_set_causal_ordering(internal)
  if (!is.null(confounding)) internal <- check_and_set_confounding(internal)

  # Check the causal sampling
  internal <- check_and_set_causal_sampling(internal)
  if (asymmetric) internal <- check_and_set_asymmetric(internal)

  # Adjust max_n_coalitions
  internal$parameters$max_n_coalitions <- adjust_max_n_coalitions(internal)

  check_max_n_coalitions_fc(internal)

  internal <- set_output_parameters(internal)

  internal <- check_and_set_iterative(internal) # sets the iterative parameter if it is NULL (default)

  # Set if we are to do exact Shapley value computations or not
  internal <- set_exact(internal)

  internal <- set_extra_comp_params(internal)

  # Give warnings to the user about long computation times
  check_computability(internal)

  # Check regression if we are doing regression
  if (regression) internal <- check_regression(internal)

  return(internal)
}


#' @keywords internal
#' @author Lars Henry Berge Olsen
check_and_set_causal_ordering <- function(internal) {
  # Extract the needed variables/objects from the internal list
  n_shapley_values <- internal$parameters$n_shapley_values
  causal_ordering <- internal$parameters$causal_ordering
  is_groupwise <- internal$parameters$is_groupwise
  feat_group_txt <- ifelse(is_groupwise, "group", "feature")
  group <- internal$parameters$group
  feature_names <- internal$parameters$feature_names
  group_names <- internal$parameters$group_names

  # Get the labels of the features or groups, and the number of them
  labels_now <- if (is_groupwise) group_names else feature_names

  # If `causal_ordering` is NULL, then convert it to a list with a single component containing all features/groups
  if (is.null(causal_ordering)) causal_ordering <- list(seq(n_shapley_values))

  # Ensure that causal_ordering represents the causal ordering using the feature/group index representation
  if (is.character(unlist(causal_ordering))) {
    causal_ordering <- convert_feature_name_to_idx(causal_ordering, labels_now, feat_group_txt)
  }
  if (!is.numeric(unlist(causal_ordering))) {
    stop(paste0(
      "`causal_ordering` must be a list containg either only integers representing the ", feat_group_txt,
      " indices or the ", feat_group_txt, " names as strings. See the documentation for more details.\n"
    ))
  }

  # Ensure that causal_ordering_names represents the causal ordering using the feature name representation
  causal_ordering_names <- relist(labels_now[unlist(causal_ordering)], causal_ordering)

  # Check that the we have n_features elements and that they are 1 through n_features (i.e., no duplicates).
  causal_ordering_vec_sort <- sort(unlist(causal_ordering))
  if (length(causal_ordering_vec_sort) != n_shapley_values || any(causal_ordering_vec_sort != seq(n_shapley_values))) {
    stop(paste0(
      "`causal_ordering` is incomplete/incorrect. It must contain all ",
      feat_group_txt, " names or indices exactly once.\n"
    ))
  }

  # For groups we need to convert from group level to feature level
  if (is_groupwise) {
    group_num <- unname(lapply(group, function(x) match(x, feature_names)))
    causal_ordering_features <- lapply(causal_ordering, function(component_i) unlist(group_num[component_i]))
    causal_ordering_features_names <- relist(feature_names[unlist(causal_ordering_features)], causal_ordering_features)
    internal$parameters$causal_ordering_features <- causal_ordering_features
    internal$parameters$causal_ordering_features_names <- causal_ordering_features_names
  }

  # Update the parameters in the internal list
  internal$parameters$causal_ordering <- causal_ordering
  internal$parameters$causal_ordering_names <- causal_ordering_names
  internal$parameters$causal_ordering_names_string <-
    paste0("{", paste(sapply(causal_ordering_names, paste, collapse = ", "), collapse = "}, {"), "}")

  return(internal)
}


#' @keywords internal
#' @author Lars Henry Berge Olsen
check_and_set_confounding <- function(internal) {
  causal_ordering <- internal$parameters$causal_ordering
  causal_ordering_names <- internal$parameters$causal_ordering_names
  confounding <- internal$parameters$confounding

  # Check that confounding is either specified globally or locally
  if (length(confounding) > 1 && length(confounding) != length(causal_ordering)) {
    stop(paste0(
      "`confounding` must either be a single logical or a vector of logicals of the same length as ",
      "the number of components in `causal_ordering` (", length(causal_ordering), ").\n"
    ))
  }

  # Replicate the global confounding value across all components
  if (length(confounding) == 1) confounding <- rep(confounding, length(causal_ordering))

  # Update the parameters in the internal list
  internal$parameters$confounding <- confounding

  # String with information about which components that are subject to confounding (used by cli)
  if (all(!confounding)) {
    internal$parameters$confounding_string <- "No component with confounding"
  } else {
    internal$parameters$confounding_string <-
      paste0("{", paste(sapply(causal_ordering_names[confounding], paste, collapse = ", "), collapse = "}, {"), "}")
  }

  return(internal)
}


#' @keywords internal
#' @author Lars Henry Berge Olsen
check_and_set_causal_sampling <- function(internal) {
  confounding <- internal$parameters$confounding
  causal_ordering <- internal$parameters$causal_ordering

  # The variable `causal_sampling` represents if we are to use the causal step-wise sampling procedure. We only want to
  # do that when confounding is specified, and we have a causal ordering that contains more than one component or
  # if we have a single component where the features are subject to confounding. We must use `all` to support
  # `confounding` being a vector,  but then `length(causal_ordering) > 1`, so `causal` will be TRUE no matter what
  # `confounding` vector we have.
  internal$parameters$causal_sampling <- !is.null(confounding) && (length(causal_ordering) > 1 || all(confounding))

  # For the causal/step-wise sampling procedure, we do not support multiple approaches and regression is inapplicable
  if (internal$parameters$causal_sampling) {
    if (internal$parameters$regression) stop("Causal Shapley values is not applicable for regression approaches.\n")
    if (internal$parameters$n_approaches > 1) stop("Causal Shapley values is not applicable for combined approaches.\n")
  }

  return(internal)
}


#' @keywords internal
#' @author Lars Henry Berge Olsen
check_and_set_asymmetric <- function(internal) {
  asymmetric <- internal$parameters$asymmetric
  # exact <- internal$parameters$exact
  causal_ordering <- internal$parameters$causal_ordering
  max_n_coalitions <- internal$parameters$max_n_coalitions

  # Get the number of coalitions that respects the (partial) causal ordering
  internal$parameters$max_n_coalitions_causal <- get_max_n_coalitions_causal(causal_ordering = causal_ordering)

  # Get the coalitions that respects the (partial) causal ordering
  internal$objects$dt_valid_causal_coalitions <- exact_coalition_table(
    m = internal$parameters$n_shapley_values,
    dt_valid_causal_coalitions = data.table(coalitions = get_valid_causal_coalitions(causal_ordering = causal_ordering))
  )

  # Normalize the weights. Note that weight of a coalition size is even spread out among the valid coalitions
  # of each size. I.e., if there is only one valid coalition of size |S|, then it gets the weight of the
  # choose(M, |S|) coalitions of said size.
  internal$objects$dt_valid_causal_coalitions[-c(1, .N), shapley_weight_norm := shapley_weight / sum(shapley_weight)]

  # Convert the coalitions to strings. Needed when sampling the coalitions in `sample_coalition_table()`.
  internal$objects$dt_valid_causal_coalitions[, coalitions_str := sapply(coalitions, paste, collapse = " ")]

  return(internal)
}


#' @keywords internal
adjust_max_n_coalitions <- function(internal) {
  is_groupwise <- internal$parameters$is_groupwise
  max_n_coalitions <- internal$parameters$max_n_coalitions
  n_features <- internal$parameters$n_features
  n_groups <- internal$parameters$n_groups
  n_shapley_values <- internal$parameters$n_shapley_values
  asymmetric <- internal$parameters$asymmetric # NULL if regular/symmetric Shapley values
  max_n_coalitions_causal <- internal$parameters$max_n_coalitions_causal # NULL if regular/symmetric Shapley values


  # Adjust max_n_coalitions
  if (isTRUE(asymmetric)) {
    # Asymmetric Shapley values

    # Set max_n_coalitions to upper bound
    if (is.null(max_n_coalitions) || max_n_coalitions > max_n_coalitions_causal) {
      max_n_coalitions <- max_n_coalitions_causal
      message(
        paste0(
          "Success with message:\n",
          "max_n_coalitions is NULL or larger than or number of coalitions respecting the causal\n",
          "ordering ", max_n_coalitions_causal, ", and is therefore set to ", max_n_coalitions_causal, ".\n"
        )
      )
    }

    # Set max_n_coalitions to lower bound
    if (isFALSE(is.null(max_n_coalitions)) &&
      max_n_coalitions < min(10, n_shapley_values + 1, max_n_coalitions_causal)) {
      if (max_n_coalitions_causal <= 10) {
        max_n_coalitions <- max_n_coalitions_causal
        message(
          paste0(
            "Success with message:\n",
            "max_n_coalitions_causal is smaller than or equal to 10, meaning there are\n",
            "so few unique causal coalitions that we should use all to get reliable results.\n",
            "max_n_coalitions is therefore set to ", max_n_coalitions_causal, ".\n"
          )
        )
      } else {
        max_n_coalitions <- min(10, n_shapley_values + 1, max_n_coalitions_causal)
        message(
          paste0(
            "Success with message:\n",
            "max_n_coalitions is smaller than max(10, n_shapley_values + 1 = ", n_shapley_values + 1,
            " max_n_coalitions_causal = ", max_n_coalitions_causal, "),",
            "which will result in unreliable results.\n",
            "It is therefore set to ", min(10, n_shapley_values + 1, max_n_coalitions_causal), ".\n"
          )
        )
      }
    }
  } else {
    # Symmetric/regular Shapley values

    if (isFALSE(is_groupwise)) { # feature wise
      # Set max_n_coalitions to upper bound
      if (is.null(max_n_coalitions) || max_n_coalitions > 2^n_features) {
        max_n_coalitions <- 2^n_features
        message(
          paste0(
            "Success with message:\n",
            "max_n_coalitions is NULL or larger than or 2^n_features = ", 2^n_features, ", \n",
            "and is therefore set to 2^n_features = ", 2^n_features, ".\n"
          )
        )
      }
      # Set max_n_coalitions to lower bound
      if (isFALSE(is.null(max_n_coalitions)) && max_n_coalitions < min(10, n_features + 1)) {
        if (n_features <= 3) {
          max_n_coalitions <- 2^n_features
          message(
            paste0(
              "Success with message:\n",
              "n_features is smaller than or equal to 3, meaning there are so few unique coalitions (",
              2^n_features, ") that we should use all to get reliable results.\n",
              "max_n_coalitions is therefore set to 2^n_features = ", 2^n_features, ".\n"
            )
          )
        } else {
          max_n_coalitions <- min(10, n_features + 1)
          message(
            paste0(
              "Success with message:\n",
              "max_n_coalitions is smaller than max(10, n_features + 1 = ", n_features + 1, "),",
              "which will result in unreliable results.\n",
              "It is therefore set to ", max(10, n_features + 1), ".\n"
            )
          )
        }
      }
    } else { # group wise
      # Set max_n_coalitions to upper bound
      if (is.null(max_n_coalitions) || max_n_coalitions > 2^n_shapley_values) {
        max_n_coalitions <- 2^n_shapley_values
        message(
          paste0(
            "Success with message:\n",
            "max_n_coalitions is NULL or larger than or 2^n_groups = ", 2^n_shapley_values, ", \n",
            "and is therefore set to 2^n_groups = ", 2^n_shapley_values, ".\n"
          )
        )
      }
      # Set max_n_coalitions to lower bound
      if (isFALSE(is.null(max_n_coalitions)) && max_n_coalitions < min(10, n_shapley_values + 1)) {
        if (n_shapley_values <= 3) {
          max_n_coalitions <- 2^n_shapley_values
          message(
            paste0(
              "Success with message:\n",
              "n_groups is smaller than or equal to 3, meaning there are so few unique coalitions (",
              2^n_shapley_values, ") that we should use all to get reliable results.\n",
              "max_n_coalitions is therefore set to 2^n_groups = ", 2^n_shapley_values, ".\n"
            )
          )
        } else {
          max_n_coalitions <- min(10, n_shapley_values + 1)
          message(
            paste0(
              "Success with message:\n",
              "max_n_coalitions is smaller than max(10, n_groups + 1 = ", n_shapley_values + 1, "),",
              "which will result in unreliable results.\n",
              "It is therefore set to ", max(10, n_shapley_values + 1), ".\n"
            )
          )
        }
      }
    }
  }

  return(max_n_coalitions)
}

check_max_n_coalitions_fc <- function(internal) {
  is_groupwise <- internal$parameters$is_groupwise
  max_n_coalitions <- internal$parameters$max_n_coalitions
  n_features <- internal$parameters$n_features
  n_groups <- internal$parameters$n_groups
  n_shapley_values <- internal$parameters$n_shapley_values

  type <- internal$parameters$type

  if (type == "forecast") {
    horizon <- internal$parameters$horizon
    explain_y_lags <- internal$parameters$explain_lags$y
    explain_xreg_lags <- internal$parameters$explain_lags$xreg
    xreg <- internal$data$xreg

    if (!is_groupwise) {
      if (max_n_coalitions <= n_shapley_values) {
        stop(paste0(
          "`max_n_coalitions` (", max_n_coalitions, ") has to be greater than the number of ",
          "components to decompose the forecast onto:\n",
          "`horizon` (", horizon, ") + `explain_y_lags` (", explain_y_lags, ") ",
          "+ sum(`explain_xreg_lags`) (", sum(explain_xreg_lags), ").\n"
        ))
      }
    } else {
      if (max_n_coalitions <= n_shapley_values) {
        stop(paste0(
          "`max_n_coalitions` (", max_n_coalitions, ") has to be greater than the number of ",
          "components to decompose the forecast onto:\n",
          "ncol(`xreg`) (", ncol(`xreg`), ") + 1"
        ))
      }
    }
  }
}

#' @author Martin Jullum
#' @keywords internal
set_output_parameters <- function(internal) {
  output_args <- internal$parameters$output_args

  # Get defaults
  output_args <- utils::modifyList(get_output_args_default(),
    output_args,
    keep.null = TRUE
  )

  check_output_args(output_args)

  internal$parameters$output_args <- output_args

  return(internal)
}

#' Gets the default values for the output arguments
#'
#' @param keep_samp_for_vS Logical.
#' Indicates whether the samples used in the Monte Carlo estimation of v_S should be returned (in `internal$output`).
#' Not used for `approach="regression_separate"` or `approach="regression_surrogate"`.
#' @param MSEv_uniform_comb_weights Logical.
#' If `TRUE` (default), then the function weights the coalitions uniformly when computing the MSEv criterion.
#' If `FALSE`, then the function use the Shapley kernel weights to weight the coalitions when computing the MSEv
#' criterion.
#' Note that the Shapley kernel weights are replaced by the sampling frequency when not all coalitions are considered.
#' @param saving_path String.
#' The path to the directory where the results of the iterative estimation procedure should be saved.
#' Defaults to a temporary directory.
#' @export
#' @author Martin Jullum
get_output_args_default <- function(keep_samp_for_vS = FALSE,
                                    MSEv_uniform_comb_weights = TRUE,
                                    saving_path = tempfile("shapr_obj_", fileext = ".rds")) {
  return(mget(methods::formalArgs(get_output_args_default)))
}

#' @keywords internal
check_output_args <- function(output_args) {
  list2env(output_args, envir = environment()) # Make accessible in the environment

  # Check the output_args elements

  # keep_samp_for_vS
  if (!(is.logical(keep_samp_for_vS) &&
    length(keep_samp_for_vS) == 1)) {
    stop("`output_args$keep_samp_for_vS` must be single logical.")
  }

  # Parameter used in the MSEv evaluation criterion
  if (!(is.logical(MSEv_uniform_comb_weights) && length(MSEv_uniform_comb_weights) == 1)) {
    stop("`output_args$MSEv_uniform_comb_weights` must be single logical.")
  }

  # saving_path
  if (!(is.character(saving_path) &&
    length(saving_path) == 1)) {
    stop("`output_args$saving_path` must be a single character.")
  }

  # Also check that saving_path exists, and abort if not...
  if (!dir.exists(dirname(saving_path))) {
    stop(
      paste0(
        "Directory ", dirname(saving_path), " in the output_args$saving_path does not exists.\n",
        "Please create the directory with `dir.create('", dirname(saving_path), "')` or use another directory."
      )
    )
  }
}


#' @author Martin Jullum
#' @keywords internal
set_extra_comp_params <- function(internal) {
  extra_computation_args <- internal$parameters$extra_computation_args

  # Get defaults
  extra_computation_args <- utils::modifyList(get_extra_comp_args_default(internal),
    extra_computation_args,
    keep.null = TRUE
  )

  # Check the output_args elements
  check_extra_computation_args(extra_computation_args)

  extra_computation_args <- trans_null_extra_est_args(extra_computation_args)

  # Check that we are not doing paired sampling when computing asymmetric Shapley values
  if (internal$parameters$asymmetric && extra_computation_args$paired_shap_sampling) {
    stop(paste0(
      "Set `paired_shap_sampling = FALSE` to compute asymmetric Shapley values.\n",
      "Asymmetric Shapley values do not support paired sampling as the paired ",
      "coalitions will not necessarily respect the causal ordering."
    ))
  }

  internal$parameters$extra_computation_args <- extra_computation_args

  return(internal)
}

#' Gets the default values for the extra estimation arguments
#'
#' @param paired_shap_sampling Logical.
#' If `TRUE` paired versions of all sampled coalitions are also included in the computation.
#' That is, if there are 5 features and e.g. coalitions (1,3,5) are sampled, then also coalition (2,4) is used for
#' computing the Shapley values. This is done to reduce the variance of the Shapley value estimates.
#' `TRUE` is the default and is recommended for highest accuracy.
#' For asymmetric, `FALSE` is the default and the only legal value.
#' @param kernelSHAP_reweighting String.
#' How to reweight the sampling frequency weights in the kernelSHAP solution after sampling.
#' The aim of this is to reduce the randomness and thereby the variance of the Shapley value estimates.
#' The options are one of `'none'`, `'on_N'`, `'on_all'`, `'on_all_cond'` (default).
#' `'none'` means no reweighting, i.e. the sampling frequency weights are used as is.
#' `'on_N'` means the sampling frequencies are averaged over all coalitions with the same original sampling
#' probabilities.
#' `'on_all'` means the original sampling probabilities are used for all coalitions.
#' `'on_all_cond'` means the original sampling probabilities are used for all coalitions, while adjusting for the
#' probability that they are sampled at least once.
#' `'on_all_cond'` is preferred as it performs the best in simulation studies, see Olsen & Jullum (2024).
#' @param compute_sd Logical. Whether to estimate the standard deviations of the Shapley value estimates. This is TRUE
#' whenever sampling based kernelSHAP is applied (either iteratively or with a fixed number of coalitions).
#' @param n_boot_samps Integer. The number of bootstrapped samples (i.e. samples with replacement) from the set of all
#' coalitions used to estimate the standard deviations of the Shapley value estimates.
#' @param max_batch_size Integer. The maximum number of coalitions to estimate simultaneously within each iteration.
#' A larger numbers requires more memory, but may have a slight computational advantage.
#' @param min_n_batches Integer. The minimum number of batches to split the computation into within each iteration.
#' Larger numbers gives more frequent progress updates. If parallelization is applied, this should be set no smaller
#' than the number of parallel workers.
#' @inheritParams default_doc_export
#' @export
#' @author Martin Jullum
get_extra_comp_args_default <- function(internal, # Only used to get the default value of compute_sd
                                        paired_shap_sampling = isFALSE(internal$parameters$asymmetric),
                                        kernelSHAP_reweighting = "on_all_cond",
                                        compute_sd = isFALSE(internal$parameters$exact),
                                        n_boot_samps = 100,
                                        max_batch_size = 10,
                                        min_n_batches = 10) {
  return(mget(methods::formalArgs(get_extra_comp_args_default)[-1])) # [-1] to exclude internal
}

#' @keywords internal
check_extra_computation_args <- function(extra_computation_args) {
  list2env(extra_computation_args, envir = environment()) # Make accessible in the environment

  # paired_shap_sampling
  if (!is.logical(paired_shap_sampling) && length(paired_shap_sampling) == 1) {
    stop("`paired_shap_sampling` must be a single logical.")
  }

  # kernelSHAP_reweighting
  if (!(length(kernelSHAP_reweighting) == 1 && kernelSHAP_reweighting %in%
    c("none", "on_N", "on_all", "on_all_cond"))) {
    stop("`kernelSHAP_reweighting` must be one of `none`, `on_N`, `on_all`, `on_all_cond`.\n")
  }

  # compute_sd
  if (!(is.logical(compute_sd) &&
    length(compute_sd) == 1)) {
    stop("`extra_computation_args$compute_sd` must be single logical.")
  }

  # n_boot_samps
  if (!(is.wholenumber(n_boot_samps) &&
    length(n_boot_samps) == 1 &&
    !is.na(n_boot_samps) &&
    n_boot_samps > 0)) {
    stop("`extra_computation_args$n_boot_samps` must be a single positive integer.")
  }

  # max_batch_size
  if (!is.null(max_batch_size) &&
    !((is.wholenumber(max_batch_size) || is.infinite(max_batch_size)) &&
      length(max_batch_size) == 1 &&
      !is.na(max_batch_size) &&
      max_batch_size > 0)) {
    stop("`extra_computation_args$max_batch_size` must be NULL, Inf or a single positive integer.")
  }

  # min_n_batches
  if (!is.null(min_n_batches) &&
    !(is.wholenumber(min_n_batches) &&
      length(min_n_batches) == 1 &&
      !is.na(min_n_batches) &&
      min_n_batches > 0)) {
    stop("`extra_computation_args$min_n_batches` must be NULL or a single positive integer.")
  }
}

#' @keywords internal
trans_null_extra_est_args <- function(extra_computation_args) {
  list2env(extra_computation_args, envir = environment())

  # Translating NULL to always return n_batches = 1 (if just one approach)
  extra_computation_args$min_n_batches <- ifelse(is.null(min_n_batches), 1, min_n_batches)
  extra_computation_args$max_batch_size <- ifelse(is.null(max_batch_size), Inf, max_batch_size)

  return(extra_computation_args)
}

#' @keywords internal
check_and_set_iterative <- function(internal) {
  iterative <- internal$parameters$iterative
  approach <- internal$parameters$approach

  # Always iterative = FALSE for vaeac and regression_surrogate
  if (any(approach %in% c("vaeac", "regression_surrogate"))) {
    unsupported <- approach[approach %in% c("vaeac", "regression_surrogate")]

    if (isTRUE(iterative)) {
      warning(
        paste0(
          "Iterative estimation of Shapley values are not supported for approach = ",
          paste0(unsupported, collapse = ", "), ". Setting iterative = FALSE."
        )
      )
    }

    internal$parameters$iterative <- FALSE
  } else {
    # Sets the default value of iterative to TRUE if computing more than 5 Shapley values for all other approaches
    if (is.null(iterative)) {
      n_shapley_values <- internal$parameters$n_shapley_values # n_features if feature-wise and n_groups if group-wise
      internal$parameters$iterative <- isTRUE(n_shapley_values > 5)
    }
  }

  return(internal)
}

#' @keywords internal
set_exact <- function(internal) {
  max_n_coalitions <- internal$parameters$max_n_coalitions
  n_shapley_values <- internal$parameters$n_shapley_values
  iterative <- internal$parameters$iterative
  asymmetric <- internal$parameters$asymmetric
  max_n_coalitions_causal <- internal$parameters$max_n_coalitions_causal

  if (isFALSE(iterative) &&
    (
      (isTRUE(asymmetric) && max_n_coalitions == max_n_coalitions_causal) ||
        (max_n_coalitions == 2^n_shapley_values)
    )
  ) {
    exact <- TRUE
  } else {
    exact <- FALSE
  }

  internal$parameters$exact <- exact

  return(internal)
}


#' @keywords internal
check_computability <- function(internal) {
  is_groupwise <- internal$parameters$is_groupwise
  max_n_coalitions <- internal$parameters$max_n_coalitions
  n_features <- internal$parameters$n_features
  n_groups <- internal$parameters$n_groups
  exact <- internal$parameters$exact
  causal_sampling <- internal$parameters$causal_sampling # NULL if regular/symmetric Shapley values
  asymmetric <- internal$parameters$asymmetric # NULL if regular/symmetric Shapley values
  max_n_coalitions_causal <- internal$parameters$max_n_coalitions_causal # NULL if regular/symmetric Shapley values

  if (asymmetric) {
    if (isTRUE(exact)) {
      if (max_n_coalitions_causal > 5000 && max_n_coalitions > 5000) {
        warning(
          paste0(
            "Due to computation time, we recommend not computing asymmetric Shapley values exactly \n",
            "with all valid causal coalitions (", max_n_coalitions_causal, ") when larger than 5000.\n",
            "Consider reducing max_n_coalitions and enabling iterative estimation with iterative = TRUE.\n"
          )
        )
      }
    }
  }

  # Force user to use a natural number for n_coalitions if m > 13
  if (isTRUE(exact)) {
    if (isFALSE(is_groupwise) && n_features > 13) {
      warning(
        paste0(
          "Due to computation time, we recommend not computing Shapley values exactly \n",
          "with all 2^n_features (", 2^n_features, ") coalitions for n_features > 13.\n",
          "Consider reducing max_n_coalitions and enabling iterative estimation with iterative = TRUE.\n"
        )
      )
    }
    if (isTRUE(is_groupwise) && n_groups > 13) {
      warning(
        paste0(
          "Due to computation time, we recommend not computing Shapley values exactly \n",
          "with all 2^n_groups (", 2^n_groups, ") coalitions for n_groups > 13.\n",
          "Consider reducing max_n_coalitions and enabling iterative estimation with iterative = TRUE.\n"
        )
      )
    }
    if (isTRUE(causal_sampling) && !is.null(max_n_coalitions_causal) && max_n_coalitions_causal > 1000) {
      paste0(
        "Due to computation time, we recommend not computing causal Shapley values exactly \n",
        "with all valid causal coalitions when there are more than 1000 due to the long causal sampling time. \n",
        "Consider reducing max_n_coalitions and enabling iterative estimation with iterative = TRUE.\n"
      )
    }
  } else {
    if (isFALSE(is_groupwise) && n_features > 30) {
      warning(
        "Due to computation time, we strongly recommend enabling iterative estimation with iterative = TRUE",
        " when n_features > 30.\n"
      )
    }
    if (isTRUE(is_groupwise) && n_groups > 30) {
      warning(
        "Due to computation time, we strongly recommend enabling iterative estimation with iterative = TRUE",
        " when n_groups > 30.\n"
      )
    }
    if (isTRUE(causal_sampling) && !is.null(max_n_coalitions_causal) && max_n_coalitions_causal > 1000) {
      warning(
        paste0(
          "Due to computation time, we strongly recommend enabling iterative estimation with iterative = TRUE ",
          "when the number of valid causal coalitions are more than 1000 due to the long causal sampling time. \n"
        )
      )
    }
  }
}


#' Check that the group parameter has the right form and content
#'
#' @keywords internal
check_groups <- function(feature_names, group) {
  if (!is.list(group)) {
    stop("group must be a list")
  }

  group_features <- unlist(group)

  # Checking that the group_features are characters
  if (!all(is.character(group_features))) {
    stop("All components of group should be a character.")
  }

  # Check that all features in group are in feature labels or used by model
  if (!all(group_features %in% feature_names)) {
    missing_group_feature <- group_features[!(group_features %in% feature_names)]
    stop(
      paste0(
        "The group feature(s) ", paste0(missing_group_feature, collapse = ", "), " are not\n",
        "among the features in the data: ", paste0(feature_names, collapse = ", "), ". Delete from group."
      )
    )
  }

  # Check that all feature used by model are in group
  if (!all(feature_names %in% group_features)) {
    missing_features <- feature_names[!(feature_names %in% group_features)]
    stop(
      paste0(
        "The data feature(s) ", paste0(missing_features, collapse = ", "), " do not\n",
        "belong to one of the groups. Add to a group."
      )
    )
  }

  # Check uniqueness of group_features
  if (length(group_features) != length(unique(group_features))) {
    dups <- group_features[duplicated(group_features)]
    stop(
      paste0(
        "Feature(s) ", paste0(dups, collapse = ", "), " are found in more than one group or ",
        "multiple times per group.\n",
        "Make sure each feature is only represented in one group, and only once."
      )
    )
  }

  # Check that there are at least two groups
  if (length(group) == 1) {
    stop(
      paste0(
        "You have specified only a single group named ", names(group), ", containing the features: ",
        paste0(group_features, collapse = ", "), ".\n ",
        "The predictions must be decomposed in at least two groups to be meaningful."
      )
    )
  }
}


#' @keywords internal
check_approach <- function(internal) {
  # Check length of approach

  approach <- internal$parameters$approach
  n_features <- internal$parameters$n_features
  supported_approaches <- get_supported_approaches()

  if (!(is.character(approach) &&
    (length(approach) == 1 || length(approach) == n_features - 1) &&
    all(is.element(approach, supported_approaches)))
  ) {
    stop(
      paste0(
        "`approach` must be one of the following: '", paste0(supported_approaches, collapse = "', '"), "'.\n",
        "These can also be combined (except 'regression_surrogate' and 'regression_separate') by passing a vector ",
        "of length one less than the number of features (", n_features - 1, ")."
      )
    )
  }

  if (length(approach) > 1 && any(grepl("regression", approach))) {
    stop("The `regression_separate` and `regression_surrogate` approaches cannot be combined with other approaches.")
  }
}

#' Gets the implemented approaches
#'
#' @return Character vector.
#' The names of the implemented approaches that can be passed to argument `approach` in [explain()].
#'
#' @export
get_supported_approaches <- function() {
  substring(rownames(attr(methods(prepare_data), "info")), first = 14)
}




#' @keywords internal
#' @author Lars Henry Berge Olsen
check_regression <- function(internal) {
  output_size <- internal$parameters$output_size
  type <- internal$parameters$type
  keep_samp_for_vS <- internal$parameters$output_args$keep_samp_for_vS

  # Check that the model outputs one-dimensional predictions
  if (output_size != 1) {
    stop("`regression_separate` and `regression_surrogate` only support models with one-dimensional output")
  }

  # Check that we are NOT explaining a forecast model
  if (type == "forecast") {
    stop("`regression_separate` and `regression_surrogate` does not support `forecast`.")
  }

  # Check that we are not to keep the Monte Carlo samples
  if (keep_samp_for_vS) {
    stop(paste(
      "`keep_samp_for_vS` must be `FALSE` for the `regression_separate` and `regression_surrogate`",
      "approaches as there are no Monte Carlo samples to keep for these approaches."
    ))
  }

  # Remove n_MC_samples if we are doing regression, as we are not doing MC sampling
  internal$parameters$n_MC_samples <- NULL

  return(internal)
}

#' @keywords internal
compare_vecs <- function(vec1, vec2, vec_type, name1, name2) {
  if (!identical(vec1, vec2)) {
    if (is.null(names(vec1))) {
      text_vec1 <- paste(vec1, collapse = ", ")
    } else {
      text_vec1 <- paste(names(vec1), vec1, sep = ": ", collapse = ", ")
    }
    if (is.null(names(vec2))) {
      text_vec2 <- paste(vec2, collapse = ", ")
    } else {
      text_vec2 <- paste(names(vec2), vec1, sep = ": ", collapse = ", ")
    }

    stop(paste0(
      "Feature ", vec_type, " are not identical for ", name1, " and ", name2, ".\n",
      name1, " provided: ", text_vec1, ",\n",
      name2, " provided: ", text_vec2, ".\n"
    ))
  }
}

#' @keywords internal
set_iterative_parameters <- function(internal, prev_iter_list = NULL) {
  iterative <- internal$parameters$iterative

  paired_shap_sampling <- internal$parameters$extra_computation_args$paired_shap_sampling

  iterative_args <- internal$parameters$iterative_args

  iterative_args <- utils::modifyList(get_iterative_args_default(internal),
    iterative_args,
    keep.null = TRUE
  )

  # Force setting the number of coalitions and iterations for non-iterative method
  if (isFALSE(iterative)) {
    iterative_args$max_iter <- 1
    iterative_args$initial_n_coalitions <- iterative_args$max_n_coalitions
  }

  # If paired_shap_sampling is TRUE, we need the number of coalitions to be even
  if (paired_shap_sampling) {
    iterative_args$initial_n_coalitions <- ceiling(iterative_args$initial_n_coalitions * 0.5) * 2
  }

  check_iterative_args(iterative_args)

  # Translate any null input
  iterative_args <- trans_null_iterative_args(iterative_args)

  internal$parameters$iterative_args <- iterative_args

  if (!is.null(prev_iter_list)) {
    # Update internal with the iter_list from prev_shapr_object
    internal$iter_list <- prev_iter_list

    # Conveniently allow running non-iterative estimation one step further
    if (isFALSE(iterative)) {
      internal$parameters$iterative_args$max_iter <- length(internal$iter_list) + 1
      internal$parameters$iterative_args$n_coal_next_iter_factor_vec <- NULL
    }

    # Update convergence data with NEW iterative arguments
    internal <- check_convergence(internal)

    # Check for convergence based on last iter_list with new iterative arguments
    check_vs_prev_shapr_object(internal)

    # Prepare next iteration
    internal <- prepare_next_iteration(internal)
  } else {
    internal$iter_list <- list()
    internal$iter_list[[1]] <- list(
      n_coalitions = iterative_args$initial_n_coalitions,
      new_n_coalitions = iterative_args$initial_n_coalitions,
      exact = internal$parameters$exact,
      compute_sd = internal$parameters$extra_computation_args$compute_sd,
      n_coal_next_iter_factor = iterative_args$n_coal_next_iter_factor_vec[1],
      n_batches = set_n_batches(iterative_args$initial_n_coalitions, internal)
    )
  }

  return(internal)
}

#' @keywords internal
check_iterative_args <- function(iterative_args) {
  list2env(iterative_args, envir = environment())

  # initial_n_coalitions
  if (!(is.wholenumber(initial_n_coalitions) &&
    length(initial_n_coalitions) == 1 &&
    !is.na(initial_n_coalitions) &&
    initial_n_coalitions <= max_n_coalitions &&
    initial_n_coalitions > 2)) {
    stop("`iterative_args$initial_n_coalitions` must be a single integer between 2 and `max_n_coalitions`.")
  }

  # fixed_n_coalitions
  if (!is.null(fixed_n_coalitions_per_iter) &&
    !(is.wholenumber(fixed_n_coalitions_per_iter) &&
      length(fixed_n_coalitions_per_iter) == 1 &&
      !is.na(fixed_n_coalitions_per_iter) &&
      fixed_n_coalitions_per_iter <= max_n_coalitions &&
      fixed_n_coalitions_per_iter > 0)) {
    stop(
      "`iterative_args$fixed_n_coalitions_per_iter` must be NULL or a single positive integer no larger than",
      "`max_n_coalitions`."
    )
  }

  # max_iter
  if (!is.null(max_iter) &&
    !((is.wholenumber(max_iter) || is.infinite(max_iter)) &&
      length(max_iter) == 1 &&
      !is.na(max_iter) &&
      max_iter > 0)) {
    stop("`iterative_args$max_iter` must be NULL, Inf or a single positive integer.")
  }

  # convergence_tol
  if (!is.null(convergence_tol) &&
    !(length(convergence_tol) == 1 &&
      !is.na(convergence_tol) &&
      convergence_tol >= 0)) {
    stop("`iterative_args$convergence_tol` must be NULL, 0, or a positive numeric.")
  }

  # n_coal_next_iter_factor_vec
  if (!is.null(n_coal_next_iter_factor_vec) &&
    !(all(!is.na(n_coal_next_iter_factor_vec)) &&
      all(n_coal_next_iter_factor_vec <= 1) &&
      all(n_coal_next_iter_factor_vec >= 0))) {
    stop("`iterative_args$n_coal_next_iter_factor_vec` must be NULL or a vector or numerics between 0 and 1.")
  }
}

#' @keywords internal
trans_null_iterative_args <- function(iterative_args) {
  list2env(iterative_args, envir = environment())

  # Translating NULL to always return n_batches = 1 (if just one approach)
  iterative_args$max_iter <- ifelse(is.null(max_iter), Inf, max_iter)

  return(iterative_args)
}


#' @keywords internal
set_n_batches <- function(n_coalitions, internal) {
  min_n_batches <- internal$parameters$extra_computation_args$min_n_batches
  max_batch_size <- internal$parameters$extra_computation_args$max_batch_size
  n_unique_approaches <- internal$parameters$n_unique_approaches


  # Restrict the sizes of the batches to max_batch_size, but require at least min_n_batches and n_unique_approaches
  suggested_n_batches <- max(min_n_batches, n_unique_approaches, ceiling(n_coalitions / max_batch_size))

  # Set n_batches to no less than n_coalitions
  n_batches <- min(n_coalitions, suggested_n_batches)

  return(n_batches)
}

#' @keywords internal
check_vs_prev_shapr_object <- function(internal) {
  iter <- length(internal$iter_list)

  converged <- internal$iter_list[[iter]]$converged
  converged_exact <- internal$iter_list[[iter]]$converged_exact
  converged_sd <- internal$iter_list[[iter]]$converged_sd
  converged_max_iter <- internal$iter_list[[iter]]$converged_max_iter
  converged_max_n_coalitions <- internal$iter_list[[iter]]$converged_max_n_coalitions

  if (isTRUE(converged)) {
    message0 <- "Convergence reached before estimation start.\n"
    if (isTRUE(converged_exact)) {
      message0 <- c(
        message0,
        "All coalitions estimated. No need for further estimation.\n"
      )
    }
    if (isTRUE(converged_sd)) {
      message0 <- c(
        message0,
        "Convergence tolerance reached. Consider decreasing `iterative_args$tolerance`.\n"
      )
    }
    if (isTRUE(converged_max_iter)) {
      message0 <- c(
        message0,
        "Maximum number of iterations reached. Consider increasing `iterative_args$max_iter`.\n"
      )
    }
    if (isTRUE(converged_max_n_coalitions)) {
      message0 <- c(
        message0,
        "Maximum number of coalitions reached. Consider increasing `max_n_coalitions`.\n"
      )
    }
    stop(message0)
  }
}

# Get functions ========================================================================================================
#' Function to specify arguments of the iterative estimation procedure
#'
#' @details The functions sets default values for the iterative estimation procedure, according to the function
#' defaults.
#' If the argument `iterative` of [explain()] is FALSE, it sets parameters corresponding to the use of a
#' non-iterative estimation procedure
#'
#' @param max_iter Integer. Maximum number of estimation iterations
#' @param initial_n_coalitions Integer. Number of coalitions to use in the first estimation iteration.
#' @param fixed_n_coalitions_per_iter Integer. Number of `n_coalitions` to use in each iteration.
#' `NULL` (default) means setting it based on estimates based on a set convergence threshold.
#' @param convergence_tol Numeric. The t variable in the convergence threshold formula on page 6 in the paper
#' Covert and Lee (2021), 'Improving KernelSHAP: Practical Shapley Value Estimation via Linear Regression'
#' https://arxiv.org/pdf/2012.01536. Smaller values requires more coalitions before convergence is reached.
#' @param n_coal_next_iter_factor_vec Numeric vector. The number of `n_coalitions` that must be used to reach
#' convergence in the next iteration is estimated.
#' The number of `n_coalitions` actually used in the next iteration is set to this estimate multiplied by
#' `n_coal_next_iter_factor_vec[i]` for iteration `i`.
#' It is wise to start with smaller numbers to avoid using too many `n_coalitions` due to uncertain estimates in
#' the first iterations.
#' @inheritParams default_doc_export
#'
#' @export
#' @author Martin Jullum
get_iterative_args_default <- function(internal,
                                       initial_n_coalitions = ceiling(
                                         min(
                                           200,
                                           max(
                                             5,
                                             internal$parameters$n_features,
                                             (2^internal$parameters$n_features) / 10
                                           ),
                                           internal$parameters$max_n_coalitions
                                         )
                                       ),
                                       fixed_n_coalitions_per_iter = NULL,
                                       max_iter = 20,
                                       convergence_tol = 0.02,
                                       n_coal_next_iter_factor_vec = c(seq(0.1, 1, by = 0.1), rep(1, max_iter - 10))) {
  iterative <- internal$parameters$iterative
  max_n_coalitions <- internal$parameters$max_n_coalitions

  if (isTRUE(iterative)) {
    ret_list <- mget(
      c(
        "initial_n_coalitions",
        "fixed_n_coalitions_per_iter",
        "max_n_coalitions",
        "max_iter",
        "convergence_tol",
        "n_coal_next_iter_factor_vec"
      )
    )
  } else {
    ret_list <- list(
      initial_n_coalitions = max_n_coalitions,
      fixed_n_coalitions_per_iter = NULL,
      max_n_coalitions = max_n_coalitions,
      max_iter = 1,
      convergence_tol = NULL,
      n_coal_next_iter_factor_vec = NULL
    )
  }
  return(ret_list)
}

#' Additional setup for regression-based methods
#'
#' @inheritParams default_doc_export
#'
#' @export
#' @keywords internal
additional_regression_setup <- function(internal, model, predict_model) {
  # This step needs to be called after predict_model is set, and therefore arrives at a later stage in explain()

  # Add the predicted response of the training and explain data to the internal list for regression-based methods.
  # Use isTRUE as `regression` is not present (NULL) for non-regression methods (i.e., Monte Carlo-based methods).
  if (isTRUE(internal$parameters$regression)) {
    internal <- regression.get_y_hat(internal = internal, model = model, predict_model = predict_model)
  }

  return(internal)
}
