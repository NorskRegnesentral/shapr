# Shapr functions ======================================================================================================
#' @rdname setup_approach
#'
#' @param regression_model A tidymodels object of class `model_specs`. Default is a linear regression model
#' ([parsnip::linear_reg()]). See \href{https://www.tidymodels.org/find/parsnip/}{tidymodels} for all possible models.
#' @param regression_tune_values Either NULL (default) or a data.frame/data.table/tibble containing the possible
#' hyper-parameter value combinations to try. The column names must match the names of the tuneable parameters specified
#' in `regression_model`.
#' @param regression_vfold_cv_para Either NULL (default) or a list containing
#' the parameters to be sent to [rsample::vfold_cv()].
#' @param regression_recipe_func Either NULL (default) or a function that must return
#' the RHS of the formula for arbitrary feature name inputs.
#' @inheritParams default_doc_explain
#'
#' @export
#' @author Lars Henry Berge Olsen
setup_approach.regression_separate <- function(internal,
                                               regression_model = parsnip::linear_reg(),
                                               regression_tune_values = NULL,
                                               regression_vfold_cv_para = NULL,
                                               regression_recipe_func = NULL,
                                               ...) {
  # Check that tidymodels is installed
  if (!requireNamespace("tidymodels", quietly = TRUE)) {
    stop("`tidymodels` is not installed. Please run install.packages('tidymodels')")
  }

  # Give a small warning that the time
  message(paste(
    "When using `approach = 'regression_separate'` the `explanation$timing$timing_secs` object can be",
    "missleading as `setup_computation` does not contain the training times of the regerssion models",
    "as they are trained on the fly in `compute_vS`. This is to reduce memory usage and for",
    "improved efficeny.\n"
  )) # TODO: should we add the time somewhere else?

  # Small printout to the user
  if (internal$parameters$verbose == 2) message("Starting 'setup_approach.regression_separate'.")

  # Check that the model outputs one-dimensional predictions
  if (internal$parameters$output_size != 1) {
    stop("`regression_separate` only supports models with one-dimensional output")
  }

  # Check that we are not to keep the Monte Carlo samples
  if (internal$parameters$keep_samp_for_vS) {
    stop("`keep_samp_for_vS` must be `FALSE` as there are no Monte Carlo samples to keep for this approach.")
  }

  # Add the default parameter values for the non-user specified parameters for the separate regression approach
  defaults <- mget(c("regression_model",
                     "regression_tune_values",
                     "regression_vfold_cv_para",
                     "regression_recipe_func"))
  internal <- insert_defaults(internal, defaults)

  # Check that it is a function that returns the RHS of the formula for arbitrary feature name inputs
  check_regression_recipe_func(internal$parameters$regression_recipe_func)

  # Check that `regression_vfold_cv_para` is either NULL or a named list that only contains recognized parameters
  check_regression_vfold_cv_para(internal$parameters$regression_vfold_cv_para)

  # Check and get if we are to tune the hyperparameters of the regression model
  internal$parameters$regression_tune <-
    get_regression_tune(internal$parameters$regression_model, internal$parameters$regression_tune_values)

  # Predict the response of the training and explain data. Former is the response the regression models are fitted to
  model <- eval.parent(match.call()[["model"]])
  internal$data$x_train_predicted_response <- predict_model(model, internal$data$x_train)
  internal$data$x_explain_predicted_response <- predict_model(model, internal$data$x_explain)

  # Small printout to the user
  if (internal$parameters$verbose == 2) message("Done with 'setup_approach.regression_separate'.\n")

  # Return the updated internal list
  return(internal)
}

#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.regression_separate <- function(internal, index_features = NULL, ...) {
  # Extract objects we need later
  data <- internal$data
  objects <- internal$objects
  parameters <- internal$parameters
  verbose <- parameters$verbose
  features <- objects$X$features[index_features]
  feature_names <- internal$parameters$feature_names
  regression_model <- parameters$regression_model
  regression_tune <- parameters$regression_tune
  regression_tune_values <- parameters$regression_tune_values
  regression_vfold_cv_para <- parameters$regression_vfold_cv_para
  regression_recipe_func <- parameters$regression_recipe_func

  # Small printout to the user
  if (verbose == 2) {
    message(paste0(
      "Working on batch ", internal$objects$X[id_combination == index_features[1]]$batch, " of ",
      internal$parameters$n_batches, " in `prepare_data.regression_separate()`."
    ))
  }

  # Initialize empty data table with specific column names and ensure that id_combination is integer. The data table
  # will contain the contribution function values for the coalitions given by `index_features` and all explicands.
  dt_res_column_names <- c("id_combination", paste0("p_hat1_", seq_len(internal$parameters$n_explain)))
  dt_res <- data.table(integer(0), matrix(ncol = length(dt_res_column_names) - 1, nrow = 0))
  data.table::setnames(dt_res, dt_res_column_names)

  # Iterate over the coalitions provided by index_features.
  # Note that index_features will never be NULL and never contain the empty or grand coalitions.
  for (j in seq_along(features)) {
    # Get the column indices of the features in current coalition
    current_coalition <- features[[j]]

    # Extract the current training (and add f(x) as response) and explain data
    current_x_train <- data$x_train[, ..current_coalition][, "y_train_hat" := data$x_train_predicted_response]
    current_x_explain <- data$x_explain[, ..current_coalition]

    # Create a recipe to the current training data.
    regression_recipe <- recipes::recipe(y_train_hat ~ ., data = current_x_train)

    # Update the recipe if user has provided a function for this. User is responsible for that the function works.
    # This function can, e.g., add transformations, normalization, dummy encoding, interactions, and so on.
    if (!is.null(regression_recipe_func)) regression_recipe <- regression_recipe_func(regression_recipe)

    # Combine workflow, model specification, and recipe
    regression_workflow <-
      workflows::workflow() %>%
      workflows::add_model(regression_model) %>%
      workflows::add_recipe(regression_recipe)

    # Check if we are to tune hyperparameters in the regression model, as we then need to update the workflow.
    # If we are not doing any hyperparameter tuning, then the workflow above is enough.
    if (regression_tune) {
      if (verbose == 2) message("Start tuning model...")

      # Set up the V-fold cross validation using the user provided parameters in `regression_vfold_cv_para`.
      # Note if `regression_vfold_cv_para` is NULL, then we use the default parameters in `vfold_cv()`.
      regression_folds <- do.call(rsample::vfold_cv, c(list(data = current_x_train), regression_vfold_cv_para))

      # Add the hyperparameter tuning to the workflow
      regression_results <-
        regression_workflow %>%
        tune::tune_grid(
          resamples = regression_folds,
          grid = regression_tune_values,
          metrics = yardstick::metric_set(rmse)
        )

      # Small printout to the user
      if (verbose == 2) print(regression_results %>% tune::collect_metrics(), n = 10^4) # Large number to print all rows
      if (verbose == 2) {
        best_results <- regression_results %>% tune::show_best(n = 1)
        feature_names <- names(regression_tune_values)
        feature_values <- best_results[feature_names]
        feature_names <- c(feature_names, "rmse")
        feature_values <- c(feature_values, round(best_results$mean, 3))
        message(paste0("Best CV: ", paste(paste(feature_names, "=", feature_values), collapse = "\t")))
      }

      # Update the workflow by finalizing it using the hyperparameters that attained the best rmse
      regression_workflow <-
        regression_workflow %>%
        tune::finalize_workflow(regression_results %>% tune::select_best("rmse"))
    }

    # Fit the model to the training data based on the specified workflow
    regression_fit <- regression_workflow %>% fit(data = current_x_train)

    # Compute the predicted response for the explicands, i.e., the v(S, x_i) for all explicands x_i.
    pred_explicand <- predict(regression_fit, new_data = current_x_explain)$.pred

    # Add the new contribution function values for the current coalitions S to the result data table as a new row
    dt_res <-
      rbind(dt_res, data.table(as.integer(index_features[j]), matrix(pred_explicand, nrow = 1)), use.names = FALSE)
  }

  # Set id_combination to be the key
  data.table::setkey(dt_res, id_combination)

  # Return the estimated contribution function values
  return(dt_res)
}




# Get functions ========================================================================================================
#' Get if model is to be tuned
#'
#' That is, if the regression model contains hyper-parameters we are to tune using cross validation.
#' See \href{https://www.tidymodels.org/find/parsnip/#model-args}{tidymodels} for default model hyper-parameters.
#'
#' @inheritParams setup_approach.regression_separate
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
get_regression_tune <- function(regression_model, regression_tune_values) {
  # Check that the regression model is a tidymodels object
  if (is.null(regression_model) || !"model_spec" %in% class(regression_model)) {
    stop("`regression_model` must be a tidymodels object with class 'model_spec'. See documentation.")
  }

  # Check if we are to tune some model hyper-parameters
  regression_para <- lapply(regression_model$args, function(para) rlang::quo_get_expr(para))
  regression_para_tune <- lapply(regression_para, function(para) !is.null(para) && para == "tune()")
  regression_para_tune_names <- names(regression_para_tune)[unlist(regression_para_tune)]
  regression_tune <- any(unlist(regression_para_tune))

  # Check that user have provided a tuning
  if (isTRUE(regression_tune) && is.null(regression_tune_values)) {
    stop("`regression_tune_values` must be provided when `regression_model` contains hyper-parameters to tune.")
  }

  # Get the names of the hyper-parameters the user provided values for
  regression_tune_values_names <- names(regression_tune_values)

  # Check that user have provided values for the hyper-parameters to tune
  if (!(all(regression_tune_values_names %in% regression_para_tune_names) &&
    all(regression_para_tune_names %in% regression_tune_values_names))) {
    stop(paste0(
      "The tunable parameters in `regression_model` ('",
      paste(regression_para_tune_names, collapse = "', '"), "') and `regression_tune_values` ('",
      paste(regression_tune_values_names, collapse = "', '"), "') must match."
    ))
  }

  # Return if we are to tune some model hyper-parameters
  return(regression_tune)
}


# Check functions ======================================================================================================
#' Check regression_recipe_func
#'
#' Check that regression_recipe_func is a function that returns the RHS of the formula for arbitrary feature name inputs
#'
#' @inheritParams setup_approach.regression_separate
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
check_regression_recipe_func <- function(regression_recipe_func) {
  if (!is.null(regression_recipe_func) && !is.function(regression_recipe_func)) {
    stop("`regression_recipe_func` must be a function. See documentation.")
  }
}

#' Check the parameters that are sent to [rsample::vfold_cv()]
#'
#' Check that `regression_vfold_cv_para` is either NULL or a named list that only contains recognized parameters
#'
#' @inheritParams setup_approach.regression_separate
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
check_regression_vfold_cv_para <- function(regression_vfold_cv_para) {
  if (!is.null(regression_vfold_cv_para)) {
    # Check that regression_vfold_cv_para is a named list
    if (!is.list(regression_vfold_cv_para) || is.null(names(regression_vfold_cv_para))) {
      stop("`regression_vfold_cv_para` must be a named list. See documentation using '?shapr::explain()'.")
    }

    # Check that all entries are parameters in the rsample::vfold_cv() function
    unknown_para_names <-
      names(regression_vfold_cv_para)[!names(regression_vfold_cv_para) %in% formalArgs(rsample::vfold_cv)[-1]]
    if (length(unknown_para_names) > 0) {
      stop(paste0(
        "The following parameters in `regression_vfold_cv_para` are not supported by `rsample::vfold_cv()`: '",
        paste0(unknown_para_names, collapse = "', '"), "'."
      ))
    }
  }
}