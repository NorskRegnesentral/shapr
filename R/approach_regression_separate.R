# Shapr functions ======================================================================================================
#' @rdname setup_approach
#'
#' @param regression_model A `tidymodels` object of class `model_specs`. Default is a linear regression model
#' ([parsnip::linear_reg()]). See \href{https://www.tidymodels.org/find/parsnip/}{tidymodels} for all possible models,
#' and see the vignette for how to add new/own models.
#' @param regression_tune_values Either `NULL` (default), a data.frame/data.table/tibble, or a function.
#' The data.frame must contain the possible hyperparameter value combinations to try.
#' The column names must match the names of the tuneable parameters specified in `regression_model`.
#' If `regression_tune_values` is a function, then it should take one argument `x` which is the training data
#' for the current combination/coalition and returns a data.frame/data.table/tibble with the properties described above.
#' Using a function allows the hyperparameter values to change based on the size of the combination.
#' @param regression_vfold_cv_para Either `NULL` (default) or a list containing
#' the parameters to be sent to [rsample::vfold_cv()].
#' @param regression_recipe_func Either `NULL` (default) or a function that must return
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

  # Small printout to the user
  if (internal$parameters$verbose == 2) message("Starting 'setup_approach.regression_separate'.")
  if (internal$parameters$verbose == 2) regression_sep_time_message() # TODO: maybe remove

  # Add the default parameter values for the non-user specified parameters for the separate regression approach
  defaults <-
    mget(c("regression_model", "regression_tune_values", "regression_vfold_cv_para", "regression_recipe_func"))
  internal <- insert_defaults(internal, defaults)

  # Check the parameters to the regression approach
  internal <- check_regression_parameters(internal = internal)

  # Get the predicted response of the training and explain data
  internal <- get_regression_y_hat(internal = internal, model = eval.parent(match.call()[["model"]]))

  # Small printout to the user
  if (internal$parameters$verbose == 2) message("Done with 'setup_approach.regression_separate'.")

  return(internal) # Return the updated internal list
}

#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.regression_separate <- function(internal, index_features = NULL, ...) {
  # Get the features in the batch
  features <- internal$objects$X$features[index_features]

  # Small printout to the user about which batch that are currently worked on
  if (internal$parameters$verbose == 2) regression_prep_message_batch(internal, index_features)

  # Initialize empty data table with specific column names and id_combination (transformed to integer later). The data
  # table will contain the contribution function values for the coalitions given by `index_features` and all explicands.
  dt_res_column_names <- c("id_combination", paste0("p_hat1_", seq_len(internal$parameters$n_explain)))
  dt_res <- data.table(matrix(ncol = length(dt_res_column_names), nrow = 0, dimnames = list(NULL, dt_res_column_names)))

  # Iterate over the coalitions provided by index_features.
  # Note that index_features will never be NULL and never contain the empty or grand coalitions.
  for (comb_idx in seq_along(features)) {
    # Get the column indices of the features in current coalition/combination
    current_comb <- features[[comb_idx]]

    # Extract the current training (and add y_hat as response) and explain data
    current_x_train <- internal$data$x_train[, ..current_comb][, "y_hat" := internal$data$x_train_y_hat]
    current_x_explain <- internal$data$x_explain[, ..current_comb]

    # Fit the current separate regression model to the current training data
    if (internal$parameters$verbose == 2) regression_prep_message_comb(internal, index_features, comb_idx)
    current_regression_fit <- regression_train(
      x = current_x_train,
      regression_model = internal$parameters$regression_model,
      regression_tune = internal$parameters$regression_tune,
      regression_tune_values = internal$parameters$regression_tune_values,
      regression_vfold_cv_para = internal$parameters$regression_vfold_cv_para,
      regression_recipe_func = internal$parameters$regression_recipe_func,
      verbose = internal$parameters$verbose
    )

    # Compute the predicted response for the explicands, i.e., the v(S, x_i) for all explicands x_i.
    pred_explicand <- predict(current_regression_fit, new_data = current_x_explain)$.pred

    # Add the new contribution function values for the current coalitions S to the result data table as a new row
    dt_res <- rbind(dt_res, data.table(index_features[comb_idx], matrix(pred_explicand, nrow = 1)), use.names = FALSE)
  }

  # Set id_combination to be the key
  dt_res[, id_combination := as.integer(id_combination)]
  data.table::setkey(dt_res, id_combination)

  # Return the estimated contribution function values
  return(dt_res)
}

# Train functions ======================================================================================================
#' Train a [tidymodels()] model
#'
#' Function that trains a [tidymodels()] model based on the provided input parameters.
#' This function allows for cross validating the hyperparameters of the model.
#'
#' @inheritParams setup_approach.regression_separate
#' @inheritParams explain
#' @param x Data.table containing the data. Either the training data or the explicands. If `x` is the explicands,
#' then `index_features` must be provided.
#' @param regression_tune Logical (default is `FALSE`). If `TRUE`, then we are to tune the hyperparemeters based on
#' the values provided in `regression_tune_values`. Note that no checks are conducted as this is checked earlier in
#' `setup_approach.regression_separate` and `setup_approach.regression_surrogate`.
#' @param regression_response_var String (default is `y_hat`) containing the name of the response variable.
#' @param regression_sur_n_comb Integer (default is `NULL`). The number of times each training observations
#' has been augmented. If `NULL`, then we assume that we are doing separate regression.
#'
#' @return A trained [tidymodels()] model based on the provided input parameters.
#' @export
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression_train <- function(x,
                             regression_model = parsnip::linear_reg(),
                             regression_tune = FALSE,
                             regression_tune_values = NULL,
                             regression_vfold_cv_para = NULL,
                             regression_recipe_func = NULL,
                             regression_response_var = "y_hat",
                             regression_sur_n_comb = NULL,
                             verbose = 0) {
  # Create a recipe to the augmented training data
  regression_recipe <- recipes::recipe(as.formula(paste(regression_response_var, "~ .")), data = x)

  # Update the recipe if user has provided a function for this. User is responsible for that the function works.
  # This function can, e.g., add transformations, normalization, dummy encoding, interactions, and so on.
  if (!is.null(regression_recipe_func)) regression_recipe <- regression_recipe_func(regression_recipe)

  # Combine workflow, model specification, and recipe
  regression_workflow <-
    workflows::add_recipe(workflows::add_model(workflows::workflow(), regression_model), regression_recipe)

  # Check if we are to tune hyperparameters in the regression model, as we then need to update the workflow.
  # If we are not doing any hyperparameter tuning, then the workflow above is enough.
  if (regression_tune) {
    # Set up the V-fold cross validation using the user provided parameters in `regression_vfold_cv_para`.
    # Note if `regression_vfold_cv_para` is NULL, then we use the default parameters in `vfold_cv()`.
    regression_folds <- do.call(rsample::vfold_cv, c(list(data = x), regression_vfold_cv_para))

    # Check if we are doing surrogate regression, as we then need to update the indices as the augmented
    # training data is highly correlated due to the augmentations which will mess up the cross validation.
    # Since one there assumes that the training and evaluation data are independent. The following code ensures
    # that all augmentations of a single training observations are either in the training or evaluation data.
    if (!is.null(regression_sur_n_comb)) {
      if (!is.null(regression_vfold_cv_para) && any(names(regression_vfold_cv_para) != "v")) {
        stop("The `regression_vfold_cv_para` parameter supports only the `v` parameter for surrogate regression.")
      }

      n <- nrow(x) / regression_sur_n_comb # Get the number of training observations (before augmentation)
      n_folds <- nrow(regression_folds) # Get the number of folds
      folds <- sample(rep(seq_len(n_folds), length.out = n)) # Sample in which fold the i'th obs is in the eval data
      indices <- lapply(split(seq_len(n), folds), function(x) setdiff(seq_len(n), x)) # Sample the training indices

      # Loop over the folds, extend the indices to reflect the augmentation, and insert the updated training indices
      for (fold_idx in seq(n_folds)) {
        regression_folds$splits[[fold_idx]]$in_id <-
          unlist(lapply(
            indices[[fold_idx]],
            function(idx) seq(regression_sur_n_comb * (idx - 1) + 1, regression_sur_n_comb * idx)
          ))
      }
    }

    # Extract the grid of hyperparameter values. Note that regression_tune_values is either a data.frame or a function.
    if (is.data.frame(regression_tune_values)) {
      regression_grid <- regression_tune_values
    } else {
      regression_grid <- regression_tune_values(x[, -..regression_response_var])
    }

    # Add the hyperparameter tuning to the workflow
    regression_results <- tune::tune_grid(
      object = regression_workflow,
      resamples = regression_folds,
      grid = regression_grid,
      metrics = yardstick::metric_set(yardstick::rmse)
    )

    # Small printout to the user
    if (verbose == 2) regression_cv_message(regression_results = regression_results, regression_grid = regression_grid)

    # Update the workflow by finalizing it using the hyperparameters that attained the best rmse
    regression_workflow <- tune::finalize_workflow(regression_workflow, tune::select_best(regression_results, "rmse"))
  }

  # Fit the model to the augmented training data
  regression_fit <- fit(regression_workflow, data = x)

  # Return the trained model
  return(regression_fit)
}


# Get functions ========================================================================================================
#' Get the predicted responses
#'
#' @inheritParams default_doc
#'
#' @return The same `internal` list, but added vectors `internal$data$x_train_y_hat` and
#' `internal$data$x_explain_y_hat` containing the predicted response of the training and explain data.
#' @author Lars Henry Berge Olsen
#' @keywords internal
get_regression_y_hat <- function(internal, model) {
  # Predict the response of the training and explain data. Former is the response the regression models are fitted to.
  internal$data$x_train_y_hat <- predict_model(model, internal$data$x_train)
  internal$data$x_explain_y_hat <- predict_model(model, internal$data$x_explain)
  return(internal)
}

#' Get if model is to be tuned
#'
#' That is, if the regression model contains hyperparameters we are to tune using cross validation.
#' See \href{https://www.tidymodels.org/find/parsnip/#model-args}{tidymodels} for default model hyperparameters.
#'
#' @inheritParams setup_approach.regression_separate
#' @inheritParams explain
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
get_regression_tune <- function(regression_model, regression_tune_values, x_train) {
  # Check that the regression model is a tidymodels object
  if (is.null(regression_model) || !"model_spec" %in% class(regression_model)) {
    stop("`regression_model` must be a tidymodels object with class 'model_spec'. See documentation.")
  }

  # Check if we are to tune some model hyperparameters
  regression_para <- lapply(regression_model$args, function(para) rlang::quo_get_expr(para))
  regression_para_tune <- lapply(regression_para, function(para) !is.null(para) && grepl("tune()", para))
  regression_para_tune_names <- names(regression_para_tune)[unlist(regression_para_tune)]
  regression_tune <- any(unlist(regression_para_tune))

  # Check that user have provided a tuning
  if (isTRUE(regression_tune) && is.null(regression_tune_values)) {
    stop("`regression_tune_values` must be provided when `regression_model` contains hyperparameters to tune.")
  }

  # Check function or tibble
  if (!is.null(regression_tune_values) &&
      !is.data.frame(regression_tune_values) &&
      !is.function(regression_tune_values)) {
    stop("`regression_tune_values` must be of either class `data.frame` or `function`. See documentation.")
  }

  # Get the grid values. And if user provided a function, then check that it is a data.frame.
  regression_tune_values_grid <- regression_tune_values
  if (is.function(regression_tune_values)) {
    regression_tune_values_grid <- regression_tune_values(x_train)
    if (!is.data.frame(regression_tune_values_grid)) {
      stop("The output of the user provided `regression_tune_values` function must be of class `data.frame`.")
    }
  }

  # Get the names of the hyperparameters the user provided values for
  regression_tune_values_names <- names(regression_tune_values_grid)

  # Check that user have provided values for the hyperparameters to tune
  if (!(all(regression_tune_values_names %in% regression_para_tune_names) &&
    all(regression_para_tune_names %in% regression_tune_values_names))) {
    stop(paste0(
      "The tunable parameters in `regression_model` ('",
      paste(regression_para_tune_names, collapse = "', '"), "') and `regression_tune_values` ('",
      paste(regression_tune_values_names, collapse = "', '"), "') must match."
    ))
  }

  # Return if we are to tune some model hyperparameters
  return(regression_tune)
}

# Check functions ======================================================================================================
#' Check regression parameters
#'
#' @inheritParams default_doc
#'
#' @return The same `internal` list, but added logical indicator `internal$parameters$regression_tune`
#' if we are to tune the regression model/models.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
check_regression_parameters <- function(internal) {
  # Check that it is a function that returns the RHS of the formula for arbitrary feature name inputs
  check_regression_recipe_func(
    regression_recipe_func = internal$parameters$regression_recipe_func,
    x_explain = internal$data$x_explain
  )

  # Check that `regression_vfold_cv_para` is either NULL or a named list that only contains recognized parameters
  check_regression_vfold_cv_para(regression_vfold_cv_para = internal$parameters$regression_vfold_cv_para)

  # Check that `check_regression_n_comb` is a valid value (only applicable for surrogate regression)
  check_regression_n_comb(
    regression_surr_n_comb = internal$parameters$regression_surr_n_comb,
    used_n_combinations = internal$parameters$used_n_combinations
  )

  # Check and get if we are to tune the hyperparameters of the regression model
  internal$parameters$regression_tune <- get_regression_tune(
    regression_model = internal$parameters$regression_model,
    regression_tune_values = internal$parameters$regression_tune_values,
    x_train = internal$data$x_train
  )

  return(internal)
}

#' Check `regression_recipe_func`
#'
#' Check that regression_recipe_func is a function that returns the
#' RHS of the formula for arbitrary feature name inputs.
#'
#' @inheritParams explain
#' @inheritParams setup_approach.regression_separate
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
check_regression_recipe_func <- function(regression_recipe_func, x_explain) {
  if (!is.null(regression_recipe_func) && !is.function(regression_recipe_func)) {
    stop("`regression_recipe_func` must be a function. See documentation.")
  }

  if (!is.null(regression_recipe_func) && is.function(regression_recipe_func)) {
    x_temp = copy(x_explain)[, y_hat_temp := 1]
    if (class(regression_recipe_func(recipes::recipe(y_hat_temp ~ ., data = x_temp))) != "recipe") {
      stop("The output of the `regression_recipe_func` must be of class `recipe`.")
    }
  }

}

#' Check the parameters that are sent to [rsample::vfold_cv()]
#'
#' Check that `regression_vfold_cv_para` is either NULL or a named list that only contains recognized parameters.
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

    # Ensure that we have at least two folds in the cross validation procedure
    if ("v" %in% names(regression_vfold_cv_para) && regression_vfold_cv_para[["v"]] <= 1) {
      stop("The parameter `v` in `regression_vfold_cv_para` must be strictly larger than 1.")
    }
  }
}

# Message functions ====================================================================================================
#' Produce time message for separate regression
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression_sep_time_message <- function() {
  message(paste(
    "When using `approach = 'regression_separate'` the `explanation$timing$timing_secs` object \n",
    "can be missleading as `setup_computation` does not contain the training times of the \n",
    "regression models as they are trained on the fly in `compute_vS`. This is to reduce memory \n",
    "usage and to improve efficency.\n"
  )) # TODO: should we add the time somewhere else?
}

#' Produce message about which batch prepare_data is working on
#' @inheritParams default_doc
#' @inheritParams default_doc_explain
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression_prep_message_batch <- function(internal, index_features) {
  message(paste0(
    "Working on batch ", internal$objects$X[id_combination == index_features[1]]$batch, " of ",
    internal$parameters$n_batches, " in `prepare_data.", internal$parameters$approach, "()`."
  ))
}

#' Produce message about which combination prepare_data is working on
#' @inheritParams default_doc
#' @inheritParams default_doc_explain
#' @param comb_idx Integer. The index of the combination in a specific batch.
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression_prep_message_comb <- function(internal, index_features, comb_idx) {
  message(paste0(
    "Working on combination with id ", internal$objects$X$id_combination[index_features[comb_idx]],
    " of ", internal$parameters$used_n_combinations, "."
  ))
}

#' Produce message about which batch prepare_data is working on
#'
#' @param regression_results The results of the CV procedures.
#' @param regression_grid Object containing the hyperparameter values.
#' @param n_cv Integer (default is 10) specifying the number of CV hyperparameter configurations to print.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression_cv_message <- function(regression_results, regression_grid, n_cv = 10) {
  # Get the feature names and add evaluation metric rmse
  feature_names <- names(regression_grid)
  feature_names_rmse <- c(feature_names, "rmse", "rmse_std_err")

  # Let n_cv be the minimum of the provided value and the number of possible printouts
  n_cv <- min(n_cv, nrow(regression_grid))

  # Extract the n_cv best results
  best_results <- tune::show_best(regression_results, n = n_cv)

  # Needed to make prinout tables prettier to ensure same column dimensions for all settings.
  regression_grid_best = best_results[,feature_names]
  regression_grid_best$rmse <- round(best_results$mean, 2)
  regression_grid_best$rmse_std <- round(best_results$std_err, 2)
  width <- sapply(regression_grid_best, function(x) max(nchar(as.character(unique(x)))))

  # Message title of the results
  message(paste0("Results of the ", best_results$n[1], "-fold cross validation (top ", n_cv, " best configurations):"))

  # Iterate over the n_cv best results and print out the hyper parameter values and the rmse and rmse_std_err
  for (row_idx in seq_len(nrow(best_results))) {
    best_result <- best_results[row_idx, ]
    feature_values <- best_result[feature_names]
    feature_values_rmse <- c(
      feature_values,
      format(round(best_result$mean, 2), nsmall = 2), format(round(best_result$std_err, 2), nsmall = 2)
    )
    values_fixed_len <- sapply(
      seq_along(feature_values_rmse),
      function(x) format(as.character(feature_values_rmse[x]), width = width[x], justify = "left")
    )
    message(paste0("#", row_idx, ": ", paste(paste(feature_names_rmse, "=", values_fixed_len), collapse = "  "), ""))
  }

  message("") # Empty message to get a blank line
}