# Shapr functions ======================================================================================================
#' @rdname setup_approach
#'
#' @param regression.model A `tidymodels` object of class `model_specs`. Default is a linear regression model, i.e.,
#' [parsnip::linear_reg()]. See \href{https://www.tidymodels.org/find/parsnip/}{tidymodels} for all possible models,
#' and see the vignette for how to add new/own models. Note, to make it easier to call `explain()` from Python, the
#' `regression.model` parameter can also be a string specifying the model which will be parsed and evaluated. For
#' example, `"parsnip::rand_forest(mtry = hardhat::tune(), trees = 100, engine = "ranger", mode = "regression")"`
#' is also a valid input. It is essential to include the package prefix if the package is not loaded.
#' @param regression.tune_values Either `NULL` (default), a data.frame/data.table/tibble, or a function.
#' The data.frame must contain the possible hyperparameter value combinations to try.
#' The column names must match the names of the tunable parameters specified in `regression.model`.
#' If `regression.tune_values` is a function, then it should take one argument `x` which is the training data
#' for the current coalition and returns a data.frame/data.table/tibble with the properties described above.
#' Using a function allows the hyperparameter values to change based on the size of the coalition See the regression
#' vignette for several examples.
#' Note, to make it easier to call [explain()] from Python, the `regression.tune_values` can also be a string
#' containing an R function. For example,
#' `"function(x) return(dials::grid_regular(dials::mtry(c(1, ncol(x)))), levels = 3))"` is also a valid input.
#' It is essential to include the package prefix if the package is not loaded.
#' @param regression.vfold_cv_para Either `NULL` (default) or a named list containing
#' the parameters to be sent to [rsample::vfold_cv()]. See the regression vignette for
#' several examples.
#' @param regression.recipe_func Either `NULL` (default) or a function that that takes in a [recipes::recipe()]
#' object and returns a modified [recipes::recipe()] with potentially additional recipe steps. See the regression
#' vignette for several examples.
#' Note, to make it easier to call [explain()] from Python, the `regression.recipe_func` can also be a string
#' containing an R function. For example,
#' `"function(recipe) return(recipes::step_ns(recipe, recipes::all_numeric_predictors(), deg_free = 2))"` is also
#' a valid input. It is essential to include the package prefix if the package is not loaded.
#' @inheritParams default_doc_export
#'
#' @export
#' @author Lars Henry Berge Olsen
setup_approach.regression_separate <- function(internal,
                                               regression.model = parsnip::linear_reg(),
                                               regression.tune_values = NULL,
                                               regression.vfold_cv_para = NULL,
                                               regression.recipe_func = NULL,
                                               ...) {
  # Check that required libraries are installed
  regression.check_namespaces()

  # Small printout to the user

  # Add the default parameter values for the non-user specified parameters for the separate regression approach
  defaults <-
    mget(c("regression.model", "regression.tune_values", "regression.vfold_cv_para", "regression.recipe_func"))
  internal <- insert_defaults(internal, defaults)

  # Check the parameters to the regression approach
  internal <- regression.check_parameters(internal = internal)

  # Small printout to the user

  return(internal) # Return the updated internal list
}

#' @inheritParams default_doc_export
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.regression_separate <- function(internal, index_features = NULL, ...) {
  # Load `workflows`, needed when parallelized as we call predict with a workflow object. Checked installed above.
  requireNamespace("workflows", quietly = TRUE)

  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X
  verbose <- internal$parameters$verbose

  # Get the features in the batch
  features <- X$features[index_features]


  # Initialize empty data table with specific column names and id_coalition (transformed to integer later). The data
  # table will contain the contribution function values for the coalitions given by `index_features` and all explicands.
  dt_res_column_names <- c("id_coalition", paste0("p_hat1_", seq_len(internal$parameters$n_explain)))
  dt_res <- data.table(matrix(
    ncol = length(dt_res_column_names),
    nrow = 0L,
    dimnames = list(NULL, dt_res_column_names)
  ))

  # Iterate over the coalitions provided by index_features.
  # Note that index_features will never be NULL and never contain the empty or grand coalitions.
  for (comb_idx in seq_along(features)) {
    # Get the column indices of the features in current coalition
    current_comb <- features[[comb_idx]]

    # Extract the current training (and add y_hat as response) and explain data
    current_x_train <- internal$data$x_train[, ..current_comb][, "y_hat" := internal$data$x_train_y_hat]
    current_x_explain <- internal$data$x_explain[, ..current_comb]


    # Fit the current separate regression model to the current training data
    regression.current_fit <- regression.train_model(
      x = current_x_train,
      seed = internal$parameters$seed,
      verbose = verbose,
      regression.model = internal$parameters$regression.model,
      regression.tune = internal$parameters$regression.tune,
      regression.tune_values = internal$parameters$regression.tune_values,
      regression.vfold_cv_para = internal$parameters$regression.vfold_cv_para,
      regression.recipe_func = internal$parameters$regression.recipe_func,
      current_comb = current_comb
    )

    # Compute the predicted response for the explicands, i.e., the v(S, x_i) for all explicands x_i.
    pred_explicand <- predict(regression.current_fit, new_data = current_x_explain)$.pred

    # Add the new contribution function values for the current coalitions S to the result data table as a new row
    dt_res <- rbind(dt_res, data.table(index_features[comb_idx], matrix(pred_explicand, nrow = 1)), use.names = FALSE)
  }

  # Set id_coalition to be the key
  dt_res[, id_coalition := as.integer(id_coalition)]
  data.table::setkey(dt_res, id_coalition)

  # Return the estimated contribution function values
  return(dt_res)
}

# Train functions ======================================================================================================
#' Train a Tidymodels Model via Workflows
#'
#' Function that trains a `tidymodels` model via `workflows` based on the provided input parameters.
#' This function allows for cross validating the hyperparameters of the model.
#'
#' @inheritParams setup_approach.regression_separate
#' @inheritParams explain
#' @param x Data.table containing the training data.
#' @param regression.tune Logical (default is `FALSE`). If `TRUE`, then we are to tune the hyperparemeters based on
#' the values provided in `regression.tune_values`. Note that no checks are conducted as this is checked earlier in
#' `setup_approach.regression_separate` and `setup_approach.regression_surrogate`.
#' @param regression.response_var String (default is `y_hat`) containing the name of the response variable.
#' @param regression.surrogate_n_comb Integer (default is `NULL`). The number of times each training observations
#' has been augmented. If `NULL`, then we assume that we are doing separate regression.
#' @param current_comb Integer vector. The current combination of features, passed to verbosity printing function.
#'
#' @return A trained `tidymodels` model based on the provided input parameters.
#' @export
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.train_model <- function(x,
                                   seed = 1,
                                   verbose = NULL,
                                   regression.model = parsnip::linear_reg(),
                                   regression.tune = FALSE,
                                   regression.tune_values = NULL,
                                   regression.vfold_cv_para = NULL,
                                   regression.recipe_func = NULL,
                                   regression.response_var = "y_hat",
                                   regression.surrogate_n_comb = NULL,
                                   current_comb = NULL) {
  # Create a recipe to the augmented training data
  regression.recipe <- recipes::recipe(as.formula(paste(regression.response_var, "~ .")), data = x)

  # Update the recipe if user has provided a function for this. User is responsible for that the function works.
  # This function can, e.g., add transformations, normalization, dummy encoding, interactions, and so on.
  if (!is.null(regression.recipe_func)) regression.recipe <- regression.recipe_func(regression.recipe)

  # Combine workflow, model specification, and recipe
  regression.workflow <-
    workflows::add_recipe(workflows::add_model(workflows::workflow(), regression.model), regression.recipe)

  # Check if we are to tune hyperparameters in the regression model, as we then need to update the workflow.
  # If we are not doing any hyperparameter tuning, then the workflow above is enough.
  if (regression.tune) {
    # Set up the V-fold cross validation using the user provided parameters in `regression.vfold_cv_para`.
    # Note if `regression.vfold_cv_para` is NULL, then we use the default parameters in `vfold_cv()`.
    regression.folds <- do.call(rsample::vfold_cv, c(list(data = x), regression.vfold_cv_para))

    # Check if we are doing surrogate regression, as we then need to update the indices as the augmented
    # training data is highly correlated due to the augmentations which will mess up the cross validation.
    # Since one there assumes that the training and evaluation data are independent. The following code ensures
    # that all augmentations of a single training observations are either in the training or evaluation data.
    if (!is.null(regression.surrogate_n_comb)) {
      if (!is.null(regression.vfold_cv_para) && any(names(regression.vfold_cv_para) != "v")) {
        cli::cli_abort(
          "The `regression.vfold_cv_para` parameter supports only the `v` parameter for surrogate regression."
        )
      }

      n <- nrow(x) / regression.surrogate_n_comb # Get the number of training observations (before augmentation)
      n_folds <- nrow(regression.folds) # Get the number of folds
      folds <- sample(rep(seq_len(n_folds), length.out = n)) # Sample in which fold the i'th obs is in the eval data
      indices <- lapply(split(seq_len(n), folds), function(x) setdiff(seq_len(n), x)) # Sample the training indices

      # Loop over the folds, extend the indices to reflect the augmentation, and insert the updated training indices
      for (fold_idx in seq(n_folds)) {
        regression.folds$splits[[fold_idx]]$in_id <-
          unlist(lapply(
            indices[[fold_idx]],
            function(idx) seq(regression.surrogate_n_comb * (idx - 1) + 1, regression.surrogate_n_comb * idx)
          ))
      }
    }

    # Extract the grid of hyperparameter values. Note that regression.tune_values is either a data.frame or a function.
    if (is.data.frame(regression.tune_values)) {
      regression.grid <- regression.tune_values
    } else {
      regression.grid <- regression.tune_values(x[, -..regression.response_var])
    }

    # Add the hyperparameter tuning to the workflow
    regression.results <- tune::tune_grid(
      object = regression.workflow,
      resamples = regression.folds,
      grid = regression.grid,
      metrics = yardstick::metric_set(yardstick::rmse)
    )
    # Small printout to the user
    if ("vS_details" %in% verbose) {
      regression.cv_message(
        regression.results = regression.results,
        regression.grid = regression.grid,
        current_comb = current_comb
      )
    }

    # Set seed for reproducibility. Without this we get different results based on if we run in parallel or sequential
    set.seed(seed)

    # Update the workflow by finalizing it using the hyperparameters that attained the best rmse
    regression.workflow <-
      tune::finalize_workflow(regression.workflow, tune::select_best(regression.results, metric = "rmse"))
  }

  # Fit the model to the augmented training data and return the trained model
  return(parsnip::fit(regression.workflow, data = x))
}


# Get functions ========================================================================================================
#' Convert the string into an R object
#'
#' @param string A character vector/string containing the text to convert into R code.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.get_string_to_R <- function(string) {
  return(eval(parse(text = string)))
}

#' Get the predicted responses
#'
#' @inheritParams default_doc_internal
#'
#' @return The same `internal` list, but added vectors `internal$data$x_train_y_hat` and
#' `internal$data$x_explain_y_hat` containing the predicted response of the training and explain data.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.get_y_hat <- function(internal, model, predict_model) {
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
#' @inheritParams default_doc_internal
#'
#' @return A boolean variable indicating if the regression model is to be tuned.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.get_tune <- function(regression.model, regression.tune_values, x_train) {
  # Check that the regression model is a tidymodels object
  if (is.null(regression.model) || !"model_spec" %in% class(regression.model)) {
    cli::cli_abort("`regression.model` must be a tidymodels object with class 'model_spec'. See documentation.")
  }

  # Check if we are to tune some model hyperparameters
  regression.para <- lapply(regression.model$args, function(para) rlang::quo_get_expr(para))
  regression.para_tune <- lapply(regression.para, function(para) !is.null(para) && grepl("tune()", para))
  regression.para_tune_names <- names(regression.para_tune)[unlist(regression.para_tune)]
  regression.tune <- any(unlist(regression.para_tune))

  # Check that user have provided a tuning
  if (isTRUE(regression.tune) && is.null(regression.tune_values)) {
    cli::cli_abort(
      "`regression.tune_values` must be provided when `regression.model` contains hyperparameters to tune."
    )
  }

  # Check function or tibble
  if (!is.null(regression.tune_values) &&
    !is.data.frame(regression.tune_values) &&
    !is.function(regression.tune_values)) {
    cli::cli_abort("`regression.tune_values` must be of either class `data.frame` or `function`. See documentation.")
  }

  # Get the grid values. And if user provided a function, then check that it is a data.frame.
  regression.tune_values_grid <- regression.tune_values
  if (is.function(regression.tune_values)) {
    regression.tune_values_grid <- regression.tune_values(x_train)
    if (!is.data.frame(regression.tune_values_grid)) {
      cli::cli_abort("The output of the user provided `regression.tune_values` function must be of class `data.frame`.")
    }
  }

  # Get the names of the hyperparameters the user provided values for
  regression.tune_values_names <- names(regression.tune_values_grid)

  # Check that user have provided values for the hyperparameters to tune
  if (!(all(regression.tune_values_names %in% regression.para_tune_names) &&
    all(regression.para_tune_names %in% regression.tune_values_names))) {
    cli::cli_abort(paste0(
      "The tunable parameters in `regression.model` ('",
      paste(regression.para_tune_names, collapse = "', '"), "') and `regression.tune_values` ('",
      paste(regression.tune_values_names, collapse = "', '"), "') must match."
    ))
  }

  # Return if we are to tune some model hyperparameters
  return(regression.tune)
}

# Check functions ======================================================================================================
#' Check regression parameters
#'
#' @inheritParams default_doc_internal
#'
#' @return The same `internal` list, but added logical indicator `internal$parameters$regression.tune`
#' if we are to tune the regression model/models.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.check_parameters <- function(internal) {
  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions


  # Convert the objects to R-objects if they are strings
  if (is.character(internal$parameters$regression.model)) {
    internal$parameters$regression.model <- regression.get_string_to_R(internal$parameters$regression.model)
  }
  if (is.character(internal$parameters$regression.tune_values)) {
    internal$parameters$regression.tune_values <- regression.get_string_to_R(internal$parameters$regression.tune_values)
  }
  if (is.character(internal$parameters$regression.recipe_func)) {
    internal$parameters$regression.recipe_func <- regression.get_string_to_R(internal$parameters$regression.recipe_func)
  }

  # Check that it is a function that returns the RHS of the formula for arbitrary feature name inputs
  regression.check_recipe_func(
    regression.recipe_func = internal$parameters$regression.recipe_func,
    x_explain = internal$data$x_explain
  )

  # Check that `regression.vfold_cv_para` is either NULL or a named list that only contains recognized parameters
  regression.check_vfold_cv_para(regression.vfold_cv_para = internal$parameters$regression.vfold_cv_para)

  # Check that `regression.check_sur_n_comb` is a valid value (only applicable for surrogate regression)
  regression.check_sur_n_comb(
    regression.surrogate_n_comb = internal$parameters$regression.surrogate_n_comb,
    n_coalitions = n_coalitions
  )

  # Check and get if we are to tune the hyperparameters of the regression model
  internal$parameters$regression.tune <- regression.get_tune(
    regression.model = internal$parameters$regression.model,
    regression.tune_values = internal$parameters$regression.tune_values,
    x_train = internal$data$x_train
  )

  return(internal)
}

#' Check `regression.recipe_func`
#'
#' Check that regression.recipe_func is a function that returns the
#' RHS of the formula for arbitrary feature name inputs.
#'
#' @inheritParams default_doc_internal
#' @inheritParams setup_approach.regression_separate
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.check_recipe_func <- function(regression.recipe_func, x_explain) {
  if (!is.null(regression.recipe_func) && !is.function(regression.recipe_func)) {
    cli::cli_abort("`regression.recipe_func` must be a function. See documentation.")
  }

  if (!is.null(regression.recipe_func) && is.function(regression.recipe_func)) {
    x_temp <- copy(x_explain)[, "y_hat_temp" := 1]
    regression.recipe_func_output <- regression.recipe_func(recipes::recipe(y_hat_temp ~ ., data = x_temp))
    if (!"recipe" %in% class(regression.recipe_func_output)) {
      cli::cli_abort("The output of the `regression.recipe_func` must be of class `recipe`.")
    }
  }
}

#' Check the parameters that are sent to [rsample::vfold_cv()]
#'
#' Check that `regression.vfold_cv_para` is either NULL or a named list that only contains recognized parameters.
#'
#' @inheritParams setup_approach.regression_separate
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.check_vfold_cv_para <- function(regression.vfold_cv_para) {
  if (!is.null(regression.vfold_cv_para)) {
    # Check that regression.vfold_cv_para is a named list
    if (!is.list(regression.vfold_cv_para) || is.null(names(regression.vfold_cv_para))) {
      cli::cli_abort("`regression.vfold_cv_para` must be a named list. See the documentation of {.fn shapr::explain}.")
    }

    # Check that all entries are parameters in the rsample::vfold_cv() function
    unknown_para_names <-
      names(regression.vfold_cv_para)[!names(regression.vfold_cv_para) %in% methods::formalArgs(rsample::vfold_cv)[-1]]
    if (length(unknown_para_names) > 0) {
      cli::cli_abort(paste0(
        "The following parameters in `regression.vfold_cv_para` are not supported by `rsample::vfold_cv()`: '",
        paste0(unknown_para_names, collapse = "', '"), "'."
      ))
    }

    # Ensure that we have at least two folds in the cross validation procedure
    if ("v" %in% names(regression.vfold_cv_para) && regression.vfold_cv_para[["v"]] <= 1) {
      cli::cli_abort("The parameter `v` in `regression.vfold_cv_para` must be strictly larger than 1.")
    }
  }
}

#' Check that needed libraries are installed
#'
#' This function checks that the `parsnip`, `recipes`, `workflows`, `tune`, `dials`,
#' `yardstick`, `hardhat` and `rsample`, packages are available.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.check_namespaces <- function() {
  namespaces <- c("parsnip", "recipes", "workflows", "tune", "dials", "yardstick", "hardhat", "rsample")
  for (namespace in namespaces) {
    if (!requireNamespace(namespace, quietly = TRUE)) {
      cli::cli_abort(paste0(
        "`", namespace, "` is not installed. Please run {.run install.packages('", namespace, "')} to install it ",
        "or run {.run install.packages('tidymodels')} to install all relevant packages for the regression approaches."
      ))
    }
  }
}

# Message functions ====================================================================================================
#' Produce message about which batch prepare_data is working on
#'
#' @param regression.results The results of the CV procedures.
#' @param regression.grid Object containing the hyperparameter values.
#' @param n_cv Integer (default is 10) specifying the number of CV hyperparameter configurations to print.
#' @inheritParams regression.train_model
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.cv_message <- function(regression.results, regression.grid, n_cv = 10, current_comb) {
  # Get the feature names and add evaluation metric rmse
  feature_names <- names(regression.grid)
  feature_names_rmse <- c(feature_names, "rmse", "rmse_std_err")

  # Let n_cv be the minimum of the provided value and the number of possible printouts
  n_cv <- min(n_cv, nrow(regression.grid))

  # Extract the n_cv best results
  best_results <- tune::show_best(regression.results, n = n_cv, metric = "rmse")

  # Needed to make prinout tables prettier to ensure same column dimensions for all settings.
  regression.grid_best <- best_results[, feature_names]
  regression.grid_best$rmse <- round(best_results$mean, 2)
  regression.grid_best$rmse_std <- round(best_results$std_err, 2)
  width <- sapply(regression.grid_best, function(x) max(nchar(as.character(unique(x)))))

  # Regression_separate adds the v(S), while separate does not add anything, but prints the Extra info thing
  if (!is.null(current_comb)) {
    this_vS <- paste0("for  v(", paste0(current_comb, collapse = " "), ") ")
  } else {
    cli::cli_h2("Extra info about the tuning of the regression model")
    this_vS <- ""
  }

  msg0 <- paste0("Top ", n_cv, " best configs ", this_vS, "(using ", best_results$n[1], "-fold CV)")
  msg <- NULL

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
    msg <-
      c(msg, paste0("#", row_idx, ": ", paste(paste(feature_names_rmse, "=", values_fixed_len), collapse = "  "), "\n"))
  }
  cli::cli({
    cli::cli_h3(msg0)
    for (i in seq_along(msg)) cli::cli_text(msg[i])
  })
}
