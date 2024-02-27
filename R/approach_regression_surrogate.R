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
setup_approach.regression_surrogate <- function(internal,
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
  if (internal$parameters$verbose == 2) {
    message("Starting 'setup_approach.regression_surrogate'.")
  }

  # Check that the model outputs one-dimensional predictions
  if (internal$parameters$output_size != 1) {
    stop("`regression_separate` only supports models with one-dimensional output")
  }

  # Check that we are not to keep the Monte Carlo samples
  if (internal$parameters$keep_samp_for_vS) {
    stop("`keep_samp_for_vS` must be `FALSE` as there are no Monte Carlo samples to keep for this approach.")
  }

  # Add the default parameter values for the non-user specified parameters for the separate regression approach
  defaults <-
    mget(c("regression_model", "regression_tune_values", "regression_vfold_cv_para", "regression_recipe_func"))
  internal <- insert_defaults(internal, defaults)

  # Check that it is a function that returns the RHS of the formula for arbitrary feature name inputs
  check_regression_recipe_func(internal$parameters$regression_recipe_func)

  # Check that `regression_vfold_cv_para` is either NULL or a named list that only contains recognized parameters
  check_regression_vfold_cv_para(internal$parameters$regression_vfold_cv_para)

  # Check and get if we are to tune the hyperparameters of the regression model
  internal$parameters$regression_tune <-
    get_regression_tune(internal$parameters$regression_model, internal$parameters$regression_tune_values)


  # TODO: comb_prob = if (use_Shapley_weights) internal$objects$X$shapley_weight[2:n_comb_max] else NULL

  # Predict the response of the training and explain data. Former is the response the regression models are fitted to
  model <- eval.parent(match.call()[["model"]])
  internal$data$x_train_y_hat <- predict_model(model, internal$data$x_train)
  internal$data$x_explain_y_hat <- predict_model(model, internal$data$x_explain)

  # Augment the training data
  x_train_augmented <- regression_surr_augment(internal, x = internal$data$x_train, y_hat = internal$data$x_train_y_hat)

  # Create a recipe to the augmented training data
  regression_recipe <- recipes::recipe(y_hat ~ ., data = x_train_augmented)

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
    regression_folds <- do.call(rsample::vfold_cv, c(list(data = x_train_augmented), regression_vfold_cv_para))

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

    # Update the workflow by finalizing it using the hyperparameters that attained the best rmse
    regression_workflow <-
      regression_workflow %>%
      tune::finalize_workflow(regression_results %>% tune::select_best("rmse"))
  }

  regression_fit <- regression_workflow %>% fit(data = x_train_augmented) # Fit the model to the augmented training data
  internal$objects$regression_surrogate_model <- regression_fit # Store the fitted model in the internal list

  # Small printout to the user
  if (internal$parameters$verbose == 2) message("Done with 'setup_approach.regression_surrogate'.\n")

  return(internal) # Return the updated internal list
}


# Augment function =====================================================================================================
#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.regression_surrogate <- function(internal, index_features = NULL, ...) {
  # Augment the explicand data
  x_explain_augmented <- regression_surr_augment(internal, x = internal$data$x_explain, index_features = index_features)

  # Compute the predicted response for the explicands, i.e., v(S, x_i) for all explicands x_i and S in index_features
  pred_explicand <- predict(internal$objects$regression_surrogate_model, new_data = x_explain_augmented)$.pred

  # Insert the predicted contribution functions values into a data table of the correct setup
  dt_res <- data.table(as.integer(index_features), matrix(pred_explicand, nrow = length(index_features)))
  data.table::setnames(dt_res, c("id_combination", paste0("p_hat1_", seq_len(internal$parameters$n_explain))))
  data.table::setkey(dt_res, id_combination) # Set id_combination to be the key

  return(dt_res)
}



#' Title
#'
#' @param internal
#' @param x
#' @param y_hat
#' @param index_features
#' @param augment_add_id_comb
#' @param augment_include_grand
#' @param augment_masks_as_factor
#' @param augment_comb_prob
#' @param augment_weights
#'
#' @return
#' @export
#'
#' @examples
regression_surr_augment <- function(internal,
                                    x,
                                    y_hat = NULL,
                                    index_features = NULL,
                                    augment_masks_as_factor = FALSE,
                                    augment_include_grand = FALSE,
                                    augment_add_id_comb = FALSE,
                                    augment_comb_prob = NULL,
                                    augment_weights = NULL) {
  if (!is.null(augment_weights)) augment_weights <- match.arg(augment_weights, c("Shapley", "uniform"))

  # Get some of the parameters
  S <- internal$objects$S
  n_comb_max <- internal$parameters$used_n_combinations - 2 # Remove empty and grand coalitions
  if (augment_include_grand) n_comb_max <- n_comb_max + 1 # Update based on if we are to include the grand comb
  n_comb_per_observation <- internal$parameters$n_comb_per_observation
  if (!is.null(index_features)) n_comb_per_observation <- length(index_features)

  # TODO: remove when added check before
  if (n_comb_per_observation > n_comb_max) n_comb_per_observation <- n_comb_max

  # Small checks
  if (!is.null(augment_comb_prob) && length(augment_comb_prob) != n_comb_max) {
    stop(paste("`augment_comb_prob` must be of length", n_comb_max, "."))
  }

  if (augment_include_grand && augment_shapley_weights) {
    stop(paste(
      "`augment_include_grand` and `augment_shapley_weights` cannot both",
      "be TRUE because this entails too large weight for the grand coalition."
    ))
  }

  # Get the number of observations (either the same as n_train or n_explain)
  n_obs <- nrow(x)

  # Get the names of the categorical/factor features and the continuous/non-categorical/numeric features.
  feature_classes <- internal$objects$feature_specs$classes
  feature_cat <- names(feature_classes)[feature_classes == "factor"]
  feature_cont <- names(feature_classes)[feature_classes != "factor"]

  # Get the indices of the order of the cat and cont features in
  feature_cat_idx <- which(names(feature_classes) %in% feature_cat)
  feature_cont_idx <- which(names(feature_classes) %in% feature_cont)

  # Check if we are to augment the training data or the explicands
  if (is.null(index_features)) {
    # Training: get matrix of dimension n_obs x n_comb_per_observation containing the indices of the active coalitions
    if (n_comb_per_observation >= n_comb_max) { # Start from two to exclude the empty set
      # TODO: remove `comb_active_idx <- matrix(rep(seq(2, n_comb_max), times = n_obs), nrow = n_obs, byrow = TRUE)`
      comb_active_idx <- matrix(rep(seq(2, n_comb_max + 1), times = n_obs), ncol = n_obs)
    } else {
      comb_active_idx <- sapply(seq(n_obs), function(x) { # Add 1 as we want to exclude the empty set
        sample.int(n = n_comb_max, size = n_comb_per_observation, prob = augment_comb_prob) + 1
      })
    }
  } else {
    # Explicands: get matrix of dimension n_obs x #index_features containing the indices of the active coalitions
    comb_active_idx <- matrix(rep(index_features, times = n_obs), ncol = n_obs)
  }

  # Extract the active coalitions for each explicand. The number of rows are n_obs * n_comb_per_explicands,
  # where the first n_comb_per_explicands rows are connected to the first explicand and so on. Set the column names.
  id_comb <- as.vector(comb_active_idx)
  comb_active <- S[id_comb, , drop = FALSE]
  colnames(comb_active) <- names(feature_classes)

  # Repeat the feature values as many times as there are active coalitions
  x_augmented <- x[rep(seq_len(n_obs), each = n_comb_per_observation), ]

  # Mask the categorical features. Add a new level called "level_masked" when value is masked.
  x_augmented[, (feature_cat) := lapply(seq_along(.SD), function(col) {
    levels(.SD[[col]]) <- c(levels(.SD[[col]]), "level_masked")
    .SD[[col]][comb_active[, feature_cat_idx[col]] == 0] <- "level_masked"
    return(.SD[[col]])
  }), .SDcols = feature_cat]

  # Mask the continuous/non-categorical features
  x_augmented[, (feature_cont) :=
                lapply(seq_along(.SD), function(col) .SD[[col]] * comb_active[, feature_cont_idx[col]]),
              .SDcols = feature_cont
  ]

  # Add new columns indicating when the continuous features are masked
  masked_columns <- paste0("mask_", feature_cont)
  x_augmented <- cbind(x_augmented, setNames(data.table(1 * (comb_active[, feature_cont_idx] == 0)), masked_columns))

  # Convert the binary masks to factor if user has specified so
  if (augment_masks_as_factor) x_augmented[, (masked_columns) := lapply(.SD, as.factor), .SDcols = masked_columns]

  # Add either uniform weights or Shapley kernel weights
  if (!is.null(augment_weights)) {
    x_augmented[, weight := if (augment_weights == "Shapley") internal$objects$X$shapley_weight[id_comb] else 1]
  }

  # Add the id_comb as a factor
  if (augment_add_id_comb) x_augmented[, ("id_comb") := factor(id_comb)]

  # Add repeated responses if provided
  if (!is.null(y_hat)) x_augmented[, ("y_hat") := rep(y_hat, each = n_comb_per_observation)]

  return(x_augmented)
}
