# Shapr functions ======================================================================================================
#' @rdname setup_approach
#'
#' @inheritParams default_doc_explain
#' @inheritParams setup_approach.regression_separate
#' @param regression.surrogate_n_comb Integer (default is `internal$parameters$used_n_combinations`) specifying the
#' number of unique combinations/coalitions to apply to each training observation. Maximum allowed value is
#' "`internal$parameters$used_n_combinations` - 2". By default, we use all coalitions, but this can take a lot of memory
#' in larger dimensions. Note that by "all", we mean all coalitions chosen by `shapr` to be used. This will be all
#' \eqn{2^{n_{\text{features}}}} coalitions (minus empty and grand coalition) if `shapr` is in the exact mode. If the
#' user sets a lower value than `internal$parameters$used_n_combinations`, then we sample this amount of unique
#' coalitions separately for each training observations. That is, on average, all coalitions should be equally trained.
#'
#' @export
#' @author Lars Henry Berge Olsen
setup_approach.regression_surrogate <- function(internal,
                                                regression.model = parsnip::linear_reg(),
                                                regression.tune_values = NULL,
                                                regression.vfold_cv_para = NULL,
                                                regression.recipe_func = NULL,
                                                regression.surrogate_n_comb =
                                                  internal$parameters$used_n_combinations - 2,
                                                ...) {
  # Check that required libraries are installed
  regression.check_namespaces()

  # Small printout to the user
  if (internal$parameters$verbose == 2) message("Starting 'setup_approach.regression_surrogate'.")

  # Add the default parameter values for the non-user specified parameters for the separate regression approach
  defaults <- mget(c(
    "regression.model", "regression.tune_values", "regression.vfold_cv_para",
    "regression.recipe_func", "regression.surrogate_n_comb"
  ))
  internal <- insert_defaults(internal, defaults)

  # Check the parameters to the regression approach
  internal <- regression.check_parameters(internal)

  # Augment the training data
  x_train_augmented <- regression.surrogate_aug_data(
    internal = internal, x = internal$data$x_train, y_hat = internal$data$x_train_y_hat, augment_include_grand = TRUE
  )

  # Fit the surrogate regression model and store it in the internal list
  if (internal$parameters$verbose == 2) message("Start training the surrogate model.")
  internal$objects$regression.surrogate_model <- regression.train_model(
    x = x_train_augmented,
    seed = internal$parameters$seed,
    verbose = internal$parameters$verbose,
    regression.model = internal$parameters$regression.model,
    regression.tune = internal$parameters$regression.tune,
    regression.tune_values = internal$parameters$regression.tune_values,
    regression.vfold_cv_para = internal$parameters$regression.vfold_cv_para,
    regression.recipe_func = internal$parameters$regression.recipe_func,
    regression.surrogate_n_comb = regression.surrogate_n_comb + 1 # Add 1 as augment_include_grand = TRUE above
  )

  # Small printout to the user
  if (internal$parameters$verbose == 2) message("Done with 'setup_approach.regression_surrogate'.")

  return(internal) # Return the updated internal list
}

#' @inheritParams default_doc
#' @rdname prepare_data
#' @export
#' @author Lars Henry Berge Olsen
prepare_data.regression_surrogate <- function(internal, index_features = NULL, ...) {
  # Load `workflows`, needed when parallelized as we call predict with a workflow object. Checked installed above.
  requireNamespace("workflows", quietly = TRUE)

  # Small printout to the user about which batch that are currently worked on
  if (internal$parameters$verbose == 2) regression.prep_message_batch(internal, index_features)

  # Augment the explicand data
  x_explain_aug <- regression.surrogate_aug_data(internal, x = internal$data$x_explain, index_features = index_features)

  # Compute the predicted response for the explicands, i.e., v(S, x_i) for all explicands x_i and S in index_features
  pred_explicand <- predict(internal$objects$regression.surrogate_model, new_data = x_explain_aug)$.pred

  # Insert the predicted contribution functions values into a data table of the correct setup
  dt_res <- data.table(as.integer(index_features), matrix(pred_explicand, nrow = length(index_features)))
  data.table::setnames(dt_res, c("id_combination", paste0("p_hat1_", seq_len(internal$parameters$n_explain))))
  data.table::setkey(dt_res, id_combination) # Set id_combination to be the key

  return(dt_res)
}

# Augment function =====================================================================================================
#' Augment the training data and the explicands
#'
#' @inheritParams default_doc
#' @inheritParams regression.train_model
#' @param y_hat Vector of numerics (optional) containing the predicted responses for the observations in `x`.
#' @param index_features Array of integers (optional) containing which coalitions to consider. Must be provided if
#' `x` is the explicands.
#' @param augment_add_id_comb Logical (default is `FALSE`). If `TRUE`, an additional column is adding containing
#' which coalition was applied.
#' @param augment_include_grand Logical (default is `FALSE`). If `TRUE`, then the grand coalition is included.
#' If `index_features` are provided, then `augment_include_grand` has no effect. Note that if we sample the
#' combinations then the grand coalition is equally likely to be samples as the other coalitions (or weighted if
#' `augment_comb_prob` is provided).
#' @param augment_masks_as_factor Logical (default is `FALSE`). If `TRUE`, then the binary masks are converted
#' to factors. If `FALSE`, then the binary masks are numerics.
#' @param augment_comb_prob Array of numerics (default is `NULL`). The length of the array must match the number of
#' combinations being considered, where each entry specifies the probability of sampling the corresponding coalition.
#' This is useful if we want to generate more training data for some specific coalitions. One possible choice would be
#' `augment_comb_prob = if (use_Shapley_weights) internal$objects$X$shapley_weight[2:actual_n_combinations] else NULL`.
#' @param augment_weights String (optional). Specifying which type of weights to add to the observations.
#' If `NULL` (default), then no weights are added. If `"Shapley"`, then the Shapley weights for the different
#' combinations are added to corresponding observations where the coalitions was applied. If `uniform`, then
#' all observations get an equal weight of one.
#'
#' @return A data.table containing the augmented data.
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.surrogate_aug_data <- function(internal,
                                          x,
                                          y_hat = NULL,
                                          index_features = NULL,
                                          augment_masks_as_factor = FALSE,
                                          augment_include_grand = FALSE,
                                          augment_add_id_comb = FALSE,
                                          augment_comb_prob = NULL,
                                          augment_weights = NULL) {
  # Get some of the parameters
  S <- internal$objects$S
  actual_n_combinations <- internal$parameters$used_n_combinations - 2 # Remove empty and grand coalitions
  regression.surrogate_n_comb <- internal$parameters$regression.surrogate_n_comb
  if (!is.null(index_features)) regression.surrogate_n_comb <- length(index_features) # Applicable from prep_data()
  if (augment_include_grand) {
    actual_n_combinations <- actual_n_combinations + 1 # Add 1 to include the grand comb
    regression.surrogate_n_comb <- regression.surrogate_n_comb + 1
  }
  if (regression.surrogate_n_comb > actual_n_combinations) regression.surrogate_n_comb <- actual_n_combinations

  # Small checks
  if (!is.null(augment_weights)) augment_weights <- match.arg(augment_weights, c("Shapley", "uniform"))

  if (!is.null(augment_comb_prob) && length(augment_comb_prob) != actual_n_combinations) {
    stop(paste("`augment_comb_prob` must be of length", actual_n_combinations, "."))
  }

  if (!is.null(augment_weights) && augment_include_grand && augment_weights == "Shapley") {
    stop(paste(
      "`augment_include_grand = TRUE` and `augment_weights = 'Shapley'` cannot occure",
      "because this entails too large weight for the grand coalition."
    ))
  }

  # Get the number of observations (either the same as n_train or n_explain)
  n_obs <- nrow(x)

  # Get the names of the categorical/factor features and the continuous/non-categorical/numeric features.
  feature_classes <- internal$objects$feature_specs$classes
  feature_cat <- names(feature_classes)[feature_classes == "factor"]
  feature_cont <- names(feature_classes)[feature_classes != "factor"]

  # Get the indices of the order of the cat and cont features
  feature_cat_idx <- which(names(feature_classes) %in% feature_cat)
  feature_cont_idx <- which(names(feature_classes) %in% feature_cont)

  # Check if we are to augment the training data or the explicands
  if (is.null(index_features)) {
    # Training: get matrix (n_obs x regression.surrogate_n_comb) containing the indices of the active coalitions
    if (regression.surrogate_n_comb >= actual_n_combinations) { # Start from two to exclude the empty set
      comb_active_idx <- matrix(rep(seq(2, actual_n_combinations + 1), times = n_obs), ncol = n_obs)
    } else {
      comb_active_idx <- sapply(seq(n_obs), function(x) { # Add 1 as we want to exclude the empty set
        sample.int(n = actual_n_combinations, size = regression.surrogate_n_comb, prob = augment_comb_prob) + 1
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
  x_augmented <- x[rep(seq_len(n_obs), each = regression.surrogate_n_comb), ]

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
  if (length(feature_cont) > 0) {
    masked_columns <- paste0("mask_", feature_cont)
    x_augmented <- cbind(x_augmented, setNames(data.table(1 * (comb_active[, feature_cont_idx] == 0)), masked_columns))
  }

  # Convert the binary masks to factor if user has specified so
  if (augment_masks_as_factor) x_augmented[, (masked_columns) := lapply(.SD, as.factor), .SDcols = masked_columns]

  # Add either uniform weights or Shapley kernel weights
  if (!is.null(augment_weights)) {
    x_augmented[, "weight" := if (augment_weights == "Shapley") internal$objects$X$shapley_weight[id_comb] else 1]
  }

  # Add the id_comb as a factor
  if (augment_add_id_comb) x_augmented[, "id_comb" := factor(id_comb)]

  # Add repeated responses if provided
  if (!is.null(y_hat)) x_augmented[, "y_hat" := rep(y_hat, each = regression.surrogate_n_comb)]

  # Return the augmented data
  return(x_augmented)
}


# Check function =======================================================================================================
#' Check the `regression.surrogate_n_comb` parameter
#'
#' Check that `regression.surrogate_n_comb` is either NULL or a valid integer.
#'
#' @inheritParams setup_approach.regression_surrogate
#' @param used_n_combinations Integer. The number of used combinations (including the empty and grand coalitions).
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
regression.check_sur_n_comb <- function(regression.surrogate_n_comb, used_n_combinations) {
  if (!is.null(regression.surrogate_n_comb)) {
    if (regression.surrogate_n_comb < 1 || used_n_combinations - 2 < regression.surrogate_n_comb) {
      stop(paste0(
        "`regression.surrogate_n_comb` (", regression.surrogate_n_comb, ") must be a positive integer less than or ",
        "equal to `used_n_combinations` minus two (", used_n_combinations - 2, ")."
      ))
    }
  }
}
