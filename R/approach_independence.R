#' @rdname setup_approach
#'
#' @inheritParams default_doc_export
#'
#' @export
setup_approach.independence <- function(internal, ...) {
  return(internal)
}

#' @rdname prepare_data
#' @export
prepare_data.independence <- function(internal, index_features = NULL, ...) {
  # This function generates the MC samples for an observation Xs* by extracting all the feature values
  # Xsbar' from another observation in the training data and merging the two observations together
  # to form the final imputed MC sample used when estimating the contribution function in shapr.

  # Make copy of the data.tables of the data
  x_train0 <- copy(internal$data$x_train)
  x_explain0 <- copy(internal$data$x_explain)

  # Extract relevant parameters
  feature_specs <- internal$objects$feature_specs
  n_MC_samples <- internal$parameters$n_MC_samples
  n_train <- internal$parameters$n_train
  n_explain <- internal$parameters$n_explain

  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X
  S <- internal$iter_list[[iter]]$S

  if (is.null(index_features)) {
    # Use all coalitions (only applies if a single approach is used)
    index_features <- X[, .I]
  }

  # Extract the relevant coalitions
  # Set `drop = FALSE` to ensure that `S0` is a matrix.
  S0 <- S[index_features, , drop = FALSE]

  # Get the categorical features
  non_numeric_features <- feature_specs$labels[feature_specs$classes != "numeric"]

  # Get the levels of the categorical features
  level_list <- lapply(x_train0[, .SD, .SDcols = non_numeric_features], FUN = levels)

  # Check if we have any categorical features (to then apply a hack for the method to work)
  if (length(non_numeric_features) > 0) {
    # We have categorical features and we convert them to integers starting
    # from 1. i.e., a categorical feature with three levels `small`, `medium`, `large`
    # will be encoded as `1`, `2`, and `3`, respectively.
    # Apply this encoding to the training data and data to be explained
    x_train0[, (non_numeric_features) := lapply(.SD, function(x) {
      as.integer(x)
    }),
    .SDcols = non_numeric_features
    ]
    x_explain0[, (non_numeric_features) := lapply(.SD, function(x) {
      as.integer(x)
    }),
    .SDcols = non_numeric_features
    ]
  }

  # Convert the data.tables to matrices as all entries are numeric.
  x_train0_mat <- as.matrix(x_train0)
  x_explain0_mat <- as.matrix(x_explain0)

  # Get coalition indices.
  # We repeat each coalition index `min(n_MC_samples, n_train)` times. We use `min`
  # as we cannot sample `n_MC_samples` unique indices if `n_train` is less than `n_MC_samples`.
  index_s <- rep(seq_len(nrow(S0)), each = min(n_MC_samples, n_train))
  w0 <- 1 / min(n_MC_samples, n_train) # The inverse of the number of samples being used in practice

  # Create a list to store the MC samples, where the i-th entry is associated with the i-th explicand
  dt_l <- list()

  # Iterate over the explicands
  for (i in seq(n_explain)) {
    # Extract the ith explicand in matrix form
    x_explain00_mat <- x_explain0_mat[i, , drop = FALSE]

    # Sample the indices of the training observations we are going to splice the explicand with
    # and replicate these indices by the number of coalitions.
    index_xtrain <- c(replicate(nrow(S0), sample(x = seq(n_train), size = min(n_MC_samples, n_train), replace = FALSE)))

    # Generate data used for prediction. This splices the explicand with
    # the other sampled training observations for all relevant coalitions.
    dt_p <- observation_impute_cpp(
      index_xtrain = index_xtrain,
      index_s = index_s,
      x_train = x_train0_mat,
      x_explain = x_explain00_mat,
      S = S0
    )

    # Add keys
    dt_l[[i]] <- data.table::as.data.table(dt_p)
    data.table::setnames(dt_l[[i]], feature_specs$labels)
    dt_l[[i]][, id_coalition := index_features[index_s]]
    dt_l[[i]][, w := w0]
    dt_l[[i]][, id := i]
  }

  # Combine the list of data.tables together to a single data.table
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)

  # Check if we have any categorical features
  if (length(non_numeric_features) > 0) {
    # We have categorical features and we then go from the integer encoding applied
    # above back to the original categories/levels.

    # Iterate over the categorical features
    for (this in non_numeric_features) {
      # We extract the categories/levels for `this` categorical features
      this_levels <- level_list[[this]]

      # Then we convert `this` from integers to a factor where we specify that the `levels`
      # go from 1 to the number of levels/categories (i.e., the max integer). We need to
      # specify this in case not all the levels are presents in `dt` which can happen for rare
      # levels. We set the `labels` for each of the `levels` to be the original categories
      dt[, (this) := factor(get(this), levels = seq_along(this_levels), labels = this_levels)]
    }
  }

  # Return the final data.table
  return(dt)
}
