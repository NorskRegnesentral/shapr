#' @rdname setup_approach
#'
#' @inheritParams default_doc_explain
#'
#' @export
setup_approach.independence <- function(internal, ...) {
  return(internal)
}

#' @rdname prepare_data
#' @export
prepare_data.independence <- function(internal, index_features = NULL, ...) {

  x_train0 <- copy(internal$data$x_train)
  x_explain0 <- copy(internal$data$x_explain)

  feature_specs <- internal$objects$feature_specs
  n_samples <- internal$parameters$n_samples
  n_train <- internal$parameters$n_train
  n_explain <- internal$parameters$n_explain

  X <- internal$objects$X
  S <- internal$objects$S

  if (is.null(index_features)) {
    index_features <- X[, .I]
  }

  non_numeric_features <- feature_specs$labels[feature_specs$classes != "numeric"]

  S0 <- S[index_features, , drop = FALSE]

  if (length(non_numeric_features) > 0) {
    x_train0[, (non_numeric_features) := lapply(.SD, function(x) {
      as.numeric(as.character(x))
    }),
    .SDcols = non_numeric_features
    ]
    x_explain0[, (non_numeric_features) := lapply(.SD, function(x) {
      as.numeric(as.character(x))
    }),
    .SDcols = non_numeric_features
    ]
  }

  x_train0_mat <- as.matrix(x_train0)
  x_explain0_mat <- as.matrix(x_explain0)

  index_s <- rep(seq(nrow(S0)), each = min(n_samples, n_train))
  w <- 1 / n_samples # Yes, not n_samples0

  n_col <- n_explain

  dt_l <- list()
  for (i in seq(n_col)) {
    x_explain00_mat <- x_explain0_mat[i, , drop = FALSE]

    # sampling index_xtrain
    index_xtrain <- c(replicate(nrow(S0), sample(x = seq(n_train), size = min(n_samples, n_train), replace = F)))

    # Generate data used for prediction
    dt_p <- observation_impute_cpp(
      index_xtrain = index_xtrain,
      index_s = index_s,
      xtrain = x_train0_mat,
      xtest = x_explain00_mat,
      S = S0
    )

    # Add keys
    dt_l[[i]] <- data.table::as.data.table(dt_p)
    data.table::setnames(dt_l[[i]], feature_specs$labels)
    dt_l[[i]][, id_combination := index_s]
    dt_l[[i]][, w := w] # IS THIS NECESSARY?
    dt_l[[i]][, id := i]

    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }


  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}
