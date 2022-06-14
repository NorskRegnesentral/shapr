#' @keywords internal
setup_approach.copula <- function(internal, ...){

  parameters <- internal$parameters
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain


  # Prepare transformed data
  parameters$mu <- rep(0, ncol(x_train))
  x_train0 <- apply(
    X = x_train,
    MARGIN = 2,
    FUN = gaussian_transform
  )
  parameters$cov_mat <- get_cov_mat(x_train0)


  x_explain_gaussian <- apply(
    X = rbind(x_explain, x_train),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(x_explain)
  )

  if (is.null(dim(x_explain_gaussian))) {
    x_explain_gaussian <- t(as.matrix(x_explain_gaussian))
  }

  internal$data$x_explain_gaussian <- x_explain_gaussian # TODO: Change to this a data.table for consistency (not speed/memory)
  internal$parameters <- parameters

  return(internal)
}

#' @rdname prepare_data
#' @export
prepare_data.copula <- function(internal,  index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  cov_mat <- internal$parameters$cov_mat
  n_samples <- internal$parameters$n_samples
  mu <- internal$parameters$mu
  n_features <- internal$parameters$n_features

  x_explain_gaussian <- internal$data$x_explain_gaussian
  X <- internal$objects$X


  x_explain0 <- as.matrix(x_explain)
  dt_l <- list()
  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }


  for (i in seq_len(n_explain)) {
    l <- lapply(
      X = features,
      FUN = sample_copula,
      n_samples = n_samples,
      mu = mu,
      cov_mat = cov_mat,
      m = n_features,
      x_explain = x_explain0[i, , drop = FALSE],
      x_train = as.matrix(x_train),
      x_explain_gaussian = x_explain_gaussian[i, , drop = FALSE]
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  return(dt)
}

