#' @keywords internal
setup_approach.ctree <- function(internal,
                                 mincriterion = 0.95,
                                 minsplit = 20,
                                 minbucket = 7,
                                 sample = TRUE, ...){

  parameters <- internal$parameters


  # Add arguments to explainer object
  parameters$mincriterion <- mincriterion
  parameters$minsplit <- minsplit
  parameters$minbucket <- minbucket
  parameters$sample <- sample

  internal$parameters <- parameters

  return(internal)
}


#' @param index_features List. Default is NULL but if either various methods are being used or various mincriterion are
#' used for different numbers of conditioned features, this will be a list with the features to pass.
#'
#' @param  mc_cores Integer. Only for class \code{ctree} currently. The number of cores to use in paralellization of the
#' tree building (\code{create_ctree}) and tree sampling (\code{sample_ctree}). Defaults to 1. Note: Uses
#' parallel::mclapply which relies on forking, i.e. uses only 1 core on Windows systems.
#'
#' @param  mc_cores_create_ctree Integer. Same as \code{mc_cores}, but specific for the tree building function
#' #' Defaults to \code{mc_cores}.
#'
#' @param  mc_cores_sample_ctree Integer. Same as \code{mc_cores}, but specific for the tree building prediction
#' function.
#' Defaults to \code{mc_cores}.
#'
#' @rdname prepare_data
#' @export
prepare_data.ctree <- function(internal,  index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_samples <- internal$parameters$n_samples
  n_features <- internal$parameters$n_features
  mincriterion <- internal$parameters$mincriterion
  minsplit <- internal$parameters$minsplit
  minbucket <- internal$parameters$minbucket
  sample <- internal$parameters$sample
  labels <- internal$parameters$feature_list$labels

  X <- internal$objects$X


  dt_l <- list()


  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }


  # this is a list of all 2^M trees (where number of features = M)
  all_trees <- lapply(
    X = features,
    FUN = create_ctree,
    x_train = x_train,
    mincriterion = mincriterion,
    minsplit = minsplit,
    minbucket = minbucket
  )

  for (i in seq_len(n_explain)) {
    l <- lapply(
      X = all_trees,
      FUN = sample_ctree,
      n_samples = n_samples,
      x_explain = x_explain[i, , drop = FALSE],
      x_train = x_train,
      p = n_features,
      sample = sample
    )

    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_combination")
    dt_l[[i]][, w := 1 / n_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_combination := index_features[id_combination]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[id_combination %in% c(1, 2^n_features), w := 1.0]

  # only return unique dt
  dt2 <- dt[, sum(w), by = c("id_combination", labels, "id")]
  setnames(dt2, "V1", "w")

  return(dt2)
}
