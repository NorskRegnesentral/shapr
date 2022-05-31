
#' Helper function used in \code{\link{explain.combined}}
#'
#' @param n_features Integer vector. Note that
#' \code{length(n_features) <= 2^m}, where \code{m} equals the number
#' of features.
#' @param approach Character vector of length \code{m}. All elements should be
#' either \code{"empirical"}, \code{"gaussian"} or \code{"copula"}.
#'
#' @keywords internal
#'
#' @author Nikolai Sellereite
#'
#' @return List
#'
get_list_approaches <- function(n_features, approach) {
  l <- list()
  approach[length(approach)] <- approach[length(approach) - 1]

  x <- which(approach == "independence")
  if (length(x) > 0) {
    if (approach[1] == "independence") x <- c(0, x)
    l$independence <- which(n_features %in% x)
  }

  x <- which(approach == "empirical")
  if (length(x) > 0) {
    if (approach[1] == "empirical") x <- c(0, x)
    l$empirical <- which(n_features %in% x)
  }

  x <- which(approach == "gaussian")
  if (length(x) > 0) {
    if (approach[1] == "gaussian") x <- c(0, x)
    l$gaussian <- which(n_features %in% x)
  }

  x <- which(approach == "copula")
  if (length(x) > 0) {
    if (approach[1] == "copula") x <- c(0, x)
    l$copula <- which(n_features %in% x)
  }

  x <- which(approach == "ctree")
  if (length(x) > 0) {
    if (approach[1] == "ctree") x <- c(0, x)
    l$ctree <- which(n_features %in% x)
  }
  return(l)
}


#' @rdname explain
#' @name explain
#'
#' @export
explain.ctree_comb_mincrit <- function(x, explainer, approach,
                                       prediction_zero, n_samples, n_batches = 1
                                       , seed = 1, mincriterion, ...) {

  # For R CMD check
  row_id <- NULL

  if (length(explainer$feature_list$labels) != length(mincriterion)) {
    stop("The length of mincriterion has to be equal to 1 or the number of features.")
  }

  # Get indices of combinations
  l <- get_list_ctree_mincrit(explainer$X$n_features, mincriterion)
  explainer$return <- TRUE # this is important so that you don't use prediction() twice
  explainer$x_explain <- as.matrix(x)

  dt_l <- list()
  for (i in seq_along(l)) {
    dt_l[[i]] <- explain(x, explainer, approach, prediction_zero,
      index_S = l[[i]],
      mincriterion = as.numeric(names(l[i])),
      only_return_contrib_dt = TRUE,
      seed = seed,
      ...
    )
  }

  dt_mat <- unique(rbindlist(dt_l))
  data.table::setkey(dt_mat, row_id)
  dt_mat[, row_id := NULL]
  dt_kshap <- compute_shapley(explainer, as.matrix(dt_mat))

  # Find which element containing non-na p
  p <- attr(dt_l[[which(sapply(dt_l, function(x) all(!is.na(attr(x, "p")))))]], "p")

  res <- list(dt = dt_kshap,
              model = explainer$model,
              p = p,
              x_explain = explainer$x_explain,
              is_groupwise = explainer$is_groupwise)

  attr(res, "class") <- c("shapr", "list")

  return(res)

}

#' @keywords internal
get_list_ctree_mincrit <- function(n_features, mincriterion) {
  l <- list()

  for (k in 1:length(unique(mincriterion))) {
    x <- which(mincriterion == unique(mincriterion)[k])
    nn <- as.character(unique(mincriterion)[k])
    if (length(l) == 0) x <- c(0, x)
    l[[nn]] <- which(n_features %in% x)
  }
  return(l)
}



#' Compute Shapley values in batches
#'
#' Create a list of indexes used to compute Shapley values in batches.
#'
#' @param explainer The binary matrix \code{S} returned from \code{\link{shapr}}.
#' @param n_batches Numeric value specifying how many batches \code{S} should be split into.
#' @param index_S Numeric vector specifying which rows of \code{S} that should be considered.
#' @return A list of length \code{n_batches}.
#'
#' @details If \code{index_S} is not \code{NULL} then the number of batches is scaled such that the
#' total number of batches is equal \code{n_batches} and not within the rows specified by\code{index_S}.
#'
#' @keywords internal
create_S_batch <- function(explainer, n_batches, index_S = NULL) {

  no_samples <- explainer$n_combinations

  if (n_batches == 1) {
    if (!is.null(index_S)) {
      return(list(index_S))
    } else {
      return(list(2:nrow(explainer$S)))
    }
  }

  if (!is.null(index_S)){
    # Rescale the number of batches to the percentage of observations used for combined approach
    n_batches <- max(1, floor(length(index_S) / no_samples * n_batches))
    if (n_batches == 1) return(list(unique(index_S)))
    x0 <- index_S # TODO: Need to remove index = 1 if it exists here.
  } else {
    x0 <- 2:no_samples # Ignoring the first
  }
  S_groups <- split(x0, cut(x0, n_batches, labels = FALSE))


  return(S_groups)
}



#' Calculate Shapley values
#'
#' Sample covariate values, predict and calculate Shapley values. The sampling and prediction can be done in batches
#' if \code{n_batches} is greater than 1.
#'
#'
#' @inheritParams explain
#' @param ... Arguments passed to \code{\link{prepare_data}} with exception of \code{only_return_contrib_dt},
#' which is only passed to explain. If \code{TRUE} the
#' \code{data.table} from \code{\link{prediction}} is returned, else an object of class \code{shapr}.
#' Each column (except for \code{row_id}) correspond to the vector \code{v_D} in Equation 7 in the reference.
#' The Shapley values can be calculated by \code{t(explainer$W \%*\% dt_contrib[, -"row_id"]))}
#' @param seed Positive integer. If \code{NULL} the seed will be inherited from the calling environment.
#' @return A list. See \code{\link{explain}} for more information.
#' @export
#' @keywords internal
prepare_and_predict <- function(explainer, n_batches, prediction_zero, seed, ...) {

  # For R CMD check
  row_id <- NULL

  index_S <- list(...)$index_S
  only_return_contrib_dt <- list(...)$only_return_contrib_dt
  if(is.null(only_return_contrib_dt)) only_return_contrib_dt <- FALSE

  S_batch <- create_S_batch(explainer, n_batches, index_S)
  pred_batch <- list()
   # OLD
  # r_batch <- list()
  # p <- NA
  #
  # for (batch in seq_along(S_batch)) {
  #
  #   dt <- prepare_data(explainer, index_features = S_batch[[batch]], ...)
  #   r_batch[[batch]] <- prediction(dt, prediction_zero, explainer)
  #   r_batch[[batch]]$dt_mat[, row_id := S_batch[[batch]]]
  #   if (!is.null(r_batch[[batch]]$p)) p <- r_batch[[batch]]$p
  #   #
  #   if (length(S_batch) > 1) {
  #     cat("Batch no", batch, "of", length(S_batch), "completed.\n")
  #   }
  #
  # }

  batchfun <- function(S){
    dt <- prepare_data(explainer, index_features = S, ...)
    r_batch_i <- prediction(dt, prediction_zero, explainer)
    r_batch_i$dt_mat[, row_id := S]
    r_batch_i
  }

  r_batch <- future.apply::future_lapply(S_batch,batchfun,future.seed = seed)
  dt_mat <- rbindlist(lapply(r_batch, "[[", "dt_mat"))

  p <- unlist(lapply(r_batch, "[[", "p"),use.names = F)
  if(!is.null(p)){
    names(p) <- seq_len(nrow(explainer$x_explain)) # Safe also for n_test = 1
  } else {
    p <- NA
  }



  if (only_return_contrib_dt) {
    attr(dt_mat, "p") <- p
    return(dt_mat)
  }

  dt_mat <- unique(dt_mat)
  data.table::setkey(dt_mat, row_id)
  dt_mat[, row_id := NULL]

  dt_kshap <- compute_shapley(explainer, as.matrix(dt_mat))

  res <- list(dt = dt_kshap,
              model = explainer$model,
              p = p,
              x_explain = explainer$x_explain,
              is_groupwise = explainer$is_groupwise)

  attr(res, "class") <- c("shapr", "list")

  return(res)

}


#' @export
print.shapr <- function(x, ...) {
  print(x$shapley_values)
}


