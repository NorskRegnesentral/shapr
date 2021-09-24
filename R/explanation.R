#' Explain the output of machine learning models with more accurately estimated Shapley values
#'
#' @param x A matrix or data.frame. Contains the the features, whose
#' predictions ought to be explained (test data).
#'
#' @param explainer An \code{explainer} object to use for explaining the observations.
#' See \code{\link{shapr}}.
#'
#' @param approach Character vector of length \code{1} or \code{n_features}.
#' \code{n_features} equals the total number of features in the model. All elements should,
#' either be \code{"gaussian"}, \code{"copula"}, \code{"empirical"}, \code{"ctree"}, or \code{"independence"}.
#' See details for more information.
#'
#' @param n_samples Positive integer. Indicating the maximum number of samples to use in the
#' Monte Carlo integration for every conditional expectation. See also details.
#'
#' @param prediction_zero Numeric. The prediction value for unseen data, typically equal to the mean of
#' the response.
#'
#' @param n_batches Positive integer.
#' Specifies how many batches the total number of feature combinations should be split into when calculating the
#' contribution function for each test observation.
#' The default value is 1.
#' Increasing the number of batches may significantly reduce the RAM allocation for models with many features.
#' This typically comes with a small increase in computation time.
#'
#' @param only_return_contrib_dt Logical. Used internally in \code{explain.combined}.
#' If \code{FALSE} (default) an object of class \code{shapr} is returned.
#' If \code{TRUE} the \code{data.table} from \code{\link{prediction}} is returned.
#' Then, each column (except for \code{row_id}) correspond to the vector \code{v_D} in Equation 7 in the reference.
#' The Shapley values can be calculated by \code{t(explainer$W \%*\% dt_contrib[, -"row_id"]))}.
#'
#' @param ... Additional arguments passed to \code{\link{prepare_and_predict}}
#'
#' @details The most important thing to notice is that \code{shapr} has implemented five different
#' approaches for estimating the conditional distributions of the data, namely \code{"empirical"},
#' \code{"gaussian"}, \code{"copula"}, \code{"ctree"} and \code{"independence"}.
#' In addition, the user also has the option of combining the four approaches.
#' E.g., if you're in a situation where you have trained a model that consists of 10 features,
#' and you'd like to use the \code{"gaussian"} approach when you condition on a single feature,
#' the \code{"empirical"} approach if you condition on 2-5 features, and \code{"copula"} version
#' if you condition on more than 5 features this can be done by simply passing
#' \code{approach = c("gaussian", rep("empirical", 4), rep("copula", 5))}. If
#' \code{"approach[i]" = "gaussian"} means that you'd like to use the \code{"gaussian"} approach
#' when conditioning on \code{i} features.
#'
#' For \code{approach="ctree"}, \code{n_samples} corresponds to the number of samples
#' from the leaf node (see an exception related to the \code{sample} argument).
#' For \code{approach="empirical"}, \code{n_samples} is  the \eqn{K} parameter in equations (14-15) of
#' Aas et al. (2021), i.e. the maximum number of observations (with largest weights) that is used, see also the
#' \code{w_threshold} argument.
#'
#'
#' @return Object of class \code{c("shapr", "list")}. Contains the following items:
#' \describe{
#'   \item{dt}{data.table}
#'   \item{model}{Model object}
#'   \item{p}{Numeric vector}
#'   \item{x_test}{data.table}
#' }
#'
#' Note that the returned items \code{model}, \code{p} and \code{x_test} are mostly added due
#' to the implementation of \code{plot.shapr}. If you only want to look at the numerical results
#' it is sufficient to focus on \code{dt}. \code{dt} is a data.table where the number of rows equals
#' the number of observations you'd like to explain, and the number of columns equals \code{m +1},
#' where \code{m} equals the total number of features in your model.
#'
#' If \code{dt[i, j + 1] > 0} it indicates that the j-th feature increased the prediction for
#' the i-th observation. Likewise, if \code{dt[i, j + 1] < 0} it indicates that the j-th feature
#' decreased the prediction for the i-th observation. The magnitude of the value is also important
#' to notice. E.g. if \code{dt[i, k + 1]} and \code{dt[i, j + 1]} are greater than \code{0},
#' where \code{j != k}, and \code{dt[i, k + 1]} > \code{dt[i, j + 1]} this indicates that feature
#' \code{j} and \code{k} both increased the value of the prediction, but that the effect of the k-th
#' feature was larger than the j-th feature.
#'
#' The first column in \code{dt}, called `none`, is the prediction value not assigned to any of the features
#' (\ifelse{html}{\eqn{\phi}\out{<sub>0</sub>}}{\eqn{\phi_0}}).
#' It's equal for all observations and set by the user through the argument \code{prediction_zero}.
#' In theory this value should be the expected prediction without conditioning on any features.
#' Typically we set this value equal to the mean of the response variable in our training data, but other choices
#' such as the mean of the predictions in the training data are also reasonable.
#'
#' @export
#'
#' @author Camilla Lingjaerde, Nikolai Sellereite, Martin Jullum, Annabelle Redelmeier
#'
#'@references
#'   Aas, K., Jullum, M., & Løland, A. (2021). Explaining individual predictions when features are dependent:
#'   More accurate approximations to Shapley values. Artificial Intelligence, 298, 103502.
#'
#' @examples
#' if (requireNamespace("MASS", quietly = TRUE)) {
#'   # Load example data
#'   data("Boston", package = "MASS")
#'
#'   # Split data into test- and training data
#'   x_train <- head(Boston, -3)
#'   x_test <- tail(Boston, 3)
#'
#'   # Fit a linear model
#'   model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#'   # Create an explainer object
#'   explainer <- shapr(x_train, model)
#'
#'   # Explain predictions
#'   p <- mean(x_train$medv)
#'
#'   # Empirical approach
#'   explain1 <- explain(x_test, explainer,
#'     approach = "empirical",
#'     prediction_zero = p, n_samples = 1e2
#'   )
#'
#'   # Gaussian approach
#'   explain2 <- explain(x_test, explainer,
#'     approach = "gaussian",
#'     prediction_zero = p, n_samples = 1e2
#'   )
#'
#'   # Gaussian copula approach
#'   explain3 <- explain(x_test, explainer,
#'     approach = "copula",
#'     prediction_zero = p, n_samples = 1e2
#'   )
#'
#'   # ctree approach
#'   explain4 <- explain(x_test, explainer,
#'     approach = "ctree",
#'     prediction_zero = p
#'   )
#'
#'   # Combined approach
#'   approach <- c("gaussian", "gaussian", "empirical", "empirical")
#'   explain5 <- explain(x_test, explainer,
#'     approach = approach,
#'     prediction_zero = p, n_samples = 1e2
#'   )
#'
#'   # Print the Shapley values
#'   print(explain1$dt)
#'
#'   # Plot the results
#'   if (requireNamespace("ggplot2", quietly = TRUE)) {
#'     plot(explain1)
#'   }
#'
#'   # Group-wise explanations
#'   group <- list(A = c("lstat", "rm"), B = c("dis", "indus"))
#'   explainer_group <- shapr(x_train, model, group = group)
#'   explain_groups <- explain(
#'     x_test,
#'     explainer_group,
#'     approach = "empirical",
#'     prediction_zero = p,
#'     n_samples = 1e2
#'   )
#'   print(explain_groups$dt)
#' }
explain <- function(x, explainer, approach, prediction_zero,
                    n_samples = 1e3, n_batches = 1, only_return_contrib_dt = FALSE, ...) {
  extras <- list(...)

  # Check input for x
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a data.frame/data.table.")
  }

  if (n_batches < 1 || n_batches > nrow(explainer$S)) {
    stop("`n_batches` is smaller than 1 or greater than the number of rows in explainer$S.")
  }
  # Check input for approach
  if (!(is.vector(approach) &&
    is.atomic(approach) &&
    (length(approach) == 1 | length(approach) == length(explainer$feature_list$labels)) &&
    all(is.element(approach, c("empirical", "gaussian", "copula", "ctree", "independence"))))
  ) {
    stop(
      paste(
        "It seems that you passed a non-valid value for approach.",
        "It should be either 'empirical', 'gaussian', 'copula', 'ctree', 'independence' or",
        "a vector of length=ncol(x) with only the above characters."
      )
    )
  }

  this_class <- ""
  if (length(approach) > 1) {
    class(this_class) <- "combined"
  } else if (length(extras$mincriterion) > 1) {
    class(this_class) <- "ctree_comb_mincrit"
  } else {
    class(this_class) <- approach
  }

  UseMethod("explain", this_class)
}

#' @param seed Positive integer. If \code{NULL} the seed will be inherited from the calling environment.
#' @rdname explain
#' @export
explain.independence <- function(x, explainer, approach, prediction_zero,
                                 n_samples = 1e3, n_batches = 1, only_return_contrib_dt = FALSE, seed = 1, ...) {


  if (!is.null(seed)) set.seed(seed)

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$approach <- approach
  explainer$n_samples <- n_samples

  r <- prepare_and_predict(explainer, n_batches, prediction_zero, only_return_contrib_dt, ...)
}


#' @param type Character. Should be equal to either \code{"independence"},
#' \code{"fixed_sigma"}, \code{"AICc_each_k"} or \code{"AICc_full"}.
#'
#' @param fixed_sigma_vec Numeric. Represents the kernel bandwidth. Note that this argument is only
#' applicable when \code{approach = "empirical"}, and \code{type = "fixed_sigma"}
#'
#' @param n_samples_aicc Positive integer. Number of samples to consider in AICc optimization.
#' Note that this argument is only applicable when \code{approach = "empirical"}, and \code{type}
#' is either equal to \code{"AICc_each_k"} or \code{"AICc_full"}
#'
#' @param eval_max_aicc Positive integer. Maximum number of iterations when
#' optimizing the AICc. Note that this argument is only applicable when
#' \code{approach = "empirical"}, and \code{type} is either equal to
#' \code{"AICc_each_k"} or \code{"AICc_full"}
#'
#' @param start_aicc Numeric. Start value of \code{sigma} when optimizing the AICc. Note that this argument
#' is only applicable when \code{approach = "empirical"}, and \code{type} is either equal to
#' \code{"AICc_each_k"} or \code{"AICc_full"}
#'
#' @param w_threshold Numeric vector of length 1, with \code{0 < w_threshold <= 1} representing the minimum proportion
#' of the total empirical weight that data samples should use. If e.g. \code{w_threshold = .8} we will choose the
#' \code{K} samples with the largest weight so that the sum of the weights accounts for 80\% of the total weight.
#' \code{w_threshold} is the \eqn{\eta} parameter in equation (15) of Aas et al (2021).
#' @param cov_mat Numeric matrix. (Optional) Containing the covariance matrix of the data
#' generating distribution. \code{NULL} means it is estimated from the data if needed
#' (in the empirical approach).
#'
#' @rdname explain
#'
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero,
                              n_samples = 1e3, n_batches = 1, only_return_contrib_dt = FALSE, seed = 1,
                              w_threshold = 0.95, type = "fixed_sigma", fixed_sigma_vec = 0.1,
                              n_samples_aicc = 1000, eval_max_aicc = 20,
                              start_aicc = 0.1,
                              cov_mat = NULL, ...) {

  if (!is.null(seed)) set.seed(seed)

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$approach <- approach
  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$n_samples_aicc <- n_samples_aicc
  explainer$eval_max_aicc <- eval_max_aicc
  explainer$start_aicc <- start_aicc
  explainer$w_threshold <- w_threshold
  explainer$n_samples <- n_samples

  if (type == "independence") {
    warning(paste0(
      "Using type = 'independence' for approach = 'empirical' is deprecated.\n",
      "Please use approach = 'independence' instead in the call to explain()."
    ))
  }

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    cov_mat <- stats::cov(explainer$x_train)
  }
  # Make sure that covariance matrix is positive-definite
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }

  r <- prepare_and_predict(explainer, n_batches, prediction_zero, only_return_contrib_dt, ...)

  return(r)
}

#' @param mu Numeric vector. (Optional) Containing the mean of the data generating distribution.
#' If \code{NULL} the expected values are estimated from the data. Note that this is only used
#' when \code{approach = "gaussian"}.
#'
#' @param cov_mat Numeric matrix. (Optional) Containing the covariance matrix of the data
#' generating distribution. \code{NULL} means it is estimated from the data if needed
#' (in the Gaussian approach).
#'
#' @rdname explain
#'
#' @export
explain.gaussian <- function(x, explainer, approach, prediction_zero, n_samples = 1e3,
                             n_batches = 1, only_return_contrib_dt = FALSE, seed = 1,
                             mu = NULL, cov_mat = NULL, ...) {

  if (!is.null(seed)) set.seed(seed)

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$approach <- approach
  explainer$n_samples <- n_samples


  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    explainer$mu <- unname(colMeans(explainer$x_train))
  } else {
    explainer$mu <- mu
  }

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    cov_mat <- stats::cov(explainer$x_train)
  }

  # Make sure that covariance matrix is positive-definite
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }

  r <- prepare_and_predict(explainer, n_batches, prediction_zero, only_return_contrib_dt, ...)

  return(r)
}





#' @rdname explain
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, n_samples = 1e3,
                           n_batches = 1, only_return_contrib_dt = FALSE, seed = 1, ...) {

  if (!is.null(seed)) set.seed(seed)

  # Setup
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$approach <- approach
  explainer$n_samples <- n_samples

  # Prepare transformed data
  x_train <- apply(
    X = explainer$x_train,
    MARGIN = 2,
    FUN = gaussian_transform
  )
  x_test_gaussian <- apply(
    X = rbind(explainer$x_test, explainer$x_train),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(explainer$x_test)
  )

  if (is.null(dim(x_test_gaussian))) {
    x_test_gaussian <- t(as.matrix(x_test_gaussian))
  }

  explainer$mu <- rep(0, ncol(explainer$x_train))
  cov_mat <- stats::cov(x_train) # Gaussian transformed cov. mat
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }

  explainer$x_test_gaussian <- x_test_gaussian

  r <- prepare_and_predict(explainer, n_batches, prediction_zero, only_return_contrib_dt, ...)

  return(r)
}


#' @param mincriterion Numeric value or vector where length of vector is the number of features in model.
#' Value is equal to 1 - alpha where alpha is the nominal level of the conditional
#' independence tests.
#' If it is a vector, this indicates which mincriterion to use
#' when conditioning on various numbers of features.
#'
#' @param minsplit Numeric value. Equal to the value that the sum of the left and right daughter nodes need to exceed.
#'
#' @param minbucket Numeric value. Equal to the minimum sum of weights in a terminal node.
#'
#' @param sample Boolean. If TRUE, then the method always samples \code{n_samples} from the leaf (with replacement).
#' If FALSE and the number of obs in the leaf is less than \code{n_samples}, the method will take all observations
#' in the leaf. If FALSE and the number of obs in the leaf is more than \code{n_samples}, the method will sample
#' \code{n_samples} (with replacement). This means that there will always be sampling in the leaf unless
#' \code{sample} = FALSE AND the number of obs in the node is less than \code{n_samples}.
#
#' @rdname explain
#' @name explain
#'
#' @export
explain.ctree <- function(x, explainer, approach, prediction_zero, n_samples = 1e3,
                          n_batches = 1, only_return_contrib_dt = FALSE, seed = 1,
                          mincriterion = 0.95, minsplit = 20,
                          minbucket = 7, sample = TRUE, ...) {

  if (!is.null(seed)) set.seed(seed)

  # Checks input argument
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Add arguments to explainer object
  explainer$x_test <- preprocess_data(x, explainer$feature_list)$x_dt
  explainer$approach <- approach
  explainer$mincriterion <- mincriterion
  explainer$minsplit <- minsplit
  explainer$minbucket <- minbucket
  explainer$sample <- sample
  explainer$n_samples <- n_samples

  r <- prepare_and_predict(explainer, n_batches, prediction_zero, only_return_contrib_dt, ...)

  return(r)
}

#' @rdname explain
#' @name explain
#'
#' @export
explain.combined <- function(x, explainer, approach, prediction_zero, n_samples = 1e3,
                             n_batches = 1, only_return_contrib_dt, seed = 1, mu = NULL, cov_mat = NULL, ...) {

  # for R CMD check
  row_id <- NULL
  if (!is.null(seed)) set.seed(seed)

  # Get indices of combinations
  l <- get_list_approaches(explainer$X$n_features, approach)
  explainer$return <- TRUE
  explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
  explainer$n_samples <- n_samples

  dt_l <- list()
  # Compute shapley values for all methods
  for (i in seq_along(l)) {
    dt_l[[i]] <- explain(x, explainer, approach = names(l)[i], prediction_zero,
                         index_S = l[[i]], n_batches = n_batches,
                         only_return_contrib_dt = TRUE, seed = NULL, ...)
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
              x_test = explainer$x_test,
              is_groupwise = explainer$is_groupwise)

  attr(res, "class") <- c("shapr", "list")

  return(res)
}

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
                                       prediction_zero, n_samples, n_batches = 1,
                                       only_return_contrib_dt, seed = 1, mincriterion, ...) {

  # For R CMD check
  row_id <- NULL

  if (length(explainer$feature_list$labels) != length(mincriterion)) {
    stop("The length of mincriterion has to be equal to 1 or the number of features.")
  }
  if (!is.null(seed)) set.seed(seed)

  # Get indices of combinations
  l <- get_list_ctree_mincrit(explainer$X$n_features, mincriterion)
  explainer$return <- TRUE # this is important so that you don't use prediction() twice
  explainer$x_test <- as.matrix(x)

  dt_l <- list()
  for (i in seq_along(l)) {
    dt_l[[i]] <- explain(x, explainer, approach, prediction_zero,
      index_S = l[[i]],
      mincriterion = as.numeric(names(l[i])),
      only_return_contrib_dt = TRUE,
      seed = NULL,
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
              x_test = explainer$x_test,
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

  no_samples <- nrow(explainer$S)

  if (n_batches == 1) {
    if (!is.null(index_S)) {
      return(list(index_S))
    } else {
      return(list(1:nrow(explainer$S)))
    }
  }

  if (!is.null(index_S)) {
    # Rescale the number of batches to the percentage of observations used
    n_batches <- max(1, floor(length(index_features) / nrow(explainer$S) * n_batches))
    if (n_batches == 1) return(list(unique(index_features)))
    x0 <- index_features
  } else {
    x0 <- 1:no_samples
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
#' @return A list. See \code{\link{explain}} for more information.
#' @keywords internal
prepare_and_predict <- function(explainer, n_batches, prediction_zero, only_return_contrib_dt, ...) {

  # For R CMD check
  row_id <- NULL

  dots <- list(...)
  index_S <- dots$index_S
  dots$index_S <- NULL

  S_batch <- create_S_batch(explainer, n_batches, index_S)
  pred_batch <- list()
  r_batch <- list()
  p <- NA

  for (batch in seq_along(S_batch)) {

    dt <- prepare_data(explainer, index_features = S_batch[[batch]], ...)
    r_batch[[batch]] <- prediction(dt, prediction_zero, explainer)
    r_batch[[batch]]$dt_mat[, row_id := S_batch[[batch]]]

    if (!is.null(r_batch[[batch]]$p)) p <- r_batch[[batch]]$p

    if (length(S_batch) > 1) {
      cat("Batch no", batch, "of", length(S_batch), "completed.\n")
    }

  }

  dt_mat <- rbindlist(lapply(r_batch, "[[", "dt_mat"))

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
              x_test = explainer$x_test,
              is_groupwise = explainer$is_groupwise)

  attr(res, "class") <- c("shapr", "list")

  return(res)

}


#' @export
print.shapr <- function(x, ...) {
  print(x$dt)
}
