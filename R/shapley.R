#' Calculate Shapley weight
#'
#' @inheritParams global_arguments
#'
#' @return Numeric
#'
#' @export
#'
#' @author Nikolai Sellereite
shapley_weights <- function(m, N, s, weight_zero_m = 10^6) {
  x <- (m - 1) / (N * s * (m - s))
  x[!is.finite(x)] <- weight_zero_m
  x
}

#' Calculate weighted matrix
#'
#' @param X data.table
#'
#' @return Matrix
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
weight_matrix <- function(X, use_shapley_weights_in_W = TRUE, normalize_W_weights = TRUE) {
  if (use_shapley_weights_in_W) {
    w <- X[["shapley_weight"]] * X[["no"]]
  } else {
    w <- X[["no"]]
    w[c(1, length(w))] <- X[["shapley_weight"]][c(1, length(w))]
  }

  if (normalize_W_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  W <- weight_matrix_cpp(
    features = X[["features"]],
    m = X[.N][["nfeatures"]],
    n = X[, .N],
    w = w
  )

  return(W)
}


#' Create an explainer object with Shapley weights for test data.
#'
#' @inheritParams global_arguments
#'
#' @param x An \code{ntrain x p} numeric matrix or data.frame, where \code{p = ncol(x)} (total number of explanatory variables).Contains the variables used for training the model
#' (i.e. the explanatory variables). Note that the response variable should not be part of
#' \code{x}.
#'
#' @param model The model whose predictions we want to explain.
#'
#' @param n_combinations Integer. The number of feature combinations to sample. If \code{NULL},
#' the exact method is used and all combinations are considered. The maximum number of
#' combinations equals \code{2^p}.
#'
#' @return A list to be used by \code{explain} to compute the kernel SHAP values (\code{Kshap}).
#'
#' @export
#'
#' @author Nikolai Sellereite
#'
shapr <- function(x,
                  model,
                  n_combinations = NULL) {

  # Checks input argument
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Setup
  explainer <- as.list(environment())
  explainer$exact <- ifelse(is.null(n_combinations), TRUE, FALSE)
  explainer$n_features <- ncol(x)
  explainer$model_type <- model_type(model)

  # Checks model and features
  explainer$p <- predict_model(model, head(x))

  # Converts to data.table, otherwise copy to x_train  --------------
  x_train <- data.table::as.data.table(x)

  # Get all combinations ----------------
  dt_combinations <- feature_combinations(
    m = explainer$n_features,
    exact = explainer$exact,
    n_combinations = n_combinations,
    shapley_weight_inf_replacement = 10^6,
    reduce_dim = TRUE
  )

  # Get weighted matrix ----------------
  weighted_mat <- weight_matrix(
    X = dt_combinations,
    use_shapley_weights_in_W = ifelse(explainer$exact, TRUE, FALSE),
    normalize_W_weights = TRUE
  )

  ## Get feature matrix ---------
  feature_matrix <- feature_matrix_cpp(
    features = dt_combinations[["features"]],
    nfeatures = explainer$n_features
  )

  explainer$S <- feature_matrix
  explainer$W <- weighted_mat
  explainer$X <- dt_combinations
  explainer$x_train <- x_train
  explainer$x <- NULL
  explainer$p <- NULL

  attr(explainer, "class") <- c("explainer", "list")

  return(explainer)
}

#' @keywords internal
distance_matrix <- function(x_train, x_test = NULL, list_features) {
  if (is.null(x_test)) return(NULL)

  # Get covariance matrix
  mcov <- stats::cov(x_train)
  if (is.null(dim(x_test))) {
    x_test <- t(as.matrix(x_test))
  }
  # Note that D equals D_S(,)^2 in the paper
  D <- mahalanobis_distance_cpp(
    featureList = list_features,
    Xtrain_mat = as.matrix(x_train),
    Xtest_mat = as.matrix(x_test),
    mcov = mcov,
    S_scale_dist = TRUE
  )

  # Normalize distance rows to ensure numerical stability in later operations
  colmin <- apply(X = D, MARGIN = c(2, 3), FUN = min)
  for (i in 1:dim(D)[3]) {
    D[, , i] <- t(t(D[, , i]) - colmin[, i])
  }

  return(D)
}
