#' Calculate Shapley weight
#'
#' @param m Positive integer. Total number of features.
#' @param n_features Positive integer. Represents the number of features you want to sample from a feature
#' space consisting of \code{m} unique features. Note that \code{ 0 < = n_features <= m}.
#' @param N Positive integer. The number of unique combinations when sampling \code{n_features} features,
#' without replacement, from a sample space consisting of \code{m} different features.
#' @param weight_zero_m Positive integer. Represents the Shapley weight for two special
#' cases, i.e. the case where you have either \code{0} or \code{m} features.
#'
#' @return Numeric
#'
#' @author Nikolai Sellereite
shapley_weights <- function(m, N, n_features, weight_zero_m = 10^6) {
  x <- (m - 1) / (N * n_features * (m - n_features))
  x[!is.finite(x)] <- weight_zero_m
  x
}

#' Calculate weighted matrix
#'
#' @param X data.table
#' @param normalize_W_weights Logical. Whether to normalize the weights for the combinations to sum to 1 for
#' increased numerical stability before solving the WLS (weighted least squares). Applies to all combinations
#' except combination \code{1} and \code{2^m}.
#'
#' @return Numeric matrix. See \code{\link{weight_matrix_cpp}} for more information.
#'
#' @author Nikolai Sellereite, Martin Jullum
weight_matrix <- function(X, normalize_W_weights = TRUE) {

  # Fetch weights
  w <- X[["shapley_weight"]]

  if (normalize_W_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  W <- weight_matrix_cpp(
    features = X[["features"]],
    m = X[.N][["n_features"]],
    n = X[, .N],
    w = w
  )

  return(W)
}

#' Create an explainer object with Shapley weights for test data.
#'
#' @param x Numeric matrix or data.frame. Contains the data used for training the model.
#'
#' @param model The model whose predictions we want to explain. See \code{\link{predict_model}}
#' for more information about which models \code{shapr} supports natively.
#'
#' @param n_combinations Integer. The number of feature combinations to sample. If \code{NULL},
#' the exact method is used and all combinations are considered. The maximum number of
#' combinations equals \code{2^ncol(x)}.
#'
#' @param feature_labels Character vector. The labels/names of the features used for training the model.
#' Only applicable if you are using a custom model. Otherwise the features in use are extracted from \code{model}.
#'
#' @return Named list that contains the following items:
#' \describe{
#'   \item{exact}{Boolean. Equals \code{TRUE} if \code{n_combinations = NULL} or
#'   \code{n_combinations < 2^ncol(x)}, otherwise \code{FALSE}.}
#'   \item{n_features}{Positive integer. The number of columns in \code{x}}
#'   \item{model_type}{Character. Returned value after calling \code{model_type(model)}}
#'   \item{S}{Binary matrix. The number of rows equals the number of unique combinations, and
#'   the number of columns equals the total number of features. I.e. let's say we have a case with
#'   three features. In that case we have \code{2^3 = 8} unique combinations. If the j-th
#'   observation for the i-th row equals \code{1} it indicates that the j-th feature is present in
#'   the i-th combination. Otherwise it equals \code{0}.}
#'   \item{W}{Second item}
#'   \item{X}{data.table. Returned object from \code{\link{feature_combinations}}}
#'   \item{x_train}{data.table. Transformed \code{x} into a data.table.}
#' }
#'
#' In addition to the items above \code{model}, \code{feature_labels} (updated with the names actually used by the
#' model) and \code{n_combinations} is also present in the returned object.
#'
#' @export
#'
#' @author Nikolai Sellereite
#'
#' @examples
#' # Load example data
#' data("Boston", package = "MASS")
#' df <- Boston
#'
#' # Example using the exact method
#' x_var <- c("lstat", "rm", "dis", "indus")
#' y_var <- "medv"
#' df1 <- df[, x_var]
#' model <- lm(medv ~ lstat + rm + dis + indus, data = df)
#' explainer <- shapr(df1, model)
#'
#' print(nrow(explainer$X))
#' # 16 (which equals 2^4)
#'
#' # Example using approximation
#' y_var <- "medv"
#' x_var <- setdiff(colnames(df), y_var)
#' model <- lm(medv ~ ., data = df)
#' df2 <- df[, x_var]
#' explainer <- shapr(df2, model, n_combinations = 1e3)
#'
#' print(nrow(explainer$X))
#'
#' # Example using approximation where n_combinations > 2^m
#' x_var <- c("lstat", "rm", "dis", "indus")
#' y_var <- "medv"
#' df3 <- df[, x_var]
#' model <- lm(medv ~ lstat + rm + dis + indus, data = df)
#' explainer <- shapr(df1, model, n_combinations = 1e3)
#'
#' print(nrow(explainer$X))
#' # 16 (which equals 2^4)
shapr <- function(x,
                  model,
                  n_combinations = NULL,
                  feature_labels = NULL) {

  # Checks input argument
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Setup
  explainer <- as.list(environment())
  explainer$exact <- ifelse(is.null(n_combinations), TRUE, FALSE)
  explainer$model_type <- model_type(model)

  # Checks input argument
  feature_labels <- features(model, colnames(x), feature_labels)
  explainer$n_features <- length(feature_labels)

  # Converts to data.table, otherwise copy to x_train  --------------
  x_train <- data.table::as.data.table(x)

  # Removes variables that are not included in model   --------------
  cnms_remove <- setdiff(colnames(x), feature_labels)
  if (length(cnms_remove) > 0) x_train[, (cnms_remove) := NULL]
  data.table::setcolorder(x_train, feature_labels)

  # Checks model and features
  explainer$p <- predict_model(model, head(x_train))

  # Get all combinations ----------------
  dt_combinations <- feature_combinations(
    m = explainer$n_features,
    exact = explainer$exact,
    n_combinations = n_combinations,
    weight_zero_m = 10^6
  )

  # Get weighted matrix ----------------
  weighted_mat <- weight_matrix(
    X = dt_combinations,
    normalize_W_weights = TRUE
  )

  ## Get feature matrix ---------
  feature_matrix <- feature_matrix_cpp(
    features = dt_combinations[["features"]],
    m = explainer$n_features
  )

  explainer$S <- feature_matrix
  explainer$W <- weighted_mat
  explainer$X <- dt_combinations
  explainer$x_train <- x_train
  explainer$feature_labels <- feature_labels
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
