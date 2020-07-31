#' Explain the output of machine learning models with more accurately estimated Shapley values
#'
#' @param x A matrix or data.frame. Contains the the features, whose
#' predictions ought to be explained (test data).
#'
#' @param explainer An \code{explainer} object to use for explaining the observations.
#' See \code{\link{shapr}}.
#'
#' @param approach Character vector of length \code{1} or \code{n_features}.
#' \code{n_features} equals the total number of features in the model. All elements should
#' either be \code{"gaussian"}, \code{"copula"}, \code{"empirical"}, or \code{"ctree"}. See details for more
#' information.
#'
#' @param prediction_zero Numeric. The prediction value for unseen data, typically equal to the mean of
#' the response.
#'
#' @param ... Additional arguments passed to \code{\link{prepare_data}}
#'
#' @details The most important thing to notice is that \code{shapr} has implemented three different
#' approaches for estimating the conditional distributions of the data, namely \code{"empirical"},
#' \code{"gaussian"} and \code{"copula"}.
#'
#' In addition to this the user will also have the option of combining the three approaches.
#' E.g. if you're in a situation where you have trained a model the consists of 10 features,
#' and you'd like to use the \code{"gaussian"} approach when you condition on a single feature,
#' the \code{"empirical"} approach if you condition on 2-5 features, and \code{"copula"} version
#' if you condition on more than 5 features this can be done by simply passing
#' \code{approach = c("gaussian", rep("empirical", 4), rep("copula", 5))}. If
#' \code{"approach[i]" = "gaussian"} it means that you'd like to use the \code{"gaussian"} approach
#' when conditioning on \code{i} features.
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
#' @author Camilla Lingjaerde, Nikolai Sellereite
#'
#' @examples
#' # Load example data
#' data("Boston", package = "MASS")
#'
#' # Split data into test- and training data
#' x_train <- head(Boston, -3)
#' x_test <- tail(Boston, 3)
#'
#' # Fit a linear model
#' model <- lm(medv ~ lstat + rm + dis + indus, data = x_train)
#'
#' # Create an explainer object
#' explainer <- shapr(x_train, model)
#'
#' # Explain predictions
#' p <- mean(x_train$medv)
#'
#' # Empirical approach
#' explain1 <- explain(x_test, explainer, approach = "empirical", prediction_zero = p, n_samples = 1e2)
#'
#' # Gaussian approach
#' explain2 <- explain(x_test, explainer, approach = "gaussian", prediction_zero = p, n_samples = 1e2)
#'
#' # Gaussian copula approach
#' explain3 <- explain(x_test, explainer, approach = "copula", prediction_zero = p, n_samples = 1e2)
#'
#' # ctree approach
#' explain4 <- explain(x_test, explainer, approach = "ctree", prediction_zero = p)
#'
#' # Combined approach
#' approach <- c("gaussian", "gaussian", "empirical", "empirical")
#' explain5 <- explain(x_test, explainer, approach = approach, prediction_zero = p, n_samples = 1e2)
#'
#' # Plot the results
#' \dontrun{
#' plot(explain1)
#' }
explain <- function(x, explainer, approach, prediction_zero, ...) {
  extras <- list(...)

  # Check input for x
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Check input for approach
  if (!(is.vector(approach) &&
    is.atomic(approach) &&
    (length(approach) == 1 | length(approach) == length(explainer$feature_labels)) &&
    all(is.element(approach, c("empirical", "gaussian", "copula", "ctree", "categorical"))))
  ) {
    stop(
      paste(
        "It seems that you passed a non-valid value for approach.",
        "It should be either 'empirical', 'gaussian', 'copula', 'ctree', 'categorical', or",
        "a vector of length=ncol(x) with only the above characters."
      )
    )
  }

  # Check that x contains correct variables
  if (!all(explainer$feature_labels %in% colnames(x))) {
    stop(
      paste0(
        "\nThe test data, x, does not contain all features necessary for\n",
        "generating predictions. Please modify x so that all labels given\n",
        "by explainer$feature_labels is present in colnames(x)."
      )
    )
  }

  if (length(approach) > 1) {
    class(x) <- "combined"
  } else if (length(extras$mincriterion) > 1) {
    class(x) <- "combinedparameters"
  } else {
    class(x) <- approach
  }

  UseMethod("explain", x)
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
#' @param w_threshold Positive integer between 0 and 1.
#'
#' @rdname explain
#'
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero,
                              type = "fixed_sigma", fixed_sigma_vec = 0.1,
                              n_samples_aicc = 1000, eval_max_aicc = 20,
                              start_aicc = 0.1, w_threshold = 0.95, ...) {

  # Add arguments to explainer object
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)
  explainer$approach <- approach
  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$n_samples_aicc <- n_samples_aicc
  explainer$eval_max_aicc <- eval_max_aicc
  explainer$start_aicc <- start_aicc
  explainer$w_threshold <- w_threshold

  # Generate data
  dt <- prepare_data(explainer, ...)
  if (!is.null(explainer$return)) {
    return(dt)
  }

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

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
explain.gaussian <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, ...) {

  # Add arguments to explainer object
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)
  explainer$approach <- approach

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

  # Generate data
  dt <- prepare_data(explainer, ...)
  if (!is.null(explainer$return)) {
    return(dt)
  }

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

  return(r)
}

#' @rdname explain
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, ...) {

  # Setup
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)
  explainer$approach <- approach

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
  # Generate data
  dt <- prepare_data(explainer, x_test_gaussian = x_test_gaussian, ...)
  if (!is.null(explainer$return)) {
    return(dt)
  }

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

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
#' @param sample Boolean. If true, then method samples from the terminal node in the tree. If false, then just takes all
#'  observations in the node.
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.ctree <- function(x, explainer, approach, prediction_zero,
                          mincriterion = 0.95, minsplit = 20,
                          minbucket = 7, sample = TRUE, ...) {
  # Checks input argument
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Add arguments to explainer object
  explainer$x_test <- explainer_x_test_dt(x, explainer$feature_labels)
  explainer$approach <- approach
  explainer$mincriterion <- mincriterion
  explainer$minsplit <- minsplit
  explainer$minbucket <- minbucket
  explainer$sample <- sample

  # Generate data
  dt <- prepare_data(explainer, ...)

  if (!is.null(explainer$return)) {
    return(dt)
  } # when using a combined method, you return here

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

  return(r)
}

#' @rdname explain
#' @name explain
#'
#' @export
explain.combined <- function(x, explainer, approach, prediction_zero,
                             mu = NULL, cov_mat = NULL, ...) {
  # Get indices of combinations
  l <- get_list_approaches(explainer$X$n_features, approach)
  explainer$return <- TRUE
  explainer$x_test <- explainer_x_test(x, explainer$feature_labels)

  dt_l <- list()
  for (i in seq_along(l)) {
    dt_l[[i]] <- explain(x, explainer, approach = names(l)[i],
                         prediction_zero, index_features = l[[i]], ...)
  }
  dt <- data.table::rbindlist(dt_l, use.names = TRUE)

  r <- prediction(dt, prediction_zero, explainer)

  return(r)
}



#' @param prob_dt Data.table of probabilities (Optional) of the data generating distribution.
#' If \code{NULL} the probabilities/frequencies are are estimated from the data. Note that this is only used
#' when \code{approach = "ctree"}.
#'
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.categorical <- function(x, explainer, approach, prediction_zero, joint_prob_dt, ...) {

  # Add arguments to explainer object
  explainer$x_test <- explainer_x_test_dt(x, explainer$feature_labels)
  explainer$approach <- approach
  explainer$joint_prob_dt <- joint_prob_dt

  # Generate data
  dt <- prepare_data(explainer, ...)

  if (!is.null(explainer$return)) {
    return(dt)
  }
  #      id_combination  marg_prob joint_prob id cond_prob feat_1_ feat_2_ feat_3_ feat_1_conditioned feat_2_conditioned feat_3_conditioned
  #   1:              1 1.00000000 1.00000000  1         1       1       1       1                 NA                 NA                 NA
  #   2:              1 1.00000000 1.00000000  2         1       1       1       2                 NA                 NA                 NA
  #   3:              1 1.00000000 1.00000000  3         1       1       1       3                 NA                 NA                 NA
  #   4:              1 1.00000000 1.00000000  4         1       1       2       1                 NA                 NA                 NA
  #   5:              1 1.00000000 1.00000000  5         1       1       2       2                 NA                 NA                 NA
  # ---
  # 212:              8 0.02552940 0.02552940 23         1       3       2       2                  3                  2                  2
  # 213:              8 0.01783726 0.01783726 24         1       3       2       3                  3                  2                  3
  # 214:              8 0.00984454 0.00984454 25         1       3       3       1                  3                  3                  1
  # 215:              8 0.01784443 0.01784443 26         1       3       3       2                  3                  3                  2
  # 216:              8 0.01777305 0.01777305 27         1       3       3       3                  3                  3                  3

  # Predict
  r <- prediction(dt, prediction_zero, explainer)

  return(r)
}



#' Function to extract the column number of the conditional expectation matrix as a function of
#' all the possible x_test values
#'
#' @description
#'
#' @param tbl Data.table. Consists of all possible x_test values.
#' @param S_dt ??
#' @return list of column numbers
#'
#' @export

col_fun <- function(tbl, S_dt){
  dim <- ncol(tbl)
  v <- tbl[, 1:dim]
  v_S <- data.table(ifelse(is.na(v), 0, 1))
  colnum <- S_dt[v_S, .(id), on = names(v_S)]
  return(colnum)
}


#' Function to calculate conditional expectations of the cutoff jointly Normal random variables
#' for the x_test observations. I.e. doing what cond_expec + extract_cond_expec does together,
#' just much faster.
#'
#' @description
#'
#' @param cond_list List. Calculated using the \code{cond_prob} function.
#' @param explainer explainer object from \code{shapr} package.
#' @param x_test Matrix. Consists of all the test observations. Has the same dimension
#' as the number of joint Normal random variables calculated in \code{sim_true_Normal} function.
#' @param cond_expec_dt data.table. Calculated using the \code{cond_expec} function.
#' @param prediction_zero Numeric. Number to assigned to phi_0 in Shapley framework.
#' @param joint_prob_dt data.table The first element in the list calculated using the \code{sim_true_Normal} function.
#'
#' @return data.table
#'
#' @export

cond_expec_new <- function(cond_list, explainer, x_test, prediction_zero, joint_prob_dt){

  feat_names <- colnames(explainer$x_train)
  dim <- length(feat_names)

  S_dt <- data.table(explainer$S)
  S_dt[, id := 0:(nrow(S_dt) - 1)]
  setnames(S_dt, c(feat_names, "id"))

  mat <- unique(x_test)
  mat <- mat[, lapply(.SD, as.factor), .SDcol = feat_names] # To be removed later
  mat[, rowid := .I] # Adding identifyer to match on
  # mat <- joint_prob_dt[mat,.(rowid,feat_comb_id), on=feat_names]


  cond_expec_list <- list()
  cond_expec_list[[1]] <- NULL

  joint_prob_dt[, predict := predict_model(explainer$model, newdata = .SD), .SDcols = feat_names]

  setkey(joint_prob_dt, feat_comb_id)

  tmp <- list()
  tmp0 <- NULL
  for(i in 2:nrow(explainer$S)){
    col_names <- feat_names[as.logical(explainer$S[i, ])]
    these_cols <- c(col_names,"feat_comb_id", "predict")
    tmp0 <- merge(cond_list[[i]], joint_prob_dt[, ..these_cols], by = "feat_comb_id") # Need the whole thing here
    tmp0[, expected_value := predict * cond_prob]
    cond_expec_list[[i]] <- tmp0[, list(cond_expec=sum(expected_value)), by = col_names]
    tmp[[i]] <- cbind(cond_expec_list[[i]][mat, .(rowid, cond_expec), on = col_names, allow.cartesian = TRUE],
                      colnum = i - 1)
  }
  tmp_dt <- rbindlist(tmp, use.names = T)

  final_dt <- dcast(tmp_dt, formula = "rowid~colnum", value.var = "cond_expec")
  x_test_id <- mat[x_test, on = feat_names]
  S_char_vec <- as.character(1:(nrow(explainer$S) - 1))
  final_dt_x_test <- cbind("0" = prediction_zero, final_dt[x_test_id, ..S_char_vec,on = "rowid"])

  return(final_dt_x_test)
}


#' Function to calculate the true Shapley values based on the conditional expectations calculated using \code{cond_expec}
#'
#' @description
#'
#' @param explainer explainer object from shapr package.
#' @param cond_expec_mat list. Calculated using \code{cond_expec_new} function.
#' @param x_test vector of test observations. Has the same dimension as the number of joint Normal random variables calculated in \code{sim_true_Normal} function.
#'
#' @return vector of Shapley values.
#'
#' @export

true_Kshap <- function(explainer, cond_expec_mat, x_test){
  dim <- ncol(x_test)
  Kshap <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer$W))
  for (i in 1:nrow(x_test)) {
    Kshap[i, ] = explainer$W %*% t(as.matrix(cond_expec_mat[i, ]))
  }
  Kshap <- data.table(Kshap)
  setnames(Kshap, 1:(dim + 1), c("none", names(x_test)))

  return(Kshap)
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
#' @examples
#' m <- 3
#' n_features <- c(0, 1, 1, 1, 2, 2, 2, 3)
#' approach <- c("gaussian", "copula", "copula")
#' l <- shapr:::get_list_approaches(n_features, approach)
#' str(l)
get_list_approaches <- function(n_features, approach) {
  l <- list()
  approach[length(approach)] <- approach[length(approach) - 1]

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

#' @keywords internal
explainer_x_test <- function(x_test, feature_labels) {

  # Remove variables that were not used for training
  x <- data.table::as.data.table(x_test)
  cnms_remove <- setdiff(colnames(x), feature_labels)
  if (length(cnms_remove) > 0) x[, (cnms_remove) := NULL]
  data.table::setcolorder(x, feature_labels)

  return(as.matrix(x))
}

#' @keywords internal
explainer_x_test_dt <- function(x_test, feature_labels) {

  # Remove variables that were not used for training
  # Same as explainer_x_test() but doesn't convert to a matrix
  # Useful for ctree method which sometimes takes categorical features
  x <- data.table::as.data.table(x_test)
  cnms_remove <- setdiff(colnames(x), feature_labels)
  if (length(cnms_remove) > 0) x[, (cnms_remove) := NULL]
  data.table::setcolorder(x, feature_labels)

  return(x)
}


#' @rdname explain
#' @name explain
#'
#' @export
explain.combinedparameters <- function(x, explainer, approach,
                                       prediction_zero, mincriterion, ...) {

  # Get indices of combinations
  l <- get_list_parameters(explainer$X$n_features, mincriterion)
  explainer$return <- TRUE # this is important so that you don't use prediction() twice
  explainer$x_test <- as.matrix(x)

  dt_l <- list()
  for (i in seq_along(l)) {
    dt_l[[i]] <- explain(x, explainer, approach, prediction_zero,
                         index_features = l[[i]],
                         mincriterion = as.numeric(names(l[i])), ...)
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE)

  r <- prediction(dt, prediction_zero, explainer)
  return(r)
}

#' @keywords internal
get_list_parameters <- function(n_features, mincriterion) {
  l <- list()

  for (k in 1:length(unique(mincriterion))) {
    x <- which(mincriterion == unique(mincriterion)[k])
    nn <- as.character(unique(mincriterion)[k])
    if (length(l) == 0) x <- c(0, x)
    l[[nn]] <- which(n_features %in% x)
  }
  return(l)
}

#' @keywords internal
explainer_x_test <- function(x_test, feature_labels) {

  # Remove variables that were not used for training
  x <- data.table::as.data.table(x_test)
  cnms_remove <- setdiff(colnames(x), feature_labels)
  if (length(cnms_remove) > 0) x[, (cnms_remove) := NULL]
  data.table::setcolorder(x, feature_labels)

  return(as.matrix(x))
}
