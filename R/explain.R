
#' @export
explain_new <- function(x,explainer, approach, prediction_zero,
                        n_samples = 1e3, n_batches = 1, seed = 1, ...){


  explain_setup(x = x,
                explainer = explainer,
                approach = approach,
                prediction_zero = prediction_zero,
                n_samples = n_samples,
                n_batches = n_batches,
                seed = seed, ...)

  # setup <- explain_setup(x=x,
  #                        explainer = explainer,
  #                        approach = approach,
  #                        prediction_zero = prediction_zero,
  #                        n_samples = n_samples,
  #                        n_batches = n_batches, ...)
  #
  # vS_dt <- compute_vS(setup = setup)
  #
  #
  # output <- compute_Shapley(vS_dt = vS_dt,
  #                           setup = setup)
  # return(output)

}

#' @export
explain_setup <- function(x, explainer, approach, prediction_zero,
                          n_samples = 1e3, n_batches = 1, seed = 1, ...) {

  #TODO: May consider only returning the new objects to avoid uncessary copying/overwriting existing object
  #      which is almost identical


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

  # Add arguments to explainer object
  explainer$x_test <- preprocess_data(x, explainer$feature_list)$x_dt
  explainer$approach <- approach
  explainer$prediction_zero <- prediction_zero
  explainer$n_samples <- n_samples
  explainer$n_batches <- n_batches
  explainer$seed <- seed
  explainer$S_batch <- create_S_batch(explainer, n_batches, NULL)

  explainer <- setup_approach(explainer, ...)

}

#' @export
setup_approach <- function(explainer,...){

  this_class <- ""
  # TODO: Currently we ignore combined approaches. Sort out that later (it used to work)

#  if (length(approach) > 1) {
#    class(this_class) <- "combined"
#  }  else {
    class(this_class) <- explainer$approach
#  }

  UseMethod("setup_approach", this_class)

  }

#' @export
setup_approach.independence <- function(explainer,...){
  return(explainer)
}

#' @export
setup_approach.empirical <- function(explainer,
                                     seed = 1,
                                     w_threshold = 0.95,
                                     type = "fixed_sigma",
                                     fixed_sigma_vec = 0.1,
                                     n_samples_aicc = 1000,
                                     eval_max_aicc = 20,
                                     start_aicc = 0.1,
                                     cov_mat = NULL,...){

  # Add arguments to explainer object
  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$n_samples_aicc <- n_samples_aicc
  explainer$eval_max_aicc <- eval_max_aicc
  explainer$start_aicc <- start_aicc
  explainer$w_threshold <- w_threshold

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

  return(explainer)
}

#' @export
setup_approach.gaussian <- function(explainer,
                                    mu = NULL,
                                    cov_mat = NULL, ...){

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

  return(explainer)
}


#' @export
setup_approach.copula <- function(explainer, ...){

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

  return(explainer)
}


#' @export
setup_approach.ctree <- function(explainer,
                                  mincriterion = 0.95,
                                  minsplit = 20,
                                  minbucket = 7,
                                  sample = TRUE, ...){

  # Add arguments to explainer object
  explainer$mincriterion <- mincriterion
  explainer$minsplit <- minsplit
  explainer$minbucket <- minbucket
  explainer$sample <- sample

  return(explainer)
}



#' @export
batchfun <- function(S,explainer){
  dt <- prepare_data(explainer, index_features = S, ...)
  r_batch_i <- prediction(dt, explainer$prediction_zero, explainer)
  r_batch_i$dt_mat[, row_id := S]
  r_batch_i
}



#
# test <- list(a=1,b=2)
#
# a = function(test){
#   test$c = 3
#   this_class <- ""
#   class(this_class) <- "this"
#   UseMethod("jaja", this_class)
# }
#
# jaja.this <- function(x){
#   x$d = 2
#   print(x)
# }
#
#
# aa = function(test){
#   test$c = 3
#   aa_(test)
# }
#
# aa_ <- function(test){
#   this_class <- ""
#   class(this_class) <- "this"
#   UseMethod("jaja", this_class)
# }
#
#
#
# aa(testtest)
#
#
# # Add arguments to explainer object
# explainer$x_test <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
# explainer$approach <- approach
# explainer$n_samples <- n_samples
#

