
#' @export
compute_vS <- function(S,explainer){
  dt <- batch_prepare_vS(S,explainer)
  batch_get_preds(dt,S,explainer)
}

#' @export
batch_prepare_vS <- function(S,explainer){
  dt <- prepare_data(explainer, index_features = S) # TODO: Modify prepare_data such that it gives does not give copies of the same observation
}

#' @export
batch_get_preds <- function(dt,S,explainer){
  compute_preds(dt, explainer)
  r_batch_i$dt_mat[, row_id := S]
  r_batch_i
}

get_p <- function(dt_vS,explainer){# TODO: Add max_id_comb to explainer to be extracted and used here

  max_id_combination <- explainer$X[,max(id_combination)]
  p <- unlist(dt_vS[id_combination==max_id_combination,][,id_combination:=NULL])

  p

}

compute_preds <- function(dt, explainer) {

  id_combination <- p_hat <- NULL # due to NSE notes in R CMD check

  # Setup
  feature_names <- colnames(explainer$x_test)

  # Predictions
  if (!all(dt[, unique(id_combination)] == 1)) { # Avoid warnings when predicting with empty newdata
    dt[id_combination != 1, p_hat := predict_model(explainer$model, newdata = .SD), .SDcols = feature_names]
  }
  dt[id_combination == 1, p_hat := explainer$prediction_zero]

  return(dt)
}

compute_MCint <- function(dt, explainer) {

  w <- k <- p_hat <- NULL # due to NSE notes in R CMD check

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination)]
  data.table::setkeyv(dt_res, c("id", "id_combination"))
  dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = "k")
  #dt_mat[, id_combination := NULL]

  dt_mat
}

#' @export
explain_new <- function(x,explainer, approach, prediction_zero,
                        n_samples = 1e3, n_batches = 1, seed = 1, keep_samp_for_vS = FALSE, ...){


  explainer <- explain_setup(x = x,
                             explainer = explainer,
                             approach = approach,
                             prediction_zero = prediction_zero,
                             n_samples = n_samples,
                             n_batches = n_batches,
                             seed = seed, ...)

# Starting off with a loop instead of a lapply call
  dt_vS_list <- list()
  if(keep_samp_for_vS){
    dt_samp_for_vS_list <- list()
  }
  for(i in seq_along(explainer$S_batch)){

    S <- explainer$S_batch[[i]]
    dt <- batch_prepare_vS(S = S,explainer = explainer) # Make it optional to store and return the dt_list
    compute_preds(dt,explainer) # Updating dt by reference

    dt_vS_list[[i]] <- compute_MCint(dt,explainer)
    if(keep_samp_for_vS){
      dt_samp_for_vS_list[[i]] <- dt
    }
  }

  dt_vS <- rbindlist(dt_vS_list)

  if(keep_samp_for_vS){
    dt_samp_for_vS <- rbindlist(dt_samp_for_vS_list)
  }

  p <- get_p(dt_vS,explainer) ### CONT HERE ###

  #r_batch <- future.apply::future_lapply(X = explainer$S_batch,
  #                                       FUN = compute_vS,
  #                                       explainer = explainer,
  #                                       future.seed = explainer$seed)


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
  explainer$S_batch <- create_S_batch(explainer, n_batches, NULL) # TODO: Modify create_S_batch such that id_combination=1 is kept outside (and handled elsewhere)

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




#
# #  # Check that the number of test observations equals max(id)
# #  stopifnot(nrow(explainer$x_test) == dt[, max(id)])
# data.table::setkeyv(dt, c("id", "id_combination"))
#
#   # Reducing the prediction data.table
#   max_id_combination <- nrow(explainer$S)
#   V1 <- keep <- NULL # due to NSE notes in R CMD check
#   dt[, keep := TRUE]
#   first_element <- dt[, tail(.I, 1), .(id, id_combination)][id_combination %in% c(1, max_id_combination), V1]
#   dt[id_combination %in% c(1, max_id_combination), keep := FALSE]
#   dt[first_element, c("keep", "w") := list(TRUE, 1.0)]
#   dt <- dt[keep == TRUE][, keep := NULL]
#
#
#   if (dt[, max(id_combination)] < max_id_combination) {
#     p_all <- NULL
#   } else {
#     p_all <- dt[id_combination == max_id_combination, p_hat]
#     names(p_all) <- 1:nrow(explainer$x_test)
#   }
#
#
#
#   return(r)
# }
#


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

