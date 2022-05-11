
#' @export
postprocess_vS_list <- function(vS_list,explainer,keep_samp_for_vS = FALSE){
  # Appending the zero-prediction to the list
  dt_vS0 <- as.data.table(rbind(c(1,rep(explainer$prediction_zero,nrow(explainer$x_test)))))
  names(dt_vS0) <- c("id_combination",1:nrow(explainer$x_test))

  # Extracting/merging the data tables from the batch running
  #TODO: Need a memory and speed optimized way to transform the output form dt_vS_list to two different lists,
  # I.e. without copying the data more than once. For now I have modified run_batch such that it if keep_samp_for_vS=FALSE
  # then there is only one copy, but there are two if keep_samp_for_vS=TRUE. This might be OK since the latter is used rarely
  if(keep_samp_for_vS){
    vS_list[[length(vS_list)+1]] <- list(dt_vS0,NULL)

    dt_vS <- rbindlist(lapply(vS_list, `[[`, 1))

    dt_samp_for_vS <- rbindlist(lapply(vS_list, `[[`, 2))
    setorder(dt_samp_for_vS,id_combination)

  } else {
    vS_list[[length(vS_list)+1]] <- dt_vS0

    dt_vS <- rbindlist(vS_list)
    dt_samp_for_vS <- NULL
  }

  setorder(dt_vS,id_combination)

  output <- list(dt_vS = dt_vS,
                 dt_samp_for_vS = dt_samp_for_vS)
  return(output)
}

#' @export
finalize_explanation <- function(vS_list,explainer,keep_samp_for_vS = FALSE){

  processed_vS_list <- postprocess_vS_list(vS_list = vS_list,
                                           explainer = explainer,
                                           keep_samp_for_vS = keep_samp_for_vS)

  # Extract the predictions we are explaining
  p <- get_p(processed_vS_list$dt_vS,explainer)

  # Compute the Shapley values
  dt_shapley <- compute_shapley_new(explainer, processed_vS_list$dt_vS)

  output <- list(dt_shapley=dt_shapley,
                 explainer = explainer,
                 p = p,
                 dt_vS = processed_vS_list$dt_vS,
                 dt_samp_for_vS = processed_vS_list$dt_samp_for_vS)
  return(output)
}


#' @export
init_explainer <- function(...){
  list(...)
}


#' @export
create_S_batch_new <- function(explainer, n_batches,seed=NULL){

  if(length(explainer$approach)>1){
    explainer$X[!(n_features %in% c(0,explainer$n_features)),approach:=explainer$approach[n_features]]

    # Finding the number of batches per approach
    batch_count_dt <- explainer$X[!is.na(approach),list(n_batches_per_approach=pmax(1,round(.N/(explainer$n_combinations-2)*n_batches)),
                                                        n_S_per_approach = .N),by=approach]
    batch_count_dt[,n_leftover_first_batch:=n_S_per_approach%%n_batches_per_approach]
    setorder(batch_count_dt,-n_leftover_first_batch)

    approach_vec <- batch_count_dt[,approach]
    n_batch_vec <- batch_count_dt[,n_batches_per_approach]

    # Randomize order before ordering spreading the batches on the different approaches as evenly as possible with respect to shapley_weight
    set.seed(seed)
    explainer$X[,randomorder:=sample(.N)]
    setorder(explainer$X,randomorder) # To avoid smaller id_combinations always proceeding large ones
    setorder(explainer$X,shapley_weight)

    batch_counter <- 0
    for(i in seq_along(approach_vec)){
      explainer$X[approach==approach_vec[i],batch:=ceiling(.I/.N*n_batch_vec[i])+batch_counter]
      batch_counter <- explainer$X[approach==approach_vec[i],max(batch)]
    }
  } else {
    explainer$X[!(n_features %in% c(0,explainer$n_features)),approach:=explainer$approach]

    # Sprading the batches
    set.seed(seed)
    explainer$X[,randomorder:=sample(.N)]
    setorder(explainer$X,randomorder)
    setorder(explainer$X,shapley_weight)
    explainer$X[!(n_features %in% c(0,explainer$n_features)),batch:=ceiling(.I/.N*n_batches)]

  }

  # Assigning batch 1 (which always is the smallest) to the full prediction.
  explainer$X[,randomorder:=NULL]
  explainer$X[id_combination==max(id_combination),batch:=1]
  setkey(explainer$X,id_combination)

  # Create a list of the batch splits
  S_groups <- split(explainer$X[id_combination!=1,id_combination],explainer$X[id_combination!=1,batch])

  return(S_groups)
}


#' @export
compute_vS <- function(S,explainer){
  dt <- batch_prepare_vS(S,explainer)
  batch_get_preds(dt,S,explainer)
}

#' @export
batch_prepare_vS <- function(S,explainer){

  max_id_combination <- explainer$n_combinations

  # TODO: Check what is the fastest approach to deal with the last observation.
  # Not doing this for the largest id combination (should check if this is faster or slower, actually)
  # An alternative would be to delete rows from the dt which is provided by prepare_data.
  if(!(max_id_combination %in% S)){
    dt <- prepare_data(explainer, index_features = S)
  } else {
    S <- S[S!=max_id_combination]
    dt <- prepare_data(explainer, index_features = S)
    dt_max <- data.table(explainer$x_test,id_combination=max_id_combination,w=1,id=seq_len(nrow(explainer$x_test)))
    dt <- rbind(dt,dt_max)
    setkey(dt,id,id_combination)
  }
}

#' @export
batch_get_preds <- function(dt,S,explainer){
  compute_preds(dt, explainer)
  r_batch_i$dt_mat[, row_id := S]
  r_batch_i
}

get_p <- function(dt_vS,explainer){

  max_id_combination <- explainer$n_combinations
  p <- unlist(dt_vS[id_combination==max_id_combination,][,id_combination:=NULL])

  p

}

compute_preds <- function(dt, explainer) {

  id_combination <- p_hat <- NULL # due to NSE notes in R CMD check

  # Setup
  feature_names <- explainer$feature_list$labels

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
run_batch <- function(S,explainer,keep_samp_for_vS){
  dt <- batch_prepare_vS(S = S,explainer = explainer) # Make it optional to store and return the dt_list
  compute_preds(dt,explainer) # Updating dt by reference

  dt_vS <- compute_MCint(dt,explainer)

  if(keep_samp_for_vS){
    return(list(dt_vS = dt_vS,dt_samp_for_vS=dt))
  } else {
    return(dt_vS = dt_vS)
  }
}


#' @export
explain_new <- function(x,explainer, approach, prediction_zero,
                        n_samples = 1e3, n_batches = 1, seed = 1, keep_samp_for_vS = FALSE, ...){

  # Adding setups the explainer object
  explainer <- explain_setup(x = x,
                             explainer = explainer,
                             approach = approach,
                             prediction_zero = prediction_zero,
                             n_samples = n_samples,
                             n_batches = n_batches,
                             seed = seed, ...)

  # Accross all batches get the data we will predict on, predict on them, and do the MC integration
  vS_list <- future.apply::future_lapply(X = explainer$S_batch,
                                                FUN = run_batch,
                                                explainer = explainer,
                                                keep_samp_for_vS = keep_samp_for_vS,
                                                future.seed = explainer$seed)
  #alternative using for loop
  # batch_out_list <- list()
  # for(i in seq_along(explainer$S_batch)){
  #   S <- explainer$S_batch[[i]]
  #   dt <- batch_prepare_vS(S = S,explainer = explainer) # Make it optional to store and return the dt_list
  #   compute_preds(dt,explainer) # Updating dt by reference
  #
  #   dt_vS <- compute_MCint(dt,explainer)
  #
  #   if(keep_samp_for_vS){
  #     batch_out_list[[i]] <- list(dt_vS = dt_vS,dt_samp_for_vS=dt)
  #   } else {
  #     batch_out_list[[i]] <- dt_vS
  #   }
  # }

  output <- finalize_explanation(vS_list = vS_list,
                                 explainer = explainer,
                                 keep_samp_for_vS = keep_samp_for_vS)

  return(output)

}

#' Compute shapley values
#' @param explainer An \code{explain} object.
#' @param dt_vS The contribution matrix.
#' @return A \code{data.table} with shapley values for each test observation.
#' @export
#' @keywords internal
compute_shapley_new <- function(explainer, dt_vS) {

  if (!explainer$is_groupwise) {
    shap_names <- explainer$feature_list$labels
  } else {
    shap_names <- names(explainer$group)
  }


  kshap <- t(explainer$W %*% as.matrix(dt_vS[,-"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", shap_names)

  return(dt_kshap)

}


#' @export
explain_setup <- function(x, explainer, approach, prediction_zero,
                          n_samples = 1e3, n_batches = 1, seed = 1, ...) {

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
  explainer$S_batch <- create_S_batch_new(explainer, n_batches)

  explainer <- setup_approach(explainer, ...)

}

#' @export
get_cov_mat <- function(x_train,min_eigen_value = 1e-06){
  cov_mat <- stats::cov(x_train)
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= min_eigen_value)) {
    cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  }
  return(cov_mat)
}

#' @export
get_mu_vec <- function(x_train,min_eigen_value = 1e-06){
  unname(colMeans(x_train))
}


#' @export
setup_approach <- function(explainer,...){

  this_class <- ""
  # TODO: Currently we ignore combined approaches. Sort out that later (it used to work)

  if (length(explainer$approach) > 1) {
    class(this_class) <- "combined"
  }  else {
    class(this_class) <- explainer$approach
  }

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
    explainer$cov_mat <- get_cov_mat(explainer$x_train)
  }

  return(explainer)
}

#' @export
setup_approach.gaussian <- function(explainer,
                                    mu = NULL,
                                    cov_mat = NULL, ...){

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    explainer$mu <- get_mu_vec(explainer$x_train)
  } else {
    explainer$mu <- mu
  }

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    explainer$cov_mat <- get_cov_mat(explainer$x_train)
  } else {
    explainer$cov_mat <- cov_mat
  }

  return(explainer)
}


#' @export
setup_approach.copula <- function(explainer, ...){

  # Prepare transformed data

  explainer$mu <- rep(0, ncol(explainer$x_train))
  x_train <- apply(
    X = explainer$x_train,
    MARGIN = 2,
    FUN = gaussian_transform
  )
  explainer$cov_mat <- get_cov_mat(x_train)


  x_test_gaussian <- apply(
    X = rbind(explainer$x_test, explainer$x_train),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(explainer$x_test)
  )

  if (is.null(dim(x_test_gaussian))) {
    x_test_gaussian <- t(as.matrix(x_test_gaussian))
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

