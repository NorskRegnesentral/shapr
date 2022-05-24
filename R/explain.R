
#' @export
postprocess_vS_list <- function(vS_list,internal){


  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
  prediction_zero <- internal$parameters$prediction_zero
  n_explain <- internal$parameters$n_explain

  # Appending the zero-prediction to the list
  dt_vS0 <- as.data.table(rbind(c(1,rep(prediction_zero,n_explain))))
  names(dt_vS0) <- c("id_combination",seq_len(n_explain))

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
finalize_explanation <- function(vS_list,internal){

  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS

  processed_vS_list <- postprocess_vS_list(vS_list = vS_list,
                                           internal = internal)

  # Extract the predictions we are explaining
  p <- get_p(processed_vS_list$dt_vS,internal)

  # Compute the Shapley values
  dt_shapley <- compute_shapley_new(internal, processed_vS_list$dt_vS)


  # TODO: Consider adding some of the output here to internal as well
  output <- list(dt_shapley=dt_shapley,
                 internal = internal,
                 p = p,
                 dt_vS = processed_vS_list$dt_vS,
                 dt_samp_for_vS = processed_vS_list$dt_samp_for_vS)
  attr(output, "class") <- c("shapr", "list")


  return(output)
}


#' @export
init_explainer <- function(...){
  list(...)
}


#' @export
create_S_batch_new <- function(internal,seed=NULL){


  n_features0 <- internal$parameters$n_features
  approach0 <- internal$parameters$approach
  n_combinations <- internal$parameters$n_combinations
  n_batches <- internal$parameters$n_batches

  X <- internal$objects$X


  if(length(approach0)>1){
    X[!(n_features %in% c(0,n_features0)),approach:=approach0[n_features]]

    # Finding the number of batches per approach
    batch_count_dt <- X[!is.na(approach),list(n_batches_per_approach=pmax(1,round(.N/(n_combinations-2)*n_batches)),
                                                        n_S_per_approach = .N),by=approach]
    batch_count_dt[,n_leftover_first_batch:=n_S_per_approach%%n_batches_per_approach]
    setorder(batch_count_dt,-n_leftover_first_batch)

    approach_vec <- batch_count_dt[,approach]
    n_batch_vec <- batch_count_dt[,n_batches_per_approach]

    # Randomize order before ordering spreading the batches on the different approaches as evenly as possible with respect to shapley_weight
    set.seed(seed)
    X[,randomorder:=sample(.N)]
    setorder(X,randomorder) # To avoid smaller id_combinations always proceeding large ones
    setorder(X,shapley_weight)

    batch_counter <- 0
    for(i in seq_along(approach_vec)){
      X[approach==approach_vec[i],batch:=ceiling(.I/.N*n_batch_vec[i])+batch_counter]
      batch_counter <- X[approach==approach_vec[i],max(batch)]
    }
  } else {
    X[!(n_features %in% c(0,n_features0)),approach:=approach0]

    # Spreading the batches
    set.seed(seed)
    X[,randomorder:=sample(.N)]
    setorder(X,randomorder)
    setorder(X,shapley_weight)
    X[!(n_features %in% c(0,n_features0)),batch:=ceiling(.I/.N*n_batches)]

  }

  # Assigning batch 1 (which always is the smallest) to the full prediction.
  X[,randomorder:=NULL]
  X[id_combination==max(id_combination),batch:=1]
  setkey(X,id_combination)

  # Create a list of the batch splits
  S_groups <- split(X[id_combination!=1,id_combination],X[id_combination!=1,batch])

  return(S_groups)
}


#' @export
compute_vS <- function(S,explainer){
  dt <- batch_prepare_vS(S,explainer)
  batch_get_preds(dt,S,explainer)
}

#' @export
batch_prepare_vS <- function(S,internal){

  max_id_combination <- internal$parameters$n_combinations
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain

  # TODO: Check what is the fastest approach to deal with the last observation.
  # Not doing this for the largest id combination (should check if this is faster or slower, actually)
  # An alternative would be to delete rows from the dt which is provided by prepare_data.
  if(!(max_id_combination %in% S)){
    dt <- prepare_data(internal, index_features = S) #TODO: Need to handle the need for model for the AIC-versions here (skip for Python)
  } else {
    S <- S[S!=max_id_combination]
    dt <- prepare_data(internal, index_features = S)
    dt_max <- data.table(x_explain,id_combination=max_id_combination,w=1,id=seq_len(n_explain))
    dt <- rbind(dt,dt_max)
    setkey(dt,id,id_combination)
  }
  return(dt)
}

#' @export
batch_get_preds <- function(dt,S,explainer){
  compute_preds(dt, explainer)
  r_batch_i$dt_mat[, row_id := S]
  r_batch_i
}

get_p <- function(dt_vS,internal){

  max_id_combination <- internal$parameters$n_combinations
  p <- unlist(dt_vS[id_combination==max_id_combination,][,id_combination:=NULL])

  p

}

compute_preds <- function(dt, internal,model) {

  id_combination <- p_hat <- NULL # due to NSE notes in R CMD check

  # Setup
  feature_names <- internal$parameters$feature_list$labels
  prediction_zero <- internal$parameters$prediction_zero

  # Predictions
  if (!all(dt[, unique(id_combination)] == 1)) { # Avoid warnings when predicting with empty newdata
    dt[id_combination != 1, p_hat := predict_model(model, newdata = .SD), .SDcols = feature_names]
  }
  dt[id_combination == 1, p_hat := prediction_zero]

  return(dt)
}

compute_MCint <- function(dt) {

  w <- k <- p_hat <- NULL # due to NSE notes in R CMD check

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination)]
  data.table::setkeyv(dt_res, c("id", "id_combination"))
  dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = "k")
  #dt_mat[, id_combination := NULL]

  dt_mat
}

#' @export
run_batch <- function(S,internal,model){

  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS

  dt <- batch_prepare_vS(S = S,internal = internal) # Make it optional to store and return the dt_list
  compute_preds(dt,internal,model) # Updating dt by reference

  dt_vS <- compute_MCint(dt)

  if(keep_samp_for_vS){
    return(list(dt_vS = dt_vS,dt_samp_for_vS=dt))
  } else {
    return(dt_vS = dt_vS)
  }
}

#' @export
process_all_data <- function(internal,model){

  # Extracting model specs from model
  if(internal$parameters$ignore_model){
    model <- NULL
    feature_list_model <- get_model_specs.default("")
  } else {
    feature_list_model <- get_model_specs(model)
  }

  # process x_train
  processed_list <- preprocess_data(
    x = internal$data$x_train,
    feature_list = feature_list_model
  )
  internal$data$x_train <- processed_list$x_dt
  internal$parameters$feature_list <- processed_list$updated_feature_list

  # process x_explain
  internal$data$x_explain <- preprocess_data(internal$data$x_explain, internal$parameters$feature_list)$x_dt

  # get number of features and observationst to explain
  internal$parameters$n_features <- ncol(internal$data$x_explain)
  internal$parameters$n_explain <-  nrow(internal$data$x_explain)
  internal$parameters$n_train <-  nrow(internal$data$x_train)


  # Processes groups if specified. Otherwise do nothing
  internal$parameters$is_groupwise <- !is.null(internal$parameters$group)
  if (internal$parameters$is_groupwise) {
    group_list <- process_groups(
      group = internal$parameters$group,
      feature_labels = internal$parameters$feature_list$labels
    )
    internal$parameters$group <- group_list$group
    internal$parameters$group_num <- group_list$group_num
  }
  return(internal)
}

#' @export
test_model <- function(x,model){
  tmp <- predict_model(model, x)
  if (!(all(is.numeric(tmp)) & length(tmp) == 2)) {
    stop(
      paste0(
        "The predict_model function of class ", class(model), " is invalid.\n",
        "See the 'Advanced usage' section of the vignette:\n",
        "vignette('understanding_shapr', package = 'shapr')\n",
        "for more information on running shapr with custom models.\n"
      )
    )
  }
}



#' @export
shapley_setup <- function(internal){

  exact <- internal$parameters$exact
  n_features <- internal$parameters$n_features
  n_combinations <- internal$parameters$n_combinations
  group_num <- internal$parameters$group_num
  is_groupwise <- internal$parameters$is_groupwise


  X <- internal$objects$X

  # Get all combinations ----------------
  X <- feature_combinations(
    m = n_features,
    exact = exact,
    n_combinations = n_combinations,
    weight_zero_m = 10^6,
    group_num = group_num
  )

  # Get weighted matrix ----------------
  W <- weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = is_groupwise
  )

  ## Get feature matrix ---------
  S <- feature_matrix_cpp(
    features = X[["features"]],
    m = n_features
  )

  #### Updating parameters ####

  # Updating parameters$exact as done in feature_combinations
  if (!exact && n_combinations > (2^n_features - 2)) {
    internal$parameters$exact <- TRUE
  }

  internal$parameters$n_combinations <- nrow(S) # Updating this parameter in the end based on what is actually used. This will be obsolete later
  internal$parameters$group_num <- NULL # TODO: Checking whether I could just do this processing where needed instead of storing it

  internal$objects <- list(X = X, W = W, S = S)

  internal$objects$S_batch <- create_S_batch_new(internal)


  return(internal)
}

#' @export
get_supported_approaches <- function(){
  substring(rownames(attr(methods(prepare_data),"info")),first = 14)
}

#' @export
get_parameters <- function(approach,prediction_zero,n_combinations,group,n_samples,n_batches,seed,keep_samp_for_vS,...){
  parameters <- list(approach = approach,
                     prediction_zero = prediction_zero,
                     n_combinations = n_combinations,
                     group = group,
                     n_samples = n_samples,
                     n_batches = n_batches,
                     seed = seed,
                     keep_samp_for_vS = keep_samp_for_vS)
  parameters <- append(parameters,list(...))
  if(is.null(parameters$ignore_model)){
    parameters$ignore_model <- FALSE
  }
  parameters$exact <- ifelse(is.null(parameters$n_combinations), TRUE, FALSE)

  # TODO: Add any additional internal parameters here
  # TODO: Add testing of correct format for the input here

  #if (n_batches < 1 || n_batches > nrow(explainer$S)) {
  #  stop("`n_batches` is smaller than 1 or greater than the number of rows in explainer$S.")
  #}


  return(parameters)
}

#' @export
get_data <- function(x_train,x_explain){

  # Check format for x_train
  if (!is.matrix(x_train) & !is.data.frame(x_train)) {
    stop("x_train should be a matrix or a dataframe.")
  }
  # Check format of x_explain
  if (!is.matrix(x_explain) & !is.data.frame(x_explain)) {
    stop("x should be a matrix or a data.frame/data.table.")
  }

  data <- list(x_train = x_train,
               x_explain = x_explain)
}

#' @export
check_approach <- function(internal){
  # Check input for approach

  approach <- internal$parameters$approach
  n_features <- internal$parameters$n_features
  if (!(is.vector(approach) &&
        is.atomic(approach) &&
        (length(approach) == 1 | length(approach) == n_features) &&
        all(is.element(approach, get_supported_approaches())))
  ) {
    stop(
      paste(
        "It seems that you passed a non-valid value for approach.",
        "It should be either \n",paste0(get_supported_approaches(),collapse=", "),"\nor",
        "a vector of length=ncol(x) with only the above characters."
      )
    )
  }
}


#' @export
explain_final <- function(x_train,
                          x_explain,
                          model = NULL,
                          approach,
                          prediction_zero,
                          n_combinations = NULL,
                          group = NULL,
                          n_samples = 1e3,
                          n_batches = 1,
                          seed = 1,
                          keep_samp_for_vS = FALSE,
                          ...){ # ... is further arguments passed to setup_approach

  # Overview of what should happen in this function (and in what order)

  # Basic check of the necessary input being available
  # LATER: Make list with all parameters (n_combinations, seed, n_batches, approach, etc)
  # LATER: Make a list with all internal objects (X, S, S_batch, etc)
  # extract feature info from model if available
  # check compatability of x_train, x_explain and model
  # Merge all non-model stuff from shapr() into explain_setup

  # Setup
  #explainer <- init_explainer(environment(),...)

  # This is where we store everything
  internal <- list()

  # Structure the input
  internal$parameters <- get_parameters(approach = approach,
                                        prediction_zero = prediction_zero,
                                        n_combinations = n_combinations,
                                        group = group,
                                        n_samples = n_samples,
                                        n_batches = n_batches,
                                        seed = seed,
                                        keep_samp_for_vS = keep_samp_for_vS,...)

  internal$data <- get_data(x_train,x_explain)

  ##### DATA CHECKING AND PROCESSING ###########
  internal <- process_all_data(internal,model)

  # Checking that the prediction function works (duplicate in Python)
  test_model(head(internal$data$x_train, 2),model)

  # Checking the format of approach
  check_approach(internal)

  # setup the Shapley framework
  internal <- shapley_setup(internal)

  # Setup for approach
  internal <- setup_approach(internal, ...)

  # Accross all batches get the data we will predict on, predict on them, and do the MC integration
  vS_list <- future.apply::future_lapply(X = internal$objects$S_batch,
                                         FUN = run_batch,
                                         internal = internal,
                                         model = model,
                                         future.seed = internal$parameters$seed)

  output <- finalize_explanation(vS_list = vS_list,
                                 internal = internal)


  return(output)

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

  attr(output, "class") <- c("shapr", "list")


  return(output)

}

#' Compute shapley values
#' @param explainer An \code{explain} object.
#' @param dt_vS The contribution matrix.
#' @return A \code{data.table} with shapley values for each test observation.
#' @export
#' @keywords internal
compute_shapley_new <- function(internal, dt_vS) {

  is_groupwise <- internal$parameters$is_groupwise
  labels <- internal$parameters$feature_list$labels
  W <- internal$objects$W

  if (!is_groupwise) {
    shap_names <- labels
  } else {
    shap_names <- names(internal$parameters$group) #TODO: Add group_names (and feature_names) to internal earlier
  }


  kshap <- t(W %*% as.matrix(dt_vS[,-"id_combination"]))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", shap_names)

  return(dt_kshap)

}


#' @export
explain_setup <- function(x, explainer, approach, prediction_zero,
                          n_samples = 1e3, n_batches = 1, seed = 1, ...) {



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
setup_approach <- function(internal,...){

  approach <- internal$parameters$approach

  this_class <- ""

  if (length(approach) > 1) {
    class(this_class) <- "combined"
  }  else {
    class(this_class) <- approach
  }

  UseMethod("setup_approach", this_class)

  }

#' @export
setup_approach.independence <- function(internal,...){
  return(internal)
}

#' @export
setup_approach.empirical <- function(internal,
                                     seed = 1,
                                     w_threshold = 0.95,
                                     type = "fixed_sigma",
                                     fixed_sigma_vec = 0.1,
                                     n_samples_aicc = 1000,
                                     eval_max_aicc = 20,
                                     start_aicc = 0.1,
                                     cov_mat = NULL,...){

  parameters <- internal$parameters
  x_train <- internal$data$x_train

  # Add arguments to explainer object
  parameters$type <- type
  parameters$fixed_sigma_vec <- fixed_sigma_vec
  parameters$n_samples_aicc <- n_samples_aicc
  parameters$eval_max_aicc <- eval_max_aicc
  parameters$start_aicc <- start_aicc
  parameters$w_threshold <- w_threshold

  if (type == "independence") {
    warning(paste0(
      "Using type = 'independence' for approach = 'empirical' is deprecated.\n",
      "Please use approach = 'independence' instead in the call to explain()."
    ))
  }


  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    parameters$cov_mat <- get_cov_mat(x_train)
  }

  internal$parameters <- parameters

  return(internal)
}

#' @export
setup_approach.gaussian <- function(internal,
                                    mu = NULL,
                                    cov_mat = NULL, ...){

  parameters <- internal$parameters
  x_train <- internal$data$x_train

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    parameters$mu <- get_mu_vec(x_train)
  } else {
    parameters$mu <- mu
  }

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    parameters$cov_mat <- get_cov_mat(x_train)
  } else {
    parameters$cov_mat <- cov_mat
  }

  internal$parameters <- parameters

  return(internal)
}


#' @export
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
    X = rbind(x_explain, x_train0),
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


#' @export
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

#' @export
setup_approach.combined <- function(internal,...){

  org_approach <- internal$parameters$approach
  unique_approaches <- unique(org_approach)

  for(i in unique_approaches){
    internal$parameters$approach <- i
    internal <- setup_approach(internal,...)
  }
  internal$parameters$approach <- org_approach

  return(internal)
}



#
# #  # Check that the number of test observations equals max(id)
# #  stopifnot(nrow(explainer$x_explain) == dt[, max(id)])
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
#     names(p_all) <- 1:nrow(explainer$x_explain)
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
# explainer$x_explain <- as.matrix(preprocess_data(x, explainer$feature_list)$x_dt)
# explainer$approach <- approach
# explainer$n_samples <- n_samples
#

