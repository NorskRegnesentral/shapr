
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

  internal$model <- NULL # Clearnig out the model (only added for AICc-types of empirical approach)

  internal$output <- processed_vS_list

  output <- list(shapley_values=dt_shapley,
                 internal = internal,
                 pred_explain = p)
  attr(output, "class") <- c("shapr", "list")

  return(output)
}


#' @export
init_explainer <- function(...){
  list(...)
}




#' @export
compute_vS <- function(S,explainer){
  dt <- batch_prepare_vS(S,explainer)
  batch_get_preds(dt,S,explainer)
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




#' @export
test_model <- function(internal,model){
  x <- head(internal$data$x_train, 2)
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
get_supported_approaches <- function(){
  substring(rownames(attr(methods(prepare_data),"info")),first = 14)
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



