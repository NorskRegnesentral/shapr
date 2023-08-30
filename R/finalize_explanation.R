#' Computes the Shapley values given `v(S)`
#'
#' @inherit explain
#' @inheritParams default_doc
#' @param vS_list List
#' Output from [compute_vS()]
#'
#' @export
finalize_explanation <- function(vS_list, internal) {
  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  # Extract the predictions we are explaining
  p <- get_p(processed_vS_list$dt_vS, internal)

  # internal$timing$postprocessing <- Sys.time()

  # Compute the Shapley values
  dt_shapley <- compute_shapley_new(internal, processed_vS_list$dt_vS)

  # internal$timing$shapley_computation <- Sys.time()

  # Clearing out the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$tmp <- NULL

  internal$output <- processed_vS_list

  output <- list(
    shapley_values = dt_shapley,
    internal = internal,
    pred_explain = p
  )
  attr(output, "class") <- c("shapr", "list")

  # Compute the MSEv evaluation criterion if the output of the predictive model is a scalar.
  # TODO: check if it makes sense for output_size > 1.
  if (internal$parameters$output_size == 1) {
    # Compute the MSEv evaluation criterion
    MSEv_evaluation_criterion = compute_MSEv_evaluation_criterion(
      internal = internal,
      processed_vS_list = processed_vS_list,
      p = p,
      exclude_empty_and_grand_coalition = FALSE,
      return_as_dt = TRUE)
    output$MSEv_evaluation_criterion = MSEv_evaluation_criterion
  }

  return(output)
}


#' @keywords internal
postprocess_vS_list <- function(vS_list, internal) {
  id_combination <- NULL # due to NSE

  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
  prediction_zero <- internal$parameters$prediction_zero
  n_explain <- internal$parameters$n_explain

  # Appending the zero-prediction to the list
  dt_vS0 <- as.data.table(rbind(c(1, rep(prediction_zero, n_explain))))

  # Extracting/merging the data tables from the batch running
  # TODO: Need a memory and speed optimized way to transform the output form dt_vS_list to two different lists,
  # I.e. without copying the data more than once. For now I have modified run_batch such that it
  # if keep_samp_for_vS=FALSE
  # then there is only one copy, but there are two if keep_samp_for_vS=TRUE. This might be OK since the
  # latter is used rarely
  if (keep_samp_for_vS) {
    names(dt_vS0) <- names(vS_list[[1]][[1]])

    vS_list[[length(vS_list) + 1]] <- list(dt_vS0, NULL)

    dt_vS <- rbindlist(lapply(vS_list, `[[`, 1))

    dt_samp_for_vS <- rbindlist(lapply(vS_list, `[[`, 2))
    data.table::setorder(dt_samp_for_vS, id_combination)
  } else {
    names(dt_vS0) <- names(vS_list[[1]])

    vS_list[[length(vS_list) + 1]] <- dt_vS0

    dt_vS <- rbindlist(vS_list)
    dt_samp_for_vS <- NULL
  }

  data.table::setorder(dt_vS, id_combination)

  output <- list(
    dt_vS = dt_vS,
    dt_samp_for_vS = dt_samp_for_vS
  )
  return(output)
}

#' @keywords internal
get_p <- function(dt_vS, internal) {
  id_combination <- NULL # due to NSE

  max_id_combination <- internal$parameters$n_combinations
  p <- unlist(dt_vS[id_combination == max_id_combination, ][, id_combination := NULL])

  if (internal$parameters$type == "forecast") {
    names(p) <- apply(internal$parameters$output_labels, 1, function(x) paste0("explain_idx_", x[1], "_horizon_", x[2]))
  }

  return(p)
}

#' Compute shapley values
#' @param explainer An `explain` object.
#' @param dt_vS The contribution matrix.
#' @return A `data.table` with Shapley values for each test observation.
#' @export
#' @keywords internal
compute_shapley_new <- function(internal, dt_vS) {
  is_groupwise <- internal$parameters$is_groupwise
  feature_names <- internal$parameters$feature_names
  W <- internal$objects$W
  type <- internal$parameters$type

  if (!is_groupwise) {
    shap_names <- feature_names
  } else {
    shap_names <- names(internal$parameters$group) # TODO: Add group_names (and feature_names) to internal earlier
  }

  # If multiple horizons with explain_forecast are used, we only distribute value to those used at each horizon
  if (type == "forecast") {
    id_combination_mapper_dt <- internal$objects$id_combination_mapper_dt
    horizon <- internal$parameters$horizon
    cols_per_horizon <- internal$objects$cols_per_horizon
    W_list <- internal$objects$W_list

    kshap_list <- list()
    for (i in seq_len(horizon)) {
      W0 <- W_list[[i]]

      dt_vS0 <- merge(dt_vS, id_combination_mapper_dt[horizon == i], by = "id_combination", all.y = TRUE)
      data.table::setorder(dt_vS0, horizon_id_combination)
      these_vS0_cols <- grep(paste0("p_hat", i, "_"), names(dt_vS0))

      kshap0 <- t(W0 %*% as.matrix(dt_vS0[, these_vS0_cols, with = FALSE]))
      kshap_list[[i]] <- data.table::as.data.table(kshap0)

      if (!is_groupwise) {
        names(kshap_list[[i]]) <- c("none", cols_per_horizon[[i]])
      } else {
        names(kshap_list[[i]]) <- c("none", shap_names)
      }
    }

    dt_kshap <- cbind(internal$parameters$output_labels, rbindlist(kshap_list, fill = TRUE))
  } else {
    kshap <- t(W %*% as.matrix(dt_vS[, -"id_combination"]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", shap_names)
  }

  return(dt_kshap)
}



#' Mean Squared Error of the Contribution Function `v(S)`
#'
#' @param internal List with the different parameters, data and functions used internally in the [explain()] function.
#' @param processed_vS_list List of the processed contribution function estimates. Output from the \code{shapr:::postprocess_vS_list()} function.
#' @param p Numeric vector with the predicted responses for the observations which are to be explained. Output from the \code{shapr:::get_p()} function.
#' @param exclude_empty_and_grand_coalition Boolean. If `TRUE`, we exclude the empty and grand coalitions
#' when computing the MSEv evaluation criterion. This is reasonable as they are identical for all methods, i.e.,
#' their contribution function is independent of the used method as they are special cases not effected by
#' the used method. If `TRUE`, we exclude the empty and grand coalitions.
#' @param return_as_dt Boolean. If the computes MSEv evaluation criterion are to be returned as \code{\link[data.table]{data.table}}.
#'
#' @return
#' List containing:
#' \describe{
#'  \item{`MSEv_evaluation_criterion`}{Numeric scalar (or \code{\link[data.table]{data.table}} based on parameter `return_as_dt`) with the overall MSEv evaluation criterion averaged over both the coalitions and observations.}
#'  \item{`MSEv_evaluation_criterion_for_each_explicand`}{Numeric vector (or \code{\link[data.table]{data.table}} based on parameter `return_as_dt`) with the mean squared error for each explicand, i.e., only averaged over the coalitions.}
#'  \item{`MSEv_evaluation_criterion_for_each_coalition`}{Numeric vector (or \code{\link[data.table]{data.table}} based on parameter `return_as_dt`) with the mean squared error for each coalition, i.e., only averaged over the observations.}
#' }
#'
#' @description Function that computes the Mean Squared Error (MSEv) of the contribution function
#' v(s) as proposed by \href{https://arxiv.org/pdf/2006.01272.pdf}{Frye et al. (2019)} and used by
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#'
#' @details
#' The MSEv evaluation criterion does not rely on access to the true contribution functions nor the
#' true Shapley values to be computed. A lower value indicates better approximations, however, the
#' scale and magnitude of the MSEv criterion is not directly interpretable in regard to the precision
#' of the final estimated Shapley values. \href{https://arxiv.org/pdf/2305.09536.pdf}{Olsen et al. (2022)}
#' illustrates in Figure 11 a fairly strong linear relationship between the MSEv criterion and the
#' MAE between the estimated and true Shapley values in a simulation study. Note that explicands
#' refer to the observations whose predictions we are to explain.
#'
#' @examples
#' library(xgboost)
#' library(data.table)
#' seed(1)
#'
#' data("airquality")
#' data <- data.table::as.data.table(airquality)
#' data <- data[complete.cases(data), ]
#'
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' ind_x_explain <- 1:6
#' x_train <- data[-ind_x_explain, ..x_var]
#' y_train <- data[-ind_x_explain, get(y_var)]
#' x_explain <- data[ind_x_explain, ..x_var]
#'
#' # Fitting a basic xgboost model to the training data
#' model <- xgboost::xgboost(
#'   data = as.matrix(x_train),
#'   label = y_train,
#'   nround = 20,
#'   verbose = FALSE
#' )
#'
#' # Specifying the phi_0, i.e. the expected prediction without any features
#' p0 <- mean(y_train)
#'
#' # Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
#' # the gaussian (conditional) approach
#' explanation <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   prediction_zero = p0,
#'   seed = 1
#' )
#'
#' # The `compute_MSEv_evaluation_criterion` function is intended to be only called internally in
#' # the `shapr` package, but the user can still use it outside of the package.
#' # Exclude the empty and grand coalitions from the computations and return the result as a data.table
#' compute_MSEv_evaluation_criterion(internal = explanation$internal,
#'                                   processed_vS_list = explanation$internal$output,
#'                                   exclude_empty_and_grand_coalition = TRUE,
#'                                   return_as_dt = TRUE)
#'
#' # Include the empty and grand coalitions from the computations and return the result as a data.table
#' compute_MSEv_evaluation_criterion(internal = explanation$internal,
#'                                   processed_vS_list = explanation$internal$output,
#'                                   exclude_empty_and_grand_coalition = FALSE,
#'                                   return_as_dt = TRUE)
#'
#' # Exclude the empty and grand coalitions from the computations and return the result as a list
#' compute_MSEv_evaluation_criterion(internal = explanation$internal,
#'                                   processed_vS_list = explanation$internal$output,
#'                                   exclude_empty_and_grand_coalition = TRUE,
#'                                   return_as_dt = FALSE)
#'
#' # Include the empty and grand coalitions from the computations and return the result as a list
#' compute_MSEv_evaluation_criterion(internal = explanation$internal,
#'                                   processed_vS_list = explanation$internal$output,
#'                                   exclude_empty_and_grand_coalition = FALSE,
#'                                   return_as_dt = FALSE)
#'
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
compute_MSEv_evaluation_criterion = function(internal,
                                             processed_vS_list,
                                             p = shapr:::get_p(processed_vS_list$dt_vS, internal),
                                             exclude_empty_and_grand_coalition = TRUE,
                                             return_as_dt = TRUE) {

  # Get the number of unique coalitions, where two of them are the empty and full set.
  # This is 2^M if internal$parameters$exact is TRUE or some value below 2^M if sampled version.
  n_combinations = internal$parameters$n_combinations

  # Get the number of observations to explain
  n_explain = internal$parameters$n_explain

  # Get the imputed samples
  dt_vS = processed_vS_list$dt_vS

  # Check if we are to remove the empty and grand coalitions, which are
  # identical for all methods and thus is not effected by the used method.
  if (exclude_empty_and_grand_coalition) {
    dt_vS = dt_vS[-c(1, n_combinations),]
  }

  # Square the difference between the estimated contribution function
  # v(S) = E_{\hat{p}(X_sbar | X_s = x_s)} [f(X_sbar, x_s) | X_s = x_s)]
  # and the predicted response f(x).
  dt_squared_difference = as.matrix(dt_vS[,!"id_combination"][,Map(`-`, p, .SD)]^2)

  # Compute the mean squared error for each observation, i.e., only averaged over the coalitions.
  MSEv_evaluation_criterion_for_each_explicand = colMeans(dt_squared_difference)
  names(MSEv_evaluation_criterion_for_each_explicand) = paste0("id_", seq(n_explain))

  # Compute the mean squared error for each coalition, i.e., only averaged over the explicands.
  MSEv_evaluation_criterion_for_each_coalition = rowMeans(dt_squared_difference)
  MSEv_evaluation_criterion_for_each_coalition_sd = apply(dt_squared_difference, 1, sd)

  # Set the names
  if (exclude_empty_and_grand_coalition) {
    id_combination_numbers = seq(2, n_combinations-1)
  } else {
    id_combination_numbers = seq(1, n_combinations)
  }
  names(MSEv_evaluation_criterion_for_each_coalition) = paste0("id_combination_", id_combination_numbers)

  # Compute the overall mean squared error averaged over both the coalitions and explicands.
  MSEv_evaluation_criterion = mean(dt_squared_difference)

  # If we are to return the results as data.table, then we overwrite the previous results.
  if (return_as_dt) {
    MSEv_evaluation_criterion =
      data.table("MSEv_evaluation_criterion" = MSEv_evaluation_criterion)
    MSEv_evaluation_criterion_for_each_explicand =
      data.table("id" = seq(n_explain),
                 "MSEv_evaluation_criterion" = MSEv_evaluation_criterion_for_each_explicand)
    MSEv_evaluation_criterion_for_each_coalition =
      data.table("id_combination" = id_combination_numbers,
                 "features" = internal$objects$X$features[id_combination_numbers],
                 "MSEv_evaluation_criterion" = MSEv_evaluation_criterion_for_each_coalition,
                 "MSEv_evaluation_criterion_sd" = MSEv_evaluation_criterion_for_each_coalition_sd)

    # Create a list of the data tables to return
    return_list = list(
      MSEv_evaluation_criterion = MSEv_evaluation_criterion,
      MSEv_evaluation_criterion_for_each_explicand = MSEv_evaluation_criterion_for_each_explicand,
      MSEv_evaluation_criterion_for_each_coalition = MSEv_evaluation_criterion_for_each_coalition
    )
  } else {
    # Create a list of the numeric vectors to return
    return_list = list(
      MSEv_evaluation_criterion = MSEv_evaluation_criterion,
      MSEv_evaluation_criterion_for_each_explicand = MSEv_evaluation_criterion_for_each_explicand,
      MSEv_evaluation_criterion_for_each_coalition = MSEv_evaluation_criterion_for_each_coalition,
      MSEv_evaluation_criterion_for_each_coalition_sd = MSEv_evaluation_criterion_for_each_coalition_sd
    )
  }

  # Return the return list
  return(return_list)
}
