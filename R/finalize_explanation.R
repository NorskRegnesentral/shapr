#' Finalizes the explanation object
#'
#' @inherit explain
#' @inheritParams default_doc
#'
#' @export
finalize_explanation <- function(internal) {
  MSEv_uniform_comb_weights <- internal$parameters$MSEv_uniform_comb_weights
  output_size <- internal$parameters$output_size
  dt_vS <- internal$output$dt_vS

  # Extracting iter (and deleting the last temporary empty list of iter_list)
  iter <- length(internal$iter_list) - 1
  internal$iter_list[[iter + 1]] <- NULL

  dt_shapley_est <- internal$iter_list[[iter]]$dt_shapley_est
  dt_shapley_sd <- internal$iter_list[[iter]]$dt_shapley_sd

  # Setting parameters and objects used in the end from the last iteration
  internal$objects$X <- internal$iter_list[[iter]]$X
  internal$objects$S <- internal$iter_list[[iter]]$S
  internal$objects$W <- internal$iter_list[[iter]]$W




  # Clearing out the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$tmp <- NULL

  # Clearing out the timing lists as they are added to the output separately
  internal$main_timing_list <- internal$iter_timing_list <- internal$timing_list <- NULL


  # Extract the predictions we are explaining
  p <- get_p(dt_vS, internal)


  # Compute the MSEv evaluation criterion if the output of the predictive model is a scalar.
  # TODO: check if it makes sense for output_size > 1.
  if (output_size == 1) {
    MSEv <- compute_MSEv_eval_crit(
      internal = internal,
      dt_vS = dt_vS,
      MSEv_uniform_comb_weights = MSEv_uniform_comb_weights
    )
  } else {
    MSEv <- NULL
  }

  # Extract iterative results in a simplified format
  internal$iter_results <- get_iter_results(internal$iter_list)

  output <- list(
    shapley_values = dt_shapley_est,
    shapley_values_sd = dt_shapley_sd,
    internal = internal,
    pred_explain = p,
    MSEv = MSEv
  )
  attr(output, "class") <- c("shapr", "list")

  return(output)
}

get_iter_results <- function(iter_list) {
  ret <- list()
  ret$dt_iter_shapley_est <- rbindlist(lapply(iter_list, `[[`, "dt_shapley_est"), idcol = "iter")
  ret$dt_iter_shapley_sd <- rbindlist(lapply(iter_list, `[[`, "dt_shapley_sd"), idcol = "iter")
  ret$iter_info_dt <- iter_list_to_dt(iter_list)
  return(ret)
}

iter_list_to_dt <- function(iter_list, what = c(
                              "exact", "compute_sd", "reduction_factor", "n_combinations",
                              "converged", "converged_exact", "converged_sd", "converged_max_iter",
                              "est_required_combinations", "est_remaining_combinations"
                            )) {
  extracted <- lapply(iter_list, function(x) x[what])
  ret <- do.call(rbind, lapply(extracted, as.data.table))
  return(ret)
}




#' @keywords internal
get_p <- function(dt_vS, internal) {
  id_combination <- NULL # due to NSE

  iter <- length(internal$iter_list)
  max_id_combination <- internal$iter_list[[iter]]$n_combinations


  p <- unlist(dt_vS[id_combination == max_id_combination, ][, id_combination := NULL])

  if (internal$parameters$type == "forecast") {
    names(p) <- apply(internal$parameters$output_labels, 1, function(x) paste0("explain_idx_", x[1], "_horizon_", x[2]))
  }

  return(p)
}









#' Mean Squared Error of the Contribution Function `v(S)`
#'
#' @inheritParams explain
#' @inheritParams default_doc
#' @param dt_vS Data.table of dimension `n_combinations` times `n_explain + 1` containing the contribution function
#' estimates. The first column is assumed to be named `id_combination` and containing the ids of the combinations.
#' The last row is assumed to be the full combination, i.e., it contains the predicted responses for the observations
#' which are to be explained.
#' @param MSEv_skip_empty_full_comb Logical. If `TRUE` (default), we exclude the empty and grand
#' combinations/coalitions when computing the MSEv evaluation criterion. This is reasonable as they are identical
#' for all methods, i.e., their contribution function is independent of the used method as they are special cases not
#' effected by the used method. If `FALSE`, we include the empty and grand combinations/coalitions. In this situation,
#' we also recommend setting `MSEv_uniform_comb_weights = TRUE`, as otherwise the large weights for the empty and
#' grand combinations/coalitions will outweigh all other combinations and make the MSEv criterion uninformative.
#'
#' @return
#' List containing:
#' \describe{
#'  \item{`MSEv`}{A \code{\link[data.table]{data.table}} with the overall MSEv evaluation criterion averaged
#'  over both the combinations/coalitions and observations/explicands. The \code{\link[data.table]{data.table}}
#'  also contains the standard deviation of the MSEv values for each explicand (only averaged over the combinations)
#'  divided by the square root of the number of explicands.}
#'  \item{`MSEv_explicand`}{A \code{\link[data.table]{data.table}} with the mean squared error for each
#'  explicand, i.e., only averaged over the combinations/coalitions.}
#'  \item{`MSEv_combination`}{A \code{\link[data.table]{data.table}} with the mean squared error for each
#'  combination/coalition, i.e., only averaged over the explicands/observations.
#'  The \code{\link[data.table]{data.table}} also contains the standard deviation of the MSEv values for
#'  each combination divided by the square root of the number of explicands.}
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
#' @keywords internal
#' @author Lars Henry Berge Olsen
compute_MSEv_eval_crit <- function(internal,
                                   dt_vS,
                                   MSEv_uniform_comb_weights,
                                   MSEv_skip_empty_full_comb = TRUE) {
  iter <- length(internal$iter_list)
  n_combinations <- internal$iter_list[[iter]]$n_combinations

  n_explain <- internal$parameters$n_explain
  id_combination_indices <- if (MSEv_skip_empty_full_comb) seq(2, n_combinations - 1) else seq(1, n_combinations)
  n_combinations_used <- length(id_combination_indices)

  X <- internal$objects$X
  features <- X$features[id_combination_indices]

  # Extract the predicted responses f(x)
  p <- unlist(dt_vS[id_combination == n_combinations, -"id_combination"])

  # Create contribution matrix
  vS <- as.matrix(dt_vS[id_combination_indices, -"id_combination"])

  # Square the difference between the v(S) and f(x)
  dt_squared_diff_original <- sweep(vS, 2, p)^2

  # Get the weights
  averaging_weights <- if (MSEv_uniform_comb_weights) rep(1, n_combinations) else X$shapley_weight
  averaging_weights <- averaging_weights[id_combination_indices]
  averaging_weights_scaled <- averaging_weights / sum(averaging_weights)

  # Apply the `averaging_weights_scaled` to each column (i.e., each explicand)
  dt_squared_diff <- dt_squared_diff_original * averaging_weights_scaled

  # Compute the mean squared error for each observation, i.e., only averaged over the coalitions.
  # We take the sum as the weights sum to 1, so denominator is 1.
  MSEv_explicand <- colSums(dt_squared_diff)

  # The MSEv criterion for each coalition, i.e., only averaged over the explicands.
  MSEv_combination <- rowMeans(dt_squared_diff * n_combinations_used)
  MSEv_combination_sd <- apply(dt_squared_diff * n_combinations_used, 1, sd) / sqrt(n_explain)

  # The MSEv criterion averaged over both the coalitions and explicands.
  MSEv <- mean(MSEv_explicand)
  MSEv_sd <- sd(MSEv_explicand) / sqrt(n_explain)

  # Set the name entries in the arrays
  names(MSEv_explicand) <- paste0("id_", seq(n_explain))
  names(MSEv_combination) <- paste0("id_combination_", id_combination_indices)
  names(MSEv_combination_sd) <- paste0("id_combination_", id_combination_indices)

  # Convert the results to data.table
  MSEv <- data.table(
    "MSEv" = MSEv,
    "MSEv_sd" = MSEv_sd
  )
  MSEv_explicand <- data.table(
    "id" = seq(n_explain),
    "MSEv" = MSEv_explicand
  )
  MSEv_combination <- data.table(
    "id_combination" = id_combination_indices,
    "features" = features,
    "MSEv" = MSEv_combination,
    "MSEv_sd" = MSEv_combination_sd
  )

  return(list(
    MSEv = MSEv,
    MSEv_explicand = MSEv_explicand,
    MSEv_combination = MSEv_combination
  ))
}


#' Computes the Shapley values given `v(S)`
#'
#' @inherit explain
#' @inheritParams default_doc
#' @param vS_list List
#' Output from [compute_vS()]
#'
#' @export
finalize_explanation_forecast <- function(vS_list, internal) { # Temporary used for forecast only (the old function)
  MSEv_uniform_comb_weights <- internal$parameters$MSEv_uniform_comb_weights

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

  # Clearing out the timing lists as they are added to the output separately
  internal$main_timing_list <- internal$iter_timing_list <- internal$timing_list <- NULL


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
    output$MSEv <- compute_MSEv_eval_crit(
      internal = internal,
      dt_vS = processed_vS_list$dt_vS,
      MSEv_uniform_comb_weights = MSEv_uniform_comb_weights
    )
  }

  return(output)
}
