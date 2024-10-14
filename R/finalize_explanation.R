#' Gathers the final output to create the explanation object
#'
#' @inheritParams default_doc_explain
#'
#' @export
finalize_explanation <- function(internal) {
  MSEv_uniform_comb_weights <- internal$parameters$output_args$MSEv_uniform_comb_weights
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
  iterative_results <- get_iter_results(internal$iter_list)

  output <- list(
    shapley_values = dt_shapley_est,
    shapley_values_sd = dt_shapley_sd,
    pred_explain = p,
    MSEv = MSEv,
    iterative_results = iterative_results,
    saving_path = internal$parameters$output_args$saving_path,
    internal = internal
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
                              "exact", "compute_sd", "n_coal_next_iter_factor", "n_coalitions", "n_batches",
                              "converged", "converged_exact", "converged_sd", "converged_max_iter",
                              "est_required_coalitions", "est_remaining_coalitions", "overall_conv_measure"
                            )) {
  extracted <- lapply(iter_list, function(x) x[what])
  ret <- do.call(rbindlist, list(l = lapply(extracted, as.data.table), fill = TRUE))
  return(ret)
}




#' @keywords internal
get_p <- function(dt_vS, internal) {
  id_coalition <- NULL # due to NSE

  iter <- length(internal$iter_list)
  max_id_coalition <- internal$iter_list[[iter]]$n_coalitions


  p <- unlist(dt_vS[id_coalition == max_id_coalition, ][, id_coalition := NULL])

  if (internal$parameters$type == "forecast") {
    names(p) <- apply(internal$parameters$output_labels, 1, function(x) paste0("explain_idx_", x[1], "_horizon_", x[2]))
  }

  return(p)
}









#' Mean Squared Error of the Contribution Function `v(S)`
#'
#' @inheritParams explain
#' @inheritParams default_doc
#' @param dt_vS Data.table of dimension `n_coalitions` times `n_explain + 1` containing the contribution function
#' estimates. The first column is assumed to be named `id_coalition` and containing the ids of the coalitions.
#' The last row is assumed to be the full coalition, i.e., it contains the predicted responses for the observations
#' which are to be explained.
#' @param MSEv_skip_empty_full_comb Logical. If `TRUE` (default), we exclude the empty and grand
#' coalitions when computing the MSEv evaluation criterion. This is reasonable as they are identical
#' for all methods, i.e., their contribution function is independent of the used method as they are special cases not
#' effected by the used method. If `FALSE`, we include the empty and grand coalitions. In this situation,
#' we also recommend setting `MSEv_uniform_comb_weights = TRUE`, as otherwise the large weights for the empty and
#' grand coalitions will outweigh all other coalitions and make the MSEv criterion uninformative.
#'
#' @return
#' List containing:
#' \describe{
#'  \item{`MSEv`}{A \code{\link[data.table]{data.table}} with the overall MSEv evaluation criterion averaged
#'  over both the coalitions and observations/explicands. The \code{\link[data.table]{data.table}}
#'  also contains the standard deviation of the MSEv values for each explicand (only averaged over the coalitions)
#'  divided by the square root of the number of explicands.}
#'  \item{`MSEv_explicand`}{A \code{\link[data.table]{data.table}} with the mean squared error for each
#'  explicand, i.e., only averaged over the coalitions.}
#'  \item{`MSEv_coalition`}{A \code{\link[data.table]{data.table}} with the mean squared error for each
#'  coalition, i.e., only averaged over the explicands/observations.
#'  The \code{\link[data.table]{data.table}} also contains the standard deviation of the MSEv values for
#'  each coalition divided by the square root of the number of explicands.}
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
  n_coalitions <- internal$iter_list[[iter]]$n_coalitions

  n_explain <- internal$parameters$n_explain
  id_coalition_indices <- if (MSEv_skip_empty_full_comb) seq(2, n_coalitions - 1) else seq(1, n_coalitions)
  n_coalitions_used <- length(id_coalition_indices)

  X <- internal$objects$X
  coalitions <- X$coalitions[id_coalition_indices]

  # Extract the predicted responses f(x)
  p <- unlist(dt_vS[id_coalition == n_coalitions, -"id_coalition"])

  # Create contribution matrix
  vS <- as.matrix(dt_vS[id_coalition_indices, -"id_coalition"])

  # Square the difference between the v(S) and f(x)
  dt_squared_diff_original <- sweep(vS, 2, p)^2

  # Get the weights
  averaging_weights <- if (MSEv_uniform_comb_weights) rep(1, n_coalitions) else X$shapley_weight
  averaging_weights <- averaging_weights[id_coalition_indices]
  averaging_weights_scaled <- averaging_weights / sum(averaging_weights)

  # Apply the `averaging_weights_scaled` to each column (i.e., each explicand)
  dt_squared_diff <- dt_squared_diff_original * averaging_weights_scaled

  # Compute the mean squared error for each observation, i.e., only averaged over the coalitions.
  # We take the sum as the weights sum to 1, so denominator is 1.
  MSEv_explicand <- colSums(dt_squared_diff)

  # The MSEv criterion for each coalition, i.e., only averaged over the explicands.
  MSEv_coalition <- rowMeans(dt_squared_diff * n_coalitions_used)
  MSEv_coalition_sd <- apply(dt_squared_diff * n_coalitions_used, 1, sd) / sqrt(n_explain)

  # The MSEv criterion averaged over both the coalitions and explicands.
  MSEv <- mean(MSEv_explicand)
  MSEv_sd <- sd(MSEv_explicand) / sqrt(n_explain)

  # Set the name entries in the arrays
  names(MSEv_explicand) <- paste0("id_", seq(n_explain))
  names(MSEv_coalition) <- paste0("id_coalition_", id_coalition_indices)
  names(MSEv_coalition_sd) <- paste0("id_coalition_", id_coalition_indices)

  # Convert the results to data.table
  MSEv <- data.table(
    "MSEv" = MSEv,
    "MSEv_sd" = MSEv_sd
  )
  MSEv_explicand <- data.table(
    "id" = seq(n_explain),
    "MSEv" = MSEv_explicand
  )
  MSEv_coalition <- data.table(
    "id_coalition" = id_coalition_indices,
    "coalitions" = coalitions,
    "MSEv" = MSEv_coalition,
    "MSEv_sd" = MSEv_coalition_sd
  )

  return(list(
    MSEv = MSEv,
    MSEv_explicand = MSEv_explicand,
    MSEv_coalition = MSEv_coalition
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
  MSEv_uniform_comb_weights <- internal$parameters$output_args$MSEv_uniform_comb_weights

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  # Extract the predictions we are explaining
  p <- get_p(processed_vS_list$dt_vS, internal)

  # Compute the Shapley values
  dt_shapley <- compute_shapley_new(internal, processed_vS_list$dt_vS)

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
