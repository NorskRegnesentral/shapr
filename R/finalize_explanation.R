#' Computes the Shapley values given `v(S)`
#'
#' @inherit explain
#' @inheritParams default_doc
#' @param vS_list List
#' Output from [compute_vS()]
#'
#' @export
finalize_explanation <- function(vS_list, internal) {
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

    dt_samp_for_vS <- rbindlist(lapply(vS_list, `[[`, 2), use.names = TRUE)

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
#' @param dt_vS The contribution matrix.
#'
#' @inheritParams default_doc
#'
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
  n_explain <- internal$parameters$n_explain
  n_combinations <- internal$parameters$n_combinations
  id_combination_indices <- if (MSEv_skip_empty_full_comb) seq(2, n_combinations - 1) else seq(1, n_combinations)
  n_combinations_used <- length(id_combination_indices)
  features <- internal$objects$X$features[id_combination_indices]

  # Extract the predicted responses f(x)
  p <- unlist(dt_vS[id_combination == n_combinations, -"id_combination"])

  # Create contribution matrix
  vS <- as.matrix(dt_vS[id_combination_indices, -"id_combination"])

  # Square the difference between the v(S) and f(x)
  dt_squared_diff_original <- sweep(vS, 2, p)^2

  # Get the weights
  averaging_weights <- if (MSEv_uniform_comb_weights) rep(1, n_combinations) else internal$objects$X$shapley_weight
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
