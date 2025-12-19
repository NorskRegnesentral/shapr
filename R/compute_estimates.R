#' Compute the Shapley Values and Their Standard Deviation Given `v(S)`
#'
#' @inheritParams default_doc_export
#' @param vS_list List.
#' Output from [compute_vS()].
#'
#' @return The (updated) internal list
#'
#' @export
#' @keywords internal
compute_estimates <- function(internal, vS_list) {
  verbose <- internal$parameters$verbose
  type <- internal$parameters$type

  internal$timing_list$compute_vS <- Sys.time()


  iter <- length(internal$iter_list)
  compute_sd <- internal$iter_list[[iter]]$compute_sd

  n_boot_samps <- internal$parameters$extra_computation_args$n_boot_samps

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  internal$timing_list$postprocess_vS <- Sys.time()


  if ("progress" %in% verbose) {
    cli::cli_progress_step("Computing Shapley value estimates")
  }

  # Compute the Shapley values
  dt_shapley_est <- compute_shapley(internal, processed_vS_list$dt_vS)

  internal$timing_list$compute_shapley <- Sys.time()

  if (compute_sd) {
    if ("progress" %in% verbose) {
      cli::cli_progress_step("Bootstrapping Shapley value standard deviations")
    }

    dt_shapley_sd <- bootstrap_shapley(internal, n_boot_samps = n_boot_samps, processed_vS_list$dt_vS)

    internal$timing_list$compute_bootstrap <- Sys.time()
  } else {
    dt_shapley_sd <- dt_shapley_est * 0
  }


  # Add explain_id to the output tables
  if (type != "forecast") {
    dt_shapley_est[, explain_id := .I]
    setcolorder(dt_shapley_est, "explain_id")
    dt_shapley_sd[, explain_id := .I]
    setcolorder(dt_shapley_sd, "explain_id")
  }


  internal$iter_list[[iter]]$dt_shapley_est <- dt_shapley_est
  internal$iter_list[[iter]]$dt_shapley_sd <- dt_shapley_sd
  internal$iter_list[[iter]]$vS_list <- vS_list
  internal$iter_list[[iter]]$dt_vS <- processed_vS_list$dt_vS

  # Clear the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$output <- processed_vS_list

  if ("basic" %in% verbose) {
    cli::cli_progress_done()
  }

  return(internal)
}

#' @keywords internal
postprocess_vS_list <- function(vS_list, internal) {
  keep_samp_for_vS <- internal$parameters$output_args$keep_samp_for_vS
  phi0 <- internal$parameters$phi0
  n_explain <- internal$parameters$n_explain

  # Appending the zero-prediction to the list
  dt_vS0 <- as.data.table(rbind(c(1, rep(phi0, n_explain))))

  # Extracting/merging the data tables from the batch running
  if (keep_samp_for_vS) {
    names(dt_vS0) <- names(vS_list[[1]][[1]])

    vS_list[[length(vS_list) + 1]] <- list(dt_vS0, NULL)

    dt_vS <- rbindlist(lapply(vS_list, `[[`, 1))

    dt_samp_for_vS <- rbindlist(lapply(vS_list, `[[`, 2), use.names = TRUE)

    data.table::setorder(dt_samp_for_vS, id_coalition)
  } else {
    names(dt_vS0) <- names(vS_list[[1]])

    vS_list[[length(vS_list) + 1]] <- dt_vS0

    dt_vS <- rbindlist(vS_list)
    dt_samp_for_vS <- NULL
  }

  data.table::setorder(dt_vS, id_coalition)

  dt_vS <- unique(dt_vS, by = "id_coalition") # Remove duplicated full pred row in the iterative procedure

  output <- list(
    dt_vS = dt_vS,
    dt_samp_for_vS = dt_samp_for_vS
  )
  return(output)
}


#' Compute Shapley Values
#' @param dt_vS The contribution matrix.
#'
#' @inheritParams default_doc_internal
#'
#' @return A `data.table` with Shapley values for each test observation.
#' @export
#' @keywords internal
compute_shapley <- function(internal, dt_vS) {
  is_groupwise <- internal$parameters$is_groupwise
  type <- internal$parameters$type

  iter <- length(internal$iter_list)

  W <- internal$iter_list[[iter]]$W

  shap_names <- internal$parameters$shap_names

  # If multiple horizons with explain_forecast are used, distribute value only to those used at each horizon
  if (type == "forecast") {
    id_coalition_mapper_dt <- internal$iter_list[[iter]]$id_coalition_mapper_dt
    horizon <- internal$parameters$horizon
    cols_per_horizon <- internal$objects$cols_per_horizon
    shap_names <- internal$parameters$shap_names
    W_list <- internal$objects$W_list

    kshap_list <- list()
    for (i in seq_len(horizon)) {
      W0 <- W_list[[i]]

      dt_vS0 <- merge(dt_vS, id_coalition_mapper_dt[horizon == i], by = "id_coalition", all.y = TRUE)
      data.table::setorder(dt_vS0, horizon_id_coalition)
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
    kshap <- t(W %*% as.matrix(dt_vS[, -"id_coalition"]))
    dt_kshap <- data.table::as.data.table(kshap)
    colnames(dt_kshap) <- c("none", shap_names)
  }

  return(dt_kshap)
}

#' @keywords internal
bootstrap_shapley <- function(internal, dt_vS, n_boot_samps = 100) {
  iter <- length(internal$iter_list)
  type <- internal$parameters$type
  is_groupwise <- internal$parameters$is_groupwise
  X_list <- internal$iter_list[[iter]]$X_list

  result <- list()
  if (type == "forecast") {
    n_explain <- internal$parameters$n_explain
    for (i in seq_along(X_list)) {
      X <- X_list[[i]]
      if (is_groupwise) {
        n_shapley_values <- internal$parameters$n_shapley_values
        shap_names <- internal$parameters$shap_names
      } else {
        n_shapley_values <- length(internal$parameters$horizon_features[[i]])
        shap_names <- internal$parameters$horizon_features[[i]]
      }
      dt_cols <- c(1, seq_len(n_explain) + (i - 1) * n_explain + 1)
      dt_vS_this <- dt_vS[, dt_cols, with = FALSE]
      n_coal_each_size <- choose(n_shapley_values, seq(n_shapley_values - 1))
      result[[i]] <-
        bootstrap_shapley_inner(X, n_shapley_values, shap_names, internal, dt_vS_this, n_coal_each_size, n_boot_samps)
    }
    result <- cbind(internal$parameters$output_labels, rbindlist(result, fill = TRUE))
  } else {
    X <- internal$iter_list[[iter]]$X
    n_shapley_values <- internal$parameters$n_shapley_values
    shap_names <- internal$parameters$shap_names
    n_coal_each_size <- internal$parameters$n_coal_each_size
    result <- bootstrap_shapley_inner(X, n_shapley_values, shap_names, internal, dt_vS, n_coal_each_size, n_boot_samps)
  }
  return(result)
}

#' @keywords internal
bootstrap_shapley_inner <- function(X,
                                    n_shapley_values,
                                    shap_names,
                                    internal,
                                    dt_vS,
                                    n_coal_each_size,
                                    n_boot_samps = 100) {
  type <- internal$parameters$type
  iter <- length(internal$iter_list)

  n_explain <- internal$parameters$n_explain
  paired_shap_sampling <- internal$parameters$extra_computation_args$paired_shap_sampling
  semi_deterministic_sampling <- internal$parameters$extra_computation_args$semi_deterministic_sampling
  shapley_reweight <- internal$parameters$extra_computation_args$kernelSHAP_reweighting

  if (type == "forecast") {
    # For forecast set to zero, as all coalitions except empty and grand can be sampled
    max_fixed_coal_size <- 0
  } else {
    # For semi_deterministic_sampling = FALSE, this will be 0 and all coalitions except empty and grand are sampleable
    max_fixed_coal_size <- internal$iter_list[[iter]]$dt_coal_samp_info$max_fixed_coal_size
  }

  X_org <- copy(X)

  boot_sd_array <- array(NA, dim = c(n_explain, n_shapley_values + 1, n_boot_samps))

  # Split X_org into the deterministic and sampled coalitions
  X_keep <- X_org[is.na(sample_freq), .(id_coalition, coalitions, coalition_size, N, shapley_weight)]
  X_samp <- X_org[
    !is.na(sample_freq),
    .(id_coalition, coalitions, coalitions_str, coalition_size, N, shapley_weight, sample_freq)
  ]
  X_keep_nrow <- X_keep[, .N]

  n_coalitions_boot <- X_samp[, sum(sample_freq)]

  if (paired_shap_sampling) {
    # Sample with replacement
    X_boot00 <- X_samp[
      sample.int(
        n = .N,
        size = ceiling(n_coalitions_boot * n_boot_samps / 2),
        replace = TRUE,
        prob = sample_freq
      ),
      .(id_coalition, coalitions, coalition_size, N, sample_freq)
    ]

    X_boot00[, boot_id := rep(seq(n_boot_samps), times = n_coalitions_boot / 2)]

    X_boot00_paired <- copy(X_boot00[, .(coalitions, boot_id)])
    X_boot00_paired[, coalitions := lapply(coalitions, function(x) seq(n_shapley_values)[-x])]
    X_boot00_paired[, coalitions_str := sapply(coalitions, paste, collapse = " ")]

    # Extract the paired coalitions from X_samp
    X_boot00_paired <- merge(X_boot00_paired,
      X_samp[, .(id_coalition, coalition_size, N, shapley_weight, coalitions_str)],
      by = "coalitions_str"
    )
    X_boot0 <- rbind(
      X_boot00[, .(boot_id, id_coalition, coalitions, coalition_size, N)],
      X_boot00_paired[, .(boot_id, id_coalition, coalitions, coalition_size, N)]
    )

    # Create the Shapley weight column in X_boot0 so we can row-bind with X_keep
    X_boot0[, shapley_weight := NA]
    X_boot <- rbind(X_keep[rep(seq(X_keep_nrow),
      each = n_boot_samps
    ), ][, boot_id := rep(seq(n_boot_samps), times = X_keep_nrow)], X_boot0)
    setkey(X_boot, boot_id, id_coalition)

    # Compute the Shapley weight for sampled coalitions by counting their sampling frequencies
    # Note: deterministic coalitions retain their original weight
    X_boot[is.na(shapley_weight), sample_freq := .N, by = .(id_coalition, boot_id)]
    X_boot <- unique(X_boot, by = c("id_coalition", "boot_id"))
    X_boot[is.na(shapley_weight), shapley_weight := as.numeric(sample_freq)]
  } else {
    X_boot0 <- X_samp[
      sample.int(
        n = .N,
        size = n_coalitions_boot * n_boot_samps,
        replace = TRUE,
        prob = sample_freq
      ),
      .(id_coalition, coalitions, coalition_size, N)
    ]
    X_boot <- rbind(X_keep[rep(1:2, each = n_boot_samps), -"shapley_weight"], X_boot0)
    X_boot[, boot_id := rep(seq(n_boot_samps), times = n_coalitions_boot + 2)]

    setkey(X_boot, boot_id, id_coalition)
    X_boot[, sample_freq := .N, by = .(id_coalition, boot_id)]
    X_boot <- unique(X_boot, by = c("id_coalition", "boot_id"))
    X_boot[, shapley_weight := as.numeric(sample_freq)]
    if (type == "forecast") {
      id_coalition_mapper_dt <- internal$iter_list[[iter]]$id_coalition_mapper_dt
      full_ids <- id_coalition_mapper_dt$id_coalition[id_coalition_mapper_dt$full]
      X_boot[coalition_size == 0 | id_coalition %in% full_ids, shapley_weight := X_org[1, shapley_weight]]
      X_boot[coalition_size == 0 | id_coalition %in% full_ids, sample_freq := NA_integer_]
    } else {
      X_boot[coalition_size %in% c(0, n_shapley_values), shapley_weight := X_org[1, shapley_weight]]
      X_boot[coalition_size %in% c(0, n_shapley_values), sample_freq := NA_integer_]
    }
  }

  for (i in seq_len(n_boot_samps)) {
    this_X <- X_boot[boot_id == i] # This is highly inefficient, but the best way to deal with the reweighting for now

    # Split into the coalitions that have been deterministically included and the sampled ones
    this_X_keep <- this_X[is.na(sample_freq)]
    this_X_samp <- this_X[!is.na(sample_freq)]

    # Reweight the sampled coalitions
    kernelSHAP_reweighting(
      X = this_X_samp,
      m = n_shapley_values,
      reweight = shapley_reweight,
      max_fixed_coal_size = max_fixed_coal_size,
      n_coal_each_size = n_coal_each_size
    )

    # For semi-deterministic sampling, reweight the sampled coalitions. The deterministic ones are already reweighted.
    if (semi_deterministic_sampling) {
      weight_sample <- internal$iter_list[[iter]]$dt_coal_samp_info$weight_sample
      this_X_samp[, shapley_weight := weight_sample * shapley_weight / sum(shapley_weight)]
    }

    # Merge deterministic and sampled coalitions. Use that the coalitions in this_X_keep are paired.
    this_X <- data.table::rbindlist(list(this_X_keep[seq(1, .N / 2)], this_X_samp, this_X_keep[seq((.N / 2) + 1, .N)]),
      use.names = TRUE
    )

    W_boot <- weight_matrix(
      X = this_X,
      normalize_W_weights = TRUE
    )

    kshap_boot <- t(W_boot %*% as.matrix(dt_vS[id_coalition %in% X_boot[
      boot_id == i,
      id_coalition
    ], -"id_coalition"]))

    boot_sd_array[, , i] <- copy(kshap_boot)
  }

  std_dev_mat <- apply(boot_sd_array, c(1, 2), sd)

  dt_kshap_boot_sd <- data.table::as.data.table(std_dev_mat)
  colnames(dt_kshap_boot_sd) <- c("none", shap_names)
  dt_kshap_boot_sd[, none := 0] # Hard-set to 0, as it is zero by definition

  return(dt_kshap_boot_sd)
}
