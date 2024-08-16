#' Computes the Shapley values given `v(S)`
#'
#' @inherit explain
#' @inheritParams default_doc
#' @param vS_list List
#' Output from [compute_vS()]
#'
#' @export
compute_estimates <- function(internal, vS_list) {
  iter <- length(internal$iter_list)
  compute_sd <- internal$iter_list[[iter]]$compute_sd

  n_boot_samps <- internal$parameters$adaptive_arguments$n_boot_samps

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  internal$timing_list$postprocess_vS <- Sys.time()


  # Compute the Shapley values
  dt_shapley_est <- compute_shapley_new(internal, processed_vS_list$dt_vS)

  internal$timing_list$compute_shapley <- Sys.time()

  if (compute_sd) {
    dt_shapley_sd <- bootstrap_shapley_new(internal, n_boot_samps = n_boot_samps, processed_vS_list$dt_vS)
  } else {
    dt_shapley_sd <- dt_shapley_est * 0
  }

  internal$timing_list$compute_bootstrap <- Sys.time()


  # Adding explain_id to the output dt
  dt_shapley_est[, explain_id := .I]
  setcolorder(dt_shapley_est, "explain_id")
  dt_shapley_sd[, explain_id := .I]
  setcolorder(dt_shapley_sd, "explain_id")


  internal$iter_list[[iter]]$dt_shapley_est <- dt_shapley_est
  internal$iter_list[[iter]]$dt_shapley_sd <- dt_shapley_sd
  internal$iter_list[[iter]]$vS_list <- vS_list
  internal$iter_list[[iter]]$dt_vS <- processed_vS_list$dt_vS

  # internal$timing$shapley_computation <- Sys.time()

  # Clearing out the tmp list with model and predict_model (only added for AICc-types of empirical approach)
  internal$output <- processed_vS_list

  return(internal)
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

  dt_vS <- unique(dt_vS, by = id_combination) # To remove duplicated full pred row in the iterative procedure

  output <- list(
    dt_vS = dt_vS,
    dt_samp_for_vS = dt_samp_for_vS
  )
  return(output)
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
  type <- internal$parameters$type

  iter <- length(internal$iter_list)

  W <- internal$iter_list[[iter]]$W

  if (!is_groupwise) {
    shap_names <- feature_names
  } else {
    shap_names <- names(internal$parameters$group) # TODO: Add group_names (and shap_names) to internal earlier
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

bootstrap_shapley <- function(internal, dt_vS, n_boot_samps = 100, seed = 123) {
  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X

  set.seed(seed)

  X_org <- copy(X)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  shap_names <- internal$parameters$feature_names
  paired_shap_sampling <- internal$parameters$paired_shap_sampling
  shapley_reweight <- internal$parameters$shapley_reweighting

  boot_sd_array <- array(NA, dim = c(n_explain, n_features + 1, n_boot_samps))

  X_keep <- X_org[c(1, .N), .(id_combination, features, n_features, N, shapley_weight)]
  X_samp <- X_org[-c(1, .N), .(id_combination, features, n_features, N, shapley_weight, sample_freq)]
  X_samp[, features_tmp := sapply(features, paste, collapse = " ")]

  n_combinations_boot <- X_samp[, sum(sample_freq)]

  for (i in seq_len(n_boot_samps)) {
    if (paired_shap_sampling) {
      # Sample with replacement
      X_boot00 <- X_samp[
        sample.int(
          n = .N,
          size = ceiling(n_combinations_boot / 2),
          replace = TRUE,
          prob = sample_freq
        ),
        .(id_combination, features, n_features, N)
      ]

      X_boot00[, features_tmp := sapply(features, paste, collapse = " ")]
      # Not sure why I have to two the next two lines in two steps, but I don't get it to work otherwise
      boot_features_dup <- lapply(X_boot00$features, function(x) seq(n_features)[-x])
      X_boot00[, features_dup := boot_features_dup]
      X_boot00[, features_dup_tmp := sapply(features_dup, paste, collapse = " ")]

      # Extract the paired coalitions from X_samp
      X_boot00_paired <- merge(X_boot00[, .(features_dup_tmp)],
        X_samp[, .(id_combination, features, n_features, N, features_tmp)],
        by.x = "features_dup_tmp", by.y = "features_tmp"
      )
      X_boot0 <- rbind(
        X_boot00[, .(id_combination, features, n_features, N)],
        X_boot00_paired[, .(id_combination, features, n_features, N)]
      )
    } else {
      X_boot0 <- X_samp[
        sample.int(
          n = .N,
          size = n_combinations_boot,
          replace = TRUE,
          prob = sample_freq
        ),
        .(id_combination, features, n_features, N)
      ]
    }


    X_boot0[, shapley_weight := .N / n_combinations_boot, by = "id_combination"]
    X_boot0 <- unique(X_boot0, by = "id_combination")

    X_boot <- rbind(X_keep, X_boot0)
    data.table::setorder(X_boot, id_combination)

    shapley_reweighting(X_boot, reweight = shapley_reweight) # reweights the shapley weights by reference

    W_boot <- shapr::weight_matrix(
      X = X_boot,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    kshap_boot <- t(W_boot %*% as.matrix(dt_vS[id_combination %in% X_boot[, id_combination], -"id_combination"]))

    boot_sd_array[, , i] <- copy(kshap_boot)
  }

  std_dev_mat <- apply(boot_sd_array, c(1, 2), sd)

  dt_kshap_boot_sd <- data.table::as.data.table(std_dev_mat)
  colnames(dt_kshap_boot_sd) <- c("none", shap_names)

  return(dt_kshap_boot_sd)
}

bootstrap_shapley_new <- function(internal, dt_vS, n_boot_samps = 100, seed = 123) {
  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X

  set.seed(seed)

  is_groupwise <- internal$parameters$is_groupwise

  n_explain <- internal$parameters$n_explain
  paired_shap_sampling <- internal$parameters$paired_shap_sampling
  shapley_reweight <- internal$parameters$shapley_reweighting

  X_org <- copy(X)

  if (isFALSE(is_groupwise)) {
    n_features0 <- internal$parameters$n_features
    shap_names <- internal$parameters$feature_names
  } else {
    # Groupwise explanation
    # Temporary rename group objects to feature objects for code simplicity below

    n_features0 <- internal$parameters$n_groups
    shap_names <- names(internal$parameters$group)
    X_org[, n_features := n_groups]
    X_org[, features := groups]
  }

  boot_sd_array <- array(NA, dim = c(n_explain, n_features0 + 1, n_boot_samps))

  X_keep <- X_org[c(1, .N), .(id_combination, features, n_features, N, shapley_weight)]
  X_samp <- X_org[-c(1, .N), .(id_combination, features, n_features, N, shapley_weight, sample_freq)]
  X_samp[, features_tmp := sapply(features, paste, collapse = " ")]

  n_combinations_boot <- X_samp[, sum(sample_freq)]

  ### Currently only supporting non-paired sampling

  X_boot0 <- X_samp[
    sample.int(
      n = .N,
      size = n_combinations_boot * n_boot_samps,
      replace = TRUE,
      prob = sample_freq
    ),
    .(id_combination, features, n_features, N, shapley_weight)
  ]
  X_boot <- rbind(X_keep[rep(1:2, each = n_boot_samps), ], X_boot0)
  X_boot[, boot_id := rep(seq(n_boot_samps), times = n_combinations_boot + 2)]

  setkey(X_boot, boot_id, id_combination)
  X_boot[, shapley_weight := .N / n_combinations_boot, by = .(id_combination, boot_id)]
  X_boot <- unique(X_boot, by = c("id_combination", "boot_id"))
  X_boot[, shapley_weight := mean(shapley_weight), by = .(N, boot_id)]
  X_boot[n_features %in% c(0, n_features0), shapley_weight := X_keep[1, shapley_weight]]

  for (i in seq_len(n_boot_samps)) {
    W_boot <- shapr::weight_matrix(
      X = X_boot[boot_id == i],
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    kshap_boot <- t(W_boot %*% as.matrix(dt_vS[id_combination %in% X_boot[boot_id == i,
                                                                          id_combination], -"id_combination"]))

    boot_sd_array[, , i] <- copy(kshap_boot)
  }

  std_dev_mat <- apply(boot_sd_array, c(1, 2), sd)

  dt_kshap_boot_sd <- data.table::as.data.table(std_dev_mat)
  colnames(dt_kshap_boot_sd) <- c("none", shap_names)

  return(dt_kshap_boot_sd)
}
