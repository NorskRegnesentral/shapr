library(Rcpp)
Rcpp::sourceCpp("R/frida_utils.cpp")

#'
#' @inherit explain
#' @inheritParams default_doc
#' @param vS_list List
#' Output from [compute_vS()]
#'
#' @export
compute_estimates <- function(internal, vS_list) {

  internal$timing_list$compute_vS <- Sys.time()


  iter <- length(internal$iter_list)
  compute_sd <- internal$iter_list[[iter]]$compute_sd
  adaptive <- internal$parameters$adaptive

  n_boot_samps <- internal$parameters$adaptive_arguments$n_boot_samps

  processed_vS_list <- postprocess_vS_list(
    vS_list = vS_list,
    internal = internal
  )

  internal$timing_list$postprocess_vS <- Sys.time()

  dt_vS = processed_vS_list$dt_vS

  # Compute the Shapley values
  dt_shapley_est <- compute_shapley_new(internal, processed_vS_list$dt_vS)
  if (adaptive){
    if (internal$parameters$adaptive_arguments$allow_feature_reduction){
      keep = internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_est
      dt_shapley_est = cbind(dt_shapley_est, keep)
      # First, in compute_shapley_new, we add the sum of the shapley values of the removed features
      # to prediction_zero. Then, we must revert prediction_zero to the original
      # to ensure correctness.
      dt_shapley_est$none = internal$parameters$prediction_zero
      setcolorder(dt_shapley_est, c("none", internal$parameters$shap_names_org))
    }
  }
  # print(dt_shapley_est)
  # print(sum(dt_shapley_est))
  # internal <- compute_shapley_frida(internal, processed_vS_list$dt_vS)

  # shapley_frida <- internal$iter_list[[iter]]$frida_shapley_values

  # inds = which(colnames(dt_shapley_est) %in% internal$parameters$feature_names )
  # print(sum(abs(dt_shapley_est[, ..inds] - shapley_frida))/length(shapley_frida))
  # print(dt_shapley_est[, ..inds] - shapley_frida)

  internal$timing_list$compute_shapley <- Sys.time()

  if (compute_sd) {
    dt_shapley_sd <- bootstrap_shapley_new(internal, n_boot_samps = n_boot_samps, processed_vS_list$dt_vS)
    # dt_shapley_sd2 <- bootstrap_shapley_new(internal, n_boot_samps = n_boot_samps, processed_vS_list$dt_vS, seed = 685153)
    # internal <- bootstrap_shapley_frida(internal, n_boot_samps = n_boot_samps)
    # frida_boot_sd <- internal$iter_list[[iter]]$frida_boot_shapley_values

    # inds = which(colnames(dt_shapley_sd) %in% internal$parameters$feature_names )

    # # print(dt_shapley_sd[, ..inds])
    # # print(frida_boot_shapley_values)
    # print(sum(abs(dt_shapley_sd[, ..inds] - frida_boot_sd))/length(frida_boot_sd))
    # # print(sum(abs(dt_shapley_sd2[, ..inds] - frida_boot_sd))/length(frida_boot_sd))
    # # print(sum(abs(dt_shapley_sd[, ..inds] - dt_shapley_sd2[, ..inds] ))/length(frida_boot_sd))
    # writeLines(" ")

    # Combine with last SD estimate for dropped variables
    if (adaptive){
      if (internal$parameters$adaptive_arguments$allow_feature_reduction){
        keep = internal$iter_list[[iter]]$shap_reduction$reduced_dt_shapley_sd
        dt_shapley_sd = cbind(dt_shapley_sd, keep)
        setcolorder(dt_shapley_sd, c("none", internal$parameters$shap_names_org))
      }
    }
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
  keep_samp_for_vS <- internal$parameters$keep_samp_for_vS
  prediction_zero <- internal$parameters$prediction_zero
  n_explain <- internal$parameters$n_explain

  iter <- length(internal$iter_list)
  sum_reduced_shapley_est <- internal$iter_list[[iter]]$shap_reduction$sum_reduced_shapley_est
  adaptive <- internal$parameters$adaptive

  if (adaptive){
    if (internal$parameters$adaptive_arguments$allow_feature_reduction & (!is.null(sum_reduced_shapley_est))){
      prediction_zero = prediction_zero + sum_reduced_shapley_est
    }
  }
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

    data.table::setorder(dt_samp_for_vS, id_coalition)
  } else {
    names(dt_vS0) <- names(vS_list[[1]])

    vS_list[[length(vS_list) + 1]] <- dt_vS0

    dt_vS <- rbindlist(vS_list)
    dt_samp_for_vS <- NULL
  }

  data.table::setorder(dt_vS, id_coalition)

  dt_vS <- unique(dt_vS, by = "id_coalition") # To remove duplicated full pred row in the iterative procedure

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
  type <- internal$parameters$type

  iter <- length(internal$iter_list)

  W <- internal$iter_list[[iter]]$W

  shap_names <- internal$parameters$shap_names

  # If multiple horizons with explain_forecast are used, we only distribute value to those used at each horizon
  if (type == "forecast") {
    id_coalition_mapper_dt <- internal$objects$id_coalition_mapper_dt
    horizon <- internal$parameters$horizon
    cols_per_horizon <- internal$objects$cols_per_horizon
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

compute_A <- function(A, X, S, n_row_all, n_row_this_iter){
  n_features = ncol(S)

  # A_new = matrix(0, nrow = n_features, n_features)
  # for (i in 2:(nrow(S) - 1)) {
  #   A_new = A_new + X[i, shapley_weight] * S[i, ]%*%t(S[i, ])/n_row_this_iter
  # }

  A_new = create_A_new_cpp(S[2:(nrow(S) - 1), ], diag(X[2:(nrow(S) - 1), shapley_weight]))

  A = A * (n_row_all - n_row_this_iter)/n_row_all + A_new * n_row_this_iter/n_row_all

  return(A)
}

compute_b <- function(b, dt_vS, X, S, n_row_all, n_row_this_iter, p0){

  vS = dt_vS[, -"id_coalition"]
  X = X[-c(1, .N), ]
  S = S[-c(1, nrow(S)), ]
  vS = vS[-c(1, .N), ]

  b = b*(n_row_all - n_row_this_iter)/n_row_all + (t(vS) - p0) %*% (S*X[, shapley_weight]) / n_row_all

  return(b)
}

calculate_shapley_values_frida <- function(A, b, preds, p0){

  n_features = ncol(A)

  A_inv_one = as.vector(solve_cpp(A, as.matrix(rep(1, n_features))))
  A_inv_vec = solve_cpp(A, t(b))

  numerator = colSums(A_inv_vec) - (preds - p0)
  numerator = matrix(rep(as.numeric(numerator), n_features), nrow = n_features, byrow = TRUE)

  shapley_values = A_inv_vec - A_inv_one * numerator / sum(A_inv_one)
  return(t(shapley_values))
}

compute_shapley_frida <- function(internal, dt_vS){

  iter <- length(internal$iter_list)

  X = internal$iter_list[[iter]]$X
  S = internal$iter_list[[iter]]$S

  if (iter > 1){
    # Select which rows where samples in this iteration
    X_prev = internal$iter_list[[iter-1]]$X
    X_prev[, coalitions_tmp := sapply(coalitions, paste, collapse = " ")]
    X[, coalitions_tmp := sapply(coalitions, paste, collapse = " ")]

    merged = merge(X_prev[,.(coalitions_tmp,old_sample_freq=sample_freq)],X[,.(coalitions_tmp,new_sample_freq=sample_freq, id_coalition = id_coalition)],by="coalitions_tmp",all.y=TRUE,all.x=FALSE)
    setorder(merged, id_coalition)
    merged[is.na(old_sample_freq), old_sample_freq := 0]
    merged[,this_sample_freq:=new_sample_freq-old_sample_freq]

    merged[c(1, .N), this_sample_freq := 1]
    inds = which(merged$this_sample_freq > 0)

    S_curr = S[inds, ]
    X_curr = X[inds, ]

    X_curr[, "shapley_weight" := merged[inds, this_sample_freq]]
    X_curr[c(1, .N), shapley_weight := X[1, shapley_weight]]

    X_curr[, sample_freq := X_curr[, shapley_weight]]
    X_curr[c(1, .N), sample_freq := 1]


    dt_vS_curr = dt_vS[inds, ]
  } else {
    S_curr = S
    X_curr = X

    dt_vS_curr = dt_vS
  }

  internal$iter_list[[iter]]$S_curr = S_curr
  internal$iter_list[[iter]]$X_curr = X_curr
  internal$iter_list[[iter]]$dt_vS_curr = dt_vS_curr

  n_row_all = X[-c(1, .N), sum(shapley_weight)]

  # Effective number of rows in this iteration
  n_row_this_iter = X_curr[-c(1, .N), sum(shapley_weight)]

  n_features = internal$parameters$n_features
  if (iter == 1) {
    A = matrix(0, n_features, n_features)
    b = 0
  } else {
    A = internal$iter_list[[iter-1]]$A
    b = internal$iter_list[[iter-1]]$b
  }

  p0 = internal$parameters$prediction_zero
  if (adaptive){
    if (internal$parameters$adaptive_arguments$allow_feature_reduction){
      p0 = p0 + internal$iter_list[[iter]]$shap_reduction$sum_reduced_shapley_est
    }
  }
  preds = dt_vS_curr[.N, -"id_coalition"]

  A = compute_A(A, X_curr, S_curr, n_row_all, n_row_this_iter)
  internal$iter_list[[iter]]$A = A

  b = compute_b(b, dt_vS_curr, X_curr, S_curr, n_row_all, n_row_this_iter, p0)
  internal$iter_list[[iter]]$b = b

  shapley_values = calculate_shapley_values_frida(A, b, preds, p0)
  internal$iter_list[[iter]]$frida_shapley_values = shapley_values
  return(internal)
}

bootstrap_shapley <- function(internal, dt_vS, n_boot_samps = 100, seed = 123) {
  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X

  set.seed(seed)

  X_org <- copy(X)
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  shap_names <- internal$parameters$shap_names
  paired_shap_sampling <- internal$parameters$paired_shap_sampling
  shapley_reweight <- internal$parameters$shapley_reweighting

  boot_sd_array <- array(NA, dim = c(n_explain, n_features + 1, n_boot_samps))

  X_keep <- X_org[c(1, .N), .(id_coalition, features, n_features, N, shapley_weight)]
  X_samp <- X_org[-c(1, .N), .(id_coalition, features, n_features, N, shapley_weight, sample_freq)]
  X_samp[, features_tmp := sapply(features, paste, collapse = " ")]

  n_coalitions_boot <- X_samp[, sum(sample_freq)]

  for (i in seq_len(n_boot_samps)) {
    if (paired_shap_sampling) {
      # Sample with replacement
      X_boot00 <- X_samp[
        sample.int(
          n = .N,
          size = ceiling(n_coalitions_boot / 2),
          replace = TRUE,
          prob = sample_freq
        ),
        .(id_coalition, features, n_features, N)
      ]

      X_boot00[, features_tmp := sapply(features, paste, collapse = " ")]
      # Not sure why I have to two the next two lines in two steps, but I don't get it to work otherwise
      boot_features_dup <- lapply(X_boot00$features, function(x) seq(n_features)[-x])
      X_boot00[, features_dup := boot_features_dup]
      X_boot00[, features_dup_tmp := sapply(features_dup, paste, collapse = " ")]

      # Extract the paired coalitions from X_samp
      X_boot00_paired <- merge(X_boot00[, .(features_dup_tmp)],
        X_samp[, .(id_coalition, features, n_features, N, features_tmp)],
        by.x = "features_dup_tmp", by.y = "features_tmp"
      )
      X_boot0 <- rbind(
        X_boot00[, .(id_coalition, features, n_features, N)],
        X_boot00_paired[, .(id_coalition, features, n_features, N)]
      )
    } else {
      X_boot0 <- X_samp[
        sample.int(
          n = .N,
          size = n_coalitions_boot,
          replace = TRUE,
          prob = sample_freq
        ),
        .(id_coalition, features, n_features, N)
      ]
    }


    X_boot0[, shapley_weight := .N / n_coalitions_boot, by = "id_coalition"]
    X_boot0 <- unique(X_boot0, by = "id_coalition")

    X_boot <- rbind(X_keep, X_boot0)
    data.table::setorder(X_boot, id_coalition)

    shapley_reweighting(X_boot, reweight = shapley_reweight) # reweights the shapley weights by reference

    W_boot <- shapr::weight_matrix(
      X = X_boot,
      normalize_W_weights = TRUE,
      is_groupwise = FALSE
    )

    kshap_boot <- t(W_boot %*% as.matrix(dt_vS[id_coalition %in% X_boot[, id_coalition], -"id_coalition"]))

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
  shap_names <- internal$parameters$shap_names
  n_shapley_values <- internal$parameters$n_shapley_values


  X_org <- copy(X)

  boot_sd_array <- array(NA, dim = c(n_explain, n_shapley_values + 1, n_boot_samps))

  X_keep <- X_org[c(1, .N), .(id_coalition, coalitions, coalition_size, N)]
  X_samp <- X_org[-c(1, .N), .(id_coalition, coalitions, coalition_size, N, shapley_weight, sample_freq)]
  X_samp[, coalitions_tmp := sapply(coalitions, paste, collapse = " ")]

  n_coalitions_boot <- X_samp[, sum(sample_freq)]

  if (paired_shap_sampling) {
    # Sample with replacement
    X_boot00 <- X_samp[
      sample.int(
        n = .N,
        size = ceiling(n_coalitions_boot * n_boot_samps / 2),
        # size = ceiling(n_coalitions_boot / 2)* n_boot_samps,
        replace = TRUE,
        prob = sample_freq
      ),
      .(id_coalition, coalitions, coalition_size, N, sample_freq)
    ]

    X_boot00[, boot_id := rep(seq(n_boot_samps), times = n_coalitions_boot/2)]
    # X_boot00[, boot_id := rep(seq(n_boot_samps), times = ceiling(n_coalitions_boot/2))]


    X_boot00_paired <- copy(X_boot00[,.(coalitions,boot_id)])
    X_boot00_paired[,coalitions:=lapply(coalitions, function(x) seq(n_shapley_values)[-x])]
    X_boot00_paired[, coalitions_tmp := sapply(coalitions, paste, collapse = " ")]

    # Extract the paired coalitions from X_samp
    X_boot00_paired <- merge(X_boot00_paired,
                             X_samp[, .(id_coalition, coalition_size , N, shapley_weight, coalitions_tmp)],
                             by = "coalitions_tmp"
    )
    X_boot0 <- rbind(
      X_boot00[, .(boot_id, id_coalition, coalitions , coalition_size     , N)],
      X_boot00_paired[, .(boot_id,id_coalition, coalitions, coalition_size, N)]
    )

    X_boot <- rbind(X_keep[rep(1:2, each = n_boot_samps), ][,boot_id:=rep(seq(n_boot_samps), times = 2)], X_boot0)
    setkey(X_boot, boot_id, id_coalition)
    X_boot[, sample_freq := .N / n_coalitions_boot, by = .(id_coalition, boot_id)]
    X_boot <- unique(X_boot, by = c("id_coalition", "boot_id"))
    X_boot[, shapley_weight := sample_freq]
    X_boot[coalition_size %in% c(0, n_shapley_values), shapley_weight := X_org[1, shapley_weight]]

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
  X_boot <- rbind(X_keep[rep(1:2, each = n_boot_samps), ], X_boot0)
  X_boot[, boot_id := rep(seq(n_boot_samps), times = n_coalitions_boot + 2)]

  setkey(X_boot, boot_id, id_coalition)
  X_boot[, sample_freq := .N / n_coalitions_boot, by = .(id_coalition, boot_id)]
  X_boot <- unique(X_boot, by = c("id_coalition", "boot_id"))
  X_boot[, shapley_weight := sample_freq]
  X_boot[coalition_size %in% c(0, n_shapley_values), shapley_weight := X_org[1, shapley_weight]]
  }

  for (i in seq_len(n_boot_samps)) {

    this_X <- X_boot[boot_id == i] # This is highly inefficient, but probably the best way to deal with the reweighting for now
    shapley_reweighting(this_X, reweight = shapley_reweight)

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

  return(dt_kshap_boot_sd)
}


bootstrap_shapley_frida <- function(internal, n_boot_samps = 100, seed = 123) {
  iter <- length(internal$iter_list)

  X_curr <- internal$iter_list[[iter]]$X_curr
  dt_vS_curr <- internal$iter_list[[iter]]$dt_vS_curr

  set.seed(seed)

  is_groupwise <- internal$parameters$is_groupwise

  n_explain <- internal$parameters$n_explain
  paired_shap_sampling <- internal$parameters$paired_shap_sampling
  shapley_reweight <- internal$parameters$shapley_reweighting
  shap_names <- internal$parameters$shap_names
  n_shapley_values <- internal$parameters$n_shapley_values


  X_org <- copy(X_curr)

  boot_sd_array <- array(NA, dim = c(n_explain, n_shapley_values, n_boot_samps))

  X_keep <- X_org[c(1, .N), .(id_coalition, coalitions, coalition_size, N)]
  X_samp <- X_org[-c(1, .N), .(id_coalition, coalitions, coalition_size, N, shapley_weight, sample_freq)]
  X_samp[, coalitions_tmp := sapply(coalitions, paste, collapse = " ")]
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

    X_boot00[, boot_id := rep(seq(n_boot_samps), times = n_coalitions_boot/2)]


    X_boot00_paired <- copy(X_boot00[,.(coalitions,boot_id)])
    X_boot00_paired[,coalitions:=lapply(coalitions, function(x) seq(n_shapley_values)[-x])]
    X_boot00_paired[, coalitions_tmp := sapply(coalitions, paste, collapse = " ")]

    # Extract the paired coalitions from X_samp
    X_boot00_paired <- merge(X_boot00_paired,
                             X_samp[, .(id_coalition, coalition_size , N, shapley_weight, coalitions_tmp)],
                             by = "coalitions_tmp"
    )
    X_boot0 <- rbind(
      X_boot00[, .(boot_id, id_coalition, coalitions , coalition_size     , N)],
      X_boot00_paired[, .(boot_id,id_coalition, coalitions, coalition_size, N)]
    )

    X_boot <- rbind(X_keep[rep(1:2, each = n_boot_samps), ][,boot_id:=rep(seq(n_boot_samps), times = 2)], X_boot0)
    setkey(X_boot, boot_id, id_coalition)
    # I Martin sin versjon er det X_boot[, sample_freq := .N/n_coalitions_boot, by = .(id_coalition, boot_id)]
    # Tror jeg må ha det sånn som under med de nåværende versjonene av compute_A(), compute_b() og compute_shapley_values_frida()
    # MÅ sjekke nøye før man endrer tilbake til Martin sin versjon
    X_boot[, sample_freq := .N, by = .(id_coalition, boot_id)]
    X_boot <- unique(X_boot, by = c("id_coalition", "boot_id"))
    X_boot[, shapley_weight := sample_freq]
    X_boot[coalition_size %in% c(0, n_shapley_values), shapley_weight := X_org[1, shapley_weight]]

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
    X_boot <- rbind(X_keep[rep(1:2, each = n_boot_samps), ], X_boot0)
    X_boot[, boot_id := rep(seq(n_boot_samps), times = n_coalitions_boot + 2)]

    setkey(X_boot, boot_id, id_coalition)
    # I Martin sin versjon er det X_boot[, sample_freq := .N/n_coalitions_boot, by = .(id_coalition, boot_id)]
    # Tror jeg må ha det sånn som under med de nåværende versjonene av compute_A(), compute_b() og compute_shapley_values_frida()
    # MÅ sjekke nøye før man endrer tilbake til Martin sin versjon
    X_boot[, sample_freq := .N, by = .(id_coalition, boot_id)]
    X_boot <- unique(X_boot, by = c("id_coalition", "boot_id"))
    X_boot[, shapley_weight := sample_freq]
    X_boot[coalition_size %in% c(0, n_shapley_values), shapley_weight := X_org[1, shapley_weight]]
  }

  n_features = internal$parameters$n_features
  n_row_all = internal$iter_list[[iter]]$X[-c(1, .N), sum(shapley_weight)]

  p0 = internal$parameters$prediction_zero
  if (adaptive){
    if (internal$parameters$adaptive_arguments$allow_feature_reduction){
      p0 = p0 + internal$iter_list[[iter]]$shap_reduction$sum_reduced_shapley_est
    }
  }
  preds = dt_vS_curr[.N, -"id_coalition"]

  n_explain = internal$parameters$n_explain

  if (iter == 1) {
    A_list = replicate(n_boot_samps, matrix(0, n_features, n_features), simplify = FALSE)
    b_list = replicate(n_boot_samps, matrix(0, n_explain, n_features), simplify = FALSE)
  } else {
    A_list = internal$iter_list[[iter-1]]$A_boot
    b_list = internal$iter_list[[iter-1]]$b_boot
  }

  S_curr = internal$iter_list[[iter]]$S_curr

  for (i in seq_len(n_boot_samps)) {

    this_X <- X_boot[boot_id == i] # This is highly inefficient, but probably the best way to deal with the reweighting for now
    inds = X_curr[, .I[id_coalition %in% this_X$id_coalition]]
    this_S = S_curr[inds, ]
    this_dt_vS = dt_vS_curr[inds, ]

    # shapley_reweighting(this_X, reweight = shapley_reweight) # TODO: Implement reweighting for Frida

    # Effective number of rows in this iteration
    n_row_this_iter = this_X[-c(1, .N), sum(shapley_weight)]
    A_list[[i]] = compute_A(A_list[[i]], this_X, this_S, n_row_all, n_row_this_iter)

    b_list[[i]] = compute_b(b_list[[i]], this_dt_vS, this_X, this_S, n_row_all, n_row_this_iter, p0)
    boot_sd_array[, , i] = calculate_shapley_values_frida(A_list[[i]], b_list[[i]], preds, p0)
  }

  std_dev_mat <- apply(boot_sd_array, c(1, 2), sd)

  internal$iter_list[[iter]]$A_boot = A_list
  internal$iter_list[[iter]]$b_boot = b_list

  dt_kshap_boot_sd <- data.table::as.data.table(std_dev_mat)
  # colnames(dt_kshap_boot_sd) <- c("none", shap_names)

  internal$iter_list[[iter]]$frida_boot_shapley_values = std_dev_mat
  return(internal)
}
