#' Set up the kernelSHAP framework
#'
#' @inheritParams default_doc_explain
#'
#' @export
#' @keywords internal
shapley_setup <- function(internal) {
  verbose <- internal$parameters$verbose
  n_shapley_values <- internal$parameters$n_shapley_values
  n_features <- internal$parameters$n_features
  approach <- internal$parameters$approach
  is_groupwise <- internal$parameters$is_groupwise
  paired_shap_sampling <- internal$parameters$paired_shap_sampling
  shapley_reweighting <- internal$parameters$shapley_reweighting
  coal_feature_list <- internal$objects$coal_feature_list


  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  exact <- internal$iter_list[[iter]]$exact
  prev_coal_samples <- internal$iter_list[[iter]]$prev_coal_samples

  if ("progress" %in% verbose) {
    cli::cli_progress_step("Sampling coalitions")
  }


  X <- create_coalition_table(
    m = n_shapley_values,
    exact = exact,
    n_coalitions = n_coalitions,
    weight_zero_m = 10^6,
    paired_shap_sampling = paired_shap_sampling,
    prev_coal_samples = prev_coal_samples,
    coal_feature_list = coal_feature_list,
    approach0 = approach,
    shapley_reweighting = shapley_reweighting
  )



  coalition_map <- X[, .(id_coalition,
    coalitions_str = sapply(coalitions, paste, collapse = " ")
  )]


  # Get weighted matrix ----------------
  W <- weight_matrix(
    X = X,
    normalize_W_weights = TRUE
  )


  ## Get feature matrix ---------
  S <- coalition_matrix_cpp(
    coalitions = X[["features"]],
    m = n_features
  )

  #### Updating parameters ####

  # Updating parameters$exact as done in create_coalition_table. I don't think this is necessary now. TODO: Check.
  # Moreover, it does not apply to grouping, so must be adjusted anyway.
  if (!exact && n_coalitions >= 2^n_shapley_values) {
    internal$iter_list[[iter]]$exact <- TRUE
    internal$parameters$exact <- TRUE # Since this means that all coalitions have been sampled
  }

  # Updating n_coalitions in the end based on what is actually used. I don't think this is necessary now. TODO: Check.
  internal$iter_list[[iter]]$n_coalitions <- nrow(S)

  # This will be obsolete later
  internal$parameters$group_num <- NULL # TODO: Checking whether I could just do this processing where needed
  # instead of storing it


  if (isFALSE(exact)) {
    # Storing the feature samples
    repetitions <- X[-c(1, .N), sample_freq]

    unique_coal_samples <- X[-c(1, .N), coalitions]

    coal_samples <- unlist(
      lapply(
        seq_along(unique_coal_samples),
        function(i) {
          rep(
            list(unique_coal_samples[[i]]),
            repetitions[i]
          )
        }
      ),
      recursive = FALSE
    )
  } else {
    coal_samples <- NA
  }

  internal$iter_list[[iter]]$X <- X
  internal$iter_list[[iter]]$W <- W
  internal$iter_list[[iter]]$S <- S
  internal$iter_list[[iter]]$coalition_map <- coalition_map
  internal$iter_list[[iter]]$S_batch <- create_S_batch(internal)
  internal$iter_list[[iter]]$coal_samples <- coal_samples

  return(internal)
}

#' Define coalitions, and fetch additional information about each unique coalition
#'
#' @param m Positive integer.
#' Total number of features/groups.
#' @param exact Logical.
#' If `TRUE` all `2^m` coalitions are generated, otherwise a subsample of the coalitions is used.
#' @param n_coalitions Positive integer.
#' Note that if `exact = TRUE`, `n_coalitions` is ignored.
#' @param weight_zero_m Numeric.
#' The value to use as a replacement for infinite coalition weights when doing numerical operations.
#' @param paired_shap_sampling Logical.
#' Whether to do paired sampling of coalitions.
#' @param prev_coal_samples List.
#' A list of previously sampled coalitions.
#' @param approach0 Character vector.
#' Contains the approach to be used for eastimation of each coalition size. Same as `approach` in `explain()`.
#' @param coal_feature_list List.
#' A list mapping each coalition to the features it contains.
#' @inheritParams explain
#' @return A data.table with columns about the that contains the following columns:
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
#'
#' @examples
#' # All coalitions
#' x <- create_coalition_table(m = 3)
#' nrow(x) # Equals 2^3 = 8
#'
#' # Subsample of coalitions
#' x <- create_coalition_table(exact = FALSE, m = 10, n_coalitions = 1e2)
create_coalition_table <- function(m, exact = TRUE, n_coalitions = 200, weight_zero_m = 10^6,
                                   paired_shap_sampling = TRUE, prev_coal_samples = NULL,
                                   coal_feature_list = as.list(seq_len(m)),
                                   approach0 = "gaussian",
                                   shapley_reweighting = "none") {
  if (exact) {
    dt <- exact_coalition_table(m, weight_zero_m)
  } else {
    dt <- sample_coalition_table(m,
      n_coalitions,
      weight_zero_m,
      paired_shap_sampling = paired_shap_sampling,
      prev_coal_samples = prev_coal_samples,
      shapley_reweighting = shapley_reweighting
    )
    stopifnot(
      data.table::is.data.table(dt),
      !is.null(dt[["p"]])
    )
    p <- NULL # due to NSE notes in R CMD check
    dt[, p := NULL]
  }

  dt[, features := lapply(coalitions, FUN = coal_feature_mapper, coal_feature_list = coal_feature_list)]

  # Adding approach to X (needed for the combined approaches)
  if (length(approach0) > 1) {
    dt[!(coalition_size %in% c(0, m)), approach := approach0[coalition_size]]
  } else {
    dt[, approach := approach0]
  }


  return(dt)
}

#' @keywords internal
shapley_reweighting <- function(X, reweight = "on_N") {
  # Updates the shapley weights in X based on the reweighting strategy BY REFERENCE


  if (reweight == "on_N") {
    X[-c(1, .N), shapley_weight := mean(shapley_weight), by = N]
  } else if (reweight == "on_coal_size") {
    X[-c(1, .N), shapley_weight := mean(shapley_weight), by = coalition_size]
  } else if (reweight == "on_all") {
    m <- X[.N, coalition_size]
    X[-c(1, .N), shapley_weight := shapley_weights(
      m = m,
      N = N,
      n_components = coalition_size,
      weight_zero_m = 10^6
    ) / sum_shapley_weights(m)]
  } else if (reweight == "on_N_sum") {
    X[-c(1, .N), shapley_weight := sum(shapley_weight), by = N]
  } else if (reweight == "on_all_cond") {
    m <- X[.N, coalition_size]
    K <- X[, sum(sample_freq)]
    X[-c(1, .N), shapley_weight := shapley_weights(
      m = m,
      N = N,
      n_components = coalition_size,
      weight_zero_m = 10^6
    ) / sum_shapley_weights(m)]
    X[-c(1, .N), cond := 1 - (1 - shapley_weight)^K]
    X[-c(1, .N), shapley_weight := shapley_weight / cond]
  } else if (reweight == "on_all_cond_paired") {
    m <- X[.N, coalition_size]
    K <- X[, sum(sample_freq)]
    X[-c(1, .N), shapley_weight := shapley_weights(
      m = m,
      N = N,
      n_components = coalition_size,
      weight_zero_m = 10^6
    ) / sum_shapley_weights(m)]
    X[-c(1, .N), cond := 1 - (1 - 2 * shapley_weight)^(K / 2)]
    X[-c(1, .N), shapley_weight := 2 * shapley_weight / cond]
  }
  # strategy= "none" or something else do nothing
  return(NULL)
}


#' @keywords internal
exact_coalition_table <- function(m, weight_zero_m = 10^6) {
  dt <- data.table::data.table(id_coalition = seq(2^m))
  coalitions0 <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
  dt[, coalitions := unlist(coalitions0, recursive = FALSE)]
  dt[, coalition_size := length(coalitions[[1]]), id_coalition]
  dt[, N := .N, coalition_size]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m)]
  dt[, sample_freq := NA]
  return(dt)
}

#' @keywords internal
sample_coalition_table <- function(m,
                                   n_coalitions = 200,
                                   weight_zero_m = 10^6,
                                   paired_shap_sampling = TRUE,
                                   prev_coal_samples = NULL,
                                   shapley_reweighting) {
  # Setup
  coal_samp_vec <- seq(m - 1)
  n <- sapply(coal_samp_vec, choose, n = m)
  w <- shapley_weights(m = m, N = n, coal_samp_vec) * n
  p <- w / sum(w)


  if (!is.null(prev_coal_samples)) {
    coal_sample_all <- prev_coal_samples
    unique_samples <- length(unique(prev_coal_samples))
    n_coalitions <- min(2^m, n_coalitions)
    # Adjusts for the the unique samples, zero and m samples
  } else {
    coal_sample_all <- list()
    unique_samples <- 0
  }

  while (unique_samples < n_coalitions - 2) {
    if (paired_shap_sampling == TRUE) {
      n_samps <- ceiling((n_coalitions - unique_samples - 2) / 2) # Sample -2 as we add zero and m samples below
    } else {
      n_samps <- n_coalitions - unique_samples - 2 # Sample -2 as we add zero and m samples below
    }

    # Sample the coalition size ----------
    coal_size_sample <- sample(
      x = coal_samp_vec,
      size = n_samps,
      replace = TRUE,
      prob = p
    )

    # Sample specific coalitions -------
    coal_sample <- sample_features_cpp(m, coal_size_sample)
    if (paired_shap_sampling == TRUE) {
      coal_sample_paired <- lapply(coal_sample, function(x) seq(m)[-x])
      coal_sample_all <- c(coal_sample_all, coal_sample, coal_sample_paired)
    } else {
      coal_sample_all <- c(coal_sample_all, coal_sample)
    }
    unique_samples <- length(unique(coal_sample_all))
  }

  # Add zero and full prediction
  coal_sample_all <- c(list(integer(0)), coal_sample_all, list(c(1:m)))
  X <- data.table(coalition_size = sapply(coal_sample_all, length))
  X[, coalition_size := as.integer(coalition_size)]

  # Get number of occurences and duplicated rows-------
  is_duplicate <- NULL # due to NSE notes in R CMD check
  r <- helper_feature(m, coal_sample_all)
  X[, is_duplicate := r[["is_duplicate"]]]

  # When we sample coalitions the Shapley weight is equal
  # to the frequency of the given coalition
  X[, sample_freq := r[["sample_frequence"]]] # We keep an unscaled version of the sampling frequency for bootstrapping
  X[, shapley_weight := as.numeric(sample_freq)] # Convert to double for later calculations

  # Populate table and remove duplicated rows -------
  X[, coalitions := coal_sample_all]
  if (any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  X[, is_duplicate := NULL]
  data.table::setkeyv(X, "coalition_size")


  #### TODO: Check if this could be removed: ####
  ### Start of possible removal ###
  # Make feature list into character
  X[, coalitions_tmp := sapply(coalitions, paste, collapse = " ")]

  # Aggregate weights by how many samples of a coalition we observe
  X <- X[, .(
    coalition_size = data.table::first(coalition_size),
    shapley_weight = sum(shapley_weight),
    sample_freq = sum(sample_freq),
    coalitions = coalitions[1]
  ), coalitions_tmp]

  X[, coalitions_tmp := NULL]
  #### End of possible removal ####

  data.table::setorder(X, coalition_size)

  # Add shapley weight and number of coalitions
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[data.table::between(coalition_size, 1, m - 1)]]
  X[ind, p := p[coalition_size]]
  X[ind, N := n[coalition_size]]

  # Set column order and key table
  data.table::setkeyv(X, "coalition_size")
  X[, id_coalition := .I]
  X[, N := as.integer(N)]
  nms <- c("id_coalition", "coalitions", "coalition_size", "N", "shapley_weight", "p", "sample_freq")
  data.table::setcolorder(X, nms)

  shapley_reweighting(X, reweight = shapley_reweighting) # Reweights the shapley weights in X by reference

  return(X)
}


#' Calculate Shapley weight
#'
#' @param m Positive integer. Total number of features/feature groups.
#' @param n_components Positive integer. Represents the number of features/feature groups you want to sample from
#' a feature space consisting of `m` unique features/feature groups. Note that ` 0 < = n_components <= m`.
#' @param N Positive integer. The number of unique coalitions when sampling `n_components` features/feature
#' groups, without replacement, from a sample space consisting of `m` different features/feature groups.
#' @param weight_zero_m Positive integer. Represents the Shapley weight for two special
#' cases, i.e. the case where you have either `0` or `m` features/feature groups.
#'
#' @return Numeric
#' @keywords internal
#'
#' @author Nikolai Sellereite
shapley_weights <- function(m, N, n_components, weight_zero_m = 10^6) {
  x <- (m - 1) / (N * n_components * (m - n_components))
  x[!is.finite(x)] <- weight_zero_m
  x
}

#' @keywords internal
sum_shapley_weights <- function(m) {
  coal_samp_vec <- seq(m - 1)
  n <- sapply(coal_samp_vec, choose, n = m)
  w <- shapley_weights(m = m, N = n, coal_samp_vec) * n
  return(sum(w))
}


#' @keywords internal
helper_feature <- function(m, coal_sample) {
  x <- coalition_matrix_cpp(coal_sample, m)
  dt <- data.table::data.table(x)
  cnms <- paste0("V", seq(m))
  data.table::setnames(dt, cnms)
  dt[, sample_frequence := as.integer(.N), by = cnms]
  dt[, is_duplicate := duplicated(dt)]
  dt[, (cnms) := NULL]

  return(dt)
}




#' @keywords internal
coal_feature_mapper <- function(x, coal_feature_list) {
  if (length(x) != 0) {
    unlist(coal_feature_list[x])
  } else {
    integer(0)
  }
}

#' Calculate weighted matrix
#'
#' @param X data.table
#' @param normalize_W_weights Logical. Whether to normalize the weights for the coalitions to sum to 1 for
#' increased numerical stability before solving the WLS (weighted least squares). Applies to all coalitions
#' except coalition `1` and `2^m`.
#'
#' @return Numeric matrix. See [weight_matrix_cpp()] for more information.
#' @keywords internal
#'
#' @export
#' @author Nikolai Sellereite, Martin Jullum
weight_matrix <- function(X, normalize_W_weights = TRUE) {
  # Fetch weights
  w <- X[["shapley_weight"]]

  if (normalize_W_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  W <- weight_matrix_cpp(
    coalitions = X[["coalitions"]],
    m = X[.N][["coalition_size"]],
    n = X[, .N],
    w = w
  )
  return(W)
}

#' @keywords internal
create_S_batch_forecast <- function(internal, seed = NULL) { # This is temporary used for forecast only. to be removed
  n_shapley_values <- internal$parameters$n_shapley_values
  approach0 <- internal$parameters$approach
  n_batches <- 10 # TODO: fix this! similartly to how it is done in non-forecast

  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions

  X <- internal$objects$X

  if (!is.null(seed)) set.seed(seed)

  if (length(approach0) > 1) {
    X[!(n_coalitions %in% c(0, n_shapley_values)), approach := approach0[n_coalitions]]

    # Finding the number of batches per approach
    batch_count_dt <- X[!is.na(approach), list(
      n_batches_per_approach =
        pmax(1, round(.N / (n_coalitions - 2) * n_batches)),
      n_S_per_approach = .N
    ), by = approach]

    # Ensures that the number of batches corresponds to `n_batches`
    if (sum(batch_count_dt$n_batches_per_approach) != n_batches) {
      # Ensure that the number of batches is not larger than `n_batches`.
      # Remove one batch from the approach with the most batches.
      while (sum(batch_count_dt$n_batches_per_approach) > n_batches) {
        batch_count_dt[
          which.max(n_batches_per_approach),
          n_batches_per_approach := n_batches_per_approach - 1
        ]
      }

      # Ensure that the number of batches is not lower than `n_batches`.
      # Add one batch to the approach with most coalitions per batch
      while (sum(batch_count_dt$n_batches_per_approach) < n_batches) {
        batch_count_dt[
          which.max(n_S_per_approach / n_batches_per_approach),
          n_batches_per_approach := n_batches_per_approach + 1
        ]
      }
    }

    batch_count_dt[, n_leftover_first_batch := n_S_per_approach %% n_batches_per_approach]
    data.table::setorder(batch_count_dt, -n_leftover_first_batch)

    approach_vec <- batch_count_dt[, approach]
    n_batch_vec <- batch_count_dt[, n_batches_per_approach]

    # Randomize order before ordering spreading the batches on the different approaches as evenly as possible
    # with respect to shapley_weight
    X[, randomorder := sample(.N)]
    data.table::setorder(X, randomorder) # To avoid smaller id_coalitions always proceeding large ones
    data.table::setorder(X, shapley_weight)

    batch_counter <- 0
    for (i in seq_along(approach_vec)) {
      X[approach == approach_vec[i], batch := ceiling(.I / .N * n_batch_vec[i]) + batch_counter]
      batch_counter <- X[approach == approach_vec[i], max(batch)]
    }
  } else {
    X[!(n_coalitions %in% c(0, n_shapley_values)), approach := approach0]

    # Spreading the batches
    X[, randomorder := sample(.N)]
    data.table::setorder(X, randomorder)
    data.table::setorder(X, shapley_weight)
    X[!(coalition_size %in% c(0, n_shapley_values)), batch := ceiling(.I / .N * n_batches)]
  }

  # Assigning batch 1 (which always is the smallest) to the full prediction.
  X[, randomorder := NULL]
  X[id_coalition == max(id_coalition), batch := 1]
  setkey(X, id_coalition)

  # Create a list of the batch splits
  S_groups <- split(X[id_coalition != 1, id_coalition], X[id_coalition != 1, batch])

  return(S_groups)
}

#' @keywords internal
create_S_batch <- function(internal, seed = NULL) {
  n_shapley_values <- internal$parameters$n_shapley_values
  approach0 <- internal$parameters$approach

  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  n_batches <- internal$iter_list[[iter]]$n_batches

  exact <- internal$iter_list[[iter]]$exact


  coalition_map <- internal$iter_list[[iter]]$coalition_map


  X0 <- copy(internal$iter_list[[iter]]$X)

  if (iter > 1) {
    prev_coalition_map <- internal$iter_list[[iter - 1]]$coalition_map
    new_id_coalitions <- coalition_map[
      !(coalitions_str %in% prev_coalition_map[-c(1, .N), coalitions_str, ]),
      id_coalition
    ]
    X0 <- X0[id_coalition %in% new_id_coalitions]
  }

  # Reduces n_batches if it is larger than the number of new_id_coalitions
  n_batches <- min(n_batches, X0[, .N] - 2)


  if (!is.null(seed)) set.seed(seed)

  if (length(approach0) > 1) {
    X0[!(coalition_size %in% c(0, n_shapley_values)), approach := approach0[coalition_size]]

    # Finding the number of batches per approach
    batch_count_dt <- X0[!is.na(approach), list(
      n_batches_per_approach =
        pmax(1, round(.N / (n_coalitions - 2) * n_batches)),
      n_S_per_approach = .N
    ), by = approach]

    # Ensures that the number of batches corresponds to `n_batches`
    if (sum(batch_count_dt$n_batches_per_approach) != n_batches) {
      # Ensure that the number of batches is not larger than `n_batches`.
      # Remove one batch from the approach with the most batches.
      while (sum(batch_count_dt$n_batches_per_approach) > n_batches) {
        batch_count_dt[
          which.max(n_batches_per_approach),
          n_batches_per_approach := n_batches_per_approach - 1
        ]
      }

      # Ensure that the number of batches is not lower than `n_batches`.
      # Add one batch to the approach with most coalitions per batch
      while (sum(batch_count_dt$n_batches_per_approach) < n_batches) {
        batch_count_dt[
          which.max(n_S_per_approach / n_batches_per_approach),
          n_batches_per_approach := n_batches_per_approach + 1
        ]
      }
    }

    batch_count_dt[, n_leftover_first_batch := n_S_per_approach %% n_batches_per_approach]
    data.table::setorder(batch_count_dt, -n_leftover_first_batch)

    approach_vec <- batch_count_dt[, approach]
    n_batch_vec <- batch_count_dt[, n_batches_per_approach]

    # Randomize order before ordering spreading the batches on the different approaches as evenly as possible
    # with respect to shapley_weight
    X0[, randomorder := sample(.N)]
    data.table::setorder(X0, randomorder) # To avoid smaller id_coalitions always proceeding large ones
    data.table::setorder(X0, shapley_weight)

    batch_counter <- 0
    for (i in seq_along(approach_vec)) {
      X0[approach == approach_vec[i], batch := ceiling(.I / .N * n_batch_vec[i]) + batch_counter]
      batch_counter <- X0[approach == approach_vec[i], max(batch)]
    }
  } else {
    X0[!(coalition_size %in% c(0, n_shapley_values)), approach := approach0]

    # Spreading the batches
    X0[, randomorder := sample(.N)]
    data.table::setorder(X0, randomorder)
    data.table::setorder(X0, shapley_weight)
    X0[!(coalition_size %in% c(0, n_shapley_values)), batch := ceiling(.I / .N * n_batches)]
  }

  # Assigning batch 1 (which always is the smallest) to the full prediction.
  X0[, randomorder := NULL]
  X0[id_coalition == max(id_coalition), batch := 1]
  setkey(X0, id_coalition)

  # Create a list of the batch splits
  S_groups <- split(X0[id_coalition != 1, id_coalition], X0[id_coalition != 1, batch])

  return(S_groups)
}


#' Sets up everything for the Shapley values computation in [shapr::explain()]
#'
#' @inheritParams default_doc
#' @inheritParams explain
#' @inherit default_doc
#' @export
setup_computation <- function(internal, model, predict_model) {
  # model and predict_model are only needed for type AICc of approach empirical, otherwise ignored
  type <- internal$parameters$type

  # setup the Shapley framework
  internal <- if (type == "forecast") shapley_setup_forecast(internal) else shapley_setup(internal)

  # Setup for approach
  internal <- setup_approach(internal, model = model, predict_model = predict_model)

  return(internal)
}

#' @keywords internal
shapley_setup_forecast <- function(internal) {
  n_shapley_values <- internal$parameters$n_shapley_values
  n_features <- internal$parameters$n_features
  approach <- internal$parameters$approach
  is_groupwise <- internal$parameters$is_groupwise
  paired_shap_sampling <- internal$parameters$paired_shap_sampling
  shapley_reweighting <- internal$parameters$shapley_reweighting

  coal_feature_list <- internal$objects$coal_feature_list
  horizon <- internal$parameters$horizon
  feature_names <- internal$parameters$feature_names

  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  exact <- internal$iter_list[[iter]]$exact
  prev_coal_samples <- internal$iter_list[[iter]]$prev_coal_samples

  X_list <- W_list <- list()

  # Find columns to be included in each of the different horizons
  col_del_list <- list()
  col_del_list[[1]] <- numeric()
  if (horizon > 1) {
    k <- 2
    for (i in rev(seq_len(horizon)[-1])) {
      col_del_list[[k]] <- c(unlist(col_del_list[[k - 1]]), grep(paste0(".F", i), feature_names))
      k <- k + 1
    }
  }

  cols_per_horizon <- lapply(rev(col_del_list), function(x) if (length(x) > 0) feature_names[-x] else feature_names)

  horizon_features <- lapply(cols_per_horizon, function(x) which(internal$parameters$feature_names %in% x))

  # Apply create_coalition_table, weigth_matrix and coalition_matrix_cpp to each of the different horizons
  for (i in seq_along(horizon_features)) {
    # TODO: Somethis is not correct in these next 20 lines of code. Something was messed up after
    # Removing the group stuff and generalizing to coalitions.

    this_featcomb <- horizon_features[[i]]

    this_coal_feature_list <- lapply(coal_feature_list, function(x) x[x %in% this_featcomb])
    this_coal_feature_list <- this_coal_feature_list[sapply(this_coal_feature_list, function(x) length(x) != 0)]

    n_this_featcomb <- length(this_coal_feature_list)


    n_coalitions_here <- min(2^n_this_featcomb, n_coalitions)
    exact_here <- ifelse(n_coalitions_here == 2^n_this_featcomb, TRUE, exact)


    X_list[[i]] <- create_coalition_table(
      m = n_this_featcomb,
      exact = exact_here,
      n_coalitions = n_coalitions_here,
      weight_zero_m = 10^6,
      paired_shap_sampling = paired_shap_sampling,
      prev_coal_samples = prev_coal_samples,
      coal_feature_list = this_coal_feature_list,
      approach0 = approach,
      shapley_reweighting = FALSE
    )


    W_list[[i]] <- weight_matrix(
      X = X_list[[i]],
      normalize_W_weights = TRUE
    )
  }

  # Merge the coalition data.table to single one to use for computing conditional expectations later on
  X <- rbindlist(X_list, idcol = "horizon")
  X[, N := NA]
  X[, shapley_weight := NA]
  data.table::setorderv(X, c("coalition_size", "horizon"), order = c(1, -1))
  X[, horizon_id_coalition := id_coalition]
  X[, id_coalition := 0]
  X[!duplicated(coalitions), id_coalition := .I]
  X[, tmp_coalitions := as.character(coalitions)]
  X[, id_coalition := max(id_coalition), by = tmp_coalitions]
  X[, tmp_coalitions := NULL]

  # Extracts a data.table allowing mapping from X to X_list/W_list to be used in the compute_shapley function
  id_coalition_mapper_dt <- X[, .(horizon, horizon_id_coalition, id_coalition)]

  X[, horizon := NULL]
  X[, horizon_id_coalition := NULL]
  data.table::setorder(X, coalition_size)
  X <- X[!duplicated(id_coalition)]

  W <- NULL # Included for consistency. Necessary weights are in W_list instead

  ## Get feature matrix ---------
  S <- coalition_matrix_cpp(
    coalitions = X[["features"]],
    m = n_features
  )


  #### Updating parameters ####

  # Updating parameters$exact as done in create_coalition_table
  if (!exact && n_coalitions >= 2^n_shapley_values) {
    internal$parameters$exact <- TRUE # Note that this is exact only if all horizons use the exact method.
  }

  internal$iter_list[[iter]]$n_coalitions <- nrow(S) # Updating this parameter in the end based on what is used.

  # This will be obsolete later
  internal$parameters$group_num <- NULL # TODO: Checking whether I could just do this processing where needed
  # instead of storing it

  internal$objects$X <- X
  internal$objects$W <- W
  internal$objects$S <- S
  internal$objects$S_batch <- create_S_batch_forecast(internal)

  internal$objects$id_coalition_mapper_dt <- id_coalition_mapper_dt
  internal$objects$cols_per_horizon <- cols_per_horizon
  internal$objects$W_list <- W_list
  internal$objects$X_list <- X_list


  return(internal)
}
