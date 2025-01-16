#' Set up the kernelSHAP framework
#'
#' @inheritParams default_doc_export
#'
#' @export
#' @keywords internal
shapley_setup <- function(internal) {
  verbose <- internal$parameters$verbose
  n_shapley_values <- internal$parameters$n_shapley_values
  n_features <- internal$parameters$n_features
  approach <- internal$parameters$approach
  is_groupwise <- internal$parameters$is_groupwise
  paired_shap_sampling <- internal$parameters$extra_computation_args$paired_shap_sampling
  kernelSHAP_reweighting <- internal$parameters$extra_computation_args$kernelSHAP_reweighting
  coal_feature_list <- internal$objects$coal_feature_list
  causal_sampling <- internal$parameters$causal_sampling
  causal_ordering <- internal$parameters$causal_ordering
  causal_ordering_features <- internal$parameters$causal_ordering_features
  confounding <- internal$parameters$confounding
  dt_valid_causal_coalitions <- internal$objects$dt_valid_causal_coalitions # NULL if asymmetric is FALSE
  max_n_coalitions_causal <- internal$parameters$max_n_coalitions_causal # NULL if asymmetric is FALSE


  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  exact <- internal$iter_list[[iter]]$exact
  prev_coal_samples <- internal$iter_list[[iter]]$prev_coal_samples
  prev_coal_samples_n_unique <- internal$iter_list[[iter]]$prev_coal_samples_n_unique

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
    prev_coal_samples_n_unique = prev_coal_samples_n_unique,
    coal_feature_list = coal_feature_list,
    approach0 = approach,
    kernelSHAP_reweighting = kernelSHAP_reweighting,
    dt_valid_causal_coalitions = dt_valid_causal_coalitions
  )

  coalition_map <- X[, .(id_coalition, coalitions_str)]



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

  # Updating parameters$exact as done in create_coalition_table, if all coalitions have been sampled
  if (!exact && n_coalitions >= min(2^n_shapley_values, max_n_coalitions_causal)) {
    internal$iter_list[[iter]]$exact <- TRUE
    internal$parameters$exact <- TRUE
  }

  # Updating n_coalitions in the end based on what is actually used.
  internal$iter_list[[iter]]$n_coalitions <- nrow(S)
  # The number of sampled coalitions to be used for convergence detection only (exclude the zero and full prediction)
  internal$iter_list[[iter]]$n_sampled_coalitions <- internal$iter_list[[iter]]$n_coalitions - 2


  if (isFALSE(exact)) {
    coal_samples <- rep(X[-c(1, .N), coalitions_str], X[-c(1, .N), sample_freq])
  } else {
    coal_samples <- NA
  }

  internal$iter_list[[iter]]$X <- X
  internal$iter_list[[iter]]$W <- W
  internal$iter_list[[iter]]$S <- S
  internal$iter_list[[iter]]$coalition_map <- coalition_map
  internal$iter_list[[iter]]$S_batch <- create_S_batch(internal)
  internal$iter_list[[iter]]$coal_samples <- coal_samples
  internal$iter_list[[iter]]$coal_samples_n_unique <- nrow(X) - 2 # Subtract empty and grand coalition

  # If we are doing causal Shapley values, then get the step-wise data generating process for each coalition
  if (causal_sampling) {
    # Convert causal_ordering to be on the feature level also for group-wise Shapley values,
    # as shapr must know the features to include in each causal sampling step and not the group.
    causal_ordering <- if (is_groupwise) causal_ordering_features else causal_ordering
    S_causal_steps <- get_S_causal_steps(S = S, causal_ordering = causal_ordering, confounding = confounding)
    S_causal_steps_strings <-
      get_S_causal_steps(S = S, causal_ordering = causal_ordering, confounding = confounding, as_string = TRUE)

    # Find all unique set of features to condition on
    S_causal_unlist <- do.call(c, unlist(S_causal_steps, recursive = FALSE))
    S_causal_steps_unique <- unique(S_causal_unlist[grepl("\\.S(?!bar)", names(S_causal_unlist), perl = TRUE)]) # Get S
    S_causal_steps_unique <- S_causal_steps_unique[!sapply(S_causal_steps_unique, is.null)] # Remove NULLs
    S_causal_steps_unique <- S_causal_steps_unique[lengths(S_causal_steps_unique) > 0] # Remove extra integer(0)
    S_causal_steps_unique <- c(list(integer(0)), S_causal_steps_unique, list(seq(n_features)))
    S_causal_steps_unique_S <- coalition_matrix_cpp(coalitions = S_causal_steps_unique, m = n_features)

    # Insert into the internal list
    internal$iter_list[[iter]]$S_causal_steps <- S_causal_steps
    internal$iter_list[[iter]]$S_causal_steps_strings <- S_causal_steps_strings
    internal$iter_list[[iter]]$S_causal_steps_unique <- S_causal_steps_unique
    internal$iter_list[[iter]]$S_causal_steps_unique_S <- S_causal_steps_unique_S
  }

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
#' @param prev_coal_samples Character vector.
#' A vector of previously sampled coalitions as characters.
#' Each string contains a coalition and the feature indices in the coalition is separated by a space.
#' For example, "1 5 8" is a coalition with features 1, 5, and 8.
#' @param prev_coal_samples_n_unique Positive integer.
#' The number of unique coalitions in `prev_coal_samples`.
#' This is a separate argument to avoid recomputing the number unnecessarily.
#' @param n_samps_scale Positive integer.
#' Integer that scales the number of coalitions `n_coalitions` to sample as sampling is cheap,
#' while checking for `n_coalitions` unique coalitions is expensive, thus we over sample the
#' number of coalitions by a factor of `n_samps_scale` and determine when we have `n_coalitions` unique
#' coalitions and only use the coalitions up to this point and throw away the remaining coalitions.
#' @param approach0 Character vector.
#' Contains the approach to be used for estimation of each coalition size. Same as `approach` in [explain()].
#' @param coal_feature_list List.
#' A list mapping each coalition to the features it contains.
#' @param dt_valid_causal_coalitions data.table. Only applicable for asymmetric Shapley
#' values explanations, and is `NULL` for symmetric Shapley values.
#' The data.table contains information about the coalitions that respects the causal ordering.
#' @inheritParams explain
#' @return A data.table with info about the coalitions to use
#'
#' @keywords internal
#'
#' @author Nikolai Sellereite, Martin Jullum, Lars Henry Berge Olsen
#'
#' @examples
#' \dontrun{
#' # All coalitions
#' x <- create_coalition_table(m = 3)
#' nrow(x) # Equals 2^3 = 8
#'
#' # Subsample of coalitions
#' x <- shapr:::create_coalition_table(m = 10, exact = FALSE, n_coalitions = 1e2)
#' }
create_coalition_table <- function(m,
                                   exact = TRUE,
                                   n_coalitions = 200,
                                   weight_zero_m = 10^6,
                                   paired_shap_sampling = TRUE,
                                   prev_coal_samples = NULL,
                                   prev_coal_samples_n_unique = NULL,
                                   n_samps_scale = 10,
                                   coal_feature_list = as.list(seq_len(m)),
                                   approach0 = "gaussian",
                                   kernelSHAP_reweighting = "none",
                                   dt_valid_causal_coalitions = NULL) {
  if (exact) {
    dt <- exact_coalition_table(
      m = m,
      weight_zero_m = weight_zero_m,
      dt_valid_causal_coalitions = dt_valid_causal_coalitions
    )
  } else {
    dt <- sample_coalition_table(
      m = m,
      n_coalitions = n_coalitions,
      weight_zero_m = weight_zero_m,
      paired_shap_sampling = paired_shap_sampling,
      prev_coal_samples = prev_coal_samples,
      prev_coal_samples_n_unique = prev_coal_samples_n_unique,
      n_samps_scale = n_samps_scale,
      kernelSHAP_reweighting = kernelSHAP_reweighting,
      dt_valid_causal_coalitions = dt_valid_causal_coalitions
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
    dt[!(coalition_size %in% c(0, m)), approach := approach0]
  }

  return(dt)
}

#' @keywords internal
kernelSHAP_reweighting <- function(X, reweight = "on_N") {
  # Updates the shapley weights in X based on the reweighting strategy BY REFERENCE

  if (reweight == "on_N") {
    X[-c(1, .N), shapley_weight := mean(shapley_weight), by = N]
  } else if (reweight == "on_all") {
    m <- X[.N, coalition_size]
    X[-c(1, .N), shapley_weight := shapley_weights(
      m = m,
      N = N,
      n_components = coalition_size,
      weight_zero_m = 10^6
    ) / sum_shapley_weights(m)]
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
  }
  # strategy= "none" or something else do nothing
  return(NULL)
}

#' Get table with all (exact) coalitions
#'
#' @inheritParams create_coalition_table
#' @keywords internal
exact_coalition_table <- function(m,
                                  dt_valid_causal_coalitions = NULL,
                                  weight_zero_m = 10^6) {
  # Create all valid coalitions for regular/symmetric or asymmetric Shapley values
  if (is.null(dt_valid_causal_coalitions)) {
    # Regular/symmetric Shapley values: use all 2^m coalitions
    coalitions0 <- unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
  } else {
    # Asymmetric Shapley values: use only the coalitions that respect the causal ordering
    coalitions0 <- dt_valid_causal_coalitions[, coalitions]
  }

  dt <- data.table::data.table(id_coalition = seq_along(coalitions0))
  dt[, coalitions := coalitions0]
  dt[, coalitions_str := sapply(coalitions, paste, collapse = " ")]
  dt[, coalition_size := length(coalitions[[1]]), id_coalition]
  dt[, N := .N, coalition_size]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m)]
  dt[, sample_freq := NA]
  return(dt)
}

#' Get table with sampled coalitions
#'
#' @inheritParams create_coalition_table
#' @keywords internal
sample_coalition_table <- function(m,
                                   n_coalitions = 200,
                                   weight_zero_m = 10^6,
                                   paired_shap_sampling = TRUE,
                                   prev_coal_samples = NULL,
                                   prev_coal_samples_n_unique = NULL,
                                   kernelSHAP_reweighting,
                                   n_samps_scale = 10,
                                   dt_valid_causal_coalitions = NULL) {
  # Setup
  coal_samp_vec <- seq(m - 1)
  n <- choose(m, coal_samp_vec)
  w <- shapley_weights(m = m, N = n, coal_samp_vec) * n
  p <- w / sum(w)

  # Check if we are to do asymmetric or symmetric/regular Shapley values
  asymmetric <- !is.null(dt_valid_causal_coalitions)

  if (!is.null(prev_coal_samples)) {
    coal_sample_all <- prev_coal_samples
    unique_samples <- prev_coal_samples_n_unique
    n_coalitions <- min(2^m, n_coalitions)
    # Adjusts for the the unique samples, zero and m samples
  } else {
    coal_sample_all <- c()
    unique_samples <- 0
  }

  # Loop until we have drawn enough unique samples
  while (unique_samples < n_coalitions - 2) {
    # Get the number of samples to draw
    n_samps <- as.integer(n_coalitions * n_samps_scale / ifelse(paired_shap_sampling, 2, 1))

    # Sample the coalition sizes
    coal_size_sample <- sample(x = coal_samp_vec, size = n_samps, replace = TRUE, prob = p)

    # Sample the coalitions based on if we are computing regular/symmetric or asymmetric Shapley values
    if (asymmetric) {
      # Sample the causal coalitions from the valid causal coalitions with the Shapley weight as the probability
      # The weights of each coalition size is split evenly among the members of each coalition size, such that
      # all.equal(p, dt_valid_causal_coalitions[-c(1,.N), sum(shapley_weight_norm), by = coalition_size][, V1])
      coalitions <-
        dt_valid_causal_coalitions[-c(1, .N)][
          sample(.N, n_samps, replace = TRUE, prob = shapley_weight), coalitions_str
        ]
    } else {
      # Sample the (paired) coalitions as strings
      coalitions <- sample_coalitions_cpp_str_paired(m, coal_size_sample, paired_shap_sampling)
    }

    # Add the new coalitions to the previously sampled coalitions
    coal_sample_all <- c(coal_sample_all, coalitions)

    # Get the cumulative number of unique coalitions for each coalition in coal_sample_all
    dt_cumsum <- data.table(coalitions = coal_sample_all, N_S = cumsum(!duplicated(coal_sample_all)))[, L := .I]

    # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
    dt_N_S_and_L <- dt_cumsum[N_S != data.table::shift(N_S, type = "lag", fill = 0)]

    # Get the number of unique coalitions
    unique_samples <- dt_N_S_and_L[.N, N_S]
  }

  # Post processing: keep only the coalitions until n_coalitions - 2
  coal_sample_all <- coal_sample_all[seq(dt_N_S_and_L[N_S == n_coalitions - 2, L])]

  ## Create the X data table for the sampled coalitions
  X <- data.table(coalitions_str = coal_sample_all)[, .(sample_freq = .N), by = coalitions_str]
  X[, shapley_weight := as.numeric(sample_freq)]

  # Convert coalition strings to vector of integers as in old setup.
  X[, coalitions := lapply(strsplit(coalitions_str, " "), as.integer)]
  X[, coalition_size := as.integer(sapply(coalitions, length))] # as.integer to match old format
  X[, N := as.integer(n[coalition_size])] # use as.integer to match the format in the old code
  X[, p := p[coalition_size]]

  # Add the empty and grand coalitions to X
  X_empty_coalition <- data.table(
    coalitions_str = NA_character_, # list(character(0)) makes column into a list instead of character vector
    sample_freq = 1L,
    shapley_weight = weight_zero_m,
    coalitions = list(integer(0)), # empty coalition. Need to be list for this to be a data.table of one row
    coalition_size = 0L,
    N = 1L,
    p = NA
  )
  X_full_coalition <- data.table(
    coalitions_str = paste(seq(m), collapse = " "),
    sample_freq = 1L,
    shapley_weight = weight_zero_m,
    coalitions = list(seq(m)),
    coalition_size = as.integer(m),
    N = 1L,
    p = NA
  )
  X <- data.table::rbindlist(list(X_empty_coalition, X, X_full_coalition), use.names = TRUE)

  # Add id column and order the data table
  data.table::setkeyv(X, "coalition_size")
  data.table::setorder(X, "coalition_size")
  X[, id_coalition := .I]
  colorder <- c("id_coalition", "coalitions", "coalitions_str", "coalition_size", "N", "shapley_weight", "p")
  data.table::setcolorder(X, colorder)

  # Reweight the Shapley weights in X by reference
  kernelSHAP_reweighting(X, reweight = kernelSHAP_reweighting)

  return(X)
}


#' Calculate Shapley weight
#'
#' @param n_components Positive integer. Represents the number of features/feature groups you want to sample from
#' a feature space consisting of `m` unique features/feature groups. Note that ` 0 < = n_components <= m`.
#' @param N Positive integer. The number of unique coalitions when sampling `n_components` features/feature
#' groups, without replacement, from a sample space consisting of `m` different features/feature groups.
#' @inheritParams create_coalition_table
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
helper_coalition <- function(m, coal_sample) {
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
#' @param X data.table.
#' Output from [create_coalition_table()].
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
create_S_batch <- function(internal, seed = NULL) {
  n_shapley_values <- internal$parameters$n_shapley_values
  approach0 <- internal$parameters$approach
  type <- internal$parameters$type

  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  n_batches <- internal$iter_list[[iter]]$n_batches

  exact <- internal$iter_list[[iter]]$exact


  coalition_map <- internal$iter_list[[iter]]$coalition_map

  if (type == "forecast") {
    id_coalition_mapper_dt <- internal$iter_list[[iter]]$id_coalition_mapper_dt
    full_ids <- id_coalition_mapper_dt$id_coalition[id_coalition_mapper_dt$full]
  }

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
    if (type == "forecast") {
      X0[!(coalition_size == 0 | id_coalition %in% full_ids), approach := approach0[coalition_size]]
    } else {
      X0[!(coalition_size %in% c(0, n_shapley_values)), approach := approach0[coalition_size]]
    }

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
    if (type == "forecast") {
      X0[!(coalition_size == 0 | id_coalition %in% full_ids), approach := approach0]
    } else {
      X0[!(coalition_size %in% c(0, n_shapley_values)), approach := approach0]
    }

    # Spreading the batches
    X0[, randomorder := sample(.N)]
    data.table::setorder(X0, randomorder)
    data.table::setorder(X0, shapley_weight)
    if (type == "forecast") {
      X0[!(coalition_size == 0 | id_coalition %in% full_ids), batch := ceiling(.I / .N * n_batches)]
    } else {
      X0[!(coalition_size %in% c(0, n_shapley_values)), batch := ceiling(.I / .N * n_batches)]
    }
  }

  # Assigning batch 1 (which always is the smallest) to the full prediction.
  X0[, randomorder := NULL]
  if (type == "forecast") {
    X0[id_coalition %in% full_ids, batch := 1]
  } else {
    X0[id_coalition == max(id_coalition), batch := 1]
  }
  setkey(X0, id_coalition)

  # Create a list of the batch splits
  S_groups <- split(X0[id_coalition != 1, id_coalition], X0[id_coalition != 1, batch])

  return(S_groups)
}

#' @keywords internal
shapley_setup_forecast <- function(internal) {
  n_shapley_values <- internal$parameters$n_shapley_values
  n_features <- internal$parameters$n_features
  approach <- internal$parameters$approach
  is_groupwise <- internal$parameters$is_groupwise
  paired_shap_sampling <- internal$parameters$extra_computation_args$paired_shap_sampling
  kernelSHAP_reweighting <- internal$parameters$extra_computation_args$kernelSHAP_reweighting

  coal_feature_list <- internal$objects$coal_feature_list
  horizon <- internal$parameters$horizon
  horizon_group <- internal$parameters$horizon_group
  feature_names <- internal$parameters$feature_names

  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  exact <- internal$iter_list[[iter]]$exact

  prev_coal_samples <- internal$iter_list[[iter]]$prev_coal_samples # A list of length length(horizon_features)
  prev_coal_samples_n_unique <- internal$iter_list[[iter]]$prev_coal_samples_n_unique # Same as in the previous line

  # Lists to store the sampled coalitions for each horizon and the number of unique coalitions
  coal_samples <- coal_samples_n_unique <- list()

  X_list <- W_list <- list()

  cols_per_horizon <- internal$parameters$horizon_features
  horizon_features <- lapply(cols_per_horizon, function(x) which(internal$parameters$feature_names %in% x))

  # Apply create_coalition_table, weigth_matrix and coalition_matrix_cpp to each of the different horizons
  for (i in seq_along(horizon_features)) {
    if (is_groupwise && !is.null(horizon_group)) {
      this_coal_feature_list <- coal_feature_list[sapply(
        names(coal_feature_list),
        function(x) x %in% horizon_group[[i]]
      )]
    } else {
      this_coal_feature_list <- lapply(coal_feature_list, function(x) x[x %in% horizon_features[[i]]])
      this_coal_feature_list <- this_coal_feature_list[sapply(this_coal_feature_list, function(x) length(x) != 0)]
    }

    n_this_featcomb <- length(this_coal_feature_list)
    n_coalitions_here <- min(2^n_this_featcomb, n_coalitions)

    X_list[[i]] <- create_coalition_table(
      m = n_this_featcomb,
      exact = exact,
      n_coalitions = n_coalitions_here,
      weight_zero_m = 10^6,
      paired_shap_sampling = paired_shap_sampling,
      prev_coal_samples = prev_coal_samples[[i]],
      prev_coal_samples_n_unique = prev_coal_samples_n_unique[[i]],
      coal_feature_list = this_coal_feature_list,
      approach0 = approach,
      kernelSHAP_reweighting = kernelSHAP_reweighting
    )

    W_list[[i]] <- weight_matrix(
      X = X_list[[i]],
      normalize_W_weights = TRUE
    )

    ### Store the coalitions for this horizon for the next iteration
    # Getting the sampled coalitions for each horizon. We do not store this if exact, as then all is used.
    if (isFALSE(exact)) {
      coal_samples[[i]] <- rep(X_list[[i]][-c(1, .N), coalitions_str], X_list[[i]][-c(1, .N), sample_freq])
    } else {
      coal_samples[[i]] <- NA
    }

    # Extract the number of unique coalitions from the previous iteration
    coal_samples_n_unique[[i]] <- nrow(X_list[[i]]) - 2 # Subtract empty and grand coalition
  }

  # Merge the coalition data.table to single one to use for computing conditional expectations later on
  X <- rbindlist(X_list, idcol = "horizon")
  X[, N := NA]
  data.table::setorderv(X, c("coalition_size", "horizon"), order = c(1, -1))
  X[, horizon_id_coalition := id_coalition]
  X[, id_coalition := 0]
  X[!duplicated(features), id_coalition := .I]
  X[, tmp_coalitions := as.character(features)]
  X[, id_coalition := max(id_coalition), by = tmp_coalitions]
  X[, tmp_coalitions := NULL]

  # Extracts a data.table allowing mapping from X to X_list/W_list to be used in the compute_shapley function
  id_coalition_mapper_dt <- X[, .(horizon, horizon_id_coalition, id_coalition, full = features %in% horizon_features)]

  X[, horizon := NULL]
  X[, horizon_id_coalition := NULL]
  data.table::setorder(X, coalition_size)
  X <- X[!duplicated(id_coalition)]

  W <- NULL # Included for consistency. Necessary weights are in W_list instead

  # Note: it is correct to use features here and not coalitions as we do for normal shapley_setup().
  # This is because, here, for forecast, the features are a function of both coalitions and horizon.
  coalition_map <- X[, .(id_coalition,
    coalitions_str = sapply(features, paste, collapse = " ")
  )]

  ## Get feature matrix ---------
  S <- coalition_matrix_cpp(
    coalitions = X[["features"]],
    m = n_features
  )


  #### Updating parameters ####

  # Updating parameters$exact as done in create_coalition_table
  if (!exact && n_coalitions >= 2^n_shapley_values) {
    internal$iter_list[[iter]]$exact <- TRUE
    internal$parameters$exact <- TRUE # Note that this is exact only if all horizons use the exact method.
  }

  internal$iter_list[[iter]]$n_coalitions <- nrow(S) # Updating this parameter in the end based on what is used.

  # The number of sampled coalitions *per horizon* to be used for convergence detection only
  # Exclude the zero and full prediction
  internal$iter_list[[iter]]$n_sampled_coalitions <- length(unique(id_coalition_mapper_dt$horizon_id_coalition)) - 2

  internal$iter_list[[iter]]$X <- X
  internal$iter_list[[iter]]$W <- W
  internal$iter_list[[iter]]$S <- S
  internal$iter_list[[iter]]$id_coalition_mapper_dt <- id_coalition_mapper_dt
  internal$iter_list[[iter]]$X_list <- X_list
  internal$iter_list[[iter]]$coalition_map <- coalition_map
  internal$iter_list[[iter]]$S_batch <- create_S_batch(internal)
  internal$iter_list[[iter]]$coal_samples <- coal_samples
  internal$iter_list[[iter]]$coal_samples_n_unique <- coal_samples_n_unique

  internal$objects$cols_per_horizon <- cols_per_horizon
  internal$objects$W_list <- W_list

  return(internal)
}
