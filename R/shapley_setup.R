#' Set Up the KernelSHAP Framework
#'
#' @inheritParams default_doc_export
#'
#' @return The internal list updated with the coalitions to be estimated
#'
#' @export
#' @keywords internal
shapley_setup <- function(internal) {
  verbose <- internal$parameters$verbose
  n_shapley_values <- internal$parameters$n_shapley_values
  n_features <- internal$parameters$n_features
  n_coal_each_size <- internal$parameters$n_coal_each_size
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
  semi_deterministic_sampling <- internal$parameters$extra_computation_args$semi_deterministic_sampling

  iter <- length(internal$iter_list)

  n_coalitions <- internal$iter_list[[iter]]$n_coalitions
  exact <- internal$iter_list[[iter]]$exact
  prev_X <- internal$iter_list[[iter]]$prev_X # NULL in first iteration
  dt_coal_samp_info <- internal$iter_list[[iter]]$dt_coal_samp_info # NULL in first iteration

  if ("progress" %in% verbose) {
    cli::cli_progress_step("Sampling coalitions")
  }

  X <- create_coalition_table(
    m = n_shapley_values,
    exact = exact,
    n_coalitions = n_coalitions,
    n_coal_each_size = n_coal_each_size,
    weight_zero_m = 10^6,
    paired_shap_sampling = paired_shap_sampling,
    prev_X = prev_X,
    coal_feature_list = coal_feature_list,
    approach0 = approach,
    kernelSHAP_reweighting = kernelSHAP_reweighting,
    semi_deterministic_sampling = semi_deterministic_sampling,
    dt_coal_samp_info = dt_coal_samp_info,
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
  internal$iter_list[[iter]]$X <- X
  internal$iter_list[[iter]]$W <- W
  internal$iter_list[[iter]]$S <- S
  internal$iter_list[[iter]]$coalition_map <- coalition_map
  internal$iter_list[[iter]]$S_batch <- create_S_batch(internal)

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
#' @param n_coal_each_size Vector of integers of length `m-1`. The number of valid coalitions of each coalition size
#' 1, 2,..., m-1. For symmetric Shapley values, this is `choose(m, seq(m-1))` (default). While for asymmetric Shapley
#' values, this is the number of valid coalitions of each size in the causal ordering. Used to correctly normalize the
#' Shapley weights.
#' @param weight_zero_m Numeric.
#' The value to use as a replacement for infinite coalition weights when doing numerical operations.
#' @param paired_shap_sampling Logical.
#' Whether to do paired sampling of coalitions.
#' @param prev_X data.table. The X data.table from the previous iteration.
#' @param n_samps_scale Positive integer.
#' Integer that scales the number of coalitions `n_coalitions` to sample as sampling is cheap,
#' while checking for `n_coalitions` unique coalitions is expensive, thus we over sample the
#' number of coalitions by a factor of `n_samps_scale` and determine when we have `n_coalitions` unique
#' coalitions and only use the coalitions up to this point and throw away the remaining coalitions.
#' @param approach0 Character vector.
#' Contains the approach to be used for estimation of each coalition size. Same as `approach` in [explain()].
#' @param coal_feature_list List.
#' A list mapping each coalition to the features it contains.
#' @param dt_coal_samp_info data.table. The data.table contains information about which coalitions should be
#' deterministically included and which can be sampled, in addition to the sampling probabilities of each available
#' coalition size, and the weight given to the sampled and deterministically included coalitions (excluding empty and
#' grand coalitions which are given the `weight_zero_m` weight).
#' @param dt_valid_causal_coalitions data.table. Only applicable for asymmetric Shapley
#' value explanations, and is `NULL` for symmetric Shapley values.
#' The data.table contains information about the coalitions that respects the causal ordering.
#' @inheritParams explain
#' @inheritParams get_extra_comp_args_default
#' @return A data.table with info about the coalitions to use
#'
#' @keywords internal
#'
#' @author Nikolai Sellereite, Martin Jullum, Lars Henry Berge Olsen
create_coalition_table <- function(m,
                                   exact = TRUE,
                                   n_coalitions = 200,
                                   n_coal_each_size = choose(m, seq(m - 1)),
                                   weight_zero_m = 10^6,
                                   paired_shap_sampling = TRUE,
                                   prev_X = NULL,
                                   n_samps_scale = 10,
                                   coal_feature_list = as.list(seq_len(m)),
                                   approach0 = "gaussian",
                                   kernelSHAP_reweighting = "none",
                                   semi_deterministic_sampling = FALSE,
                                   dt_coal_samp_info = NULL,
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
      n_coal_each_size = n_coal_each_size,
      weight_zero_m = weight_zero_m,
      paired_shap_sampling = paired_shap_sampling,
      prev_X = prev_X,
      n_samps_scale = n_samps_scale,
      kernelSHAP_reweighting = kernelSHAP_reweighting,
      semi_deterministic_sampling = semi_deterministic_sampling,
      dt_coal_samp_info = dt_coal_samp_info,
      dt_valid_causal_coalitions = dt_valid_causal_coalitions
    )
    stopifnot(data.table::is.data.table(dt))
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
get_dt_coal_samp_info <- function(m, semi_deterministic_sampling = FALSE, weight_zero_m = 10^6) {
  # Get information about the sampling procedure: different for semi-deterministic sampling and regular sampling
  if (semi_deterministic_sampling) {
    # Get the number of coalition sizes for S when considering S to always be the smallest of S and Sbar.
    n_coal_sizes <- as.integer(ceiling((m - 1) / 2))

    # Get the number of coalitions of each size
    n_coal_each_size <- choose(m, seq(n_coal_sizes))

    # Get the number of coalitions deterministically included when including the different coalition sizes
    n_coal_determ <- pmin(2^m, sapply(seq(n_coal_sizes + 1), function(i) 2 + 2 * sum(choose(m, seq_len(i - 1)))))

    # Get the coalition sizes to sample
    coal_sizes_sep_sample <- c(lapply(seq(n_coal_sizes), function(size) seq(size, m - size)), NA)

    # Get the (normalized) Shapley kernel weight for each coalition sizes
    coal_size_weight <- (m - 1.0) / (seq(m - 1) * seq(m - 1, 1))
    coal_size_weight <- coal_size_weight / sum(coal_size_weight)

    # Get the weights of the coalitions deterministically included
    weight_determ <- c(sapply(seq(0, n_coal_sizes - 1), function(paired_size) {
      sum(coal_size_weight[-seq(paired_size + 1, m - paired_size - 1)])
    }), 1)

    # Get the sampling probabilities for each coalition size when included the previous sizes
    coal_sizes_sep_sample_prob <- c(lapply(seq(n_coal_sizes), function(size) {
      weigts <- coal_size_weight[seq(size, m - size)]
      return(weigts / sum(weigts))
    }), NA)

    # Get the probability of sampling the most likely coalition size when including the previous sizes
    coal_size_sampling_prob_rel <- sapply(coal_sizes_sep_sample_prob[seq(n_coal_sizes)], "[[", 1)

    # Get the number of coalitions to sample to include all coalitions of the different coalition sizes
    # based on their sampling probability and adding the previously deterministically included coalitions.
    n_coal_needed <- ceiling(n_coal_each_size / coal_size_sampling_prob_rel) + n_coal_determ[seq(n_coal_sizes)]

    # Get the max number of coalitions before we include the smaller coalition size. Ensure even numbers due to pairing.
    n_coal_max <- c(sapply(n_coal_needed, function(x) ifelse(x %% 2 == 0, x - 2, x - 1)), 2^m)

    # Create a data table with all relevant information
    dt_coal_samp_info <- data.table(
      max_fixed_coal_size = seq(0, n_coal_sizes),
      n_coal_max = n_coal_max,
      n_coal_determ = n_coal_determ,
      weight_determ = weight_determ,
      weight_sample = 1 - weight_determ,
      coal_sizes_sample = coal_sizes_sep_sample,
      coal_sizes_sample_prob = coal_sizes_sep_sample_prob
    )
  } else {
    # Get the (normalized) Shapley kernel weight for each coalition sizes
    coal_sizes_sample_prob <- (m - 1.0) / (seq(m - 1) * seq(m - 1, 1))

    # For regular sampling, the only deterministically included coalitions are the empty and grand coalitions
    dt_coal_samp_info <- data.table(
      max_fixed_coal_size = 0,
      n_coal_max = 2^m,
      n_coal_determ = 2,
      weight_determ = 0,
      weight_sample = 1,
      coal_sizes_sample = list(seq(m - 1)),
      coal_sizes_sample_prob = list(coal_sizes_sample_prob / sum(coal_sizes_sample_prob))
    )
  }

  return(dt_coal_samp_info)
}

#' @keywords internal
reweighted_shapley_weight <- function(m, max_fixed_coal_size = 0, n_coal_each_size = choose(m, seq(m - 1))) {
  # We include all coalition sizes from max_fixed_coal_size + 1 to m - max_fixed_coal_size - 1.
  # That is max_fixed_coal_size is a integer smaller or equal to ceiling((m - 1) / 2).
  # n_coal_each_size is only used for asymmetric Shapley values as there we can have, e.g.,
  # that there is only a single valid coalition of a given size and then this coalition should be given
  # the weight of all coalitions of this size. This is not the case for regular/symmetric Shapley values
  # where all (m choose n_coal_size) coalitions are available for each coalition size.

  # Checks that can be removed as this is an internal function where this should always apply
  if (length(n_coal_each_size) != m - 1) {
    cli::cli_abort(paste0("n_coal_each_size must be of length ", m - 1, "."))
  }

  if (max_fixed_coal_size > ceiling((m - 1) / 2)) {
    cli::cli_abort(paste0(
      "max_fixed_coal_size (", max_fixed_coal_size, ") is larger than the maximum (",
      ceiling((m - 1) / 2), ")."
    ))
  }

  # Get the relevant indices
  rel_ind <- seq(max_fixed_coal_size + 1, m - max_fixed_coal_size - 1)

  # Get the total weigh of each coalition size from 1 to m - 1
  weight <- sapply(seq(m - 1), function(i) (m - 1) / (i * (m - i)))

  # Set the non-relevant indices to zero
  weight[-rel_ind] <- 0

  # Normalize the weights of the remaining coalition sizes
  weight <- weight / sum(weight)

  # Divide the normalized weights by the number of coalitions
  # of each size to get the weight of each coalition
  weight[rel_ind] <- weight[rel_ind] / n_coal_each_size[rel_ind]

  return(weight)
}

#' @keywords internal
kernelSHAP_reweighting <- function(X,
                                   m,
                                   reweight = "on_all_cond",
                                   max_fixed_coal_size = 0,
                                   n_coal_each_size = choose(m, seq(m - 1))) {
  # Updates the Shapley weights in all rows in X based on the reweighting strategy BY REFERENCE.

  # Get the normalized Shapley weights for each coalition size as a vector, where normalized
  # means that the sum of the weight times the number of coalition of each size yields one.
  reweighted_shapley_weight <-
    reweighted_shapley_weight(m = m, max_fixed_coal_size = max_fixed_coal_size, n_coal_each_size = n_coal_each_size)

  # Do the reweighting
  if (reweight == "on_N") {
    X[, shapley_weight := mean(shapley_weight), by = N]
  } else if (reweight == "on_all") {
    X[, shapley_weight := reweighted_shapley_weight[coalition_size]]
  } else if (reweight == "on_all_cond") {
    K <- X[, sum(sample_freq)]
    X[, shapley_weight := reweighted_shapley_weight[coalition_size]]
    X[, cond := 1 - (1 - shapley_weight)^K]
    X[, shapley_weight := shapley_weight / cond]
    X[, cond := NULL] # Remove the condition column to ensure X has the same columns as before
  }
  # reweight = "none" or something else do nothing
  return(NULL)
}

#' Get table with all (exact) coalitions
#'
#' @inheritParams create_coalition_table
#' @keywords internal
exact_coalition_table <- function(m,
                                  max_fixed_coal_size = ceiling((m - 1) / 2),
                                  dt_valid_causal_coalitions = NULL,
                                  weight_zero_m = 10^6) {
  # Create all valid coalitions for regular/symmetric or asymmetric Shapley values
  if (is.null(dt_valid_causal_coalitions)) {
    # Regular/symmetric Shapley values: (default max_fixed_coal_size yields seq(0, m))
    # Use only coalitions of size 0, 1, ..., max_fixed_coal_size, ..., m - max_fixed_coal_size, ..., m - 1, m.

    # Checks that can be removed as this is an internal function where this should always apply
    if (max_fixed_coal_size > ceiling((m - 1) / 2)) {
      cli::cli_abort(paste0(
        "max_fixed_coal_size (", max_fixed_coal_size, ") is larger than the maximum (",
        ceiling((m - 1) / 2), ")."
      ))
    }

    # Use unique such that the coalition size where |S| = |S_bar| is not included twice
    seq_coal_size <- unique(c(seq(0, max_fixed_coal_size), m - seq(max_fixed_coal_size, 0)))

    # Make list of the relevant coalitions
    coalitions0 <- unlist(lapply(seq_coal_size, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
  } else {
    # Asymmetric Shapley values: use only the coalitions that respect the causal ordering
    coalitions0 <- dt_valid_causal_coalitions[, coalitions]
  }

  dt <- data.table::data.table(id_coalition = seq_along(coalitions0))
  dt[, coalitions := coalitions0]
  dt[, coalitions_str := sapply(coalitions, paste, collapse = " ")]
  dt[, coalition_size := lengths(coalitions)]
  dt[, N := .N, coalition_size]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = coalition_size, weight_zero_m)]
  dt[, sample_freq := NA]
  return(dt)
}


#' Get table with sampled coalitions using the semi-deterministic sampling approach
#'
#' @inheritParams create_coalition_table
#' @keywords internal
sample_coalition_table <- function(m,
                                   n_coalitions = 200,
                                   n_coal_each_size = choose(m, seq(m - 1)),
                                   weight_zero_m = 10^6,
                                   paired_shap_sampling = TRUE,
                                   prev_X = NULL,
                                   kernelSHAP_reweighting = "on_all_cond",
                                   semi_deterministic_sampling = FALSE,
                                   dt_coal_samp_info = NULL,
                                   dt_valid_causal_coalitions = NULL,
                                   n_samps_scale = 10) {
  # Check if we are to do asymmetric or symmetric/regular Shapley values. Always FALSE for semi-deterministic sampling.
  asymmetric <- !is.null(dt_valid_causal_coalitions)

  # Ensure that n_coalitions is not larger than the maximum number of coalitions
  n_coalitions <- min(2^m, n_coalitions)

  # The maximum paired coalition size to deterministically include: 0 implies only empty and grand coalitions,
  # and 1 implies all coalitions of size 1 and m - 1, and so on.
  max_fixed_coal_size <- dt_coal_samp_info[, max_fixed_coal_size]

  # Get the number of coalitions to deterministically include. Two for regular sampling (empty and grand coalitions).
  n_coal_determ <- dt_coal_samp_info[, n_coal_determ]

  # Get the number of coalitions we need to sample
  n_samples_needed <- n_coalitions - n_coal_determ

  # Get the proportions of the scaled kernel weights given to the deterministic and sampled coalitions
  weight_determ <- dt_coal_samp_info[, weight_determ]
  weight_sample <- dt_coal_samp_info[, weight_sample]

  # Get the coalition sizes to sample and the probabilities of sampling these sizes.
  coal_sizes_sample <- dt_coal_samp_info[, coal_sizes_sample[[1]]]
  coal_sizes_sample_prob <- dt_coal_samp_info[, coal_sizes_sample_prob[[1]]]

  # Create the data.table with the deterministic included coalitions and reweight them. No effect for regular sampling.
  X_determ <- exact_coalition_table(m = m, max_fixed_coal_size = max_fixed_coal_size, weight_zero_m = weight_zero_m)
  X_determ[-c(1, .N), shapley_weight := weight_determ * shapley_weight / sum(shapley_weight)]

  # Set variables based on whether we are in the first iteration or not
  if (!is.null(prev_X)) {
    # Not in the first iteration and we have sampled coalitions before
    if (isTRUE(semi_deterministic_sampling)) {
      # We sampled using the semi-deterministic sampling strategy.
      # Get the sampled coalitions from the previous iteration which are still subject for sampling
      # in this iteration. I.e., omit deterministic included coalitions.
      X_prev_rel <- prev_X[max_fixed_coal_size < coalition_size & coalition_size < m - max_fixed_coal_size]
    } else {
      # We sampled using the regular sampling strategy, so all coalitions can still be sampled, except empty and grand.
      X_prev_rel <- prev_X[!is.na(sample_freq)]
    }

    # Make array with all previous sampled coalitions
    coal_sample_all <- rep(X_prev_rel[, coalitions_str], X_prev_rel[, sample_freq])

    # Get the number of unique sampled coalitions
    n_unique_coal_sampled <- nrow(X_prev_rel)
  } else {
    # We are in the first iteration and have not sampled any coalitions yet
    coal_sample_all <- c()
    n_unique_coal_sampled <- 0
  }

  # Only sample coalitions if we need to sample more coalitions
  if (n_unique_coal_sampled < n_samples_needed) {
    # Get the number of coalitions to sample, divide by two if paired sampling
    n_samp <- as.integer(n_coalitions * n_samps_scale / ifelse(paired_shap_sampling, 2, 1))

    # Loop until we have drawn enough unique samples
    while (n_unique_coal_sampled < n_samples_needed) {
      # Sample the coalitions based on if we are computing regular/symmetric or asymmetric Shapley values
      if (asymmetric) {
        # Sample the causal coalitions from the valid causal coalitions with the Shapley weight as the probability.
        # The weights of each coalition size is split evenly among the members of each coalition size.
        coalitions <-
          dt_valid_causal_coalitions[-c(1, .N)][sample(
            x = .N, size = n_samp, replace = TRUE, prob = shapley_weight
          ), coalitions_str]
      } else {
        # Sample the coalition sizes
        coal_size_samples <- sample(x = coal_sizes_sample, size = n_samp, prob = coal_sizes_sample_prob, replace = TRUE)

        # Sample the (paired) coalitions as strings. Always paired for semi-deterministic sampling.
        coalitions <- sample_coalitions_cpp_str_paired(m, coal_size_samples, paired_shap_sampling)
      }

      # Add the new coalitions to the previously sampled coalitions
      coal_sample_all <- c(coal_sample_all, coalitions)

      # Get the cumulative number of unique coalitions for each coalition in coal_sample_all
      dt_cumsum <- data.table(coalitions = coal_sample_all, N_S = cumsum(!duplicated(coal_sample_all)))[, L := .I]

      # Extract rows where the N_S value increases (i.e., where we sample a new unique coalition)
      dt_N_S_and_L <- dt_cumsum[N_S != data.table::shift(N_S, type = "lag", fill = 0)]

      # Get the number of unique coalitions sampled
      n_unique_coal_sampled <- dt_N_S_and_L[.N, N_S]
    }

    # Post processing: keep only the coalitions until n_samples_needed
    coal_sample_all <- coal_sample_all[seq(dt_N_S_and_L[N_S == n_samples_needed, L])]
  }

  ## Create the X data table for the sampled coalitions
  X <- data.table(coalitions_str = coal_sample_all)[, .(sample_freq = .N), by = coalitions_str]
  X[, shapley_weight := as.numeric(sample_freq)]

  # Convert coalition strings to vector of integers as in old setup.
  X[, coalitions := lapply(strsplit(coalitions_str, " "), as.integer)]
  X[, coalition_size := lengths(coalitions)]
  X[, N := n_coal_each_size[coalition_size]]

  # Add id column and order the data table
  data.table::setkeyv(X, "coalition_size")
  data.table::setorder(X, "coalition_size")
  data.table::setcolorder(X, c("coalitions", "coalitions_str", "coalition_size", "N", "shapley_weight"))

  # Reweight the Shapley weights in X by reference according to the reweighting strategy
  kernelSHAP_reweighting(
    X = X,
    m = m,
    reweight = kernelSHAP_reweighting,
    max_fixed_coal_size = max_fixed_coal_size,
    n_coal_each_size = n_coal_each_size
  )

  # Make the Shapley weights sum to the weight assigned to the sampled coalitions. No effect for regular sampling.
  X[, shapley_weight := weight_sample * shapley_weight / sum(shapley_weight)]

  # Combine the deterministic and sampled coalitions
  X <- data.table::rbindlist(
    list(X_determ[seq(1, .N / 2)], X, X_determ[seq((.N / 2) + 1, .N)]),
    use.names = TRUE, fill = TRUE
  )
  X[, id_coalition := .I]

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
coal_feature_mapper <- function(x, coal_feature_list) {
  if (length(x) != 0) {
    unlist(coal_feature_list[x])
  } else {
    integer(0)
  }
}

#' Calculate Weighted Matrix
#'
#' @param X data.table.
#' Output from [create_coalition_table()].
#' @param normalize_W_weights Logical. Whether to normalize the coalition weights to sum to 1 for
#' increased numerical stability before solving the WLS (weighted least squares). Applies to all coalitions
#' except coalitions `1` and `2^m`.
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

  # NULL in first iteration, list of dt with length length(horizon_features)
  prev_X_list <- internal$iter_list[[iter]]$prev_X_list

  X_list <- W_list <- list()

  cols_per_horizon <- internal$parameters$horizon_features
  horizon_features <- lapply(cols_per_horizon, function(x) which(internal$parameters$feature_names %in% x))

  # Apply create_coalition_table, weight_matrix and coalition_matrix_cpp to each of the different horizons
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

    # Create the coalition table for this horizon
    X_list[[i]] <- create_coalition_table(
      m = n_this_featcomb,
      exact = exact,
      n_coalitions = n_coalitions_here,
      n_coal_each_size = choose(n_this_featcomb, seq(n_this_featcomb - 1)),
      weight_zero_m = 10^6,
      paired_shap_sampling = paired_shap_sampling,
      prev_X = prev_X_list[[i]],
      coal_feature_list = this_coal_feature_list,
      approach0 = approach,
      kernelSHAP_reweighting = kernelSHAP_reweighting,
      dt_coal_samp_info = get_dt_coal_samp_info(n_this_featcomb)
    )

    W_list[[i]] <- weight_matrix(
      X = X_list[[i]],
      normalize_W_weights = TRUE
    )
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

  internal$objects$cols_per_horizon <- cols_per_horizon
  internal$objects$W_list <- W_list

  return(internal)
}
