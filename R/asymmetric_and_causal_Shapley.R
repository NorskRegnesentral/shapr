# Check functions -------------------------------------------------------------------------------------------------
#' Check that all explicands has at least one valid MC sample in causal Shapley values
#'
#' @param dt Data.table containing the generated MC samples (and conditional values) after each sampling step
#' @inheritParams explain
#' @inheritParams create_marginal_data_cat
#' @inheritParams create_marginal_data_training
#'
#' @details For undocumented arguments, see [setup_approach.categorical()].
#' @keywords internal
#'
#' @author Lars Henry Berge Olsen
check_categorical_valid_MCsamp <- function(dt, n_explain, n_MC_samples, joint_prob_dt) {
  dt_factor <- dt[, .SD, .SDcols = is.factor] # Get the columns that have been inserted into
  dt_factor_names <- copy(names(dt_factor)) # Get their names. Copy as we are to change dt_factor
  dt_factor[, id := rep(seq(n_explain), each = n_MC_samples)] # Add an id column
  dt_valid_coals <- joint_prob_dt[, dt_factor_names, with = FALSE] # Get the valid feature coalitions
  dt_invalid <- dt_factor[!dt_valid_coals, on = dt_factor_names] # Get non valid coalitions
  explicand_all_invalid <- dt_invalid[, .N, by = id][N == n_MC_samples] # If all samples for an explicand are invalid
  if (nrow(explicand_all_invalid) > 0) {
    cli::cli_abort(paste0(
      "An explicand has no valid MC feature coalitions. Increase `n_MC_samples` or provide ",
      "`joint_prob_dt` containing the probaibilities for unlikely coalitions, too."
    ))
  }
}

# Convert function ------------------------------------------------------------------------------------------------
#' Convert feature names into feature indices
#'
#' Functions that takes a `causal_ordering` specified using strings and convert these strings to feature indices.
#'
#' @param labels Vector of strings containing (the order of) the feature names.
#' @param feat_group_txt String that is either "feature" or "group" based on
#' if `shapr` is computing feature- or group-wise Shapley values
#' @inheritParams explain
#'
#' @return The `causal_ordering` list, but with feature indices (w.r.t. `labels`) instead of feature names.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
convert_feature_name_to_idx <- function(causal_ordering, labels, feat_group_txt) {
  # Convert the feature names into feature indices
  causal_ordering_match <- match(unlist(causal_ordering), labels)

  # Check that user only provided valid feature names
  if (any(is.na(causal_ordering_match))) {
    cli::cli_abort(paste0(
      "`causal_ordering` contains ", feat_group_txt, " names (`",
      paste0(unlist(causal_ordering)[is.na(causal_ordering_match)], collapse = "`, `"), "`) ",
      "that are not in the data (`", paste0(labels, collapse = "`, `"), "`)."
    ))
  }

  # Recreate the causal_ordering list with the feature indices
  causal_ordering <- relist(causal_ordering_match, causal_ordering)
  return(causal_ordering)
}


# Create functions ------------------------------------------------------------------------------------------------
#' Function that samples data from the empirical marginal training distribution
#'
#' @description Sample observations from the empirical distribution P(X) using the training dataset.
#'
#' @param Sbar_features Vector of integers containing the features indices to generate marginal observations for.
#' That is, if `Sbar_features` is `c(1,4)`, then we sample `n_MC_samples` observations from \eqn{P(X_1, X_4)} using the
#' empirical training observations (with replacements). That is, we sample the first and fourth feature values from
#' the same training observation, so we do not break the dependence between them.
#' @param stable_version Logical. If `TRUE` and `n_MC_samples` > `n_train`, then we include each training observation
#' `n_MC_samples %/% n_train` times and then sample the remaining `n_MC_samples %% n_train samples`. Only the latter is
#' done when `n_MC_samples < n_train`. This is done separately for each explicand. If `FALSE`, we randomly sample the
#' from the observations.
#'
#' @inheritParams default_doc_internal
#'
#' @return Data table of dimension `n_MC_samples` \eqn{\times} `length(Sbar_features)` with the sampled observations.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
create_marginal_data_training <- function(x_train,
                                          n_explain,
                                          Sbar_features,
                                          n_MC_samples = 1000,
                                          stable_version = TRUE) {
  # Get the number of training observations
  n_train <- nrow(x_train)

  if (stable_version) {
    # If n_MC_samples > n_train, then we include each training observations n_MC_samples %/% n_train times and
    # then sample the remaining n_MC_samples %% n_train samples. Only the latter is done when n_MC_samples < n_train.
    # This is done separately for each explicand
    sampled_indices <- as.vector(sapply(
      seq(n_explain),
      function(x) {
        c(
          rep(seq(n_train), each = n_MC_samples %/% n_train),
          sample(n_train, n_MC_samples %% n_train)
        )
      }
    ))
  } else {
    # sample everything and not guarantee that we use all training observations
    sampled_indices <- sample(n_train, n_MC_samples * n_explain, replace = TRUE)
  }

  # Sample the marginal data and return them
  return(x_train[sampled_indices, Sbar_features, with = FALSE])
}

#' Create marginal categorical data for causal Shapley values
#'
#' @description
#' This function is used when we generate marginal data for the categorical approach when we have several sampling
#' steps. We need to treat this separately, as we here in the marginal step CANNOT make feature values such
#' that the combination of those and the feature values we condition in S are NOT in
#' `categorical.joint_prob_dt`. If we do this, then we cannot progress further in the chain of sampling
#' steps. E.g., X1 in (1,2,3), X2 in (1,2,3), and X3 in (1,2,3).
#' We know X2 = 2, and let causal structure be X1 -> X2 -> X3. Assume that
#' P(X1 = 1, X2 = 2, X = 3) = P(X1 = 2, X2 = 2, X = 3) = 1/2. Then there is no point
#' generating X1 = 3, as we then cannot generate X3.
#' The solution is only to generate the values which can proceed through the whole
#' chain of sampling steps. To do that, we have to ensure the marginal sampling
#' respects the valid feature coalitions for all sets of conditional features, i.e.,
#' the features in `features_steps_cond_on`.
#' We sample from the valid coalitions using the MARGINAL probabilities.
#'
#' @param Sbar_features Vector of integers containing the features indices to generate marginal observations for.
#' That is, if `Sbar_features` is `c(1,4)`, then we sample `n_MC_samples` observations from \eqn{P(X_1, X_4)}.
#' That is, we sample the first and fourth feature values from the same valid feature coalition using
#' the marginal probability, so we do not break the dependence between them.
#' @param S_original Vector of integers containing the features indices of the original coalition `S`. I.e., not the
#' features in the current sampling step, but the features are known to us before starting the chain of sampling steps.
#' @inheritParams explain
#' @details For undocumented arguments, see [setup_approach.categorical()].
#'
#' @return Data table of dimension \eqn{(`n_MC_samples` * `nrow(x_explain)`) \times `length(Sbar_features)`} with the
#' sampled observations.
#'
#' @keywords internal
#'
#' @author Lars Henry Berge Olsen
create_marginal_data_cat <- function(n_MC_samples,
                                     x_explain,
                                     Sbar_features,
                                     S_original,
                                     joint_prob_dt) {
  # Get the number of features and their names
  n_features <- ncol(x_explain)
  feature_names <- colnames(x_explain)

  # Get the feature names of the features we are to generate
  Sbar_now_names <- feature_names[Sbar_features]

  # Make a copy of the explicands and add an id
  x_explain_copy <- data.table::copy(x_explain)[, id := .I]

  # Get the features that are in S originally and the features we are creating marginal values for
  S_original_names <- feature_names[S_original]
  S_original_names_with_id <- c("id", S_original_names)
  relevant_features <- sort(c(Sbar_features, S_original))
  relevant_features_names <- feature_names[relevant_features]

  # Get the marginal probabilities for the relevant feature coalitions
  marginal_prob_dt <- joint_prob_dt[, list(prob = sum(joint_prob)), by = relevant_features_names]

  # Get all valid feature coalitions for the relevant features
  dt_valid_coalitions <- unique(joint_prob_dt[, relevant_features, with = FALSE])

  # Get relevant feature coalitions that are valid for the explicands
  dt_valid_coalitions_relevant <- data.table::merge.data.table(x_explain_copy[, S_original_names_with_id, with = FALSE],
    dt_valid_coalitions,
    by = S_original_names,
    allow.cartesian = TRUE
  )

  # Merge the relevant feature coalitions with their marginal probabilities
  dt_valid_coal_marg_prob <- data.table::merge.data.table(dt_valid_coalitions_relevant,
    marginal_prob_dt,
    by = relevant_features_names
  )
  dt_valid_coal_marg_prob[, prob := prob / sum(prob), by = id] # Make prob sum to 1 for each explicand
  data.table::setkey(dt_valid_coal_marg_prob, "id") # Set id to key so id is in increasing order

  # Sample n_MC_samples from the valid coalitions using the marginal probabilities and extract the Sbar columns
  dt_return <-
    dt_valid_coal_marg_prob[, .SD[sample(.N, n_MC_samples, replace = TRUE, prob = prob)],
      by = id
    ][, Sbar_now_names, with = FALSE]
  return(dt_return)
}


# Get functions ---------------------------------------------------------------------------------------------------
#' Get all coalitions satisfying the causal ordering
#'
#' @description
#' This function is only relevant when we are computing asymmetric Shapley values.
#' For symmetric Shapley values (both regular and causal), all coalitions are allowed.
#'
#' @inheritParams explain
#'
#' @param sort_features_in_coalitions Boolean. If `TRUE`, then the feature indices in the
#' coalitions are sorted in increasing order. If `FALSE`, then the function maintains the
#' order of features within each group given in `causal_ordering`.
#'
#' @return List of vectors containing all coalitions that respects the causal ordering.
#' @keywords internal
#' @author Lars Henry Berge Olsen
get_valid_causal_coalitions <- function(causal_ordering, sort_features_in_coalitions = TRUE) {
  # Create a list to store the possible coalitions and start with the empty coalition
  coalitions <- list(numeric(0))

  # Iterate over the remaining partial causal orderings
  for (i in seq(1, length(causal_ordering))) {
    # Get the number of features in the ith component of the (partial) causal ordering
    ith_order_length <- length(causal_ordering[[i]])

    # Create a list of vectors containing all possible feature coalitions except the empty one (with temp indices)
    ith_order_coalitions <-
      unlist(lapply(seq(ith_order_length), utils::combn, x = ith_order_length, simplify = FALSE), recursive = FALSE)

    # Get the ancestors of the ith component of the (partial) causal ordering
    ancestors <- coalitions[[length(coalitions)]]

    # Update the indices by adding the number of ancestors and concatenate the ancestors
    coalitions <-
      c(coalitions, sapply(ith_order_coalitions, function(x) c(ancestors, x + length(ancestors)), simplify = FALSE))
  }

  # Sort the causal components such that the singletons are in the right order
  if (sort_features_in_coalitions) causal_ordering <- sapply(causal_ordering, sort)

  # Convert the temporary indices to the correct feature indices
  coalitions <- sapply(coalitions, function(x) unlist(causal_ordering)[x])

  # Sort the coalitions
  if (sort_features_in_coalitions) coalitions <- sapply(coalitions, sort)

  return(coalitions)
}

#' Get the number of coalitions that respects the causal ordering
#'
#' @inheritParams explain
#'
#' @details The function computes the number of coalitions that respects the causal ordering by computing the number
#' of coalitions in each partial causal component and then summing these. We compute
#' the number of coalitions in the \eqn{i}th a partial causal component by \eqn{2^n - 1},
#' where \eqn{n} is the number of features in the \eqn{i}th partial causal component
#' and we subtract one as we do not want to include the situation where no features in
#' the \eqn{i}th partial causal component are present. In the end, we add 1 for the
#' empty coalition.
#'
#' @return Integer. The (maximum) number of coalitions that respects the causal ordering.
#' @keywords internal
#' @author Lars Henry Berge Olsen
get_max_n_coalitions_causal <- function(causal_ordering) {
  return(sum(2^sapply(causal_ordering, length)) - length(causal_ordering) + 1)
}

#' Get the steps for generating MC samples for coalitions following a causal ordering
#'
#' @inheritParams explain
#'
#' @param S  Integer matrix of dimension \code{n_coalitions_valid x m}, where \code{n_coalitions_valid} equals
#' the total number of valid coalitions that respect the causal ordering given in `causal_ordering` and \code{m} equals
#' the total number of features.
#' @param as_string Boolean.
#' If the returned object is to be a list of lists of integers or a list of vectors of strings.
#'
#' @return Depends on the value of the parameter `as_string`. If a string, then `results[j]` is a vector specifying
#' the process of generating the samples for coalition `j`. The length of `results[j]` is the number of steps, and
#' `results[j][i]` is a string of the form `features_to_sample|features_to_condition_on`. If the
#' `features_to_condition_on` part is blank, then we are to sample from the marginal distribution.
#' For `as_string == FALSE`, then we rather return a vector where `results[[j]][[i]]` contains the elements
#' `Sbar` and `S` representing the features to sample and condition on, respectively.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
get_S_causal_steps <- function(S, causal_ordering, confounding, as_string = FALSE) {
  # List to store the sampling process
  results <- vector("list", nrow(S))
  names(results) <- paste0("id_coalition_", seq_len(nrow(S)))

  # Iterate over the coalitions
  for (j in seq(2, nrow(S) - 1)) {
    # Get the given and dependent features for this coalition
    index_given <- seq_len(ncol(S))[as.logical(S[j, ])]
    index_dependent <- seq_len(ncol(S))[as.logical(1 - S[j, ])]

    # Iterate over the causal orderings
    for (i in seq_along(causal_ordering)) {
      # check overlap between index_dependent and ith causal component
      to_sample <- intersect(causal_ordering[[i]], index_dependent)

      if (length(to_sample) > 0) {
        to_condition <- unlist(causal_ordering[0:(i - 1)]) # Condition on all features in ancestor components

        # If confounding is FALSE, add intervened features in the same component to the `to_condition` set.
        # If confounding is TRUE, then no extra conditioning.

        if (!confounding[i]) {
          inter0 <- intersect(causal_ordering[[i]], index_given)
          inter0 <- if (length(inter0) == 0) NULL else inter0
          to_condition <- union(inter0, to_condition)
        }

        # Save Sbar and S (sorting is for the visual)
        to_sample <- sort(to_sample)
        to_condition <- sort(to_condition)
        tmp_name <- paste0("id_coalition_", j)
        if (as_string) {
          results[[j]] <-
            c(results[[tmp_name]], paste0(paste0(to_sample, collapse = ","), "|", paste0(to_condition, collapse = ",")))
        } else {
          results[[tmp_name]][[paste0("step_", length(results[[j]]) + 1)]] <- list(Sbar = to_sample, S = to_condition)
        }
      }
    }
  }

  return(results)
}

# Prepare data function -------------------------------------------------------------------------------------------
#' Generate Data Used for Predictions and Monte Carlo Integration for Causal Shapley Values
#'
#' This function loops over the given coalitions, and for each coalition it extracts the
#' chain of relevant sampling steps provided in `internal$object$S_causal`. This chain
#' can contain sampling from marginal and conditional distributions. We use the approach given by
#' `internal$parameters$approach` to generate the samples from the conditional distributions, and
#' we iteratively call `prepare_data()` with a modified `internal_copy` list to reuse code.
#' However, this also means that chains with the same conditional distributions will retrain a
#' model of said conditional distributions several times.
#' For the marginal distribution, we sample from the Gaussian marginals when the approach is
#' `gaussian` and from the marginals of the training data for all other approaches. Note that
#' we could extend the code to sample from the marginal (gaussian) copula, too, when `approach` is
#' `copula`.
#'
#' @inheritParams default_doc_export
#' @param ... Currently not used.
#'
#' @return A data.table containing simulated data that respects the (partial) causal ordering and the
#' the confounding assumptions. The data is used to estimate the contribution function by Monte Carlo integration.
#'
#' @export
#' @keywords internal
#' @author Lars Henry Berge Olsen
prepare_data_causal <- function(internal, index_features = NULL, ...) {
  # Recall that here, index_features is a vector of id_coalitions, i.e., indicating which rows in S to use.
  # Also note that we are guaranteed that index_features does not include the empty or grand coalition

  # Extract iteration specific variables
  iter <- length(internal$iter_list)
  X <- internal$iter_list[[iter]]$X
  S <- internal$iter_list[[iter]]$S
  S_causal_steps <- internal$iter_list[[iter]]$S_causal_steps

  # Extract the needed variables
  x_train <- internal$data$x_train
  approach <- internal$parameters$approach # Can only be single approach
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_features <- internal$parameters$n_features
  n_MC_samples <- internal$parameters$n_MC_samples
  feature_names <- internal$parameters$feature_names

  # Create a list to store the populated data tables with the MC samples
  dt_list <- list()

  # Create a copy of the internal list. We will change its x_explain, n_explain, and n_MC_samples such
  # that we can the prepare_data() function which was not originally designed for the step-wise/iterative
  # sampling process which is needed for Causal Shapley values where we sample from P(Sbar_i | S_i) and
  # the S and Sbar changes in the iterative process. So those also the number of MC samples we need to generate.
  internal_copy <- copy(internal)

  # Loop over the coalitions in the batch
  index_feature_idx <- 1
  for (index_feature_idx in seq_along(index_features)) {
    # Extract the index of the current coalition
    index_feature <- index_features[index_feature_idx]

    # Reset the internal_copy list for each new coalition
    if (index_feature_idx > 1) {
      internal_copy$data$x_explain <- x_explain
      internal_copy$parameters$n_explain <- n_explain
      internal_copy$parameters$n_MC_samples <- n_MC_samples
    }

    # Create the empty data table which we are to populate with the Monte Carlo samples for each coalition
    dt <- data.table(matrix(nrow = n_explain * n_MC_samples, ncol = n_features))
    # if (approach == "categorical") dt[, names(dt) := lapply(.SD, as.factor)] # Needed for the categorical approach
    colnames(dt) <- feature_names

    # Populate the data table with the features we condition on
    S_names <- feature_names[as.logical(S[index_feature, ])]
    dt[, (S_names) := x_explain[rep(seq(n_explain), each = n_MC_samples), .SD, .SDcols = S_names]]

    # Get the iterative sampling process for the current coalition
    S_causal_steps_now <- internal$iter_list[[iter]]$S_causal_steps[[index_feature]]

    # Loop over the steps in the iterative sampling process to generate MC samples for the unconditional features
    sampling_step_idx <- 2
    for (sampling_step_idx in seq_along(S_causal_steps_now)) {
      # Set flag indicating whether or not we are in the first sampling step, as the gaussian and copula
      # approaches need to know this to change their sampling procedure to ensure correctly generated MC samples
      internal_copy$parameters$causal_first_step <- sampling_step_idx == 1

      # Get the S (the conditional features) and Sbar (the unconditional features) in the current sampling step
      S_now <- S_causal_steps_now[[sampling_step_idx]]$S # The features to condition on in this sampling step
      Sbar_now <- S_causal_steps_now[[sampling_step_idx]]$Sbar # The features to sample in this sampling step
      Sbar_now_names <- feature_names[Sbar_now]

      # Check if we are to sample from the marginal or conditional distribution
      if (is.null(S_now)) {
        # Marginal distribution as there are no variables to condition on

        # Generate the marginal data either form the Gaussian or categorical distribution or the training data
        if (approach == "gaussian") {
          # Sample marginal data from the marginal gaussian distribution
          dt_Sbar_now_marginal_values <- create_marginal_data_gaussian(
            n_MC_samples = n_MC_samples * n_explain,
            Sbar_features = Sbar_now,
            mu = internal$parameters$gaussian.mu,
            cov_mat = internal$parameters$gaussian.cov_mat
          )
        } else if (approach == "categorical" && length(S_causal_steps_now) > 1) {
          # For categorical approach with several sampling steps, we make sure to only sample feature coalitions
          # that are present in `categorical.joint_prob_dt` when combined with the features in `S_names`.
          dt_Sbar_now_marginal_values <- create_marginal_data_cat(
            n_MC_samples = n_MC_samples,
            x_explain = x_explain,
            Sbar_features = Sbar_now,
            S_original = seq(n_features)[as.logical(S[index_feature, ])],
            joint_prob_dt = internal$parameters$categorical.joint_prob_dt
          )
        } else {
          # Sample from the training data for all approaches except the gaussian approach
          # and except the categorical approach for settings with several sampling steps
          dt_Sbar_now_marginal_values <- create_marginal_data_training(
            x_train = x_train,
            n_explain = n_explain,
            Sbar_features = Sbar_now,
            n_MC_samples = n_MC_samples,
            stable_version = TRUE
          )
        }

        # Insert the marginal values into the data table
        dt[, (Sbar_now_names) := dt_Sbar_now_marginal_values]
      } else {
        # Conditional distribution as there are variables to condition on

        # Create dummy versions of S and X only containing the current conditional features, and index_features is 1.
        internal_copy$iter_list[[iter]]$S <- matrix(0, ncol = n_features, nrow = 1)
        internal_copy$iter_list[[iter]]$S[1, S_now] <- 1
        internal_copy$iter_list[[iter]]$X <-
          data.table(id_coalition = 1, features = list(S_now), n_features = length(S_now))

        # Generate the MC samples conditioning on S_now
        dt_new <- prepare_data(internal_copy, index_features = 1, ...)

        if (approach %in% c("independence", "empirical", "ctree", "categorical")) {
          # These approaches produce weighted MC samples; they do not necessarily generate n_MC_samples samples.
          # Ensure n_MC_samples by weighted resampling (with replacement) those ids that do not have n_MC_samples.
          n_samp_now <- internal_copy$parameters$n_MC_samples
          dt_new <-
            dt_new[, .SD[if (.N == n_samp_now) seq(.N) else sample(.N, n_samp_now, replace = TRUE, prob = w)], by = id]

          # Check that dt_new has the right number of rows.
          if (nrow(dt_new) != n_explain * n_MC_samples) {
            cli::cli_abort("`dt_new` does not have the right number of rows.")
          }
        }

        # Insert/keep only the features in Sbar_now into dt
        dt[, (Sbar_now_names) := dt_new[, .SD, .SDcols = Sbar_now_names]]
      }

      # Here we check if all the generated samples are outside the joint_prob_dt
      if (approach == "categorical" && length(S_causal_steps_now) > 1) {
        check_categorical_valid_MCsamp(
          dt = dt,
          n_explain = n_explain,
          n_MC_samples = n_MC_samples,
          joint_prob_dt = internal$parameters$categorical.joint_prob_dt
        )
      }

      # Update the x_explain in internal_copy such that in the next sampling step use the values in dt
      # as the conditional feature values. Furthermore, we set n_MC_samples to 1 such that we in the next
      # step generate one new value for each of the n_MC_samples MC samples we have begun to generate.
      internal_copy$data$x_explain <- dt
      internal_copy$parameters$n_explain <- nrow(dt)
      internal_copy$parameters$n_MC_samples <- 1
    }

    # Save the now populated data table
    dt_list[[index_feature_idx]] <- dt
  }

  # Combine the list of data tables and add the id columns
  dt <- data.table::rbindlist(dt_list, fill = TRUE)
  dt[, id_coalition := rep(index_features, each = n_MC_samples * n_explain)]
  dt[, id := rep(seq(n_explain), each = n_MC_samples, times = length(index_features))]
  dt[, w := 1 / n_MC_samples]
  data.table::setcolorder(dt, c("id_coalition", "id", feature_names))

  # Aggregate the weights for the non-unique rows such that we only return a data table with unique rows.
  # Only done for these approaches as they are the only approaches that are likely to return duplicates.
  if (approach %in% c("independence", "empirical", "ctree", "categorical")) {
    dt <- dt[, list(w = sum(w)), by = c("id_coalition", "id", feature_names)]
  }

  return(dt)
}
