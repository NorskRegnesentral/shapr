# Sort functions --------------------------------------------------------------------------------------------------
# Copied from `gtools` package
#' @keyword internal
mixedsort <- function(x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, numeric.type = c("decimal", "roman"),
                      roman.case = c("upper", "lower", "both"), scientific = TRUE) {
  x[mixedorder(x,
    decreasing = decreasing, na.last = na.last, blank.last = blank.last, numeric.type = numeric.type,
    roman.case = roman.case, scientific = scientific
  )]
}

#' @keyword internal
mixedorder <- function(x, decreasing = FALSE, na.last = TRUE, blank.last = FALSE, numeric.type = c("decimal", "roman"),
                       roman.case = c("upper", "lower", "both"), scientific = TRUE) {
  numeric.type <- match.arg(numeric.type)
  roman.case <- match.arg(roman.case)
  if (length(x) < 1) {
    return(NULL)
  } else if (length(x) == 1) {
    return(1)
  }
  if (!is.character(x)) {
    return(order(x, decreasing = decreasing, na.last = na.last))
  }
  delim <- "\\$\\@\\$"
  if (numeric.type == "decimal") {
    if (scientific) {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)(?:(?:[eE])(?:(?:[-+]?)(?:[0123456789]+))|)))"
    } else {
      regex <- "((?:(?i)(?:[-+]?)(?:(?=[.]?[0123456789])(?:[0123456789]*)(?:(?:[.])(?:[0123456789]{0,}))?)))"
    }
    numeric <- function(x) as.numeric(x)
  } else if (numeric.type == "roman") {
    regex <- switch(roman.case,
      both = "([IVXCLDMivxcldm]+)",
      upper = "([IVXCLDM]+)",
      lower = "([ivxcldm]+)"
    )
    numeric <- function(x) roman2int(x)
  } else {
    stop("Unknown value for numeric.type: ", numeric.type)
  }
  x <- as.character(x)
  which.nas <- which(is.na(x))
  which.blanks <- which(x == "")
  delimited <- gsub(regex, paste(delim, "\\1", delim, sep = ""), x, perl = TRUE)
  step1 <- strsplit(delimited, delim)
  step1 <- lapply(step1, function(x) x[x > ""])
  suppressWarnings(step1.numeric <- lapply(step1, numeric))
  suppressWarnings(step1.character <- lapply(step1, function(x) ifelse(is.na(numeric(x)), toupper(x), NA)))
  maxelem <- max(sapply(step1, length))
  step1.numeric.t <- lapply(1:maxelem, function(i) sapply(step1.numeric, function(x) x[i]))
  step1.character.t <- lapply(1:maxelem, function(i) sapply(step1.character, function(x) x[i]))
  rank.numeric <- sapply(step1.numeric.t, rank)
  rank.character <- sapply(step1.character.t, function(x) as.numeric(factor(x)))
  rank.numeric[!is.na(rank.character)] <- 0
  rank.character <- t(t(rank.character) + apply(matrix(rank.numeric), 2, max, na.rm = TRUE))
  rank.overall <- ifelse(is.na(rank.character), rank.numeric, rank.character)
  order.frame <- as.data.frame(rank.overall)
  if (length(which.nas) > 0) order.frame[which.nas, ] <- if (is.na(na.last)) NA else if (na.last) Inf else -Inf
  if (length(which.blanks) > 0) {
    order.frame[which.blanks, ] <- if (is.na(blank.last)) NA else if (blank.last) 1e+99 else -1e+99
  }
  order.frame <- as.list(order.frame)
  order.frame$decreasing <- decreasing
  order.frame$na.last <- NA
  retval <- do.call("order", order.frame)
  return(retval)
}

#' Sort the list of features
#'
#' The function sort a list of positive integer vectors of variable size by first
#' sorting the elements in each vector in increasing order (if `sort_features = TRUE`),
#' then sorts the whole list based on the vector lengths (if `sort_size = TRUE`),
#' and for vectors of the same length, the function sort based on the coalitions indices
#' (if `sort_coalitions = TRUE`), i.e., vector `c(1,2,3)` comes before `c(1,3,4)`.
#'
#' @param feature_list List of positive integer vectors of variable size.
#' @param sort_size Logical. If `TRUE` (default), then we sort the `features_list` based on the coalition sizes.
#' @param sort_coalitions Logical. If `TRUE` (default), then we sort the coalitions of equal size in `features_list`
#' based on the feature indices. Note that the features in each coalition must be sorted for this to work.
#' The user either has to do this prior or set `sort_features = TRUE`.
#' @param sort_features Logical. If `TRUE` (default), then we sort the features in each coalition.
#'
#' @return
#' @keyword internal
#' @author Lars Henry Berge Olsen
#' @examples
#' sort_feature_list(list(3, 1, c(101, 7, 111), c(7, 101, 1), c(7, 99, 111), c(2, 1), c(1, 101)))
sort_feature_list <- function(features_list, sort_features = TRUE, sort_size = TRUE, sort_coalitions = TRUE) {
  # Sort each coalition to have increasing feature indices
  if (sort_features) features_list <- lapply(features_list, sort)

  # Sort the list such that the coalition size is increasing
  if (sort_size) features_list <- features_list[order(sapply(features_list, length))]

  # Sort the coalitions with equal size such coalitions with lower feature indices come first (1,2,3 before 1,3,4)
  if (sort_coalitions) {
    lengths <- sapply(features_list, length)
    lengths_unique <- unique(lengths)
    index_list <- lapply(lengths_unique, function(length) which(lengths == length))
    names(index_list) <- lengths_unique
    features_list_order <- c()
    for (length in lengths_unique) {
      length_order <- mixedorder(sapply(features_list[lengths == length], paste, collapse = ","))
      features_list_order <- c(features_list_order, index_list[[as.character(length)]][length_order])
    }
    features_list <- features_list[features_list_order]
  }

  # Return the sorted features list
  return(features_list)
}



# Check functions -------------------------------------------------------------------------------------------------
#' Check that all explicands has at least one valid MC sample in causal Shapley values
#'
#' @param dt Data.table containing the generated MC samples (and conditional values) after each sampling step
#' @inheritParams explain
#' @inheritParams create_marginal_data_categoric
#' @inheritParams create_marginal_data_training
#'
#' @keywords internal
#'
#' @author Lars Henry Berge Olsen
check_categorical_valid_MCsamp <- function(dt, n_explain, n_MC_samples, joint_probability_dt) {
  dt_factor <- dt[, .SD, .SDcols = is.factor] # Get the columns that have been inserted into
  dt_factor_names <- copy(names(dt_factor)) # Get their names. Copy as we are to change dt_factor
  dt_factor[, id := rep(seq(n_explain), each = n_MC_samples)] # Add an id column
  dt_valid_coals <- joint_probability_dt[, ..dt_factor_names] # Get the valid feature coalitions
  dt_invalid <- dt_factor[!dt_valid_coals, on = dt_factor_names] # Get non valid coalitions
  explicand_all_invalid <- dt_invalid[, .N, by = id][N == n_MC_samples] # Get if all samples for an explicand are invalid
  if (nrow(explicand_all_invalid) > 0) {
    stop(paste0(
      "An explicand has no valid MC feature coalitions. Increase `n_MC_samples` or provide ",
      "`joint_prob_dt` containing the probaibilities for unlikely coalitions, too."
    ))
  }
}


#' Auxiliary function that verifies that the coalitions respect the causal order
#'
#' @param coalitions List of integer vectors containing the coalitions indicating which
#' features to conditioning on that we are to check against the causal ordering.
#'
#' @param causal_ordering List of vectors containing the partial causal ordering.
#' The elements in the list represents the components in the causal ordering and can either
#' be a single feature index or several, that is, a vector. For example, we can have
#' `list(c(1,2), c(3, 4))`, which means that `1,2 -> 3` and `1,2 -> 4`, i.e., one and
#' two are the ancestors of three and four, but three and four are not related.
#'
#' @return Logical array indicating whether the coalitions respect the causal order or not.
#'
#' @keywords internal
#'
#' @examples
#' coalitions <- list(c(1, 2, 3, 5), c(1, 2, 4), c(1, 4))
#' causal_ordering <- list(1:3, 4:7, 8:10)
#' check_coalitions_respect_order(coalitions, causal_ordering) # c(TRUE, FALSE, FALSE)
#' check_coalitions_respect_order(c(1, 2, 3, 5), causal_ordering) # TRUE
#' check_coalitions_respect_order(list(c(1, 2, 3, 5)), causal_ordering) # TRUE
#' check_coalitions_respect_order(list(c(1, 2, 5)), causal_ordering) # FALSE
#' check_coalitions_respect_order(list(c(1:7, 10)), causal_ordering) # TRUE
#' check_coalitions_respect_order(list(c(1:3, 5:6, 10)), causal_ordering) # FALSE
#'
#' @author Lars Henry Berge Olsen
check_coalitions_respect_order <- function(coalitions, causal_ordering) {
  sapply(coalitions, function(coalition) {
    for (feature in coalition) { # Iterate over the features in the coalition
      # Get which component in the causal ordering the feature is part of
      id_component <- Position(function(component) feature %in% component, causal_ordering, nomatch = 0)
      if (id_component != 1) { # If not the root component
        ancestors <- unlist(causal_ordering[1:(id_component - 1)]) # Get ancestors of the component
        # if (!setequal(ancestors, intersect(ancestors, coalition))) return(FALSE) # Check all ancestors in coalition
        if (!all(ancestors %in% coalition)) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  })
}

#' Auxiliary function that verifies that the coalitions respect the causal order
#'
#' @param coalitions List of integer vectors containing the coalitions indicating which
#' features to conditioning on that we are to check against the causal ordering.
#'
#' @param causal_ordering List of vectors containing the partial causal ordering.
#' The elements in the list represents the components in the causal ordering and can either
#' be a single feature index or several, that is, a vector. For example, we can have
#' `list(c(1,2), c(3, 4))`, which means that `1,2 -> 3` and `1,2 -> 4`, i.e., one and
#' two are the ancestors of three and four, but three and four are not related.
#'
#' @return Logical array indicating whether the coalitions respect the causal order or not.
#'
#' @keywords internal
#'
#' @examples
#' coalitions <- list(c(1, 2, 3, 5), c(1, 2, 4), c(1, 4))
#' causal_ordering <- list(1:3, 4:7, 8:10)
#' check_coalitions_respect_order_slow(coalitions, causal_ordering) # c(TRUE, FALSE, FALSE)
#' check_coalitions_respect_order_slow(c(1, 2, 3, 5), causal_ordering) # TRUE
#' check_coalitions_respect_order_slow(list(c(1, 2, 3, 5)), causal_ordering) # TRUE
#' check_coalitions_respect_order_slow(list(c(1, 2, 5)), causal_ordering) # FALSE
#' check_coalitions_respect_order_slow(list(c(1:7, 10)), causal_ordering) # TRUE
#' check_coalitions_respect_order_slow(list(c(1:3, 5:6, 10)), causal_ordering) # FALSE
#'
#' @author Lars Henry Berge Olsen
check_coalitions_respect_order_slow <- function(coalitions, causal_ordering) {
  if (!is.list(coalitions)) coalitions <- list(coalitions) # Ensure that we are given a list and not a vector
  n_causal_ordering <- length(causal_ordering) # Get the number of causal orderings

  # Create a vector to store all ancestors for each causal position/component
  ancestors <- list(integer(0)) # The root component has no ancestors
  if (n_causal_ordering > 1) ancestors <- c(ancestors, Reduce(c, causal_ordering[-n_causal_ordering], acc = TRUE))

  # Array to store which coalitions respects the `causal_ordering`. Change to FALSE if coalition does not.
  coalition_respects_order <- rep(TRUE, length(coalitions))

  # Iterate over the coalitions
  for (coalition_idx in seq_along(coalitions)) {
    coalition <- coalitions[[coalition_idx]]

    # Iterate over the features in the coalition
    for (feature in coalition) {
      # Extract which component the feature is part of (a number between 1 and `length(causal_ordering)`)
      feature_component <- Position(function(ith_component) feature %in% ith_component, causal_ordering)

      # # The feature should always be in the causal_ordering, thus, this is not necessary
      # if (is.na(feature_position)) stop("`feature_position` should never be `NA`.")

      # Get the ancestors of the feature from the pre-computed ancestors list
      current_ancestors <- ancestors[[feature_component]]

      # Check that all ancestors of the feature are present in the coalition, if not, then set to FALSE
      if (!all(current_ancestors %in% coalition)) coalition_respects_order[coalition_idx] <- FALSE
    }
  }

  # Return whether the coalitions respect the causal order or not
  return(coalition_respects_order)
}

#' Auxiliary function that verifies that the number of coalitions is valid
#'
#' @inheritParams explain
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
check_n_coalitions_causal <- function(n_coalitions, causal_ordering) {
  # Check that we have a legit number of coalitions.
  n_coalitions_max <- get_max_n_coalitions_causal(causal_ordering)
  if (n_coalitions < 2 || n_coalitions > n_coalitions_max) {
    stop(paste0(
      "`n_coalitions` (", n_coalitions, ") must be a strictly postive integer larger than or equal to ",
      "two and less than the number of coalitions respecting the causal ordering (", n_coalitions_max, ")."
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
    stop(paste0(
      "`causal_ordering` contains ", feat_group_txt, " names (`",
      paste0(unlist(causal_ordering)[is.na(causal_ordering_match)], collapse = "`, `"), "`) ",
      "that are not in the data (`", paste0(labels, collapse = "`, `"), "`).\n"
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
#' @param n_explain Integer. The number of explicands/observations to explain.
#' @param Sbar_features Vector of integers containing the features indices to generate marginal observations for.
#' That is, if `Sbar_features` is `c(1,4)`, then we sample `n_MC_samples` observations from \eqn{P(X_1, X_4)} using the
#' empirical training observations (with replacements). That is, we sample the first and fourth feature values from
#' the same training observation, so we do not break the dependence between them.
#' @param n_explain Integer. The number of explicands/observations to explain.
#' @param stable_version Logical. If `TRUE` and `n_MC_samples` > `n_train`, then we include each training observation
#' `n_MC_samples %/% n_train` times and then sample the remaining `n_MC_samples %% n_train samples`. Only the latter is
#' done when `n_MC_samples < n_train`. This is done separately for each explicand. If `FALSE`, we randomly sample the
#' from the observations.
#'
#' @inheritParams explain
#'
#' @return Data table of dimension \eqn{`n_MC_samples` \times `length(Sbar_features)`} with the sampled observations.
#'
#' @examples
#' data("airquality")
#' data <- data.table::as.data.table(airquality)
#' data <- data[complete.cases(data), ]
#'
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' ind_x_explain <- 1:6
#' x_train <- data[-ind_x_explain, ..x_var]
#' x_train
#' create_marginal_data__training(x_train = x_train, Sbar_features = c(1, 4), n_MC_samples = 10)
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
  return(x_train[sampled_indices, ..Sbar_features])
}

#' Create marginal categorical data for causal Shapley values
#'
#' @description
#' This function is used when we generate marginal data for the categorical approach when we have several sampling
#' steps. We need to treat this separately, as we here in the marginal step CANNOT make feature values such
#' that the combination of those and the feature values we condition in S are NOT in
#' `categorical.joint_prob_dt`. If we do this, then we cannot progress further in the chain of sampling
#' steps. E.g., X1 \in {1,2,3}, X2 \in {1,2,3}, and X3 \in {1,2,3}.
#' We know X2 = 2, and let causal structure be X1 -> X2 -> X3. Assume that
#' P(X1 = 1, X2 = 2, X = 3) = P(X1 = 2, X2 = 2, X = 3) = 1/2. Then there is no point
#' generating X1 = 3, as we then cannot generate X3.
#' The solution is only to generate the values which can proceed through the whole
#' chain of sampling steps. To do that, we have to ensure the the marginal sampling
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
#' @param joint_prob_dt Data.table containing the joint probability distribution for each coalition of feature values.
#' @inheritParams explain
#'
#' @return Data table of dimension \eqn{(`n_MC_samples` * `nrow(x_explain)`) \times `length(Sbar_features)`} with the
#' sampled observations.
#'
#' @keywords internal
#'
#' @author Lars Henry Berge Olsen
create_marginal_data_categoric <- function(n_MC_samples,
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
  dt_valid_coalitions <- unique(joint_prob_dt[, ..relevant_features])

  # Get relevant feature coalitions that are valid for the explicands
  dt_valid_coalitions_relevant <- data.table::merge.data.table(x_explain_copy[, ..S_original_names_with_id],
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
    dt_valid_coal_marg_prob[, .SD[sample(.N, n_MC_samples, replace = TRUE, prob = prob)], by = id][, ..Sbar_now_names]
  return(dt_return)
}







# Get functions ---------------------------------------------------------------------------------------------------
#' Get all coalitions satisfying the causal ordering
#'
#' @inheritParams check_coalitions_respect_order
#' @param sort_features_in_coalitions Boolean. If `TRUE`, then the feature indices in the
#' coalitions are sorted in increasing order. Note that this gives the same order as creating
#' all coalitions and then removing the coalitions that does not satisfy the causal ordering
#' using [check_coalitions_respect_order()]. If `FALSE`, then the function maintains the
#' order of features within each group given in `causal_ordering`.
#'
#' @return List of vectors containing all coalitions that respects the causal ordering.
#' @keywords internal
#'
#' @examples
#' get_legit_causal_coalitions(list(1:3, 4:7, 8:10))
#' get_legit_causal_coalitions(list(1:3, c(4, 8), c(5, 7), 6, 9:10))
#' get_legit_causal_coalitions(list(3:1, c(8, 4), c(7, 5), 6, 9:10)) # Same as previous due to sorting
#'
#' # This function is equivalent of creating all coalitions and only keeping the valid coalitions
#' m <- 11
#' causal_ordering <- list(3:1, c(8, 4), c(7, 5), 6, 9:10, 11) # All m features must be in this list
#' dt <- data.table::data.table(features = unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE))
#' all.equal(
#'   get_legit_causal_coalitions(causal_ordering, sort_features_in_coalitions = TRUE),
#'   dt[check_coalitions_respect_order(features, causal_ordering)]$features
#' )
#'
#' @author Lars Henry Berge Olsen
get_legit_causal_coalitions <- function(causal_ordering, sort_features_in_coalitions = TRUE) {
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
#' @inheritParams check_coalitions_respect_order
#'
#' @details The function computes the number of coalitions that respects the causal ordering by computing the number
#' of coalitions in each partial causal component and then summing these. We compute
#' the number of coalitions in the \eqn{i}th a partial causal component by \eqn{2^n - 1},
#' where \eqn{n} is the number of features in the the \eqn{i}th partial causal component
#' and we subtract one as we do not want to include the situation where no features in
#' the \eqn{i}th partial causal component are present. In the end, we add 1 for the
#' empty coalition.
#'
#' @examples
#' get_max_n_coalitions_causal(list(1:10)) # 2^10 = 1024 (no causal order)
#' get_max_n_coalitions_causal(list(1:3, 4:7, 8:10)) # 30
#' get_max_n_coalitions_causal(list(1:3, 4:5, 6:7, 8, 9:10)) # 18
#' get_max_n_coalitions_causal(list(1:3, c(4, 8), c(5, 7), 6, 9:10)) # 18
#' get_max_n_coalitions_causal(list(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) # 11
#'
#' @return Integer. The (maximum) number of coalitions that respects the causal ordering.
#' @keywords internal
#' @author Lars Henry Berge Olsen
get_max_n_coalitions_causal <- function(causal_ordering) {
  return(sum(2^sapply(causal_ordering, length)) - length(causal_ordering) + 1)
}

#' Get the steps for generating MC samples for coalitions following a causal ordering
#'
#' @inheritParams check_coalitions_respect_order
#' @param S ADD inheritParams from somewhere else. NOTE that we assume that this S has been checked. I.e., it only
#' contains coalitions that respects the causal order.
#' @param confounding Boolean or boolean vector specifying which features are affected by confounding. If a single
#' boolean is given, then each component is given this value. Otherwise, `confounding` must be a vector of length
#' `causal_ordering` specifying if each component in the causal order is subject to confounding or not.
#' @param as_string Boolean. If the returned object is to be a list of lists of integers or a list of vectors of strings.
#'
#' @return Depends on the value of the parameter `as_string`. If a string, then `results[j]` is a vector specifying
#' the process of generating the samples for coalition `j`. The length of `results[j]` is the number of steps, and
#' `results[j][i]` is a string of the form `features_to_sample|features_to_condition_on`. If the
#' `features_to_condition_on` part is blank, then we are to sample from the marginal distribution.
#' For `as_string == FALSE`, then we rather return a vector where `results[[j]][[i]]` contains the elements
#' `Sbar` and `S` representing the features to sample and condition on, respectively.
#'
#' @examples
#' m <- 5
#' causal_ordering <- list(1:2, 3:4, 5)
#' S <- shapr::feature_matrix_cpp(get_legit_causal_coalitions(causal_ordering = causal_ordering), m = m)
#' confounding <- c(TRUE, TRUE, FALSE)
#' get_S_causal_steps(S, causal_ordering, confounding, as_string = TRUE)
#'
#' # Look at the effect of changing the confounding assumptions
#' SS1 <- get_S_causal_steps(S, causal_ordering, confounding = c(FALSE, FALSE, FALSE), as_string = TRUE)
#' SS2 <- get_S_causal_steps(S, causal_ordering, confounding = c(TRUE, FALSE, FALSE), as_string = TRUE)
#' SS3 <- get_S_causal_steps(S, causal_ordering, confounding = c(TRUE, TRUE, FALSE), as_string = TRUE)
#' SS4 <- get_S_causal_steps(S, causal_ordering, confounding = c(TRUE, TRUE, TRUE), as_string = TRUE)
#'
#' all.equal(SS1, SS2)
#' SS1[[2]] # Condition on 1 as there is no confounding in the first component
#' SS2[[2]] # Do NOT condition on 1 as there is confounding in the first component
#' SS1[[3]]
#' SS2[[3]]
#'
#' all.equal(SS1, SS3)
#' SS1[[2]] # Condition on 1 as there is no confounding in the first component
#' SS3[[2]] # Do NOT condition on 1 as there is confounding in the first component
#' SS1[[5]] # Condition on 3 as there is no confounding in the second component
#' SS3[[5]] # Do NOT condition on 3 as there is confounding in the second component
#' SS1[[6]]
#' SS3[[6]]
#'
#' all.equal(SS2, SS3)
#' SS2[[5]]
#' SS3[[5]]
#' SS2[[6]]
#' SS3[[6]]
#'
#' all.equal(SS3, SS4) # No difference as the last component is a singleton
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
get_S_causal_steps <- function(S, causal_ordering, confounding, as_string = FALSE) {
  # List to store the sampling process
  results <- vector("list", nrow(S))
  names(results) <- paste0("id_coalition_", seq(nrow(S)))

  # Iterate over the coalitions
  for (j in seq(2, nrow(S) - 1)) {
    # Get the given and dependent features for this coalition
    index_given <- seq(ncol(S))[as.logical(S[j, ])]
    index_dependent <- seq(ncol(S))[as.logical(1 - S[j, ])]

    # Iterate over the causal orderings
    for (i in seq(length(causal_ordering))) {
      # check overlap between index_dependent and ith causal component
      to_sample <- intersect(causal_ordering[[i]], index_dependent)

      if (length(to_sample) > 0) {
        to_condition <- unlist(causal_ordering[0:(i - 1)]) # Condition on all features in ancestor components

        # If confounding is FALSE, add intervened features in the same component to the `to_condition` set.
        # If confounding is TRUE, then no extra conditioning.
        if (!confounding[i]) to_condition <- union(intersect(causal_ordering[[i]], index_given), to_condition)

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


# Feature functions -----------------------------------------------------------------------------------------------
#' Create all possible coalitions
#'
#' @param m
#' @param causal_ordering
#' @param weight_zero_m
#'
#' @examples
#' m <- 5
#' causal_ordering <- list(1:2, 3:4, 5)
#' X <- feature_exact_causal(m = m, causal_ordering = causal_ordering)
#' X
#'
#' @keywords internal
#' @author Martin Jullum and Lars Henry Berge Olsen
feature_exact_causal <- function(m, causal_ordering, weight_zero_m = 10^6) {
  # TODO: talk with Martin. We do not need the split here as `causal_ordering` is going to be
  # list(1:m) when no ordering is provided and then `get_legit_causal_coalitions` will produce

  if (length(causal_ordering[[1]]) == m) {
    # Regular
    coalitions <- unlist(lapply(0:m, utils::combn, x = m, simplify = FALSE), recursive = FALSE)
  } else {
    # New version using the causal ordering
    coalitions <- get_legit_causal_coalitions(causal_ordering)
  }

  dt <- data.table::data.table(id_coalition = seq(length(coalitions)))
  dt[, features := coalitions]
  dt[, n_features := length(features[[1]]), id_coalition]
  dt[, N := .N, n_features]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  return(dt)
}

#' Sample a subset of possible coalitions that satisfy the causal ordering
#'
#' @param m
#' @param causal_ordering
#' @param n_coalitions
#' @param weight_zero_m
#'
#' @examples
#' m <- 5
#' causal_ordering <- list(1:2, 3:4, 5)
#' n_coalitions <- 5
#' X <- feature_not_exact_causal(m = m, causal_ordering = causal_ordering, n_coalitions = n_coalitions)
#' X
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
feature_not_exact_causal <- function(m, causal_ordering, n_coalitions = 200, weight_zero_m = 10^6) {
  # Check that n_coalitions is a valid number of coalitions
  check_n_coalitions_causal(n_coalitions = n_coalitions, causal_ordering = causal_ordering)

  # Get all legit coalitions
  all_coals <- feature_exact_causal(m = m, causal_ordering = causal_ordering, weight_zero_m = weight_zero_m)

  # Sample the `n_coalitions` relevant coalitions using the `shapley_weights` entries as probabilities
  rel_coals <- sample(seq(2, nrow(all_coals) - 1), size = n_coalitions - 2, prob = all_coals[-c(1, .N), shapley_weight])

  # Extract the empty, sampled/relevant (sorted), and grand coalition, and update the id_coalition counter.
  return(all_coals[c(1, sort(rel_coals), .N), ][, id_coalition := seq(.N)])
}



# Prepare data function -------------------------------------------------------------------------------------------
#' Generate data used for predictions and Monte Carlo integration for causal Shapley values
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
#' @inheritParams default_doc_explain
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
  S_causal_steps_strings <- internal$iter_list[[iter]]$S_causal_steps_strings # TODO: REMOVE

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

    # TODO: REMOVE
    message(paste0(
      "Coalition ", index_feature_idx, " of ", length(index_features),
      " (id_coalition = ", index_feature, ")."
    ))

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
      # TODO: REMOVE
      message(paste0(
        "Sampling step ", sampling_step_idx, " of ", length(S_causal_steps_now),
        " (", S_causal_steps_strings[[index_feature]][sampling_step_idx], ")."
      ))

      # Set flag indicating whether or not we are in the first sampling step, as the the gaussian and copula
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
        # TODO: Can extend to also sample from the marginals of the (gaussian) copula and vaeac
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
          dt_Sbar_now_marginal_values <- create_marginal_data_categoric(
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
          # These approaches produce weighted MC samples, i.e., the do not necessarily generate n_MC_samples MC samples.
          # We ensure n_MC_samples by weighted sampling (with replacements) those ids with not n_MC_samples MC samples.
          n_samp_now <- internal_copy$parameters$n_MC_samples
          dt_new <-
            dt_new[, .SD[if (.N == n_samp_now) seq(.N) else sample(.N, n_samp_now, replace = TRUE, prob = w)], by = id]

          # TODO: remove
          # Check that dt_new has the right number of rows.
          if (nrow(dt_new) != n_explain * n_MC_samples) stop("`dt_new` does not have the right number of rows.\n")
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
          joint_probability_dt = internal$parameters$categorical.joint_prob_dt
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
