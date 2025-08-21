#' @rdname setup_approach
#'
#' @param categorical.joint_prob_dt Data.table. (Optional)
#' Containing the joint probability distribution for each combination of feature
#' values.
#' `NULL` means it is estimated from the `x_train` and `x_explain`.
#'
#' @param categorical.epsilon Numeric value. (Optional)
#' If `categorical.joint_prob_dt` is not supplied, probabilities/frequencies are
#' estimated using `x_train`. If certain observations occur in `x_explain` and NOT in `x_train`,
#' then epsilon is used as the proportion of times that these observations occur in the training data.
#' In theory, this proportion should be zero, but this causes an error later in the Shapley computation.
#'
#' @inheritParams default_doc_export
#'
#' @export
setup_approach.categorical <- function(internal,
                                       categorical.joint_prob_dt = NULL,
                                       categorical.epsilon = 0.001,
                                       ...) {
  defaults <- mget(c("categorical.joint_prob_dt", "categorical.epsilon"))
  internal <- insert_defaults(internal, defaults)

  joint_probability_dt <- internal$parameters$categorical.joint_prob_dt
  epsilon <- internal$parameters$epsilon

  feature_names <- internal$parameters$feature_names
  feature_specs <- internal$objects$feature_specs

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  if (!all(feature_specs$classes == "factor")) {
    cli::cli_abort("All features should be factors to use the categorical method.")
  }

  # estimate joint_prob_dt if it is not passed to the function
  if (is.null(joint_probability_dt)) {
    # Get the frequency of the unique feature value combinations in the training data
    joint_prob_dt0 <- x_train[, .N, eval(feature_names)]

    # Get the feature value combinations in the explicands that are NOT in the training data and their frequency
    explain_not_in_train <- data.table::setkeyv(data.table::setDT(data.table::copy(x_explain)), feature_names)[!x_train]
    N_explain_not_in_train <- nrow(unique(explain_not_in_train))

    # Add these feature value combinations, and their corresponding frequency, to joint_prob_dt0
    if (N_explain_not_in_train > 0) {
      joint_prob_dt0 <- rbind(joint_prob_dt0, cbind(explain_not_in_train, N = categorical.epsilon))
    }

    # Compute the joint probability for each feature value combination
    joint_prob_dt0[, joint_prob := N / .N]
    joint_prob_dt0[, joint_prob := joint_prob / sum(joint_prob)]
    data.table::setkeyv(joint_prob_dt0, feature_names)

    # Remove the frequency column and add an id column
    joint_probability_dt <- joint_prob_dt0[, N := NULL][, id_all := .I]
  } else {
    # The `joint_probability_dt` is passed to explain by the user, and we do some checks.
    for (i in colnames(x_explain)) {
      # Check that feature name is present
      is_error <- !(i %in% names(joint_probability_dt))

      if (is_error > 0) {
        cli::cli_abort(paste0(i, " is in x_explain but not in joint_probability_dt."))
      }

      # Check that the feature has the same levels
      is_error <- !all(levels(x_explain[[i]]) %in% levels(joint_probability_dt[[i]]))

      if (is_error > 0) {
        cli::cli_abort(paste0(i, " in x_explain has different factor levels than in joint_probability_dt."))
      }
    }

    # Check that dt contains a `joint_prob` column; all entries are probabilities between
    # 0 and 1 (inclusive) and sum to 1.
    is_error <- !("joint_prob" %in% names(joint_probability_dt)) |
      !all(joint_probability_dt$joint_prob <= 1) |
      !all(joint_probability_dt$joint_prob >= 0) |
      (round(sum(joint_probability_dt$joint_prob), 3) != 1)

    if (is_error > 0) {
      cli::cli_abort('joint_probability_dt must include a column of joint probabilities called "joint_prob".
      All joint_prob values must be greater than or equal to 0 and less than or equal to 1.
      The sum of joint_prob must equal 1.')
    }

    # Add an id column
    joint_probability_dt <- joint_probability_dt[, id_all := .I]
  }

  # Store the `joint_probability_dt` data table
  internal$parameters$categorical.joint_prob_dt <- joint_probability_dt

  return(internal)
}


#' @inheritParams default_doc_internal
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
#' @author Annabelle Redelmeier and Lars Henry Berge Olsen
prepare_data.categorical <- function(internal, index_features = NULL, ...) {
  # Use a faster function when index_features contains only a single coalition, as in causal Shapley values.
  if (length(index_features) == 1) {
    return(prepare_data_single_coalition(internal, index_features))
  }

  # 3 id columns: id, id_coalition, and id_all
  # id: for each x_explain observation
  # id_coalition: the rows of the S matrix
  # id_all: identifies the unique combinations of feature values from
  # the training data (not necessarily the ones in the explain data)


  # Extract the needed objects/variables
  x_explain <- internal$data$x_explain
  joint_probability_dt <- internal$parameters$categorical.joint_prob_dt
  feature_names <- internal$parameters$feature_names
  feature_conditioned <- paste0(feature_names, "_conditioned")
  feature_conditioned_id <- c(feature_conditioned, "id")

  # Extract from iterative list
  iter <- length(internal$iter_list)
  S <- internal$iter_list[[iter]]$S
  S_dt <- data.table::data.table(S[index_features, , drop = FALSE])
  S_dt[S_dt == 0] <- NA
  S_dt[, id_coalition := index_features]
  data.table::setnames(S_dt, c(feature_conditioned, "id_coalition"))

  # (1) Compute marginal probabilities

  # multiply table of probabilities length(index_features) times
  joint_probability_mult <- joint_probability_dt[rep(id_all, length(index_features))]

  data.table::setkeyv(joint_probability_mult, "id_all")
  j_S_dt <- cbind(joint_probability_mult, S_dt) # combine joint probability and S matrix

  j_S_feat <- as.matrix(j_S_dt[, feature_names, with = FALSE]) # with zeros
  j_S_feat_cond <- as.matrix(j_S_dt[, feature_conditioned, with = FALSE])

  j_S_feat[which(is.na(j_S_feat_cond))] <- NA # with NAs
  j_S_feat_with_NA <- data.table::as.data.table(j_S_feat)

  # now we have a data.table with the conditioned
  # features and the feature value but no ids
  data.table::setnames(j_S_feat_with_NA, feature_conditioned)

  j_S_no_conditioned_features <- data.table::copy(j_S_dt)
  j_S_no_conditioned_features[, (feature_conditioned) := NULL]

  # dt with conditioned features (correct values) + ids + joint_prob
  j_S_all_feat <- cbind(j_S_no_conditioned_features, j_S_feat_with_NA) # features match id_all

  # compute all marginal probabilities
  marg_dt <- j_S_all_feat[, .(marg_prob = sum(joint_prob)), by = feature_conditioned]

  # (2) Compute conditional probabilities

  cond_dt <- j_S_all_feat[marg_dt, on = feature_conditioned]
  cond_dt[, cond_prob := joint_prob / marg_prob]

  # check marginal probabilities
  cond_dt_unique <- unique(cond_dt, by = feature_conditioned)
  check <- cond_dt_unique[id_coalition != 1][, .(sum_prob = sum(marg_prob)), by = "id_coalition"][["sum_prob"]]
  if (!all(round(check) == 1)) {
    msg <- paste0(
      "Not all marginal probabilities sum to 1. There could be a problem with the joint probabilities. ",
      "Consider checking."
    )
    cli::cli_warn(c("!" = msg), immediate. = TRUE)
  }

  # make x_explain
  data.table::setkeyv(cond_dt, c("id_coalition", "id_all"))
  x_explain_with_id <- data.table::copy(x_explain)[, id := .I]
  dt_just_explain <- cond_dt[x_explain_with_id, on = feature_names]

  # this is a really important step to get the proper "w" which will be used in compute_preds()
  dt_explain_just_conditioned <- dt_just_explain[, feature_conditioned_id, with = FALSE]

  cond_dt[, id_all := NULL]
  dt <- cond_dt[dt_explain_just_conditioned, on = feature_conditioned, allow.cartesian = TRUE]

  # check conditional probabilities
  check <- dt[id_coalition != 1][, .(sum_prob = sum(cond_prob)), by = c("id_coalition", "id")][["sum_prob"]]
  if (!all(round(check) == 1)) {
    msg <- paste0(
      "Not all conditional probabilities sum to 1. There could be a problem with the joint probabilities. ",
      "Consider checking."
    )
    cli::cli_warn(c("!" = msg), immediate. = TRUE)
  }

  setnames(dt, "cond_prob", "w")
  data.table::setkeyv(dt, c("id_coalition", "id"))

  # Return the relevant columns
  return(dt[, mget(c("id_coalition", "id", feature_names, "w"))])
}

#' Compute the conditional probabilities for a single coalition for the categorical approach
#'
#' The [prepare_data.categorical()] function is slow when evaluated for a single coalition.
#' This is a bottleneck for Causal Shapley values which call said function a lot with single coalitions.
#'
#' @inheritParams default_doc_internal
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
prepare_data_single_coalition <- function(internal, index_features) {
  # Extract the needed objects
  x_explain <- internal$data$x_explain
  feature_names <- internal$parameters$feature_names
  joint_probability_dt <- internal$parameters$categorical.joint_prob_dt

  # Extract from iterative list
  iter <- length(internal$iter_list)
  S <- internal$iter_list[[iter]]$S

  # Add an id column to x_explain (copy as this changes `x_explain` outside the function)
  x_explain_copy <- data.table::copy(x_explain)[, id := .I]

  # Extract the feature names of the features we are to condition on
  cond_cols <- feature_names[S[index_features, ] == 1]
  cond_cols_with_id <- c("id", cond_cols)

  # Extract the feature values to condition and including the id column
  dt_conditional_feature_values <- x_explain_copy[, cond_cols_with_id, with = FALSE]

  # Merge (right outer join) the joint_probability_dt data with the conditional feature values
  results_id_coalition <- data.table::merge.data.table(joint_probability_dt,
    dt_conditional_feature_values,
    by = cond_cols,
    allow.cartesian = TRUE
  )

  # Get the weights/conditional probabilities for each valid X_sbar conditioned on X_s for all explicands
  results_id_coalition[, w := joint_prob / sum(joint_prob), by = id]
  results_id_coalition[, c("id_all", "joint_prob") := NULL]

  # Set the index_features to their correct value
  results_id_coalition[, id_coalition := index_features]

  # Set id_coalition and id to be the keys and the two first columns for consistency with other approaches
  data.table::setkeyv(results_id_coalition, c("id_coalition", "id"))
  data.table::setcolorder(results_id_coalition, c("id_coalition", "id", feature_names))

  return(results_id_coalition)
}
