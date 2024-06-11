#' @rdname setup_approach
#'
#' @param categorical.joint_prob_dt Data.table. (Optional)
#' Containing the joint probability distribution for each combination of feature
#' values.
#' `NULL` means it is estimated from the `x_train` and `x_explain`.
#'
#' @param categorical.epsilon Numeric value. (Optional)
#' If \code{joint_probability_dt} is not supplied, probabilities/frequencies are
#' estimated using `x_train`. If certain observations occur in `x_explain` and NOT in `x_train`,
#' then epsilon is used as the proportion of times that these observations occurs in the training data.
#' In theory, this proportion should be zero, but this causes an error later in the Shapley computation.
#'
#' @inheritParams default_doc_explain
#'
#' @export
setup_approach.categorical <- function(internal,
                                       categorical.joint_prob_dt = NULL,
                                       categorical.epsilon = 0.001,
                                       ...) {
  defaults <- mget(c("categorical.joint_prob_dt", "categorical.epsilon"))
  internal <- insert_defaults(internal, defaults)

  # Extract the needed objects/variables
  joint_probability_dt <- internal$parameters$categorical.joint_prob_dt
  epsilon <- internal$parameters$epsilon
  feature_names <- internal$parameters$feature_names
  feature_specs <- internal$objects$feature_specs
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  # The method is only applicable for categorical datasets
  if (!all(feature_specs$classes == "factor")) stop("All features should be factors to use the categorical method.")

  # estimate joint_prob_dt if it is not passed to the function
  if (is.null(joint_probability_dt)) {
    # Get the frequency of the unique feature value combinations in the training data
    joint_prob_dt0 <- x_train[, .N, eval(feature_names)]

    # Get the feature value combinations in the explicands that are NOT in the training data and their frequency
    explain_not_in_train <- data.table::setkeyv(data.table::setDT(x_explain), feature_names)[!x_train]
    N_explain_not_in_train <- nrow(unique(explain_not_in_train))

    # Add these feature value combinations, and their corresponding frequency, to joint_prob_dt0
    if (N_explain_not_in_train > 0) {
      joint_prob_dt0 <- rbind(joint_prob_dt0, cbind(explain_not_in_train, N = categorical.epsilon))
    }

    # Compute the joint probability for each feature value combination
    joint_prob_dt0[, joint_prob := N / sum(N)]
    data.table::setkeyv(joint_prob_dt0, feature_names)

    # Remove the frequency column and add an id column
    joint_probability_dt <- joint_prob_dt0[, N := NULL][, id_all := .I]
  } else {
    # The `joint_probability_dt` is passed to explain by the user. Verify it.
    for (feature in colnames(x_explain)) {
      # Check that feature is present
      if (!feature %in% names(joint_probability_dt)) {
        stop(paste0("The `", feature, "` feature is in `x_explain` but not in `joint_probability_dt`."))
      }

      # Check that the feature has the same levels
      if (!all(levels(x_explain[[feature]]) %in% levels(joint_probability_dt[[feature]])) > 0) {
        stop(paste0("The `", feature, "` feature has different levels in `x_explain` than in `joint_probability_dt`."))
      }
    }

    # Check that dt contains a `joint_prob` col all entries are probabilities between 0 and 1 (inclusive) and add to 1.
    if (!("joint_prob" %in% names(joint_probability_dt)) ||
        !all(joint_probability_dt$joint_prob <= 1) ||
        !all(joint_probability_dt$joint_prob >= 0) ||
        (round(sum(joint_probability_dt$joint_prob), 3) != 1)) {
      stop('joint_probability_dt must include a column of joint probabilities called "joint_prob".
      joint_prob must all be greater or equal to 0 and less than or equal to 1.
      sum(joint_prob) must equal to 1.')
    }

    # Add an id column
    joint_probability_dt <- joint_probability_dt[, id_all := .I]
  }

  # Store the `joint_probability_dt` data table
  internal$parameters$categorical.joint_prob_dt <- joint_probability_dt

  return(internal)
}


#' @inheritParams default_doc
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
prepare_data.categorical_old <- function(internal, index_features = NULL, ...) {
  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  joint_probability_dt <- internal$parameters$categorical.joint_prob_dt

  X <- internal$objects$X
  S <- internal$objects$S

  # TODO: Remove `features` below as they are not used in this function, the same with `X` above.
  if (is.null(index_features)) { # 2,3
    features <- X$features # list of [1], [2], [2, 3]
  } else {
    features <- X$features[index_features] # list of [1],
  }
  feature_names <- internal$parameters$feature_names


  # 3 id columns: id, id_combination, and id_all
  # id: for each x_explain observation
  # id_combination: the rows of the S matrix
  # id_all: identifies the unique combinations of feature values from
  # the training data (not necessarily the ones in the explain data)


  feature_conditioned <- paste0(feature_names, "_conditioned")
  feature_conditioned_id <- c(feature_conditioned, "id")

  S_dt <- data.table::data.table(S)
  S_dt[S_dt == 0] <- NA
  S_dt[, id_combination := seq_len(nrow(S_dt))]

  data.table::setnames(S_dt, c(feature_conditioned, "id_combination"))

  # (1) Compute marginal probabilities

  # multiply table of probabilities nrow(S) times
  joint_probability_mult <- joint_probability_dt[rep(id_all, nrow(S))]

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
  cond_dt[id_combination == 1, marg_prob := 0]
  cond_dt[id_combination == 1, cond_prob := 1]

  # check marginal probabilities
  cond_dt_unique <- unique(cond_dt, by = feature_conditioned)
  check <- cond_dt_unique[id_combination != 1][, .(sum_prob = sum(marg_prob)),
    by = "id_combination"
  ][["sum_prob"]]
  if (!all(round(check) == 1)) {
    print("Warning - not all marginal probabilities sum to 1. There could be a problem
          with the joint probabilities. Consider checking.")
  }

  # make x_explain
  data.table::setkeyv(cond_dt, c("id_combination", "id_all"))
  x_explain_with_id <- data.table::copy(x_explain)[, id := .I]
  dt_just_explain <- cond_dt[x_explain_with_id, on = feature_names]

  # this is a really important step to get the proper "w" which will be used in compute_preds()
  dt_explain_just_conditioned <- dt_just_explain[, feature_conditioned_id, with = FALSE]

  cond_dt[, id_all := NULL]
  dt <- cond_dt[dt_explain_just_conditioned, on = feature_conditioned, allow.cartesian = TRUE]

  # check conditional probabilities
  check <- dt[id_combination != 1][, .(sum_prob = sum(cond_prob)),
    by = c("id_combination", "id")
  ][["sum_prob"]]
  if (!all(round(check) == 1)) {
    print("Warning - not all conditional probabilities sum to 1. There could be a problem
          with the joint probabilities. Consider checking.")
  }

  setnames(dt, "cond_prob", "w")
  data.table::setkeyv(dt, c("id_combination", "id"))

  # here we merge so that we only return the combintations found in our actual explain data
  # this merge does not change the number of rows in dt
  # dt <- merge(dt, x$X[, .(id_combination, n_features)], by = "id_combination")
  # dt[n_features %in% c(0, ncol(x_explain)), w := 1.0]
  dt[id_combination %in% c(1, 2^ncol(x_explain)), w := 1.0]
  ret_col <- c("id_combination", "id", feature_names, "w")

  # TODO: Update function such that we only compute the relevant combinations above
  return(dt[id_combination %in% index_features, mget(ret_col)])
}




#' @inheritParams default_doc
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
prepare_data.categorical <- function(internal, index_features = NULL, ...) {

  # 3 id columns: id, id_combination, and id_all
  # id: for each x_explain observation
  # id_combination: the rows of the S matrix
  # id_all: identifies the unique combinations of feature values from
  # the training data (not necessarily the ones in the explain data)

  # Extract the needed objects/variables
  x_explain <- internal$data$x_explain
  joint_probability_dt <- internal$parameters$categorical.joint_prob_dt

  feature_names <- internal$parameters$feature_names
  feature_conditioned <- paste0(feature_names, "_conditioned")
  feature_conditioned_id <- c(feature_conditioned, "id")

  S_dt <- data.table::data.table(internal$objects$S[index_features, , drop = FALSE])
  S_dt[S_dt == 0] <- NA
  S_dt[, id_combination := index_features]
  data.table::setnames(S_dt, c(feature_conditioned, "id_combination"))

  # (1) Compute marginal probabilities

  # multiply table of probabilities length(index_features) times
  joint_probability_mult <- joint_probability_dt[rep(id_all, length(index_features))]

  data.table::setkeyv(joint_probability_mult, "id_all")
  j_S_dt <- cbind(joint_probability_mult, S_dt) # combine joint probability and S matrix

  j_S_feat <- as.matrix(j_S_dt[, ..feature_names]) # with zeros
  j_S_feat_cond <- as.matrix(j_S_dt[, ..feature_conditioned])

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
  check <- cond_dt_unique[id_combination != 1][, .(sum_prob = sum(marg_prob)), by = "id_combination"][["sum_prob"]]
  if (!all(round(check) == 1)) {
    print("Warning - not all marginal probabilities sum to 1. There could be a problem
          with the joint probabilities. Consider checking.")
  }

  # make x_explain
  data.table::setkeyv(cond_dt, c("id_combination", "id_all"))
  x_explain_with_id <- data.table::copy(x_explain)[, id := .I]
  dt_just_explain <- cond_dt[x_explain_with_id, on = feature_names]

  # this is a really important step to get the proper "w" which will be used in compute_preds()
  dt_explain_just_conditioned <- dt_just_explain[, ..feature_conditioned_id]

  cond_dt[, id_all := NULL]
  dt <- cond_dt[dt_explain_just_conditioned, on = feature_conditioned, allow.cartesian = TRUE]

  # check conditional probabilities
  check <- dt[id_combination != 1][, .(sum_prob = sum(cond_prob)), by = c("id_combination", "id")][["sum_prob"]]
  if (!all(round(check) == 1)) {
    print("Warning - not all conditional probabilities sum to 1. There could be a problem
          with the joint probabilities. Consider checking.")
  }

  setnames(dt, "cond_prob", "w")
  data.table::setkeyv(dt, c("id_combination", "id"))

  # Return the relevant columns
  return(dt[, mget(c("id_combination", "id", feature_names, "w"))])
}


