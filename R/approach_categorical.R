#' @rdname setup_approach
#'
#' @param joint_prob_dt Data.table. (Optional)
#' Containing the joint probability distribution for each combination of feature
#' values.
#' `NULL` means it is estimated from the `x_train` and `x_explain`.
#'
#' @param epsilon Numeric value. (Optional)
#' If \code{joint_prob_dt} is not supplied, probabilities/frequencies are
#' estimated using `x_train`. If certain observations occur in `x_train` and NOT in `x_explain`,
#' then epsilon is used as the proportion of times that these observations occurs in the training data.
#' In theory, this proportion should be zero, but this causes an error later in the Shapley computation.
#'
#' @inheritParams default_doc_explain
#'
#' @export
setup_approach.categorical <- function(internal,
                                       joint_prob_dt = NULL,
                                       epsilon = 0.001,
                                       ...) {
  joint_prob <- N <- id_all <- NULL
  cnms <- internal$parameters$feature_names

  feature_specs <- internal$objects$feature_specs

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  # n_features <- internal$parameters$n_features

  if (!all(feature_specs$classes=="factor")) {
    stop("All test observations should be factors to use the categorical method.")
  }

  # estimate joint_prob_dt if it is not passed to the function
  if (is.null(joint_prob_dt)) {
    train <- data.table::copy(x_train)
    joint_prob_dt0 <- train[,  .N, eval(cnms)]

    test <- data.table::data.table(x_explain)

    test_not_in_train <- data.table::setkeyv(data.table::setDT(test), cnms)[!train]
    N_test_not_in_train <- nrow(unique(test_not_in_train))

    if (N_test_not_in_train > 0) {
      joint_prob_dt0 <- rbind(joint_prob_dt0, cbind(test_not_in_train, N = epsilon))
    }

    joint_prob_dt0[, joint_prob := N / .N]
    joint_prob_dt0[, joint_prob := joint_prob / sum(joint_prob)]
    data.table::setkeyv(joint_prob_dt0, cnms)

    joint_prob_dt <- joint_prob_dt0[, N := NULL][, id_all := .I]

  } else {
    for (i in colnames(x_explain)) {
      is_error <- !(i %in% names(joint_prob_dt)) |
        !all(levels(x_explain[[i]]) %in% levels(joint_prob_dt[[i]]))

      if (is_error > 0) {
        stop("All features in test observations should belong to joint_prob_dt and have the same
             levels as the features in joint_prob_dt.")
      }
    }

    is_error <- !("joint_prob" %in% names(joint_prob_dt)) |
      !all(joint_prob_dt$joint_prob <= 1) |
      !all(joint_prob_dt$joint_prob >= 0) |
      (round(sum(joint_prob_dt$joint_prob), 3) != 1)

    if (is_error > 0) {
      stop('joint_prob_dt must include a column of joint probabilities where the column is called
      "joint_prob", joint_prob_dt$joint_prob must all be greater or equal to 0 and less than or
      equal to 1, and sum(joint_prob_dt$joint_prob must equal 1.')
    }

    joint_prob_dt <- joint_prob_dt[, id_all := .I]
  }

  internal$joint_prob_dt <- joint_prob_dt

  return(internal)
}


#' @inheritParams default_doc
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
prepare_data.categorical <- function(internal, index_features = NULL, ...) {
  id <- id_combination <- w <- NULL # due to NSE notes in R CMD check

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain
  # n_features <- internal$parameters$n_features

  X <- internal$objects$X
  S <- internal$objects$S

  if (is.null(index_features)) { # 2,3
    features <- X$features # list of [1], [2], [2, 3]
  } else {
    features <- X$features[index_features] # list of [1],
  }
  feature_names <- internal$parameters$feature_names

  # id_all is the combination of feature values from the training data
  # id_all is the same as id but it's every combination of features
  # not necessarily the ones in the testing data
  joint_prob_dt <- internal$joint_prob_dt

  feature_conditioned <- paste0(feature_names, "_conditioned")
  feature_conditioned_id <- c(feature_conditioned, "id")

  S_dt <- data.table::data.table(S)
  S_dt[S_dt == 0] <- NA
  S_dt[, id_combination := 1:nrow(S_dt)]

  data.table::setnames(S_dt, c(feature_conditioned, "id_combination"))

  # multiply table of probabilities nrow(S) times
  joint_prob_mult <- joint_prob_dt[rep(id_all, nrow(S))]

  data.table::setkeyv(joint_prob_mult, "id_all")
  j_S_dt <- cbind(joint_prob_mult, S_dt) # first time with conditioned features 1s and NAs

  j_S_feat <- as.matrix(j_S_dt[, feature_names, with = FALSE])
  j_S_mat <- as.matrix(j_S_dt[, feature_conditioned, with = FALSE])

  j_S_feat[which(is.na(j_S_mat))] <- NA
  j_S_feat_with_NA <- data.table::as.data.table(j_S_feat)

  # now we have a data.table with the conditioned
  # features and the feature value but no ids or anything else
  data.table::setnames(j_S_feat_with_NA, feature_conditioned)

  j_S_no_conditioned_features <- data.table::copy(j_S_dt)
  j_S_no_conditioned_features[, (feature_conditioned) := NULL]

  # dt with conditioned features (correct values) + ids + joint_prob
  j_S_all_feat <- cbind(j_S_no_conditioned_features, j_S_feat_with_NA) # features match id_all

  # compute all marginal probabilities
  marg_dt <- j_S_all_feat[, .(marg_prob = sum(joint_prob)), by = feature_conditioned]
  cond_dt <- j_S_all_feat[marg_dt, on = feature_conditioned] # features match id_all

  # compute all conditional probabilities
  cond_dt[, cond_prob := joint_prob / marg_prob]
  cond_dt[id_combination == 1, marg_prob := 0]
  cond_dt[id_combination == 1, cond_prob := 1]

  # this is just to test marginals
  cond_dt_unique <- unique(cond_dt, by = feature_conditioned)
  test <- cond_dt_unique[id_combination != 1][, .(sum_prob = sum(marg_prob)),
                                              by = "id_combination"][["sum_prob"]]
  if (!all(round(test) == 1)) {
    print("Warning - not all marginals sum to 1. There could be a problem going on
          with the joint probabilities. Consider checking.")
  }

  # make the x_explain
  data.table::setkeyv(cond_dt, c("id_combination", "id_all"))
  x_explain_with_id <- data.table::copy(x_explain)[, id := .I]
  dt_just_explain <- cond_dt[x_explain_with_id, on = feature_names] # features match id_all

  # this is a really important step! It allows us to get the proper "w" which will
  # be used in predict()
  dt_explain_just_conditioned <- dt_just_explain[, feature_conditioned_id, with = FALSE]

  cond_dt[, id_all := NULL]

  # features, id_combination, marg_prob, and cond_prob come from cond_dt
  # features_conditioned and id come from dt_test_just_conditioned
  dt <- cond_dt[dt_explain_just_conditioned, on = feature_conditioned, allow.cartesian = TRUE] # features do not match id

  # this is just to test conditional probabilities
  test <- dt[id_combination != 1][, .(sum_prob = sum(cond_prob)),
                                  by = c("id_combination", "id")][["sum_prob"]]
  if (!all(round(test) == 1)) {
    print("Warning - not all conditional probabilities sum to 1. There could be a problem going on
          with the joint probabilities. Consider checking.")
  }

  dt[, w := cond_prob]
  dt[, cond_prob := NULL]

  data.table::setcolorder(dt, c("id_combination", "id"))
  data.table::setkeyv(dt, c("id_combination", "id"))

  # here we merge so that we only return the combintations found in our actual test data
  # this merge does not change the number of rows in dt
  # dt <- merge(dt, x$X[, .(id_combination, n_features)], by = "id_combination")
  # dt[n_features %in% c(0, ncol(x_explain)), w := 1.0]
  dt[id_combination %in% c(1, 2^ncol(x_explain)), w := 1.0]
  ret_col = c("id_combination", "id", feature_names,  "w")
  return(dt[id_combination %in% index_features, ..ret_col])
}


