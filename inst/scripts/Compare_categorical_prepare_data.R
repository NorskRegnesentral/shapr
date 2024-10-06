# File with several proposals for new versions of the `compute_conditional_prob` function used by
# the categorical approach, which are much faster.
# The `compute_conditional_prob_shapr_old` computed a lot of unnecessary things, e.g., it compute the conditional
# prob for all colaitions and then threw away all results not relevant to the coalitions in the batch at the end.
# The `compute_conditional_prob_shapr_new` computes only the relevant stuff for the applicable coalitions in the batch.

# The versions ----------------------------------------------------------------------------------------------------
compute_conditional_prob <- function(S, index_features, x_explain, joint_probability_dt) {

  # Extract the feature names and add an id column to x_explain (copy as this changes `x_explain` outside the function)
  feature_names = names(x_explain)
  x_explain_copy = data.table::copy(x_explain)[,id := .I]

  # Loop over the combinations and convert to a single data table containing all the conditional probabilities
  results = data.table::rbindlist(lapply(index_features, function(index_feature) {

    # Extract the feature names of the features we are to condition on
    cond_cols <- feature_names[S[index_feature,] == 1]
    cond_cols_with_id = c("id", cond_cols)

    # Extract the feature values to condition and including the id column
    dt_conditional_feature_values = x_explain_copy[, ..cond_cols_with_id]

    # Merge (right outer join) the joint_probability_dt data with the conditional feature values
    results_id_combination = joint_probability_dt[dt_conditional_feature_values, on = cond_cols, allow.cartesian = TRUE]

    # Get the weights/conditional probabilities for each valid X_sbar conditioned on X_s for all explicands
    results_id_combination[, w := joint_prob / sum(joint_prob), by = id]
    results_id_combination[, c("id_all", "joint_prob") := NULL]

    # If we have a combination not in the joint prob, then we delete it
    # TODO: or should we do something else?
    # TODO: Comment out the printouts. Only used to debug
    results_not_valid = results_id_combination[is.na(w)]
    str_tmp = paste(sapply(results_not_valid$id, function(i) {
      paste0("(id = ", i, ", ", paste(cond_cols, "=", results_not_valid[id == i,..cond_cols], collapse = ", "), ")")
    }), collapse = ", ")
    paste0("The following explicands where removed as they are not in `joint_probability_dt`: ", str_tmp, ".")

    # Return the data table where we remove the NA entries
    return(results_id_combination[!is.na(w)])
  }), idcol = "id_combination", use.names = TRUE)

  # Update the index_features to their correct value
  results[, id_combination := index_features[id_combination]]

  # Set id_combination and id to be the keys and the two first columns for consistency with other methods
  data.table::setkeyv(results, c("id_combination", "id"))
  data.table::setcolorder(results, c("id_combination", "id", feature_names))

  return(results)
}

compute_conditional_prob_merge <- function(S, index_features, x_explain, joint_probability_dt) {

  # Extract the feature names and add an id column to x_explain (copy as this changes `x_explain` outside the function)
  feature_names = names(x_explain)
  x_explain = data.table::copy(x_explain)[,id := .I]

  # Loop over the combinations and convert to a single data table containing all the conditional probabilities
  results = data.table::rbindlist(lapply(index_features, function(index_feature) {

    # Extract the feature names of the features we are to condition on
    cond_cols <- feature_names[S[index_feature,] == 1]
    cond_cols_with_id = c("id", cond_cols)

    # Extract the feature values to condition and including the id column
    dt_conditional_feature_values = x_explain[, ..cond_cols_with_id]

    # Merge (right outer join) the joint_probability_dt data with the conditional feature values
    results_id_combination <- data.table::merge.data.table(joint_probability_dt, dt_conditional_feature_values, by = cond_cols, allow.cartesian = TRUE)

    # Get the weights/conditional probabilities for each valid X_sbar conditioned on X_s for all explicands
    results_id_combination[, w := joint_prob / sum(joint_prob), by = id]
    results_id_combination[, c("id_all", "joint_prob") := NULL]

    # Return the data table
    return(results_id_combination)
  }), idcol = "id_combination", use.names = TRUE)

  # Update the index_features to their correct value
  results[, id_combination := index_features[id_combination]]

  # Set id_combination and id to be the keys and the two first columns for consistency with other methods
  data.table::setkeyv(results, c("id_combination", "id"))
  data.table::setcolorder(results, c("id_combination", "id", feature_names))

  return(results)
}

compute_conditional_prob_merge_one_coalition <- function(S, index_features, x_explain, joint_probability_dt) {
  if (length(index_features) != 1) stop("`index_features` must be single integer.")

  # Extract the feature names and add an id column to x_explain (copy as this changes `x_explain` outside the function)
  feature_names = names(x_explain)
  x_explain = data.table::copy(x_explain)[,id := .I]

  # Extract the feature names of the features we are to condition on
  cond_cols <- feature_names[S[index_features,] == 1]
  cond_cols_with_id = c("id", cond_cols)

  # Extract the feature values to condition and including the id column
  dt_conditional_feature_values = x_explain[, ..cond_cols_with_id]

  # Merge (right outer join) the joint_probability_dt data with the conditional feature values
  results_id_combination <- data.table::merge.data.table(joint_probability_dt, dt_conditional_feature_values, by = cond_cols, allow.cartesian = TRUE)

  # Get the weights/conditional probabilities for each valid X_sbar conditioned on X_s for all explicands
  results_id_combination[, w := joint_prob / sum(joint_prob), by = id]
  results_id_combination[, c("id_all", "joint_prob") := NULL]

  # Set the index_features to their correct value
  results_id_combination[, id_combination := index_features]

  # Set id_combination and id to be the keys and the two first columns for consistency with other methods
  data.table::setkeyv(results_id_combination, c("id_combination", "id"))
  data.table::setcolorder(results_id_combination, c("id_combination", "id", feature_names))

  return(results_id_combination)
}

compute_conditional_prob_shapr_old = function(S, index_features, x_explain, joint_probability_dt) {

  # Extract the needed objects/variables
  #x_train <- internal$data$x_train
  #x_explain <- internal$data$x_explain
  #joint_probability_dt <- internal$parameters$categorical.joint_prob_dt
  #X <- internal$objects$X
  #S <- internal$objects$S

  # if (is.null(index_features)) { # 2,3
  #   features <- X$features # list of [1], [2], [2, 3]
  # } else {
  #   features <- X$features[index_features] # list of [1],
  # }
  feature_names <- names(x_explain)

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
  dt_temp = dt[id_combination %in% index_features, mget(ret_col)]


  return(dt_temp)
}

compute_conditional_prob_shapr_new <- function(S, index_features, x_explain, joint_probability_dt) {

  # Extract the needed objects/variables
  #x_train <- internal$data$x_train
  #x_explain <- internal$data$x_explain
  #joint_probability_dt <- internal$parameters$categorical.joint_prob_dt
  #X <- internal$objects$X
  #S <- internal$objects$S

  # if (is.null(index_features)) { # 2,3
  #   features <- X$features # list of [1], [2], [2, 3]
  # } else {
  #   features <- X$features[index_features] # list of [1],
  # }
  feature_names <- names(x_explain)

  # TODO: add
  # For causal sampling, we use
  # if (causal_sampling)

  # 3 id columns: id, id_combination, and id_all
  # id: for each x_explain observation
  # id_combination: the rows of the S matrix
  # id_all: identifies the unique combinations of feature values from
  # the training data (not necessarily the ones in the explain data)


  feature_conditioned <- paste0(feature_names, "_conditioned")
  feature_conditioned_id <- c(feature_conditioned, "id")

  S_dt <- data.table::data.table(S[index_features, , drop = FALSE])
  S_dt[S_dt == 0] <- NA
  S_dt[, id_combination := index_features]

  data.table::setnames(S_dt, c(feature_conditioned, "id_combination"))

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
  #cond_dt[id_combination == 1, marg_prob := 0]
  #cond_dt[id_combination == 1, cond_prob := 1]

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

  # dt_just_explain <- rbindlist(lapply(seq(length(index_features)), function(index_features_i) {
  #   feature_names_now = feature_names[S[index_features[index_features_i],] == 1]
  #   cond_dt[x_explain_with_id, on = feature_names_now]
  # }), use.names = TRUE, fill = TRUE)

  dt_just_explain <- cond_dt[x_explain_with_id, on = feature_names]

  # TODO: bare legge til at cond prob er veldig veldig lav?


  # this is a really important step to get the proper "w" which will be used in compute_preds()
  dt_explain_just_conditioned <- dt_just_explain[, feature_conditioned_id, with = FALSE]
  dt_explain_just_conditioned <- dt_just_explain[, ..feature_conditioned_id]

  cond_dt[, id_all := NULL]

  # dt <- rbindlist(lapply(seq(length(index_features)), function(index_features_i) {
  #   feature_conditioned_now = paste0(feature_names[S[index_features[index_features_i],] == 0], "_conditioned")
  #   cond_dt[dt_explain_just_conditioned, on = feature_conditioned_now, allow.cartesian = TRUE]
  # }), use.names = TRUE, fill = TRUE)

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
  # dt[id_combination %in% c(1, 2^ncol(x_explain)), w := 1.0]
  dt_temp = dt[, mget(c("id_combination", "id", feature_names, "w"))]

  return(dt_temp)
}


# compute_conditional_prob_shapr2 <- function(S, index_features, x_explain, joint_probability_dt) {
#   # Extract the feature names
#   feature_names <- names(x_explain)
#
#   # Add an id column to x_explain
#   x_explain = copy(x_explain)[, id := .I]
#
#   # Filter the S matrix and create a data table with only relevant id_combinations
#   relevant_S <- S[index_features, , drop = FALSE]
#   S_dt <- data.table(relevant_S)
#   S_dt[S_dt == 0] <- NA
#   S_dt[, id_combination := index_features]
#
#   # Define feature names with "_conditioned"
#   feature_conditioned <- paste0(feature_names, "_conditioned")
#   feature_conditioned_id <- c(feature_conditioned, "id")
#
#   # Set column names for S_dt
#   setnames(S_dt, c(feature_conditioned, "id_combination"))
#
#   # Replicate the joint_probability_dt for the number of relevant id_combinations
#   joint_probability_mult <- joint_probability_dt[rep(id_all, each = nrow(S_dt))]
#   joint_probability_mult[, id_combination := rep(S_dt$id_combination, each = nrow(joint_probability_dt))]
#
#   # Combine joint_probability_mult with S_dt
#   j_S_dt <- cbind(joint_probability_mult, S_dt)
#
#   # Convert features to matrix and condition them with NAs
#   j_S_feat <- as.matrix(j_S_dt[, feature_names, with = FALSE])
#   j_S_feat_cond <- as.matrix(j_S_dt[, feature_conditioned, with = FALSE])
#   j_S_feat[is.na(j_S_feat_cond)] <- NA
#   j_S_feat_with_NA <- as.data.table(j_S_feat)
#   setnames(j_S_feat_with_NA, feature_conditioned)
#
#   # Combine conditioned features with joint probabilities
#   j_S_no_conditioned_features <- copy(j_S_dt)
#   j_S_no_conditioned_features[, (feature_conditioned) := NULL]
#   j_S_all_feat <- cbind(j_S_no_conditioned_features, j_S_feat_with_NA)
#
#   # Compute marginal probabilities
#   marg_dt <- j_S_all_feat[, .(marg_prob = sum(joint_prob)), by = feature_conditioned]
#
#   # Compute conditional probabilities
#   cond_dt <- j_S_all_feat[marg_dt, on = feature_conditioned]
#   cond_dt[, cond_prob := joint_prob / marg_prob]
#   cond_dt[id_combination == 1, marg_prob := 0]
#   cond_dt[id_combination == 1, cond_prob := 1]
#
#   # Check marginal probabilities
#   cond_dt_unique <- unique(cond_dt, by = feature_conditioned)
#   check <- cond_dt_unique[id_combination != 1][, .(sum_prob = sum(marg_prob)), by = "id_combination"][["sum_prob"]]
#   if (!all(round(check) == 1)) {
#     warning("Not all marginal probabilities sum to 1. There could be a problem with the joint probabilities. Consider checking.")
#   }
#
#   # Merge with x_explain
#   setkeyv(cond_dt, c("id_combination", "id_all"))
#   x_explain_with_id <- copy(x_explain)[, id := .I]
#   dt_just_explain <- cond_dt[x_explain_with_id, on = feature_names]
#
#   # Prepare the explain data
#   dt_explain_just_conditioned <- dt_just_explain[, feature_conditioned_id, with = FALSE]
#   cond_dt[, id_all := NULL]
#   dt <- cond_dt[dt_explain_just_conditioned, on = feature_conditioned, allow.cartesian = TRUE]
#
#   # Check conditional probabilities
#   check <- dt[id_combination != 1][, .(sum_prob = sum(cond_prob)), by = c("id_combination", "id")][["sum_prob"]]
#   if (!all(round(check) == 1)) {
#     warning("Not all conditional probabilities sum to 1. There could be a problem with the joint probabilities. Consider checking.")
#   }
#
#   # Rename and reorder columns
#   setnames(dt, "cond_prob", "w")
#   setkeyv(dt, c("id_combination", "id"))
#
#   # Filter and return relevant combinations
#   dt[id_combination %in% c(1, 2^ncol(x_explain)), w := 1.0]
#   ret_col <- c("id_combination", "id", feature_names, "w")
#   dt_temp <- dt[id_combination %in% index_features, ..ret_col]
#
#   return(dt_temp)
# }

# Comparing -------------------------------------------------------------------------------------------------------
library(data.table)

# Need to have loaded shapr for this to work (`devtools::load_all(".")`)
explanation = explain(
  model = model_lm_categorical,
  x_explain = x_explain_categorical,
  x_train = x_train_categorical,
  approach = "categorical",
  prediction_zero = p0,
  n_batches = 1,
  timing = FALSE
)

S = explanation$internal$objects$S
joint_probability_dt = explanation$internal$parameters$categorical.joint_prob_dt
x_explain = x_explain_categorical

# Chose any values between 2 and 15
index_features = 2:15

dt = compute_conditional_prob(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt)
merge = compute_conditional_prob_merge(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt)
shapr_old = compute_conditional_prob_shapr_old(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt)
shapr_new = compute_conditional_prob_shapr_new(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt)
all.equal(dt, shapr_new)
all.equal(merge, shapr_new)
all.equal(shapr_old, shapr_new)

# Compare with only 1 combination (dt and merge are equally fast, shapr_old is 6 times slower)
index_features = 5
rbenchmark::benchmark(dt = compute_conditional_prob(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      merge = compute_conditional_prob_merge(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      merge_one_coalition = compute_conditional_prob_merge_one_coalition(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_old = compute_conditional_prob_shapr_old(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_new = compute_conditional_prob_shapr_new(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      replications = 500)
# FOR index_features = 2
#                  test replications elapsed relative user.self sys.self user.child sys.child
# 1                  dt          500   1.596    1.136     1.535    0.028          0         0
# 2               merge          500   1.640    1.167     1.527    0.035          0         0
# 3 merge_one_coalition          500   1.405    1.000     1.324    0.024          0         0
# 5           shapr_new          500   6.200    4.413     6.014    0.103          0         0
# 4           shapr_old          500  11.203    7.974    10.032    0.267          0         0

# FOR index_features = 5
#                  test replications elapsed relative user.self sys.self user.child sys.child
# 1                  dt          500   1.529    1.374     1.463    0.045          0         0
# 2               merge          500   1.193    1.072     1.180    0.010          0         0
# 3 merge_one_coalition          500   1.113    1.000     1.098    0.013          0         0
# 5           shapr_new          500   5.705    5.126     5.599    0.068          0         0
# 4           shapr_old          500   8.105    7.282     7.964    0.121          0         0

# FOR index_features = 12
#                  test replications elapsed relative user.self sys.self user.child sys.child
# 1                  dt          500   1.679    1.119     1.623    0.031          0         0
# 2               merge          500   1.553    1.035     1.520    0.020          0         0
# 3 merge_one_coalition          500   1.501    1.000     1.463    0.019          0         0
# 5           shapr_new          500   5.783    3.853     5.619    0.058          0         0
# 4           shapr_old          500   9.833    6.551     9.389    0.269          0         0

# FOR index_features = 12
#                  test replications elapsed relative user.self sys.self user.child sys.child
# 1                  dt          500   2.561    1.891     1.996    0.094          0         0
# 2               merge          500   1.599    1.181     1.520    0.026          0         0
# 3 merge_one_coalition          500   1.354    1.000     1.337    0.013          0         0
# 5           shapr_new          500   5.323    3.931     5.246    0.065          0         0
# 4           shapr_old          500   8.170    6.034     8.019    0.131          0         0


merge = compute_conditional_prob_merge(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt)
merge_one_coalition = compute_conditional_prob_merge_one_coalition(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt)
all.equal(merge, merge_one_coalition)


# Compare with only 4 combination
index_features = c(2,6,9,12)
rbenchmark::benchmark(dt = compute_conditional_prob(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      merge = compute_conditional_prob_merge(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_old = compute_conditional_prob_shapr_old(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_new = compute_conditional_prob_shapr_new(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      replications = 100)
#        test replications elapsed relative user.self sys.self user.child sys.child
# 1        dt          100   0.961    1.016     0.940    0.013          0         0
# 2     merge          100   0.946    1.000     0.919    0.013          0         0
# 4 shapr_new          100   1.368    1.446     1.316    0.025          0         0
# 3 shapr_old          100   2.046    2.163     1.950    0.051          0         0


# Compare with half of the combinations
index_features = seq(2, 15, 2)
rbenchmark::benchmark(dt = compute_conditional_prob(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      merge = compute_conditional_prob_merge(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_old = compute_conditional_prob_shapr_old(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_new = compute_conditional_prob_shapr_new(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      replications = 100)

#        test replications elapsed relative user.self sys.self user.child sys.child
# 1        dt          100   1.614    1.075     1.559    0.028          0         0
# 2     merge          100   1.758    1.171     1.623    0.042          0         0
# 4 shapr_new          100   1.501    1.000     1.437    0.033          0         0
# 3 shapr_old          100   2.001    1.333     1.920    0.038          0         0

# Compare with all the combinations
index_features = seq(2, 15)
rbenchmark::benchmark(dt = compute_conditional_prob(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      merge = compute_conditional_prob_merge(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_old = compute_conditional_prob_shapr_old(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      shapr_new = compute_conditional_prob_shapr_new(S = S, index_features = index_features, x_explain = x_explain, joint_probability_dt = joint_probability_dt),
                      replications = 100)

#        test replications elapsed relative user.self sys.self user.child sys.child
# 1        dt          100   3.435    2.426     3.286    0.077          0         0
# 2     merge          100   3.511    2.480     3.373    0.070          0         0
# 4 shapr_new          100   1.416    1.000     1.363    0.026          0         0
# 3 shapr_old          100   2.153    1.520     2.006    0.045          0         0


