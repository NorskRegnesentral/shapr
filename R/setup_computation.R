
#' Sets up everything for the Shapley values computation in [shapr::explain()]
#'
#' @inheritParams default_doc
#' @inheritParams explain
#' @inherit default_doc
#' @export
setup_computation <- function(internal, model, predict_model) {
  # model and predict_model are only needed for type AICc of approach empirical, otherwise ignored

  # setup the Shapley framework
  internal <- shapley_setup(internal)

  # Setup for approach
  internal <- setup_approach(internal, model = model, predict_model = predict_model)

  return(internal)
}



#' @keywords internal
shapley_setup <- function(internal) {
  exact <- internal$parameters$exact
  n_features <- internal$parameters$n_features
  n_combinations <- internal$parameters$n_combinations
  is_groupwise <- internal$parameters$is_groupwise

  group_num <- internal$objects$group_num

  X <- internal$objects$X

  # Get all combinations ----------------
  X <- feature_combinations(
    m = n_features,
    exact = exact,
    n_combinations = n_combinations,
    weight_zero_m = 10^6,
    group_num = group_num
  )

  # Get weighted matrix ----------------
  W <- weight_matrix(
    X = X,
    normalize_W_weights = TRUE,
    is_groupwise = is_groupwise
  )

  ## Get feature matrix ---------
  S <- feature_matrix_cpp(
    features = X[["features"]],
    m = n_features
  )

  #### Updating parameters ####

  # Updating parameters$exact as done in feature_combinations
  if (!exact && n_combinations > (2^n_features - 2)) {
    internal$parameters$exact <- TRUE
  }

  internal$parameters$n_combinations <- nrow(S) # Updating this parameter in the end based on what is actually used.
  # This will be obsolete later
  internal$parameters$group_num <- NULL # TODO: Checking whether I could just do this processing where needed
  # instead of storing it

  internal$objects$X <- X
  internal$objects$W <- W
  internal$objects$S <- S
  internal$objects$S_batch <- create_S_batch_new(internal)


  return(internal)
}

#' Define feature combinations, and fetch additional information about each unique combination
#'
#' @param m Positive integer. Total number of features.
#' @param exact Logical. If \code{TRUE} all \code{2^m} combinations are generated, otherwise a
#' subsample of the combinations is used.
#' @param n_combinations Positive integer. Note that if \code{exact = TRUE},
#' \code{n_combinations} is ignored. However, if \code{m > 12} you'll need to add a positive integer
#' value for \code{n_combinations}.
#' @param weight_zero_m Numeric. The value to use as a replacement for infinite combination
#' weights when doing numerical operations.
#' @param group_num List. Contains vector of integers indicating the feature numbers for the
#' different groups.
#'
#' @return A data.table that contains the following columns:
#' \describe{
#' \item{id_combination}{Positive integer. Represents a unique key for each combination. Note that the table
#' is sorted by \code{id_combination}, so that is always equal to \code{x[["id_combination"]] = 1:nrow(x)}.}
#' \item{features}{List. Each item of the list is an integer vector where \code{features[[i]]}
#' represents the indices of the features included in combination \code{i}. Note that all the items
#' are sorted such that \code{features[[i]] == sort(features[[i]])} is always true.}
#' \item{n_features}{Vector of positive integers. \code{n_features[i]} equals the number of features in combination
#' \code{i}, i.e. \code{n_features[i] = length(features[[i]])}.}.
#' \item{N}{Positive integer. The number of unique ways to sample \code{n_features[i]} features
#' from \code{m} different features, without replacement.}
#' }
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
#'
#' @examples
#' # All combinations
#' x <- feature_combinations(m = 3)
#' nrow(x) # Equals 2^3 = 8
#'
#' # Subsample of combinations
#' x <- feature_combinations(exact = FALSE, m = 10, n_combinations = 1e2)
feature_combinations <- function(m, exact = TRUE, n_combinations = 200, weight_zero_m = 10^6, group_num = NULL) {
  m_group <- length(group_num) # The number of groups

  # Force user to use a natural number for n_combinations if m > 13
  if (m > 13 & is.null(n_combinations) & m_group == 0) {
    stop(
      paste0(
        "Due to computational complexity, we recommend setting n_combinations = 10 000\n",
        "if the number of features is larger than 13 for feature-wise Shapley values.\n",
        "Note that you can force the use of the exact method (i.e. n_combinations = NULL)\n",
        "by setting n_combinations equal to 2^m where m is the number of features."
      )
    )
  }

  # Not supported for m > 30
  if (m > 30 & m_group == 0) {
    stop(
      paste0(
        "Currently we are not supporting cases where the number of features is greater than 30\n",
        "for feature-wise Shapley values."
      )
    )
  }
  if (m_group > 30) {
    stop(
      paste0(
        "For computational reasons, we are currently not supporting group-wise Shapley values \n",
        "for more than 30 groups. Please reduce the number of groups."
      )
    )
  }

  if (!exact) {
    if (m_group == 0) {
      # Switch to exact for feature-wise method
      if (n_combinations > (2^m - 2)) {
        n_combinations <- 2^m - 2
        exact <- TRUE
        message(
          paste0(
            "\nSuccess with message:\n",
            "n_combinations is larger than or equal to 2^m = ", 2^m, ". \n",
            "Using exact instead."
          )
        )
      }
    } else {
      # Switch to exact for feature-wise method
      if (n_combinations > (2^m_group - 2)) {
        n_combinations <- 2^m_group - 2
        exact <- TRUE
        message(
          paste0(
            "\nSuccess with message:\n",
            "n_combinations is larger than or equal to 2^group_num = ", 2^m_group, ". \n",
            "Using exact instead."
          )
        )
      }
    }
  }

  if (m_group == 0) {
    # Here if feature-wise Shapley values
    if (exact) {
      dt <- feature_exact(m, weight_zero_m)
    } else {
      dt <- feature_not_exact(m, n_combinations, weight_zero_m)
      stopifnot(
        data.table::is.data.table(dt),
        !is.null(dt[["p"]])
      )
      p <- NULL # due to NSE notes in R CMD check
      dt[, p := NULL]
    }
  } else {
    # Here if group-wise Shapley values
    if (exact) {
      dt <- feature_group(group_num, weight_zero_m)
    } else {
      dt <- feature_group_not_exact(group_num, n_combinations, weight_zero_m)
      stopifnot(
        data.table::is.data.table(dt),
        !is.null(dt[["p"]])
      )
      p <- NULL # due to NSE notes in R CMD check
      dt[, p := NULL]
    }
  }
  return(dt)
}

#' @keywords internal
feature_exact <- function(m, weight_zero_m = 10^6) {
  features <- id_combination <- n_features <- shapley_weight <- N <- NULL # due to NSE notes in R CMD check

  dt <- data.table::data.table(id_combination = seq(2^m))
  combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
  dt[, features := unlist(combinations, recursive = FALSE)]
  dt[, n_features := length(features[[1]]), id_combination]
  dt[, N := .N, n_features]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_features, weight_zero_m)]

  return(dt)
}

#' @keywords internal
feature_not_exact <- function(m, n_combinations = 200, weight_zero_m = 10^6) {
  features <- id_combination <- n_features <- shapley_weight <- N <- NULL # due to NSE notes in R CMD check

  # Find weights for given number of features ----------
  n_features <- seq(m - 1)
  n <- sapply(n_features, choose, n = m)
  w <- shapley_weights(m = m, N = n, n_features) * n
  p <- w / sum(w)

  feature_sample_all = list()
  unique_samples = 0

  while (unique_samples < n_combinations) {

    # Sample number of chosen features ----------
    n_features_sample = c(
      0,
      sample(
        x = n_features,
        size = n_combinations - unique_samples,
        replace = TRUE,
        prob = p
      ),
      m
    )


    # Sample specific set of features -------
    feature_sample <- sample_features_cpp(m, n_features_sample)
    feature_sample_all <- c(feature_sample_all, feature_sample)
    unique_samples = length(unique(feature_sample_all))
  }

  X = data.table(n_features = sapply(feature_sample_all, length))
  X[, n_features := as.integer(n_features)]

  # Get number of occurences and duplicated rows-------
  is_duplicate <- NULL # due to NSE notes in R CMD check
  r <- helper_feature(m, feature_sample_all)
  X[, is_duplicate := r[["is_duplicate"]]]

  # When we sample combinations the Shapley weight is equal
  # to the frequency of the given combination
  X[, shapley_weight := r[["sample_frequence"]]]

  # Populate table and remove duplicated rows -------
  X[, features := feature_sample_all]
  if (any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  X[, is_duplicate := NULL]
  data.table::setkeyv(X, "n_features")

  # Make feature list into character
  X[, features_tmp := sapply(features, paste, collapse = " ")]

  # Aggregate weights by how many samples of a combination we observe
  X = X[, .(n_features = data.table::first(n_features),
            shapley_weight = sum(shapley_weight),
            features = features[1]), features_tmp]

  X[, features_tmp := NULL]
  data.table::setorder(X, n_features)

  # Add shapley weight and number of combinations
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[data.table::between(n_features, 1, m - 1)]]
  X[ind, p := p[n_features]]
  X[ind, N := n[n_features]]

  # Set column order and key table
  data.table::setkeyv(X, "n_features")
  X[, id_combination := .I]
  X[, N := as.integer(N)]
  nms <- c("id_combination", "features", "n_features", "N", "shapley_weight", "p")
  data.table::setcolorder(X, nms)

  return(X)
}

#' Calculate Shapley weight
#'
#' @param m Positive integer. Total number of features/feature groups.
#' @param n_components Positive integer. Represents the number of features/feature groups you want to sample from
#' a feature space consisting of \code{m} unique features/feature groups. Note that \code{ 0 < = n_components <= m}.
#' @param N Positive integer. The number of unique combinations when sampling \code{n_components} features/feature
#' groups, without replacement, from a sample space consisting of \code{m} different features/feature groups.
#' @param weight_zero_m Positive integer. Represents the Shapley weight for two special
#' cases, i.e. the case where you have either \code{0} or \code{m} features/feature groups.
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
helper_feature <- function(m, feature_sample) {
  sample_frequence <- is_duplicate <- NULL # due to NSE notes in R CMD check

  x <- feature_matrix_cpp(feature_sample, m)
  dt <- data.table::data.table(x)
  cnms <- paste0("V", seq(m))
  data.table::setnames(dt, cnms)
  dt[, sample_frequence := as.integer(.N), by = cnms]
  dt[, is_duplicate := duplicated(dt)]
  dt[, (cnms) := NULL]

  return(dt)
}


#' Analogue to feature_exact, but for groups instead.
#'
#' @inheritParams shapley_weights
#' @param group_num List. Contains vector of integers indicating the feature numbers for the
#' different groups.
#'
#' @return data.table with all feature group combinations, shapley weights etc.
#'
#' @keywords internal
feature_group <- function(group_num, weight_zero_m = 10^6) {

  # due to NSE notes in R CMD check
  features <- id_combination <- n_features <- shapley_weight <- N <- groups <- n_groups <- NULL

  m <- length(group_num)
  dt <- data.table::data.table(id_combination = seq(2^m))
  combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)

  dt[, groups := unlist(combinations, recursive = FALSE)]
  dt[, features := lapply(groups, FUN = group_fun, group_num = group_num)]
  dt[, n_groups := length(groups[[1]]), id_combination]
  dt[, n_features := length(features[[1]]), id_combination]
  dt[, N := .N, n_groups]
  dt[, shapley_weight := shapley_weights(m = m, N = N, n_components = n_groups, weight_zero_m)]

  return(dt)
}

#' @keywords internal
group_fun <- function(x, group_num) {
  if (length(x) != 0) {
    unlist(group_num[x])
  } else {
    integer(0)
  }
}


#' Analogue to feature_not_exact, but for groups instead.
#'
#' @inheritParams shapley_weights
#' @inheritParams feature_group
#'
#' @return data.table with all feature group combinations, shapley weights etc.
#'
#' @keywords internal
feature_group_not_exact <- function(group_num, n_combinations = 200, weight_zero_m = 10^6) {
  # due to NSE notes in R CMD check
  features <- id_combination <- n_features <- shapley_weight <- N <- groups <- n_groups <- NULL

  # Find weights for given number of features ----------
  m <- length(group_num)
  n_groups <- seq(m - 1)
  n <- sapply(n_groups, choose, n = m)
  w <- shapley_weights(m = m, N = n, n_groups) * n
  p <- w / sum(w)

  # Sample number of chosen features ----------
  X <- data.table::data.table(
    n_groups = c(
      0,
      sample(
        x = n_groups,
        size = n_combinations,
        replace = TRUE,
        prob = p
      ),
      m
    )
  )
  X[, n_groups := as.integer(n_groups)]

  # Sample specific set of features -------
  data.table::setkeyv(X, "n_groups")
  group_sample <- sample_features_cpp(m, X[["n_groups"]])

  # Get number of occurences and duplicated rows-------
  is_duplicate <- NULL # due to NSE notes in R CMD check
  r <- helper_feature(m, group_sample)
  X[, is_duplicate := r[["is_duplicate"]]]

  # When we sample combinations the Shapley weight is equal
  # to the frequency of the given combination
  X[, shapley_weight := r[["sample_frequence"]]]

  # Populate table and remove duplicated rows -------
  X[, groups := group_sample]
  if (any(X[["is_duplicate"]])) {
    X <- X[is_duplicate == FALSE]
  }
  X[, is_duplicate := NULL]

  # Add shapley weight and number of combinations
  X[c(1, .N), shapley_weight := weight_zero_m]
  X[, N := 1]
  ind <- X[, .I[data.table::between(n_groups, 1, m - 1)]]
  X[ind, p := p[n_groups]]
  X[ind, N := n[n_groups]]

  # Adding feature info
  X[, features := lapply(groups, FUN = group_fun, group_num = group_num)]
  X[, n_features := length(features[[1]]), id_combination]


  # Set column order and key table
  data.table::setkeyv(X, "n_groups")
  X[, id_combination := .I]
  X[, N := as.integer(N)]
  nms <- c("id_combination", "groups", "features", "n_groups", "n_features", "N", "shapley_weight", "p")
  data.table::setcolorder(X, nms)

  return(X)
}


#' Calculate weighted matrix
#'
#' @param X data.table
#' @param normalize_W_weights Logical. Whether to normalize the weights for the combinations to sum to 1 for
#' increased numerical stability before solving the WLS (weighted least squares). Applies to all combinations
#' except combination \code{1} and \code{2^m}.
#' @param is_groupwise Logical. Indicating whether group wise Shapley values are to be computed.
#'
#' @return Numeric matrix. See \code{\link{weight_matrix_cpp}} for more information.
#' @keywords internal
#'
#' @author Nikolai Sellereite, Martin Jullum
weight_matrix <- function(X, normalize_W_weights = TRUE, is_groupwise = FALSE) {

  # Fetch weights
  w <- X[["shapley_weight"]]

  if (normalize_W_weights) {
    w[-c(1, length(w))] <- w[-c(1, length(w))] / sum(w[-c(1, length(w))])
  }

  if (!is_groupwise) {
    W <- weight_matrix_cpp(
      subsets = X[["features"]],
      m = X[.N][["n_features"]],
      n = X[, .N],
      w = w
    )
  } else {
    W <- weight_matrix_cpp(
      subsets = X[["groups"]],
      m = X[.N][["n_groups"]],
      n = X[, .N],
      w = w
    )
  }

  return(W)
}

#' @keywords internal
create_S_batch_new <- function(internal, seed = NULL) {

  # due to NSE notes in R CMD check
  n_features <- approach <- n_leftover_first_batch <- n_S_per_approach <- NULL
  n_batches_per_approach <- randomorder <- shapley_weight <- batch <- NULL
  id_combination <- NULL

  n_features0 <- internal$parameters$n_features
  approach0 <- internal$parameters$approach
  n_combinations <- internal$parameters$n_combinations
  n_batches <- internal$parameters$n_batches

  X <- internal$objects$X


  if (length(approach0) > 1) {
    X[!(n_features %in% c(0, n_features0)), approach := approach0[n_features]]

    # Finding the number of batches per approach
    batch_count_dt <- X[!is.na(approach), list(
      n_batches_per_approach =
        pmax(1, round(.N / (n_combinations - 2) * n_batches)),
      n_S_per_approach = .N
    ), by = approach]
    batch_count_dt[, n_leftover_first_batch := n_S_per_approach %% n_batches_per_approach]
    data.table::setorder(batch_count_dt, -n_leftover_first_batch)

    approach_vec <- batch_count_dt[, approach]
    n_batch_vec <- batch_count_dt[, n_batches_per_approach]

    # Randomize order before ordering spreading the batches on the different approaches as evenly as possible
    # with respect to shapley_weight
    set.seed(seed)
    X[, randomorder := sample(.N)]
    data.table::setorder(X, randomorder) # To avoid smaller id_combinations always proceeding large ones
    data.table::setorder(X, shapley_weight)

    batch_counter <- 0
    for (i in seq_along(approach_vec)) {
      X[approach == approach_vec[i], batch := ceiling(.I / .N * n_batch_vec[i]) + batch_counter]
      batch_counter <- X[approach == approach_vec[i], max(batch)]
    }
  } else {
    X[!(n_features %in% c(0, n_features0)), approach := approach0]

    # Spreading the batches
    X[, randomorder := sample(.N)]
    data.table::setorder(X, randomorder)
    data.table::setorder(X, shapley_weight)
    X[!(n_features %in% c(0, n_features0)), batch := ceiling(.I / .N * n_batches)]
  }

  # Assigning batch 1 (which always is the smallest) to the full prediction.
  X[, randomorder := NULL]
  X[id_combination == max(id_combination), batch := 1]
  setkey(X, id_combination)

  # Create a list of the batch splits
  S_groups <- split(X[id_combination != 1, id_combination], X[id_combination != 1, batch])

  return(S_groups)
}
