#' @rdname setup_approach
#'
#' @param arf.num_trees Positive integer.
#' The number of trees in the adversarial random forest.
#'
#' @param arf.min_node_size Positive integer.
#' The minimum number of observations in each terminal node.
#'
#' @param arf.delta Non-negative numeric scalar.
#' Tuning parameter passed to [arf::adversarial_rf()].
#'
#' @param arf.max_iters Positive integer.
#' The maximum number of adversarial forest iterations.
#'
#' @param arf.alpha Numeric scalar between 0 and 1.
#' Tuning parameter passed to [arf::forde()].
#'
#' @param arf.epsilon Positive numeric scalar.
#' Small regularization constant passed to [arf::forde()].
#'
#' @param arf.parallel_train Logical scalar.
#' If `TRUE`, [arf::adversarial_rf()] and [arf::forde()] use parallel processing
#' when training the feature distribution.
#' The training step uses `ranger` threads, controlled by `options(ranger.num.threads = n)`
#' or the `R_RANGER_NUM_THREADS` environment variable. The [arf::forde()] step uses
#' `foreach` and only runs in parallel when a `foreach` backend is registered.
#'
#' @param arf.parallel_gen Logical scalar.
#' If `TRUE`, [arf::forge()] uses `foreach` parallel processing when generating
#' samples for the Monte Carlo integration used to estimate `v(S)`.
#' This can be faster for ARF-heavy explanations, but is best combined with sequential
#' shapr batching, for example `extra_computation_args = list(vS_batching_method = "forloop",
#' min_n_batches = 1, max_batch_size = Inf)`, to avoid nested parallelization with `future`.
#'
#' @inheritParams default_doc_export
#'
#' @export
setup_approach.arf <- function(internal,
                               arf.num_trees = 10L,
                               arf.min_node_size = 2L,
                               arf.delta = 0,
                               arf.max_iters = 10L,
                               arf.alpha = 0.1,
                               arf.epsilon = 1e-15,
                               arf.parallel_train = TRUE,
                               arf.parallel_gen = FALSE,
                               ...) {
  defaults <- mget(c(
    "arf.num_trees", "arf.min_node_size", "arf.delta", "arf.max_iters",
    "arf.alpha", "arf.epsilon", "arf.parallel_train", "arf.parallel_gen"
  ))

  internal <- insert_defaults(internal, defaults)

  x_train <- internal$data$x_train
  num_trees <- internal$parameters$arf.num_trees
  min_node_size <- internal$parameters$arf.min_node_size
  delta <- internal$parameters$arf.delta
  max_iters <- internal$parameters$arf.max_iters
  alpha <- internal$parameters$arf.alpha
  epsilon <- internal$parameters$arf.epsilon
  seed <- internal$parameters$seed
  parallel_train <- internal$parameters$arf.parallel_train

  check_arf_available()
  check_arf_extra_parameters(internal$parameters)
  check_arf_parameters(
    num_trees = num_trees,
    min_node_size = min_node_size,
    delta = delta,
    max_iters = max_iters,
    alpha = alpha,
    epsilon = epsilon,
    parallel_train = parallel_train,
    parallel_gen = internal$parameters$arf.parallel_gen
  )

  arf_fit <- arf::adversarial_rf(
    x_train,
    num_trees = num_trees,
    min_node_size = min_node_size,
    delta = delta,
    max_iters = max_iters,
    verbose = FALSE,
    parallel = parallel_train,
    seed = seed
  )

  internal$objects$arf_sampler <- arf::forde(
    arf_fit,
    x_train,
    finite_bounds = "local",
    alpha = alpha,
    epsilon = epsilon,
    parallel = parallel_train
  )

  return(internal)
}


#' @inheritParams default_doc_internal
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
prepare_data.arf <- function(internal, index_features = NULL, ...) {
  x_explain <- internal$data$x_explain
  n_explain <- internal$parameters$n_explain
  n_MC_samples <- internal$parameters$n_MC_samples
  n_features <- internal$parameters$n_features
  feature_names <- internal$parameters$feature_names
  arf_sampler <- internal$objects$arf_sampler
  parallel_gen <- internal$parameters$arf.parallel_gen

  iter <- length(internal$iter_list)
  x <- internal$iter_list[[iter]]$X

  if (is.null(index_features)) {
    features <- x$features
    coalition_ids <- seq_along(features)
  } else {
    features <- x$features[index_features]
    coalition_ids <- index_features
  }

  unfeatures <- lapply(features, function(x) setdiff(1:n_features, x))

  evidence_list <- vector("list", length(features))
  for (i in seq_along(features)) {
    evidence_list[[i]] <- data.table::copy(x_explain)
    data.table::set(evidence_list[[i]], j = unfeatures[[i]], value = NA)
  }

  evidence <- data.table::rbindlist(evidence_list)

  dt <- data.table::as.data.table(arf::forge(
    arf_sampler,
    n_synth = n_MC_samples,
    evidence = evidence,
    evidence_row_mode = "separate",
    parallel = parallel_gen
  ))

  dt[, w := 1 / n_MC_samples]
  dt[, id := rep(rep(seq_len(n_explain), each = n_MC_samples), length(features))]
  dt[, id_coalition := rep(seq_along(features), each = n_MC_samples * n_explain)]
  dt[, id_coalition := coalition_ids[id_coalition]]
  dt[id_coalition %in% c(1, 2^n_features), w := 1.0]
  data.table::setcolorder(dt, "id_coalition")

  # only return unique dt
  dt2 <- dt[, sum(w), by = c("id_coalition", feature_names, "id")]
  data.table::setnames(dt2, "V1", "w")

  return(dt2)
}

#' @keywords internal
check_arf_available <- function() {
  if (!requireNamespace("arf", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg arf} package is required for {.code approach = \"arf\"}.",
      "i" = "Install {.pkg arf} to use this approach."
    ))
  }
}

#' @keywords internal
check_arf_parameters <- function(num_trees,
                                 min_node_size,
                                 delta,
                                 max_iters,
                                 alpha,
                                 epsilon,
                                 parallel_train,
                                 parallel_gen) {
  if (!is_arf_whole_number(num_trees) || num_trees <= 0) {
    cli::cli_abort("{.arg arf.num_trees} must be a single positive integer.")
  }
  if (!is_arf_whole_number(min_node_size) || min_node_size <= 0) {
    cli::cli_abort("{.arg arf.min_node_size} must be a single positive integer.")
  }
  if (!is.numeric(delta) || length(delta) != 1 || is.na(delta) || delta < 0) {
    cli::cli_abort("{.arg arf.delta} must be a single non-negative numeric value.")
  }
  if (!is_arf_whole_number(max_iters) || max_iters <= 0) {
    cli::cli_abort("{.arg arf.max_iters} must be a single positive integer.")
  }
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) || alpha < 0 || alpha > 1) {
    cli::cli_abort("{.arg arf.alpha} must be a single numeric value between 0 and 1.")
  }
  if (!is.numeric(epsilon) || length(epsilon) != 1 || is.na(epsilon) || epsilon <= 0) {
    cli::cli_abort("{.arg arf.epsilon} must be a single positive numeric value.")
  }
  if (!is.logical(parallel_train) || length(parallel_train) != 1 || is.na(parallel_train)) {
    cli::cli_abort("{.arg arf.parallel_train} must be a single logical value.")
  }
  if (!is.logical(parallel_gen) || length(parallel_gen) != 1 || is.na(parallel_gen)) {
    cli::cli_abort("{.arg arf.parallel_gen} must be a single logical value.")
  }
}

#' @keywords internal
is_arf_whole_number <- function(x) {
  is.numeric(x) && length(x) == 1 && !is.na(x) && is.finite(x) && x == as.integer(x)
}

#' @keywords internal
check_arf_extra_parameters <- function(parameters) {
  if ("num.threads" %in% names(parameters)) {
    cli::cli_abort(c(
      "{.arg num.threads} cannot be passed through {.fn explain} for {.code approach = \"arf\"}.",
      "i" = "Use {.code options(ranger.num.threads = n)} or the {.envvar R_RANGER_NUM_THREADS} environment variable."
    ))
  }
}
