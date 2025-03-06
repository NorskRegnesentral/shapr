#' @rdname setup_approach
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
  defaults <- mget(c("arf.num_trees", "arf.min_node_size", "arf.delta", "arf.max_iters",
                     "arf.alpha", "arf.epsilon", "arf.parallel_train", "arf.parallel_gen"))

  internal <- insert_defaults(internal, defaults)

  x_train <- internal$data$x_train

  num_trees <- internal$parameters$arf.num_trees
  min_node_size <- internal$parameters$arf.min_node_size
  delta <- internal$parameters$arf.delta
  max_iters <- internal$parameters$arf.max_iters
  alpha <- internal$parameters$arf.alpha
  epsilon <- internal$parameters$arf.epsilon
  parallel_train <- internal$parameters$arf.parallel_train


  arf0 <- arf::adversarial_rf(x_train,
                              num_trees = num_trees,
                              min_node_size = min_node_size,
                              delta = delta,
                              max_iters = max_iters,
                              verbose = FALSE,
                              parallel = parallel_train)

  internal$objects$arf_sampler <- arf::forde(arf0,
                                             x_train,
                                             finite_bounds = "local",
                                             alpha = alpha,
                                             epsilon = epsilon,
                                             parallel = parallel_train)

  return(internal)
}


#' @inheritParams default_doc
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

  X <- internal$iter_list[[iter]]$X

  dt_l <- list()


  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }


  aa=Sys.time()
  unfeatures <- lapply(features, function(x) setdiff(1:n_features, x))

  evi_list <- list()
  for(j in seq_along(features)) {
    evi_list[[j]] <- data.table::copy(x_explain)
    data.table::set(evi_list[[j]], j = unfeatures[[j]], value = NA)
  }

  evi <- data.table::rbindlist(evi_list)

  dt <- as.data.table(arf::forge(arf_sampler,
                                 n_synth = n_MC_samples,
                                 evidence = evi,
                                 evidence_row_mode = "separate",
                                 parallel = parallel_gen))

  dt[, w := 1 / n_MC_samples]
  dt[, id := rep(rep(seq_len(n_explain),each = n_MC_samples), length(features))]
  dt[, id_coalition := rep(seq_along(features),each = n_MC_samples*n_explain)]
  dt[, id_coalition := index_features[id_coalition]]
  dt[id_coalition %in% c(1, 2^n_features), w := 1.0]
  setcolorder(dt,"id_coalition")

  # only return unique dt
  dt2 <- dt[, sum(w), by = c("id_coalition", feature_names, "id")]
  setnames(dt2, "V1", "w")

  return(dt2)
}

