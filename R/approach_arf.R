#' @rdname setup_approach
#'
#' @inheritParams default_doc_explain
#'
#' @export
setup_approach.arf <- function(internal,
                               arf.num_trees = 10L,
                               arf.min_node_size = 2L,
                               arf.delta = 0,
                               arf.max_iters = 10L,
                               ...) {
  defaults <- mget(c("arf.num_trees", "arf.min_node_size", "arf.delta", "arf.max_iters"))

  internal <- insert_defaults(internal, defaults)

  x_train <- internal$data$x_train

  num_trees <- internal$parameters$arf.num_trees
  min_node_size <- internal$parameters$arf.min_node_size
  delta <- internal$parameters$arf.delta
  max_iters <- internal$parameters$arf.max_iters

  arf0 <- arf::adversarial_rf(x_train,
                              num_trees = num_trees,
                              min_node_size = min_node_size,
                              delta = delta,
                              max_iters = max_iters,
                              verbose = FALSE)

  internal$objects$arf_sampler <- arf::forde(arf0, x_train)

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

  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X

  dt_l <- list()


  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }


  for (i in seq_len(n_explain)) {
    l <- list()
    for(j in seq_along(features)) {
      evi <- x_explain[i,.SD, .SDcols = features[[j]]]

      samp <- arf::forge(arf_sampler, n_synth = n_MC_samples, evidence = evi)

      l[[j]] <- samp

    }
    dt_l[[i]] <- data.table::rbindlist(l, idcol = "id_coalition")
    dt_l[[i]][, w := 1 / n_MC_samples]
    dt_l[[i]][, id := i]
    if (!is.null(index_features)) dt_l[[i]][, id_coalition := index_features[id_coalition]]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  dt[id_coalition %in% c(1, 2^n_features), w := 1.0]

  # only return unique dt
  dt2 <- dt[, sum(w), by = c("id_coalition", feature_names, "id")]
  setnames(dt2, "V1", "w")

  return(dt2)
}

