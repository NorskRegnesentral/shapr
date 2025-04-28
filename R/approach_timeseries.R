#' @rdname setup_approach
#'
#' @param timeseries.fixed_sigma Positive numeric scalar.
#' Represents the kernel bandwidth in the distance computation.
#' The default value is 2.
#'
#' @param timeseries.bounds Numeric vector of length two.
#' Specifies the lower and upper bounds of the timeseries.
#' The default is `c(NULL, NULL)`, i.e. no bounds.
#' If one or both of these bounds are not `NULL`, we restrict the sampled time series to be between these bounds.
#' This is useful if the underlying time series are scaled between 0 and 1, for example.
#'
#' @inheritParams default_doc_export
#'
#' @export
setup_approach.timeseries <- function(internal,
                                      timeseries.fixed_sigma = 2,
                                      timeseries.bounds = c(NULL, NULL),
                                      ...) {
  defaults <- mget(c("timeseries.fixed_sigma", "timeseries.bounds"))

  internal <- insert_defaults(internal, defaults)

  feature_names <- internal$parameters$feature_names
  feature_specs <- internal$objects$feature_specs

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  if (!all(feature_specs$classes == "numeric")) {
    cli::cli_abort("All features should be numeric to use the timeseries method.")
  }

  return(internal)
}


#' @inheritParams default_doc_export
#'
#' @rdname prepare_data
#' @export
#' @keywords internal
prepare_data.timeseries <- function(internal, index_features = NULL, ...) {
  id <- id_coalition <- w <- NULL

  x_train <- internal$data$x_train
  x_explain <- internal$data$x_explain

  timeseries.fixed_sigma <- internal$parameters$timeseries.fixed_sigma
  timeseries.upper_bound <- internal$parameters$timeseries.bounds[1]
  timeseries.lower_bound <- internal$parameters$timeseries.bounds[2]

  iter <- length(internal$iter_list)

  X <- internal$iter_list[[iter]]$X
  S <- internal$iter_list[[iter]]$S

  if (is.null(index_features)) {
    features <- X$features
  } else {
    features <- X$features[index_features]
  }
  feature_names <- internal$parameters$feature_names

  x_train <- as.matrix(x_train)
  x_explain <- as.matrix(x_explain)

  n_row <- nrow(x_explain)

  dt_l <- list()

  for (i in seq(n_row)) {
    x_explain_i <- x_explain[i, , drop = FALSE]
    dt_l[[i]] <- list()
    tmp <- list()
    tmp[[1]] <- as.data.table(x_explain_i)
    tmp[[1]][, w := 1]
    tmp[[nrow(S)]] <- as.data.table(x_explain_i)
    tmp[[nrow(S)]][, w := 1]

    for (j in 2:(nrow(S) - 1)) {
      diff_S <- diff(c(1, S[j, ], 1))
      Sbar_starts <- which(diff_S == -1)
      Sbar_ends <- which(diff_S == 1) - 1

      cond_1 <- Sbar_starts - 1
      cond_2 <- Sbar_ends + 1
      cond_1[cond_1 == 0] <- cond_2[cond_1 == 0]
      cond_2[cond_2 == (ncol(S) + 1)] <- cond_1[cond_2 == (ncol(S) + 1)]
      len_Sbar_segment <- Sbar_ends - Sbar_starts + 1

      Sbar_segments <- data.frame(Sbar_starts, Sbar_ends, cond_1, cond_2, len_Sbar_segment)

      tmp[[j]] <- matrix(rep(x_explain_i, nrow(x_train)), nrow = nrow(x_train), byrow = TRUE)

      w_vec <- exp(-0.5 * rowSums(
        (matrix(rep(x_explain_i[S[j, ] == 0, drop = FALSE], nrow(x_train)), nrow = nrow(x_train), byrow = TRUE) -
          x_train[, S[j, ] == 0, drop = FALSE])^2
      )
      / timeseries.fixed_sigma^2)

      for (k in seq_len(nrow(Sbar_segments))) {
        impute_these <- seq(Sbar_segments$Sbar_starts[k], Sbar_segments$Sbar_ends[k])

        x_explain_cond_1 <- x_explain_i[, Sbar_segments$cond_1[k]]
        x_explain_cond_2 <- x_explain_i[, Sbar_segments$cond_2[k]]

        x_train_starts <- x_train[, Sbar_segments$Sbar_starts[k]]
        x_train_ends <- x_train[, Sbar_segments$Sbar_ends[k]]

        a_explain <- x_explain_cond_1
        a_train <- x_train_starts

        b_explain <- (x_explain_cond_2 - x_explain_cond_1) / Sbar_segments$len_Sbar_segment[k]
        b_train <- (x_train_ends - x_train_starts) / Sbar_segments$len_Sbar_segment[k]

        lin_mod_explain <- a_explain + b_explain * 0:(Sbar_segments$len_Sbar_segment[k] - 1)
        lin_mod_train <- a_train + b_train %o% (0:(Sbar_segments$len_Sbar_segment[k] - 1))

        to_impute <- (x_train[, impute_these] - lin_mod_train) + matrix(rep(lin_mod_explain, nrow(x_train)),
          nrow = nrow(x_train), byrow = TRUE
        )
        # If the bounds are not null, we floor/ceiling the new time series values
        if (!is.null(timeseries.lower_bound)) {
          to_impute <- pmin(to_impute, timeseries.lower_bound)
        }
        if (!is.null(timeseries.upper_bound)) {
          to_impute <- pmax(to_impute, timeseries.upper_bound)
        }
        tmp[[j]][, impute_these] <- to_impute
      }

      tmp[[j]] <- as.data.table(tmp[[j]])
      tmp[[j]][, w := w_vec / sum(w_vec)]

      # tmp[[j]], j > 1 will have default data.table names V1, V2, ...
      # while tmp[[1]] will have the same names as the features
      names(tmp[[j]]) <- names(tmp[[1]])
    }

    dt_l[[i]] <- rbindlist(tmp, idcol = "id_coalition")
    dt_l[[i]][, id := i]
  }

  dt <- data.table::rbindlist(dt_l, use.names = TRUE, fill = TRUE)
  ret_col <- c("id_coalition", "id", feature_names, "w")
  return(dt[id_coalition %in% index_features, mget(ret_col)])
}
