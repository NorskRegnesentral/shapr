#' Get combinations
#'
#' @inheritParams global_arguments
#'
#' @details
#' The returned data.table contains the following columns
#' \describe{
#' \item{ID}{Positive integer. Unique key for combination}
#' \item{features}{List}
#' \item{nfeautres}{Positive integer}
#' \item{N}{Positive integer}
#' }
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
feature_combinations <- function(m, exact = TRUE, noSamp = 200, shapley_weight_inf_replacement = 10^6, reduce_dim = T) {
  if (!exact && noSamp > (2^m - 2) && !replace) {
    noSamp <- 2^m - 2
    cat(paste0("noSamp is larger than 2^m = ", 2^m, ". Using exact instead."))
  }
  if (exact == TRUE) {
    N <- 2^m
    X <- data.table(ID = 1:N)
    combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
    X[, features := unlist(combinations, recursive = FALSE)]
    X[, nfeatures := length(features[[1]]), ID]
    X[, N := .N, nfeatures]
    X[!(nfeatures %in% c(0, m)), shapley_weight := shapley_weights(m = m, N = N, s = nfeatures)]
    X[nfeatures %in% c(0, m), shapley_weight := shapley_weight_inf_replacement]
    X[, no := 1]
  } else {
    ## Find weights for given number of features ----------
    DT0 <- data.table(nfeatures = head(1:m, -1))
    DT0[, N := unlist(lapply(nfeatures, choose, n = m))]
    DT0[, shapley_weight := shapley_weights(m = m, N = N, s = nfeatures)]
    DT0[, samp_weight := shapley_weight * N]
    DT0[, samp_weight := samp_weight / sum(samp_weight)]

    ## Sample number of features ----------
    X <- data.table(
      nfeatures = sample(
        x = DT0[["nfeatures"]],
        size = noSamp,
        replace = TRUE,
        prob = DT0[["samp_weight"]]
      )
    )

    ## Sample specific set of features # Not optimal, as it is a bit slow for noSamp -------
    setkey(X, nfeatures)
    Samp <- sapply(X = X$nfeatures, FUN = function(x) {
      aa <- rep(NA, m)
      aa[1:x] <- sample(x = 1:m, size = x)
      aa
    })
    Samp <- t(apply(X = Samp, MARGIN = 2, FUN = sort, na.last = T))
    Samp.list <- apply(X = Samp, MARGIN = 1, FUN = function(x) {
      x[!is.na(x)]
    })

    X <- cbind(X, Samp)
    X[, no := .N, by = mget(paste0("V", 1:m))] # Counting repetitions of the same sample

    if (reduce_dim) {
      isDup <- duplicated(X)
      X[, features := Samp.list]
      X <- X[!isDup, ]
    } else {
      X[, no := 1]
      X[, features := Samp.list]
    }

    X[, paste0("V", 1:m) := NULL]
    X[, ID := .I]

    nms <- c("ID", "nfeatures", "features", "no")
    setcolorder(X, nms)

    ## Add zero features and m features ----------
    X_zero_all <- data.table(
      ID = seq(X[, max(ID)] + 1, length.out = 2),
      num_var = c(0, m),
      comb = c(list(numeric(0)), list(1:m)),
      no = 1
    )
    X <- rbindlist(list(X, X_zero_all))
    setkey(X, nfeatures)

    ## Add number of combinations
    X <- merge(x = X, y = DT0[, .(nfeatures, N, shapley_weight)], all.x = TRUE, on = "nfeatures")
    nms <- c("ID", "features", "nfeatures", "N", "shapley_weight", "no")
    setcolorder(X, nms)
    X[, ID := .I]
    X[nfeatures %in% c(0, m), `:=`(
      shapley_weight = shapley_weight_inf_replacement,
      N = 1
    )]
  }
  return(X)
}
