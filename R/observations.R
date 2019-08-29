#' Get predictions
#'
#'
#' @param W_kernel Array. Contains all nonscaled weights between training and testing observations for all combinations. Dimension \code{ntrain x nfeatures}.
#' @param S Integer matrix of dimension \code{ncomb x nfeatures}, where \code{ncomb} equals the total number of sampled/non-sampled feature combinations and \code{nfeatures} equals the total number of unique features. Note that \code{nfeatures = ncol(xtrain)}.
#' @param Xtrain Matrix, data.frame or data.table with the features from the training data. Dimension \code{ntrain x nfeatures}.
#' @param Xtest Matrix, data.frame or data.table with the features of the observation whose predictions ought to be explained (test data). Dimension \code{1 x nfeatures} or \code{nfeatures x 1}.
#' @param w_threshold Numeric. The number of samples to use is chosen to be the number of the largest weights needed to account for this fraction of the total weight.
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
observation_impute <- function(W_kernel, S, Xtrain, Xtest, w_threshold = .7, noSamp_MC = 1e3) {
  stopifnot(!is.null(dim(W_kernel)))
  stopifnot(!is.null(dim(S)))
  stopifnot(all(dim(W_kernel) == c(nrow(Xtrain),nrow(S))))
  stopifnot(all(S%in%c(0,1)))

  ## Find weights for all combinations and training data
  DT <- as.data.table(W_kernel)
  DT[, ID := .I]
  DT <- data.table::melt(data = DT, id.vars = "ID", variable.name = "comb", value.name = "w", variable.factor = FALSE)

  ## Remove training data with small weight
  setkey(DT, comb, w)
  DT[, w := w / sum(w), comb]
  DT[, wcum := cumsum(w), comb]
  DT <- DT[wcum > 1 - w_threshold][, wcum := NULL]
  DT <- DT[, tail(.SD, noSamp_MC), comb]
  DT[, comb := gsub(comb, pattern = "V", replacement = ""), comb]
  DT[, wcomb := as.integer(comb), comb][, comb := NULL]

  ## Generate data used for prediction
  DTp <- observation_impute_cpp(
    index_xtrain = DT[["ID"]],
    index_s = DT[["wcomb"]],
    xtrain = Xtrain,
    xtest = Xtest,
    S = S
  )

  ## Add keys
  DTp <- as.data.table(DTp)
  setnames(DTp, colnames(Xtrain))
  DTp[, wcomb := DT[["wcomb"]]]
  DTp[, w := DT[["w"]]]

  return(DTp)
}

