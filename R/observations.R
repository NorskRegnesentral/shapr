#' Get predictions
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
observation_impute <- function(W_kernel, S, Xtrain, Xtest, w_threshold = .7, noSamp_MC = 1e3) {

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

