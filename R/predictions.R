#' Calculate shapley weights
#'
#' @description
#' TODO: Write a better description
#'
#' @param dt data.table
#' @param prediction_zero Positive integer between 0 & 1.
#' @param explainer An object of class \code{explainer}
#'
#' @details
#' TODO: Write details about how this is done (reference to paper)
#'
#' @examples
#' # TODO: Add simple examples
#'
#' @author Nikolai Sellereite
prediction <- function(dt, prediction_zero, explainer) {

  # Predictions
  cnms <- colnames(explainer$x_test)
  data.table::setkeyv(dt, c("id", "wcomb"))
  dt[, p_hat := predict_model(explainer$model, newdata = .SD), .SDcols = cnms]
  dt[wcomb == 1, p_hat := prediction_zero]
  p_all <- predict_model(explainer$model, newdata = explainer$x_test)
  dt[wcomb == max(wcomb), p_hat := p_all[id]]

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, wcomb)]
  data.table::setkeyv(dt_res, c("id", "wcomb"))
  dt_mat <- data.table::dcast(dt_res, wcomb ~ id, value.var = "k")
  dt_mat[, wcomb := NULL]
  kshap <-  t(explainer$W %*% as.matrix(dt_mat))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", cnms)

  r <- list(dt = dt_kshap, model = explainer$model, p = p_all, x_test = explainer$x_test)
  attr(r, "class") <- c("shapr", "list")

  return(r)
}
