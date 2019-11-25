#' Calculate Shapley weights for test data
#'
#' @description This function should only be called internally, and not be used as
#' a stand-alone function.
#'
#' @param dt data.table
#' @param prediction_zero Numeric. The value to use for \code{phi_0}.
#' @param explainer An object of class \code{explainer}. See \code{\link{shapr}}.
#'
#' @details If \code{dt} does not contain three columns called \code{id}, \code{id_combination} and \code{w}
#' the function will fail. \code{id} represents a unique key for a given test observation,
#' and \code{id_combination} is a unique key for which feature combination the row represents. \code{w}
#' represents the Shapley value of feature combination given by \code{id_combination}. In addition
#' to these three columns, \code{dt} should also have columns which matches the variables used
#' when training the model.
#'
#' I.e. you have fitted a linear model using the features \code{x1},
#' \code{x2} and \code{x3}, and you want to explain 5 test observations using the exact method, i.e.
#' setting \code{exact = TRUE} in \code{\link{shapr}}, the following properties should be satisfied
#' \enumerate{
#' \item \code{colnames(dt)} equals \code{c("x1", "x2", "x3", "id", "id_combination", ""w)}
#' \item \code{dt[, max(id)]} equals the number of test observations
#' \item \code{dt[, min(id)]} equals 1L.
#' \item \code{dt[, max(id_combination)]} equals \code{2^m} where m equals the number of features.
#' \item \code{dt[, min(id_combination)]} equals 1L.
#' \item \code{dt[, type(w)]} equals \code{double}.
#' }
#'
#' @return An object of class \code{c("shapr", "list")}. For more details see \code{\link{explain}}.
#'
#' @author Nikolai Sellereite
prediction <- function(dt, prediction_zero, explainer) {

  # Checks on input data
  id <- w <- id_combination <- p_hat <- NULL # due to NSE notes in R CMD check
  stopifnot(
    data.table::is.data.table(dt),
    !is.null(dt[["id"]]),
    !is.null(dt[["id_combination"]]),
    !is.null(dt[["w"]])
  )

  # Setup
  cnms <- colnames(explainer$x_test)
  data.table::setkeyv(dt, c("id", "id_combination"))

  # Check that the number of test observations equals max(id)
  stopifnot(nrow(explainer$x_test) == dt[, max(id)])

  # Predictions
  dt[, p_hat := predict_model(explainer$model, newdata = .SD), .SDcols = cnms]
  dt[id_combination == 1, p_hat := prediction_zero]
  p_all <- predict_model(explainer$model, newdata = explainer$x_test)
  dt[id_combination == max(id_combination), p_hat := p_all[id]]

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, id_combination)]
  data.table::setkeyv(dt_res, c("id", "id_combination"))
  dt_mat <- data.table::dcast(dt_res, id_combination ~ id, value.var = "k")
  dt_mat[, id_combination := NULL]
  kshap <-  t(explainer$W %*% as.matrix(dt_mat))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", cnms)

  r <- list(dt = dt_kshap, model = explainer$model, p = p_all, x_test = explainer$x_test)
  attr(r, "class") <- c("shapr", "list")

  return(r)
}
