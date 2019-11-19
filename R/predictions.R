#' Calculate Shapley weights for test data
#'
#' @description Note that this function should only be called internally, and not used as
#' a stand-alone function.
#'
#' @param dt data.table.
#' @param prediction_zero Numeric.
#' @param explainer An object of class \code{explainer}. See \code{\link{shapr}}.
#'
#' @details If \code{dt} does not contain three columns named \code{id}, \code{wcomb} and \code{w}
#' the function will fail. \code{id} represents a unique key for a given test observation,
#' and \code{wcomb} is a unqiue key for which feature combination the row represents. \code{w}
#' represents the Shapley value of feature combination given by \code{wcomb}. In addition
#' to these three columns, \code{dt} should also have columns which matches the variables used
#' when training the model.
#'
#' I.e. you have fitted a linear model using the features \code{x1},
#' \code{x2} and \code{x3}, and you want to exlain 5 test observations using the exact method (i.e.
#' setting \code{exact = TRUE} in \code{\link{shapr}}) the following properties should be satisfied
#' \enumerate{
#' \item \code{colnames(dt)} equals \code{c("x1", "x2", "x3", "id", "wcomb", ""w)}
#' \item \code{dt[, max(id)]} equals the number of test observations
#' \item \code{dt[, min(id)]} equals 1L.
#' \item \code{dt[, max(wcomb)]} equals \code{2^m} where m equals the number of features.
#' \item \code{dt[, min(wcomb)]} equals 1L.
#' \item \code{dt[, type(w)]} equals \code{double}.
#' }
#'
#' @examples
#' # Load example data
#' data("Boston", package = "MASS")
#' df <- Boston
#'
#' # Example using the exact method
#' x_var <- c("lstat", "rm", "dis", "indus")
#' y_train <- df[, "medv", drop = FALSE]
#' df <- Boston[, x_var]
#' model <- lm(medv ~ lstat + rm + dis + indus, data = cbind(y_train, df))
#' explainer <- shapr(df, model)
#'
#'
#'
#' @author Nikolai Sellereite
prediction <- function(dt, prediction_zero, explainer) {

  # Setup
  cnms <- colnames(explainer$x_test)
  data.table::setkeyv(dt, c("id", "wcomb"))

  # Check that the number of test observations equals max(id)
  stopifnot(nrow(explainer$x_test) == dt[, max(id)])

  # Predictions
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
