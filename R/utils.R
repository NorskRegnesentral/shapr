#' @keywords internal
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  is.numeric(x) && all(abs(x - round(x)) < tol)
}

#' @keywords internal
errorfun <- function(e) {
  ret <- list(e)
  class(ret) <- "error"
  ret
}
