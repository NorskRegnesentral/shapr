#' Transforms new data to a standardized normal distribution
#'
#' @param zx Numeric vector. The first \code{n_z} items are the Gaussian data, and the last part is
#' the data with the original transformation.
#' @param n_z Positive integer. Number of elements of \code{zx} that belongs to new data.
#'
#' @return Numeric vector of length \code{n_z}
#'
#' @keywords internal
#'
#' @examples
#' zx <- rnorm(50)
#' n_z <- 30
#' x <- shapr:::inv_gaussian_transform(zx, n_z)
#' str(x)
#'
#' @author Martin Jullum
inv_gaussian_transform <- function(zx, n_z) {
  if (n_z >= length(zx)) stop("n_z should be less than length(zx)")
  ind <- 1:n_z
  z <- zx[ind]
  x <- zx[-ind]
  u <- stats::pnorm(z)
  x_new <- stats::quantile(x, probs = u)
  return(as.double(x_new))
}

#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' @param yx Numeric vector. The first \code{n_y} items is the data that is transformed, and last
#' part is the data with the original transformation.
#' @param n_y Positive integer. Number of elements of \code{yx} that belongs to the gaussian data.
#'
#' @return Vector of back-transformed Gaussian data
#'
#' @keywords internal
#'
#' @examples
#' yx <- rnorm(50)
#' n_y <- 30
#' x <- shapr:::gaussian_transform_separate(yx, n_y)
#' str(x)
#'
#' @author Martin Jullum
gaussian_transform_separate <- function(yx, n_y) {
  if (n_y >= length(yx)) stop("n_y should be less than length(yx)")
  ind <- 1:n_y
  x <- yx[-ind]
  tmp <- rank(yx)[ind]
  tmp <- tmp - rank(tmp) + 0.5
  u_y <- tmp / (length(x) + 1)
  z_y <- stats::qnorm(u_y)
  return(z_y)
}

#' Transforms a sample to standardized normal distribution
#'
#' @param x Numeric vector.The data which should be transformed to a standard normal distribution.
#'
#' @return Numeric vector of length \code{length(x)}
#'
#' @keywords internal
#'
#' @examples
#' y <- rnorm(50)
#' x <- shapr:::gaussian_transform(y)
#'
#' @author Martin Jullum
gaussian_transform <- function(x) {
  u <- rank(x) / (length(x) + 1)
  z <- stats::qnorm(u)
  return(z)
}
