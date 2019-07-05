#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' Handled in this way in order to allow using the apply function over this function
#' @param zx Vector where the first part is the Gaussian data, and last part is
#' the data with the original transformation
#' @param n_y How many elements of \code{yx} that belongs to the y-part (new data)
#' @param type The quantile type used when back-transforming. 7 (default) is the default in stats::quantile().
#'
#' @return Vector of transformed new data
#' @export
#'
#' @author Martin Jullum
inv_gaussian_transform <- function(zx, n_z, type = 7) {
  ind <- 1:n_z
  z <- zx[ind]
  x <- zx[-ind]
  u <- stats::pnorm(z)
  xNew <- stats::quantile(x, u, type = type)
  return(xNew)
}


#' Transforms new data to standardized normal (dimension 1) based on other data transformations
#'
#' Handled in this way in order to allow using the apply function over this function
#' @param yx Vector where the first part is the new data to transform,
#' and last part is the data with the original transformation
#' @param n_z How many elements of \code{zx} that belongs to the z-part (Gaussian data)
#'
#' @return Vector of back-transformed Gaussian data
#' @export
#'
#' @author Martin Jullum
gaussian_transform_separate <- function(yx, n_y) {
  ind <- 1:n_y
  y <- yx[ind]
  x <- yx[-ind]
  tmp <- rank(c(y, x))[1:length(y)]
  tmp <- tmp - rank(tmp) + 0.5
  u.y <- tmp / (length(x) + 1)
  z.y <- stats::qnorm(u.y)
  return(z.y)
}

#' Transforms a sample to standardized normal (dimension 1)
#'
#' @param x Vector of data to transform
#'
#' @return Vector of transformed data
#' @export
#'
#' @author Martin Jullum
gaussian_transform <- function(x) {
  u <- rank(x) / (length(x) + 1)
  z <- stats::qnorm(u)
  return(z)
}

