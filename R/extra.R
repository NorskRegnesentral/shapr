#' Calculate conditional mu and Sigma
#'
#' Copied from the \link[condMVNorm]{condMVN} package, but added a line to
#' ensure symmetry of resulting conditional covariance matrix
#'
#' @inheritParams global_arguments
#'
#' @return Numeric
#'
#' @export
#'
#' @author Martin Jullum
condMVN_modified <- function (mean, sigma, dependent.ind, given.ind, X.given, check.sigma = TRUE)
{
    if (missing(dependent.ind))
        return("You must specify the indices of dependent random variables in `dependent.ind'")
    if (missing(given.ind) & missing(X.given))
        return(list(condMean = mean[dependent.ind], condVar = as.matrix(sigma[dependent.ind,
                                                                              dependent.ind])))
    if (length(X.given) != length(given.ind))
        stop("lengths of `X.given' and `given.ind' must be same")
    if (check.sigma) {
        if (!isSymmetric(sigma))
            stop("sigma is not a symmetric matrix")
        eigenvalues <- eigen(sigma, only.values = TRUE)$values
        if (any(eigenvalues < 1e-08))
            stop("sigma is not positive-definite")
    }
    B <- sigma[dependent.ind, dependent.ind]
    C <- sigma[dependent.ind, given.ind, drop = FALSE]
    D <- sigma[given.ind, given.ind]
    CDinv <- C %*% solve(D)
    cMu <- c(mean[dependent.ind] + CDinv %*% (X.given - mean[given.ind]))
    cVar <- B - CDinv %*% t(C)
    cVar <- Matrix::symmpart(cVar)  # Added this line to ensure symmetry
    list(condMean = cMu, condVar = cVar)
}
