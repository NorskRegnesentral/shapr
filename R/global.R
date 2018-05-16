#' Set of global arguments used in the package
#'
#' @param m Integer. Total nnumber of features
#' @param N Integer. Number of combinations
#' @param s Integer. Number of chosen
#' @param Xtrain data.frame
#' @param Xtest data.frame
#' @param nsamples Integer. Number of samples
#' @param features List.
#' @param exact Logical
#' @param sigma Numeric
#' @param model Model object
#' @param p_default Numeric
#' @param nrows Integer
#' @param w_threshold Positive numeric.
#' @param n_threshold Postive integer.
#' @param W Matrix
#' @param I Matrix
#' @param D Matrix
#' @param S Matrix
#' @param verbose Logical
#' @param scale Logical
#' @param gaussian_sample Logical indicating whether the Gaussian conditional sampling approach should be used
#'
#' @export
#'
#' @return NULL
#'
#' @author Nikolai Sellereite
global_arguments <- function(m,
                             N,
                             s,
                             Xtrain,
                             Xtest,
                             nsamples,
                             features,
                             exact,
                             sigma,
                             model,
                             nrows,
                             scale,
                             w_threshold,
                             n_threshold,
                             verbose,
                             W,
                             S,
                             D,
                             I,
                             gaussian_sample,
                             p_default) {
    return(NULL)
}
