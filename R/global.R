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
#' @param cond_approach Either a string indicating which method should be used to estimate all conditional expectations.
#' Defaults to "empirical_fixed_sigma", with "empirical_AICc_full", "empirical_AICc_each_k","Gaussian" and "copula" being other alternatives. One can also supply a named list where the names
#' are one or more of the implemented methods, and the named lists contains one vector each, each containing the row numbers of the S-matrix
#' computed using prepare_kernelShap that whose corresponding conditional expectations should be computed with that method. Any number not
#' specified is computed with the default empirical method.
#' @param distance_metric String indicating which distance metric should be used in the empirical conditional
#' distribution. Defaults to "Euclidean", "Mahalanobis" and "Mahalanobis_scaled" being the other options. "Mahlanobis_scaled" includes
#' the 1/|S| factor in the paper is preferred for a consistent \eqn{\sigma}.
#' @param kernel_metric String indicating which kernel metric should be used in the empirical conditional distribution.
#' Defaults to "Gaussian" [\eqn{\exp(-D/2\sigma)}], with "independence" (imputing independently, ignoring any distance) being the second option
#' "Gaussian_old" [\eqn{\sqrt(\exp(-D/2\sigma))}] is also kept for reproducability.
#' @param W_kernel Array containg all unscaled weights between training and testing observations for all combinations.
#' @param Xtest_Gauss_trans Vector with the Gaussian transformed test observations
#' @param mu Mean vector of training set
#' @param Sigma Covariance matrix of training set
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
                             cond_approach = "empirical",
                             p_default,
                             distance_metric = "Euclidean",
                             kernel_metric = "Gaussian") {
    return(NULL)
}
