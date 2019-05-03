#' Set of global arguments used in the package
#'
#' @param xtrain Matrix, data.frame or data.table with the features from the training data
#' @param xtest Matrix, data.frame or data.table with the features, whose predictions ought to
#' be explained (test data)
#' @param xtrain_mat Matrix with the features from the training data
#' @param xtest_mat Matrix with the features, whose predictions ought to be explained (test data)
#' @param exact Logical. If TRUE, uses the full sum in the Shapley formula, if FALSE, uses a
#' sampling approach to approximate the sum
#' @param no_samp Integer. How many samples to use when approximating the sum in the Shapley formula
#' (previously called \code{nrows})
#' @param shapley_weight_inf_replacement Numeric. Indicating which weight to use for the full
#' conditional and unconditional expectations in kernel SHAPs weighted least squares formulation.
#' @param reduce_dim Logical. Indicating whether to reduce the dimension of the weighted least
#' squares problem by merging identical columns and adjusting their weights.
#' @param m Integer. Total number of features
#' @param model Model object. Fitted model that is used to produce the predictions
#' @param l List. The output from the \code{prepare_kshap} function
#' @param no_samp_mc Positive integer. Indicating the maximum number of samples to use in the
#' Monte Carlo integration for every conditional expectation (previously called \code{n_threshold})
#' @param verbose Integer. How much information to print during function execution (in development)
#' @param cond_approach String or list. When being a list, the elements in the list refers to the
#' rows in l$x that ought to be included in each of the approaches!
#' @param mu Numeric vector. (Optional) Containing the mean of the data generating distribution.
#' NULL means it is estimated from the data if needed (in the Gaussian approach).
#' @param sigma Numeric matrix. (Optional) Containing the covariance matrix of the data generating
#' distribution. NULL means it is estimated from the data if needed (in the Gaussian approach).
#' @param n Integer. Number of combinations
#' @param nsamples Integer. Number of samples
#' @param features List.
#' @param p_default Numeric
#' @param w Matrix
#' @param I Matrix
#' @param D Matrix
#' @param s Matrix
#' @param verbose Logical
#' @param scale Logical
#' @param cond_approach Either a string indicating which method should be used to estimate all
#' conditional expectations. Defaults to "empirical_fixed_sigma", with "empirical_AICc_full",
#' "empirical_AICc_each_k","Gaussian" and "copula" being other alternatives. One can also supply a
#' named list where the names are one or more of the implemented methods, and the named lists
#' contains one vector each, each containing the row numbers of the S-matrix computed using
#' \code{prepare_kshap} that whose corresponding conditional expectations should be computed with
#' that method. Any number not specified is computed with the default empirical method.
#' @param distance_metric String indicating which distance metric should be used in the empirical
#' conditional distribution. Defaults to "Euclidean", "Mahalanobis" and "Mahalanobis_scaled" being
#' the other options. "Mahalanobis_scaled" includes the 1/|S| factor in the paper is preferred for
#' a consistent \eqn{\sigma}.
#' @param kernel_metric String indicating which kernel metric should be used in the empirical
#' conditional distribution. Defaults to "Gaussian" [\eqn{\exp(-D/2\sigma)}], with "independence"
#' (imputing independently, ignoring any distance) being the second option "Gaussian_old"
#' [\eqn{\sqrt(\exp(-D/2\sigma))}] is also kept for reproducibility.
#' @param w_kernel Array. Contains all nonscaled weights between training and testing observations
#' for all combinations.
#' @param xtest_gauss_trans Vector with the Gaussian transformed test observations
#' @param mu Mean vector of training set
#' @param cov_mat Covariance matrix of training set
#' @export
#'
#' @return NULL
#'
#' @author Nikolai Sellereite
global_arguments <- function(m,
                             n,
                             xtrain,
                             xtest,
                             nsamples,
                             features,
                             exact,
                             cov_mat,
                             model,
                             nrows,
                             scale,
                             w_threshold,
                             n_threshold,
                             verbose,
                             w,
                             s,
                             D,
                             I,
                             cond_approach = "empirical",
                             p_default,
                             distance_metric = "Euclidean",
                             kernel_metric = "Gaussian") {
  return(NULL)
}
