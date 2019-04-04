#' Set of global arguments used in the package
#'
#' @param Xtrain Matrix, data.frame or data.table with the features from the training data
#' @param Xtest Matrix, data.frame or data.table with the features, whose predictions ought to be explained (test data)
#' @param Xtrain_mat Matrix with the features from the training data
#' @param Xtest_mat Matrix with the features, whose predictions ought to be explained (test data)
#' @param exact Logical. If TRUE, uses the full sum in the Shapley formula, if FALSE, uses a sampling approach to approximate the sum
#' @param noSamp Integer. How many samples to use when approximating the sum in the Shapley formula
#' (previously called nrows)
#' @param shapley_weight_inf_replacement Numeric. Indicating which weight to use for the full conditional and unconditional expectations in kernel SHAPs WLS formulation.
#' @param reduce_dim Logical. Indicating whether to reduce the dimension WLS problem by merging identical columns and adjusting their weights.
#' @param m Integer. Total nnumber of features
#' @param model Model object. Fitted model that is used to produce the predictions
#' @param l List. The output from the prepare_kernelShap function
#' @param noSamp_MC Postive integer. Indicating the maximum number of samples to use in the Monte Carlo integration for every conditional expectation (previously called n_threshold)
#' @param verbose Integer. How much information to print during function execution (in development)
#' @param cond_approach String or list. When being a list, the elements in the list refers to the rows in l$X that ought to be included in each of the approaches!
#' @param mu Numeric vector. (Optional) Containing the mean of the data generating distribution. NULL means it is estimated from the data if needed (in the Gaussian approach).
#' @param Sigma Numeric matrix. (Optional) Containing the covariance matrix of the data generating distribution. NULL means it is estimated from the data if needed (in the Gaussian approach).
#' @param N Integer. Number of combinations
#' @param s Integer. Number of chosen
#' @param nsamples Integer. Number of samples
#' @param features List.
#' @param sigma Numeric
#' @param p_default Numeric
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
