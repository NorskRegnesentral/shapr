#' Explain the Output of Machine Learning Models with Dependence-Aware (Conditional/Observational) Shapley Values
#'
#' @description Compute dependence-aware Shapley values for observations in `x_explain` from the specified
#' `model` using the method specified in `approach` to estimate the conditional expectation.
#' See \href{https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf}{Aas et al. (2021)}
#' for a thorough introduction to dependence-aware prediction explanation with Shapley values.
#' For an overview of the methodology and capabilities of the package, see the software paper
#' \href{https://arxiv.org/pdf/2504.01842}{Jullum et al. (2025)}, or the pkgdown site at
#' [norskregnesentral.github.io/shapr/](https://norskregnesentral.github.io/shapr/).
#'
#' @param x_train Matrix or data.frame/data.table.
#' Data used to estimate the (conditional) feature distributions
#' needed to properly estimate the conditional expectations in the Shapley formula.
#'
#' @param x_explain Matrix or data.frame/data.table.
#' Features for which predictions should be explained.
#'
#' @param model Model object.
#' The model whose predictions you want to explain.
#' Run [get_supported_models()]
#' for a table of which models `explain` supports natively. Unsupported models
#' can still be explained by passing `predict_model` and (optionally) `get_model_specs`,
#' see details for more information.
#'
#' @param approach Character vector of length `1` or one less than the number of features.
#' All elements should either be `"gaussian"`, `"copula"`, `"empirical"`, `"ctree"`, `"vaeac"`,
#' `"categorical"`, `"timeseries"`, `"independence"`, `"regression_separate"`, or `"regression_surrogate"`.
#' The two regression approaches cannot be combined with any other approach.
#' See details for more information.
#'
#' @param phi0 Numeric.
#' The prediction value for unseen data, i.e., an estimate of the expected prediction without conditioning on any
#' features.
#' Typically set this equal to the mean of the response in the training data, but alternatives such as the mean
#' of the training predictions are also reasonable.
#'
#' @param max_n_coalitions Integer.
#' Upper limit on the number of unique feature/group coalitions to use in the iterative procedure
#' (if `iterative = TRUE`).
#' If `iterative = FALSE`, it represents the number of feature/group coalitions to use directly.
#' The quantity refers to the number of unique feature coalitions if `group = NULL`,
#' and group coalitions if `group != NULL`.
#' `max_n_coalitions = NULL` corresponds to `2^n_features`.
#'
#' @param group List.
#' If `NULL`, regular feature-wise Shapley values are computed.
#' If provided, group-wise Shapley values are computed.
#' `group` then has length equal to the number of groups.
#' Each list element contains the character vectors with the features included in the corresponding group.
#' See
#' \href{https://martinjullum.com/publication/jullum-2021-efficient/jullum-2021-efficient.pdf}{Jullum et al. (2021)}
#' for more information on group-wise Shapley values.
#'
#' @param n_MC_samples Positive integer.
#' For most approaches, it indicates the maximum number of samples to use in the Monte Carlo integration
#' of every conditional expectation.
#' For `approach="ctree"`, `n_MC_samples` corresponds to the number of samples
#' from the leaf node (see an exception related to the `ctree.sample` argument in [setup_approach.ctree()]).
#' For `approach="empirical"`, `n_MC_samples` is the \eqn{K} parameter in equations (14-15) of
#' \href{https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf}{Aas et al. (2021)},
#' i.e. the maximum number of observations (with largest weights) that is used, see also the
#' `empirical.eta` argument [setup_approach.empirical()].
#'
#' @param seed Positive integer.
#' Specifies the seed before any code involving randomness is run.
#' If `NULL` (default), no seed is set in the calling environment.
#'
#' @param predict_model Function.
#' Prediction function to use when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported models.)
#' The function must have two arguments, `model` and `newdata`, which specify the model
#' and a data.frame/data.table to compute predictions for, respectively.
#' The function must give the prediction as a numeric vector.
#' `NULL` (the default) uses functions specified internally.
#' Can also be used to override the default function for natively supported model classes.
#'
#' @param get_model_specs Function.
#' An optional function for checking model/data consistency when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported models.)
#' The function takes `model` as an argument and provides a list with 3 elements:
#' \describe{
#'   \item{labels}{Character vector with the names of each feature.}
#'   \item{classes}{Character vector with the class of each feature.}
#'   \item{factor_levels}{Character vector with the levels for any categorical features.}
#' }
#' If `NULL` (the default), internal functions are used for natively supported model classes, and checking is
#' disabled for unsupported model classes.
#' Can also be used to override the default function for natively supported model classes.
#'
#' @param verbose String vector or NULL.
#' Controls verbosity (printout detail level) via one or more of `"basic"`, `"progress"`,
#'  `"convergence"`, `"shapley"` and `"vS_details"`.
#' `"basic"` (default) displays basic information about the computation and messages about parameters/checks.
#' `"progress"` displays where in the calculation process the function currently is.
#' `"convergence"` displays how close the Shapley value estimates are to convergence
#' (only when `iterative = TRUE`).
#' `"shapley"` displays intermediate Shapley value estimates and standard deviations (only when `iterative = TRUE`),
#' and the final estimates.
#' `"vS_details"` displays information about the v(S) estimates,
#' most relevant for `approach %in% c("regression_separate", "regression_surrogate", "vaeac")`.
#' `NULL` means no printout.
#' Any combination can be used, e.g., `verbose = c("basic", "vS_details")`.
#'
#' @param iterative Logical or NULL.
#' If `NULL` (default), set to `TRUE` if there are more than 5 features/groups, and `FALSE` otherwise.
#' If `TRUE`, Shapley values are estimated iteratively for faster, sufficiently accurate results.
#' First an initial number of coalitions is sampled, then bootstrapping estimates the variance of the Shapley values.
#' A convergence criterion determines if the variances are sufficiently small. If not, additional samples are added.
#' The process repeats until the variances are below the threshold.
#' Specifics for the iterative process and convergence criterion are set via `iterative_args`.
#'
#' @param iterative_args Named list.
#' Specifies the arguments for the iterative procedure.
#' See the help file of [get_iterative_args_default()] for description of the arguments and their default values.
#' @param output_args Named list.
#' Specifies certain arguments related to the output of the function.
#' See the help file of [get_output_args_default()] for description of the arguments and their default values.
#' @param extra_computation_args Named list.
#' Specifies extra arguments related to the computation of the Shapley values.
#' See the help file of [get_extra_comp_args_default()] for description of the arguments and their default values.
#'
#' @param prev_shapr_object `shapr` object or string.
#' If an object of class `shapr` is provided, or a string with a path to where intermediate results are stored,
#' then the function will use the previous object to continue the computation.
#' This is useful if the computation is interrupted or you want higher accuracy than already obtained, and therefore
#' want to continue the iterative estimation. See the
#' \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html}{general usage vignette} for examples.
#'
#' @param asymmetric Logical.
#' Not applicable for (regular) non-causal explanations.
#' If `FALSE` (default), `explain` computes regular symmetric Shapley values.
#' If `TRUE`, `explain` computes asymmetric Shapley values based on the (partial) causal ordering
#' given by `causal_ordering`. That is, `explain` only uses feature coalitions that
#' respect the causal ordering. If `asymmetric` is `TRUE` and
#' `confounding` is `NULL` (default), `explain` computes asymmetric conditional Shapley values as specified in
#' \href{https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf}{
#' Frye et al. (2020)}. If `confounding` is provided, i.e., not `NULL`, then `explain` computes asymmetric causal
#' Shapley values as specified in
#' \href{https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf}{
#' Heskes et al. (2020)}.
#'
#' @param causal_ordering List.
#' Not applicable for (regular) non-causal or asymmetric explanations.
#' `causal_ordering` is an unnamed list of vectors specifying the components of the
#' partial causal ordering that the coalitions must respect. Each vector represents
#' a component and contains one or more features/groups identified by their names
#' (strings) or indices (integers). If `causal_ordering` is `NULL` (default), no causal
#' ordering is assumed and all possible coalitions are allowed. No causal ordering is
#' equivalent to a causal ordering with a single component that includes all features
#' (`list(1:n_features)`) or groups (`list(1:n_groups)`) for feature-wise and group-wise
#' Shapley values, respectively. For feature-wise Shapley values and
#' `causal_ordering = list(c(1, 2), c(3, 4))`, the interpretation is that features 1 and 2
#' are the ancestors of features 3 and 4, while features 3 and 4 are on the same level.
#' Note: All features/groups must be included in `causal_ordering` without duplicates.
#'
#' @param confounding Logical vector.
#' Not applicable for (regular) non-causal or asymmetric explanations.
#' `confounding` is a logical vector specifying whether confounding is assumed for each component in the
#' `causal_ordering`. If `NULL` (default), no assumption about the confounding structure is made and `explain`
#' computes asymmetric/symmetric conditional Shapley values, depending on `asymmetric`.
#' If `confounding` is a single logical (`FALSE` or `TRUE`), the assumption is set globally
#' for all components in the causal ordering. Otherwise, `confounding` must have the same
#' length as `causal_ordering`, indicating the confounding assumption for each component. When `confounding` is
#' specified, `explain` computes asymmetric/symmetric causal Shapley values, depending on `asymmetric`.
#' The `approach` cannot be `regression_separate` or `regression_surrogate`, as the
#' regression-based approaches are not applicable to the causal Shapley methodology.
#'
#' @param ... Further arguments passed to specific approaches, see below.
#'
#'
#' @inheritDotParams setup_approach.categorical
#' @inheritDotParams setup_approach.copula
#' @inheritDotParams setup_approach.ctree
#' @inheritDotParams setup_approach.empirical
#' @inheritDotParams setup_approach.gaussian
#' @inheritDotParams setup_approach.independence
#' @inheritDotParams setup_approach.regression_separate
#' @inheritDotParams setup_approach.regression_surrogate
#' @inheritDotParams setup_approach.timeseries
#' @inheritDotParams setup_approach.vaeac
#'
#' @details The `shapr` package implements kernelSHAP estimation of dependence-aware Shapley values with
#' eight different Monte Carlo-based approaches for estimating the conditional distributions of the data.
#' These are all introduced in the
#' \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html}{general usage vignette}.
#' (From R: `vignette("general_usage", package = "shapr")`).
#' For an overview of the methodology and capabilities of the package, please also see the software paper
#' \href{https://arxiv.org/pdf/2504.01842}{Jullum et al. (2025)}.
#' Moreover,
#'  \href{https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf}{Aas et al. (2021)}
#' gives a general introduction to dependence-aware Shapley values and the approaches `"empirical"`,
#' `"gaussian"`, `"copula"`, and also discusses `"independence"`.
#' \href{https://martinjullum.com/publication/redelmeier-2020-explaining/redelmeier-2020-explaining.pdf}{
#' Redelmeier et al. (2020)} introduces the approach `"ctree"`.
#' \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)} introduces the `"vaeac"`
#' approach.
#' Approach `"timeseries"` is discussed in
#' \href{https://martinjullum.com/publication/jullum-2021-efficient/jullum-2021-efficient.pdf}{Jullum et al. (2021)}.
#' `shapr` has also implemented two regression-based approaches `"regression_separate"` and `"regression_surrogate"`,
#' as described in \href{https://link.springer.com/content/pdf/10.1007/s10618-024-01016-z.pdf}{Olsen et al. (2024)}.
#' It is also possible to combine the different approaches, see the
#' \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html}{
#' general usage} vignette for more information.
#'
#' The package also supports the computation of causal and asymmetric Shapley values as introduced by
#' \href{https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf}{
#' Heskes et al. (2020)} and
#' \href{https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf}{
#' Frye et al. (2020)}.
#' Asymmetric Shapley values were proposed by
#' \href{https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf}{
#' Frye et al. (2020)} as a way to incorporate causal knowledge in
#' the real world by restricting the possible feature combinations/coalitions when computing the Shapley values to
#' those consistent with a (partial) causal ordering.
#' Causal Shapley values were proposed by
#' \href{https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf}{
#' Heskes et al. (2020)} as a way to explain the total effect of features
#' on the prediction, taking into account their causal relationships, by adapting the sampling procedure in `shapr`.
#'
#' The package allows parallelized computation with progress updates through the tightly connected
#' [future::future] and [progressr::progressr] packages.
#' See the examples below.
#' For iterative estimation (`iterative=TRUE`), intermediate results may be printed to the console
#' (according to the `verbose` argument).
#' Moreover, the intermediate results are written to disk.
#' This combined batch computation of the v(S) values enables fast and accurate estimation of the Shapley values
#' in a memory-friendly manner.
#'
#' @return Object of class `c("shapr", "list")`. Contains the following items:
#' \describe{
#'   \item{`shapley_values_est`}{data.table with the estimated Shapley values with explained observation in the rows and
#'   features along the columns.
#'   The column `none` is the prediction not devoted to any of the features (given by the argument `phi0`)}
#'   \item{`shapley_values_sd`}{data.table with the standard deviation of the Shapley values reflecting the uncertainty
#'   in the coalition sampling part of the kernelSHAP procedure.
#'   These are, by definition, 0 when all coalitions are used.
#'   Only present when `extra_computation_args$compute_sd=TRUE`, which is the default when `iterative = TRUE`.}
#'   \item{`internal`}{List with the different parameters, data, functions and other output used internally.}
#'   \item{`pred_explain`}{Numeric vector with the predictions for the explained observations.}
#'   \item{`MSEv`}{List with the values of the MSEv evaluation criterion for the approach. See the
#'   \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html#msev-evaluation-criterion
#'   }{MSEv evaluation section in the general usage vignette} for details.}
#'   \item{`timing`}{List containing timing information for the different parts of the computation.
#'   `summary` contains the time stamps for the start and end time in addition to the total execution time.
#'   `overall_timing_secs` gives the time spent on different parts of the explanation computation.
#'   `main_computation_timing_secs` further decomposes the main computation time into different parts of the
#'   computation for each iteration of the iterative estimation routine, if used.}
#'   }
#'
#' @examples
#' \donttest{
#'
#' # Load example data
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' # Split data into test and training data
#' data_train <- head(airquality, -3)
#' data_explain <- tail(airquality, 3)
#'
#' x_train <- data_train[, x_var]
#' x_explain <- data_explain[, x_var]
#'
#' # Fit a linear model
#' lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
#' model <- lm(lm_formula, data = data_train)
#'
#' # Explain predictions
#' p <- mean(data_train[, y_var])
#'
#' # (Optionally) enable parallelization via the future package
#' if (requireNamespace("future", quietly = TRUE)) {
#'   future::plan("multisession", workers = 2)
#' }
#'
#' # (Optionally) enable progress updates within every iteration via the progressr package
#' if (requireNamespace("progressr", quietly = TRUE)) {
#'   progressr::handlers(global = TRUE)
#' }
#'
#' # Empirical approach
#' explain1 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "empirical",
#'   phi0 = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # Gaussian approach
#' explain2 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   phi0 = p,
#'   n_MC_samples = 1e2
#' )
#'
#' # Gaussian copula approach
#' explain3 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "copula",
#'   phi0 = p,
#'   n_MC_samples = 1e2
#' )
#'
#' if (requireNamespace("party", quietly = TRUE)) {
#'   # ctree approach
#'   explain4 <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     approach = "ctree",
#'     phi0 = p,
#'     n_MC_samples = 1e2
#'   )
#' }
#'
#' # Combined approach
#' approach <- c("gaussian", "gaussian", "empirical")
#' explain5 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = approach,
#'   phi0 = p,
#'   n_MC_samples = 1e2
#' )
#'
#' ## Printing
#' print(explain1) # The Shapley values
#' print(explain1) # The Shapley values
#'
#' # The MSEv criterion (+sd). Smaller values indicate a better approach.
#' print(explain1, what = "MSEv")
#' print(explain2, what = "MSEv")
#' print(explain3, what = "MSEv")
#'
#' ## Summary
#' summary1 <- summary(explain1)
#' summary1 # Provides a nicely formatted summary of the explanation
#'
#' # Various additional info stored in the summary object
#' # Examples
#' summary1$shapley_est # A data.table with the Shapley values
#' summary1$timing$total_time_secs # Total computation time in seconds
#' summary1$parameters$n_MC_samples # Number of Monte Carlo samples used for the numerical integration
#' summary1$parameters$empirical.type # Type of empirical approach used
#'
#' # Plot the results
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot(explain1)
#'   plot(explain1, plot_type = "waterfall")
#' }
#'
#' # Group-wise explanations
#' group_list <- list(A = c("Temp", "Month"), B = c("Wind", "Solar.R"))
#'
#' explain_groups <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   group = group_list,
#'   approach = "empirical",
#'   phi0 = p,
#'   n_MC_samples = 1e2
#' )
#'
#' print(explain_groups)
#'
#' # Separate and surrogate regression approaches with linear regression models.
#' req_pkgs <- c("parsnip", "recipes", "workflows", "rsample", "tune", "yardstick")
#' if (requireNamespace(req_pkgs, quietly = TRUE)) {
#'   explain_separate_lm <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     phi0 = p,
#'     approach = "regression_separate",
#'     regression.model = parsnip::linear_reg()
#'   )
#'
#'   explain_surrogate_lm <- explain(
#'     model = model,
#'     x_explain = x_explain,
#'     x_train = x_train,
#'     phi0 = p,
#'     approach = "regression_surrogate",
#'     regression.model = parsnip::linear_reg()
#'   )
#' }
#'
#' # Iterative estimation
#' # For illustration only. By default not used for such small dimensions as here.
#' # Restricting the initial and maximum number of coalitions as well.
#'
#' explain_iterative <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   phi0 = p,
#'   iterative = TRUE,
#'   iterative_args = list(initial_n_coalitions = 8),
#'   max_n_coalitions = 12
#' )
#'
#' # When not using all coalitions, we can also get the SD of the Shapley values,
#' # reflecting uncertainty in the coalition sampling part of the procedure.
#' print(explain_iterative, what = "shapley_sd")
#'
#' ## Summary
#' # For iterative estimation, convergence info is also provided
#' summary_iterative <- summary(explain_iterative)
#' }
#'
#' \dontshow{
#' if (requireNamespace("future", quietly = TRUE)) {
#'   # R CMD check: make sure any open connections are closed afterward
#'   if (!inherits(future::plan(), "sequential")) future::plan("sequential")
#' }
#' }
#' @export
#'
#' @author Martin Jullum, Lars Henry Berge Olsen
#'
# nolint start
#' @references
#'   - \href{https://arxiv.org/pdf/2504.01842}{
#'   Jullum, M., Olsen, L. H. B., Lachmann, J., & Redelmeier, A. (2025). shapr: Explaining Machine Learning Models
#'   with Conditional Shapley Values in R and Python. arXiv preprint arXiv:2504.01842.}
#'   - \href{https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf}{
#'   Aas, K., Jullum, M., & Løland, A. (2021). Explaining individual predictions when features are dependent:
#'   More accurate approximations to Shapley values. Artificial Intelligence, 298, 103502}
#'   - \href{https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf}{
#'   Frye, C., Rowat, C., & Feige, I. (2020). Asymmetric Shapley values:
#'   incorporating causal knowledge into model-agnostic explainability.
#'   Advances in neural information processing systems, 33, 1229-1239}
#'   - \href{https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf}{
#'   Heskes, T., Sijben, E., Bucur, I. G., & Claassen, T. (2020). Causal shapley values:
#'   Exploiting causal knowledge to explain individual predictions of complex models.
#'   Advances in neural information processing systems, 33, 4778-4789}
#'   - \href{https://martinjullum.com/publication/jullum-2021-efficient/jullum-2021-efficient.pdf}{
#'    Jullum, M., Redelmeier, A. & Aas, K. (2021). Efficient and simple prediction explanations with
#'    groupShapley: A practical perspective. Italian Workshop on Explainable Artificial Intelligence 2021.}
#'   - \href{https://martinjullum.com/publication/redelmeier-2020-explaining/redelmeier-2020-explaining.pdf}{
#'   Redelmeier, A., Jullum, M., & Aas, K. (2020). Explaining predictive models with mixed features using Shapley
#'   values and conditional inference trees. In Machine Learning and Knowledge Extraction:
#'   International Cross-Domain Conference, CD-MAKE 2020, Dublin, Ireland, August 25-28, 2020, Proceedings 4
#'   (pp. 117-137). Springer International Publishing.}
#'   - \href{https://www.theoj.org/joss-papers/joss.02027/10.21105.joss.02027.pdf}{
#'   Sellereite N., & Jullum, M. (2019). shapr: An R-package for explaining machine learning models with
#'   dependence-aware Shapley values. Journal of Open Source Software, 5(46), 2027}
#'   - \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{
#'   Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using Shapley values and variational autoencoders to
#'   explain predictive models with dependent mixed features. Journal of machine learning research, 23(213), 1-51}
#'   - \href{https://link.springer.com/content/pdf/10.1007/s10618-024-01016-z.pdf}{
#'   Olsen, L. H. B., Glad, I. K., Jullum, M., & Aas, K. (2024). A comparative study of methods for estimating
#'   model-agnostic Shapley value explanations. Data Mining and Knowledge Discovery, 1-48}
#'  -  \href{https://link.springer.com/content/pdf/10.1007/978-3-032-08324-1_9.pdf}{
#'  Olsen, L. H. B., & Jullum, M. (2025). Improving the Weighting Strategy in KernelSHAP.
#'  In World Conference on Explainable Artificial Intelligence (pp. 194-218). Springer.}
# nolint end
explain <- function(model,
                    x_explain,
                    x_train,
                    approach,
                    phi0,
                    iterative = NULL,
                    max_n_coalitions = NULL,
                    group = NULL,
                    n_MC_samples = 1e3,
                    seed = NULL,
                    verbose = "basic",
                    predict_model = NULL,
                    get_model_specs = NULL,
                    prev_shapr_object = NULL,
                    asymmetric = FALSE,
                    causal_ordering = NULL,
                    confounding = NULL,
                    extra_computation_args = list(),
                    iterative_args = list(),
                    output_args = list(),
                    ...) { # ... is further arguments passed to specific approaches


  init_time <- Sys.time()

  if (!is.null(seed)) {
    set.seed(seed)
  }
  # Gets and check feature specs from the model
  feature_specs <- get_feature_specs(get_model_specs, model)

  # Sets up and organizes input parameters
  # Checks the input parameters and their compatibility
  # Checks data/model compatibility
  internal <- setup(
    x_train = x_train,
    x_explain = x_explain,
    approach = approach,
    phi0 = phi0,
    max_n_coalitions = max_n_coalitions,
    group = group,
    n_MC_samples = n_MC_samples,
    seed = seed,
    feature_specs = feature_specs,
    verbose = verbose,
    iterative = iterative,
    iterative_args = iterative_args,
    init_time = init_time,
    prev_shapr_object = prev_shapr_object,
    asymmetric = asymmetric,
    causal_ordering = causal_ordering,
    confounding = confounding,
    output_args = output_args,
    extra_computation_args = extra_computation_args,
    model_class = class(model)[1],
    ...
  )


  # Gets predict_model (if not passed to explain)
  predict_model <- get_predict_model(predict_model = predict_model, model = model)

  # Checks that predict_model gives correct format
  test_predict_model(
    x_test = head(internal$data$x_train, 2),
    predict_model = predict_model,
    model = model,
    internal = internal
  )

  internal$timing_list$test_prediction <- Sys.time()


  internal <- additional_regression_setup(internal, model = model, predict_model = predict_model)

  # Not called for approach %in% c("regression_surrogate","vaeac")
  internal <- setup_approach(internal, model = model, predict_model = predict_model)

  internal$main_timing_list <- internal$timing_list

  converged <- FALSE
  iter <- length(internal$iter_list)

  if (!is.null(seed)) {
    set.seed(seed)
  }

  cli_startup(internal, verbose)


  while (converged == FALSE) {
    cli_iter(verbose, internal, iter)

    internal$timing_list <- list(init = Sys.time())

    # Setup the Shapley framework
    internal <- shapley_setup(internal)

    # Only actually called for approach %in% c("regression_surrogate","vaeac")
    internal <- setup_approach(internal, model = model, predict_model = predict_model)

    # Compute the vS
    vS_list <- compute_vS(internal, model, predict_model)

    # Compute Shapley value estimates and bootstrapped standard deviations
    internal <- compute_estimates(internal, vS_list)

    # Check convergence based on estimates and standard deviations (and thresholds)
    internal <- check_convergence(internal)

    # Save intermediate results
    save_results(internal)

    # Preparing parameters for next iteration (does not do anything if already converged)
    internal <- prepare_next_iteration(internal)

    # Printing iteration information
    print_iter(internal)

    # Setting globals for to simplify the loop
    converged <- internal$iter_list[[iter]]$converged

    internal$timing_list$postprocess_res <- Sys.time()

    internal$iter_timing_list[[iter]] <- internal$timing_list

    iter <- iter + 1
  }

  internal$main_timing_list$main_computation <- Sys.time()


  # Rerun after convergence to get the same output format as for the non-iterative approach
  output <- finalize_explanation(internal = internal)

  internal$main_timing_list$finalize_explanation <- Sys.time()

  output$timing <- compute_time(internal)


  # Some cleanup when doing testing
  testing <- internal$parameters$testing
  if (isTRUE(testing)) {
    output <- testing_cleanup(output)
  }


  return(output)
}

#' Clean Out Certain Output Arguments to Allow Perfect Reproducibility of the Output
#'
#' @inheritParams default_doc_export
#'
#' @return Cleaned up version of the output list used for testthat testing
#'
#' @export
#' @keywords internal
#' @author Lars Henry Berge Olsen, Martin Jullum
testing_cleanup <- function(output) {
  # Removing the timing of different function calls
  output$timing <- NULL

  # Clearing out the timing lists as well
  output$internal$main_timing_list <- NULL
  output$internal$iter_timing_list <- NULL
  output$internal$timing_list <- NULL

  # Removing paths to non-reproducible vaeac model objects
  if (isFALSE(output$internal$parameters$vaeac.extra_parameters$vaeac.save_model)) {
    output$internal$parameters[c(
      "vaeac", "vaeac.sampler", "vaeac.model", "vaeac.activation_function", "vaeac.checkpoint"
    )] <- NULL
    output$internal$parameters$vaeac.extra_parameters[c("vaeac.folder_to_save_model", "vaeac.model_description")] <-
      NULL
  }

  # Removing the model object for regression surrogate models to avoid non-reproducibility
  # in both fit-times and model object structure
  if ("regression_surrogate" %in% output$internal$parameters$approach) {
    output$internal$objects$regression.surrogate_model <- NULL
  }

  # Delete the saving_path
  output$internal$parameters$output_args$saving_path <- NULL
  output$saving_path <- NULL

  return(output)
}
