#' Explain the output of machine learning models with dependence-aware (conditional/observational) Shapley values
#'
#' @description Computes dependence-aware Shapley values for observations in `x_explain` from the specified
#' `model` by using the method specified in `approach` to estimate the conditional expectation.
#' See \href{https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf}{Aas et al. (2021)}
#' for a thorough introduction to dependence-aware prediction explanation with Shapley values.
#'
#' @param x_train Matrix or data.frame/data.table.
#' Contains the data used to estimate the (conditional) distributions for the features
#' needed to properly estimate the conditional expectations in the Shapley formula.
#'
#' @param x_explain Matrix or data.frame/data.table.
#' Contains the the features, whose predictions ought to be explained.
#'
#' @param model Model object.
#' Specifies the model whose predictions we want to explain.
#' Run [get_supported_models()]
#' for a table of which models `explain` supports natively. Unsupported models
#' can still be explained by passing `predict_model` and (optionally) `get_model_specs`,
#' see details for more information.
#'
#' @param approach Character vector of length `1` or one less than the number of features.
#' All elements should, either be `"gaussian"`, `"copula"`, `"empirical"`, `"ctree"`, `"vaeac"`,
#' `"categorical"`, `"timeseries"`, `"independence"`, `"regression_separate"`, or `"regression_surrogate"`.
#' The two regression approaches can not be combined with any other approach.
#' See details for more information.
#'
#' @param phi0 Numeric.
#' The prediction value for unseen data, i.e. an estimate of the expected prediction without conditioning on any
#' features.
#' Typically we set this value equal to the mean of the response variable in our training data, but other choices
#' such as the mean of the predictions in the training data are also reasonable.
#'
#' @param max_n_coalitions Integer.
#' The upper limit on the number of unique feature/group coalitions to use in the iterative procedure
#' (if `iterative = TRUE`).
#' If `iterative = FALSE` it represents the number of feature/group coalitions to use directly.
#' The quantity refers to the number of unique feature coalitions if `group = NULL`,
#' and group coalitions if `group != NULL`.
#' `max_n_coalitions = NULL` corresponds to `max_n_coalitions=2^n_features`.
#'
#' @param group List.
#' If `NULL` regular feature wise Shapley values are computed.
#' If provided, group wise Shapley values are computed.
#' `group` then has length equal to the number of groups.
#' The list element contains character vectors with the features included in each of the different groups.
#' See
#' \href{https://martinjullum.com/publication/jullum-2021-efficient/jullum-2021-efficient.pdf}{Jullum et al. (2021)}
#' for more information on group wise Shapley values.
#'
#' @param n_MC_samples Positive integer.
#' For most approaches, it indicates the maximum number of samples to use in the Monte Carlo integration
#' of every conditional expectation.
#' For `approach="ctree"`, `n_MC_samples` corresponds to the number of samples
#' from the leaf node (see an exception related to the `ctree.sample` argument [setup_approach.ctree()]).
#' For `approach="empirical"`, `n_MC_samples` is  the \eqn{K} parameter in equations (14-15) of
#' Aas et al. (2021), i.e. the maximum number of observations (with largest weights) that is used, see also the
#' `empirical.eta` argument [setup_approach.empirical()].
#'
#' @param seed Positive integer.
#' Specifies the seed before any randomness based code is being run.
#' If `NULL` no seed is set in the calling environment.
#'
#' @param predict_model Function.
#' The prediction function used when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported models.)
#' The function must have two arguments, `model` and `newdata` which specify, respectively, the model
#' and a data.frame/data.table to compute predictions for.
#' The function must give the prediction as a numeric vector.
#' `NULL` (the default) uses functions specified internally.
#' Can also be used to override the default function for natively supported model classes.
#'
#' @param get_model_specs Function.
#' An optional function for checking model/data consistency when `model` is not natively supported.
#' (Run [get_supported_models()] for a list of natively supported models.)
#' The function takes `model` as argument and provides a list with 3 elements:
#' \describe{
#'   \item{labels}{Character vector with the names of each feature.}
#'   \item{classes}{Character vector with the classes of each features.}
#'   \item{factor_levels}{Character vector with the levels for any categorical features.}
#' }
#' If `NULL` (the default) internal functions are used for natively supported model classes, and the checking is
#' disabled for unsupported model classes.
#' Can also be used to override the default function for natively supported model classes.
#'
#' @param verbose String vector or NULL.
#' Specifies the verbosity (printout detail level) through one or more of strings `"basic"`, `"progress"`,
#'  `"convergence"`, `"shapley"` and `"vS_details"`.
#' `"basic"` (default) displays basic information about the computation which is being performed.
#' `"progress` displays information about where in the calculation process the function currently is.
#' #' `"convergence"` displays information on how close to convergence the Shapley value estimates are
#' (only when `iterative = TRUE`) .
#' `"shapley"` displays intermediate Shapley value estimates and standard deviations (only when `iterative = TRUE`)
#' + the final estimates.
#' `"vS_details"` displays information about the v_S estimates.
#' This is most relevant for `approach %in% c("regression_separate", "regression_surrogate", "vaeac"`).
#' `NULL` means no printout.
#' Note that any combination of four strings can be used.
#' E.g. `verbose = c("basic", "vS_details")` will display basic information + details about the v(S)-estimation process.
#'
#' @param iterative Logical or NULL
#' If `NULL` (default), the argument is set to `TRUE` if there are more than 5 features/groups, and `FALSE` otherwise.
#' If eventually `TRUE`, the Shapley values are estimated iteratively in an iterative manner.
#' This provides sufficiently accurate Shapley value estimates faster.
#' First an initial number of coalitions is sampled, then bootsrapping is used to estimate the variance of the Shapley
#' values.
#' A convergence criterion is used to determine if the variances of the Shapley values are sufficiently small.
#' If the variances are too high, we estimate the number of required samples to reach convergence, and thereby add more
#' coalitions.
#' The process is repeated until the variances are below the threshold.
#' Specifics related to the iterative process and convergence criterion are set through `iterative_args`.
#'
#' @param iterative_args Named list.
#' Specifies the arguments for the iterative procedure.
#' See [get_iterative_args_default()] for description of the arguments and their default values.
#' @param output_args Named list.
#' Specifies certain arguments related to the output of the function.
#' See [get_output_args_default()] for description of the arguments and their default values.
#' @param extra_computation_args Named list.
#' Specifies extra arguments related to the computation of the Shapley values.
#' See [get_extra_comp_args_default()] for description of the arguments and their default values.
#'
#' @param prev_shapr_object `shapr` object or string.
#' If an object of class `shapr` is provided, or string with a path to where intermediate results are stored,
#' then the function will use the previous object to continue the computation.
#' This is useful if the computation is interrupted or you want higher accuracy than already obtained, and therefore
#' want to continue the iterative estimation. See the
#' \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html}{general usage} for examples.
#'
#' @param asymmetric Logical.
#' Not applicable for (regular) non-causal or asymmetric explanations.
#' If `FALSE` (default), `explain` computes regular symmetric Shapley values,
#' If `TRUE`, then `explain` compute asymmetric Shapley values based on the (partial) causal ordering
#' given by `causal_ordering`. That is, `explain` only uses the feature combinations/coalitions that
#' respect the causal ordering when computing the asymmetric Shapley values. If `asymmetric` is `TRUE` and
#' `confounding` is `NULL` (default), then `explain` computes asymmetric conditional Shapley values as specified in
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
#' Note: All features/groups must be included in the `causal_ordering` without any duplicates.
#'
#' @param confounding Logical vector.
#' Not applicable for (regular) non-causal or asymmetric explanations.
#' `confounding` is a vector of logicals specifying whether confounding is assumed or not for each component in the
#' `causal_ordering`. If `NULL` (default), then no assumption about the confounding structure is made and `explain`
#' computes asymmetric/symmetric conditional Shapley values, depending on the value of `asymmetric`.
#' If `confounding` is a single logical, i.e., `FALSE` or `TRUE`, then this assumption is set globally
#' for all components in the causal ordering. Otherwise, `confounding` must be a vector of logicals of the same
#' length as `causal_ordering`, indicating the confounding assumption for each component. When `confounding` is
#' specified, then `explain` computes asymmetric/symmetric causal Shapley values, depending on the value of
#' `asymmetric`. The `approach` cannot be `regression_separate` and `regression_surrogate` as the
#' regression-based approaches are not applicable to the causal Shapley value methodology.
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
#' \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html}{general usage}.
#' (From R: `vignette("general_usage", package = "shapr")`).
#' Moreover,
#'  \href{https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf}{Aas et al. (2021)}
#' gives a general introduction to dependence-aware Shapley values, and the three approaches `"empirical"`,
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
#' general usage} for more information.
#'
#' The package also supports the computation of causal and asymmetric Shapley values as introduced by
#' \href{https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf}{
#' Heskes et al. (2020)} and
#' \href{https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf}{
#' Frye et al. (2020)}.
#' Asymmetric Shapley values were proposed by
#' \href{https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf}{
#' Heskes et al. (2020)} as a way to incorporate causal knowledge in
#' the real world by restricting the possible feature combinations/coalitions when computing the Shapley values to
#' those consistent with a (partial) causal ordering.
#' Causal Shapley values were proposed by
#' \href{https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf}{
#' Frye et al. (2020)} as a way to explain the total effect of features
#' on the prediction, taking into account their causal relationships, by adapting the sampling procedure in `shapr`.
#'
#' The package allows for parallelized computation with progress updates through the tightly connected
#' [future::future] and [progressr::progressr] packages.
#' See the examples below.
#' For iterative estimation (`iterative=TRUE`), intermediate results may also be printed to the console
#' (according to the `verbose` argument).
#' Moreover, the intermediate results are written to disk.
#' This combined batch computing of the v(S) values, enables fast and accurate estimation of the Shapley values
#' in a memory friendly manner.
#'
#' @return Object of class `c("shapr", "list")`. Contains the following items:
#' \describe{
#'   \item{`shapley_values_est`}{data.table with the estimated Shapley values with explained observation in the rows and
#'   features along the columns.
#'   The column `none` is the prediction not devoted to any of the features (given by the argument `phi0`)}
#'   \item{`shapley_values_sd`}{data.table with the standard deviation of the Shapley values reflecting the uncertainty.
#'   Note that this only reflects the coalition sampling part of the kernelSHAP procedure, and is therefore by
#'   definition 0 when all coalitions is used.
#'   Only present when `extra_computation_args$compute_sd=TRUE`, which is the default when `iterative = TRUE`}
#'   \item{`internal`}{List with the different parameters, data, functions and other output used internally.}
#'   \item{`pred_explain`}{Numeric vector with the predictions for the explained observations}
#'   \item{`MSEv`}{List with the values of the MSEv evaluation criterion for the approach. See the
#'   \href{https://norskregnesentral.github.io/shapr/articles/general_usage.html#msev-evaluation-criterion
#'   }{MSEv evaluation section in the general usage for details}.}
#'   \item{`timing`}{List containing timing information for the different parts of the computation.
#'   `init_time` and `end_time` gives the time stamps for the start and end of the computation.
#'   `total_time_secs` gives the total time in seconds for the complete execution of `explain()`.
#'   `main_timing_secs` gives the time in seconds for the main computations.
#'   `iter_timing_secs` gives for each iteration of the iterative estimation, the time spent on the different parts
#'   iterative estimation routine.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' # Load example data
#' data("airquality")
#' airquality <- airquality[complete.cases(airquality), ]
#' x_var <- c("Solar.R", "Wind", "Temp", "Month")
#' y_var <- "Ozone"
#'
#' # Split data into test- and training data
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
#' # ctree approach
#' explain4 <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "ctree",
#'   phi0 = p,
#'   n_MC_samples = 1e2
#' )
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
#' # Print the Shapley values
#' print(explain1$shapley_values_est)
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
#' print(explain_groups$shapley_values_est)
#'
#' # Separate and surrogate regression approaches with linear regression models.
#' explain_separate_lm <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   phi0 = p,
#'   approach = "regression_separate",
#'   regression.model = parsnip::linear_reg()
#' )
#'
#' explain_surrogate_lm <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   phi0 = p,
#'   approach = "regression_surrogate",
#'   regression.model = parsnip::linear_reg()
#' )
#'
#' # Iterative estimation
#' # For illustration purposes only. By default not used for such small dimensions as here
#'
#' # Gaussian approach
#' explain_iterative <- explain(
#'   model = model,
#'   x_explain = x_explain,
#'   x_train = x_train,
#'   approach = "gaussian",
#'   phi0 = p,
#'   n_MC_samples = 1e2,
#'   iterative = TRUE,
#'   iterative_args = list(initial_n_coalitions = 10)
#' )
#' }
#'
#' @export
#'
#' @author Martin Jullum, Lars Henry Berge Olsen
#'
# nolint start
#' @references
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
#'   International Cross-Domain Conference, CD-MAKE 2020, Dublin, Ireland, August 25–28, 2020, Proceedings 4
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
#'  -  \href{https://arxiv.org/pdf/2410.04883}{
#'  Olsen, L. H. B., & Jullum, M. (2024). Improving the Sampling Strategy in KernelSHAP. arXiv e-prints, arXiv-2410}
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
                    seed = 1,
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
  # Checks the input parameters and their compatability
  # Checks data/model compatability
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

  cli_startup(internal, class(model), verbose)


  while (converged == FALSE) {
    cli_iter(verbose, internal, iter)

    internal$timing_list <- list(init = Sys.time())

    # Setup the Shapley framework
    internal <- shapley_setup(internal)

    # Only actually called for approach %in% c("regression_surrogate","vaeac")
    internal <- setup_approach(internal, model = model, predict_model = predict_model)

    # Compute the vS
    vS_list <- compute_vS(internal, model, predict_model)

    # Compute shapley value estimated and bootstrapped standard deviations
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

#' Cleans out certain output arguments to allow perfect reproducibility of the output
#'
#' @inheritParams default_doc_export
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

  # Removing paths to non-reproducable vaeac model objects
  if (isFALSE(output$internal$parameters$vaeac.extra_parameters$vaeac.save_model)) {
    output$internal$parameters[c(
      "vaeac", "vaeac.sampler", "vaeac.model", "vaeac.activation_function", "vaeac.checkpoint"
    )] <- NULL
    output$internal$parameters$vaeac.extra_parameters[c("vaeac.folder_to_save_model", "vaeac.model_description")] <-
      NULL
  }

  # Removing the fit times for regression surrogate models
  if ("regression_surrogate" %in% output$internal$parameters$approach) {
    # Deletes the fit_times for approach = regression_surrogate to make tests pass.
    # In the future we could delete this only when a new argument in explain called testing is TRUE
    output$internal$objects$regression.surrogate_model$pre$mold$blueprint$recipe$fit_times <- NULL
  }

  # Delete the saving_path
  output$internal$parameters$output_args$saving_path <- NULL
  output$saving_path <- NULL

  return(output)
}
