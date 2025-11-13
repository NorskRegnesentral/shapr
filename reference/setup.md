# Check Setup

Check Setup

## Usage

``` r
setup(
  x_train,
  x_explain,
  approach,
  phi0,
  output_size = 1,
  max_n_coalitions,
  group,
  n_MC_samples,
  seed,
  feature_specs,
  type = "regular",
  horizon = NULL,
  y = NULL,
  xreg = NULL,
  train_idx = NULL,
  explain_idx = NULL,
  explain_y_lags = NULL,
  explain_xreg_lags = NULL,
  group_lags = NULL,
  verbose,
  iterative = NULL,
  iterative_args = list(),
  is_python = FALSE,
  testing = FALSE,
  init_time = NULL,
  prev_shapr_object = NULL,
  asymmetric = FALSE,
  causal_ordering = NULL,
  confounding = NULL,
  output_args = list(),
  extra_computation_args = list(),
  model_class,
  ...
)
```

## Arguments

- x_train:

  Matrix or data.frame/data.table. Data used to estimate the
  (conditional) feature distributions needed to properly estimate the
  conditional expectations in the Shapley formula.

- x_explain:

  Matrix or data.frame/data.table. Features for which predictions should
  be explained.

- approach:

  Character vector of length `1` or one less than the number of
  features. All elements should either be `"gaussian"`, `"copula"`,
  `"empirical"`, `"ctree"`, `"vaeac"`, `"categorical"`, `"timeseries"`,
  `"independence"`, `"regression_separate"`, or
  `"regression_surrogate"`. The two regression approaches cannot be
  combined with any other approach. See details for more information.

- phi0:

  Numeric. The prediction value for unseen data, i.e., an estimate of
  the expected prediction without conditioning on any features.
  Typically set this equal to the mean of the response in the training
  data, but alternatives such as the mean of the training predictions
  are also reasonable.

- output_size:

  Scalar integer. Specifies the dimension of the output from the
  prediction model for every observation.

- max_n_coalitions:

  Integer. Upper limit on the number of unique feature/group coalitions
  to use in the iterative procedure (if `iterative = TRUE`). If
  `iterative = FALSE`, it represents the number of feature/group
  coalitions to use directly. The quantity refers to the number of
  unique feature coalitions if `group = NULL`, and group coalitions if
  `group != NULL`. `max_n_coalitions = NULL` corresponds to
  `2^n_features`.

- group:

  List. If `NULL`, regular feature-wise Shapley values are computed. If
  provided, group-wise Shapley values are computed. `group` then has
  length equal to the number of groups. Each list element contains the
  character vectors with the features included in the corresponding
  group. See [Jullum et al.
  (2021)](https://martinjullum.com/publication/jullum-2021-efficient/jullum-2021-efficient.pdf)
  for more information on group-wise Shapley values.

- n_MC_samples:

  Positive integer. For most approaches, it indicates the maximum number
  of samples to use in the Monte Carlo integration of every conditional
  expectation. For `approach="ctree"`, `n_MC_samples` corresponds to the
  number of samples from the leaf node (see an exception related to the
  `ctree.sample` argument in
  [`setup_approach.ctree()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)).
  For `approach="empirical"`, `n_MC_samples` is the \\K\\ parameter in
  equations (14-15) of Aas et al. (2021), i.e. the maximum number of
  observations (with largest weights) that is used, see also the
  `empirical.eta` argument
  [`setup_approach.empirical()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md).

- seed:

  Positive integer. Specifies the seed before any code involving
  randomness is run. If `NULL` (default), no seed is set in the calling
  environment.

- feature_specs:

  List. The output from
  [`get_model_specs()`](https://norskregnesentral.github.io/shapr/reference/get_model_specs.md)
  or
  [`get_data_specs()`](https://norskregnesentral.github.io/shapr/reference/get_data_specs.md).
  Contains the three elements:

  labels

  :   Character vector with the names of each feature.

  classes

  :   Character vector with the classes of each feature.

  factor_levels

  :   Character vector with the levels for any categorical features.

- type:

  Character. Either "regular" or "forecast", matching the function the
  call originated from, and thus the type of explanation to generate.

- horizon:

  Numeric. The forecast horizon to explain. Passed to the
  `predict_model` function.

- y:

  Matrix, data.frame/data.table or a numeric vector. Contains the
  endogenous variables used to estimate the (conditional) distributions
  needed to properly estimate the conditional expectations in the
  Shapley formula including the observations to be explained.

- xreg:

  Matrix, data.frame/data.table or a numeric vector. Contains the
  exogenous variables used to estimate the (conditional) distributions
  needed to properly estimate the conditional expectations in the
  Shapley formula including the observations to be explained. As
  exogenous variables are used contemporaneously when producing a
  forecast, this item should contain nrow(y) + horizon rows.

- train_idx:

  Numeric vector. The row indices in data and reg denoting points in
  time to use when estimating the conditional expectations in the
  Shapley value formula. If `train_idx = NULL` (default) all indices not
  selected to be explained will be used.

- explain_idx:

  Numeric vector. The row indices in data and reg denoting points in
  time to explain.

- explain_y_lags:

  Numeric vector. Denotes the number of lags that should be used for
  each variable in `y` when making a forecast.

- explain_xreg_lags:

  Numeric vector. If `xreg != NULL`, denotes the number of lags that
  should be used for each variable in `xreg` when making a forecast.

- group_lags:

  Logical. If `TRUE` all lags of each variable are grouped together and
  explained as a group. If `FALSE` all lags of each variable are
  explained individually.

- verbose:

  String vector or NULL. Controls verbosity (printout detail level) via
  one or more of `"basic"`, `"progress"`, `"convergence"`, `"shapley"`
  and `"vS_details"`. `"basic"` (default) displays basic information
  about the computation and messages about parameters/checks.
  `"progress"` displays where in the calculation process the function
  currently is. `"convergence"` displays how close the Shapley value
  estimates are to convergence (only when `iterative = TRUE`).
  `"shapley"` displays intermediate Shapley value estimates and standard
  deviations (only when `iterative = TRUE`), and the final estimates.
  `"vS_details"` displays information about the v(S) estimates, most
  relevant for
  `approach %in% c("regression_separate", "regression_surrogate", "vaeac")`.
  `NULL` means no printout. Any combination can be used, e.g.,
  `verbose = c("basic", "vS_details")`.

- iterative:

  Logical or NULL. If `NULL` (default), set to `TRUE` if there are more
  than 5 features/groups, and `FALSE` otherwise. If `TRUE`, Shapley
  values are estimated iteratively for faster, sufficiently accurate
  results. First an initial number of coalitions is sampled, then
  bootstrapping estimates the variance of the Shapley values. A
  convergence criterion determines if the variances are sufficiently
  small. If not, additional samples are added. The process repeats until
  the variances are below the threshold. Specifics for the iterative
  process and convergence criterion are set via `iterative_args`.

- iterative_args:

  Named list. Specifies the arguments for the iterative procedure. See
  [`get_iterative_args_default()`](https://norskregnesentral.github.io/shapr/reference/get_iterative_args_default.md)
  for description of the arguments and their default values.

- is_python:

  Logical. Indicates whether the function is called from the Python
  wrapper. Default is FALSE, which is never changed when calling the
  function via
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  in R. The parameter is later used to disallow running the AICc
  versions of the empirical method, as that requires data-based
  optimization, which is not supported in `shaprpy`.

- testing:

  Logical. Only used to remove random components, like timing, from the
  output when comparing with testthat. Defaults to `FALSE`.

- init_time:

  POSIXct. The time when the
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  function was called, as returned by
  [`Sys.time()`](https://rdrr.io/r/base/Sys.time.html). Used to
  calculate the total time of the
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  call.

- prev_shapr_object:

  `shapr` object or string. If an object of class `shapr` is provided,
  or a string with a path to where intermediate results are stored, then
  the function will use the previous object to continue the computation.
  This is useful if the computation is interrupted or you want higher
  accuracy than already obtained, and therefore want to continue the
  iterative estimation. See the [general usage
  vignette](https://norskregnesentral.github.io/shapr/articles/general_usage.html)
  for examples.

- asymmetric:

  Logical. Not applicable for (regular) non-causal explanations. If
  `FALSE` (default), `explain` computes regular symmetric Shapley
  values. If `TRUE`, `explain` computes asymmetric Shapley values based
  on the (partial) causal ordering given by `causal_ordering`. That is,
  `explain` only uses feature coalitions that respect the causal
  ordering. If `asymmetric` is `TRUE` and `confounding` is `NULL`
  (default), `explain` computes asymmetric conditional Shapley values as
  specified in [Frye et al.
  (2020)](https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf).
  If `confounding` is provided, i.e., not `NULL`, then `explain`
  computes asymmetric causal Shapley values as specified in [Heskes et
  al.
  (2020)](https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf).

- causal_ordering:

  List. Not applicable for (regular) non-causal or asymmetric
  explanations. `causal_ordering` is an unnamed list of vectors
  specifying the components of the partial causal ordering that the
  coalitions must respect. Each vector represents a component and
  contains one or more features/groups identified by their names
  (strings) or indices (integers). If `causal_ordering` is `NULL`
  (default), no causal ordering is assumed and all possible coalitions
  are allowed. No causal ordering is equivalent to a causal ordering
  with a single component that includes all features
  (`list(1:n_features)`) or groups (`list(1:n_groups)`) for feature-wise
  and group-wise Shapley values, respectively. For feature-wise Shapley
  values and `causal_ordering = list(c(1, 2), c(3, 4))`, the
  interpretation is that features 1 and 2 are the ancestors of features
  3 and 4, while features 3 and 4 are on the same level. Note: All
  features/groups must be included in `causal_ordering` without
  duplicates.

- confounding:

  Logical vector. Not applicable for (regular) non-causal or asymmetric
  explanations. `confounding` is a logical vector specifying whether
  confounding is assumed for each component in the `causal_ordering`. If
  `NULL` (default), no assumption about the confounding structure is
  made and `explain` computes asymmetric/symmetric conditional Shapley
  values, depending on `asymmetric`. If `confounding` is a single
  logical (`FALSE` or `TRUE`), the assumption is set globally for all
  components in the causal ordering. Otherwise, `confounding` must have
  the same length as `causal_ordering`, indicating the confounding
  assumption for each component. When `confounding` is specified,
  `explain` computes asymmetric/symmetric causal Shapley values,
  depending on `asymmetric`. The `approach` cannot be
  `regression_separate` or `regression_surrogate`, as the
  regression-based approaches are not applicable to the causal Shapley
  methodology.

- output_args:

  Named list. Specifies certain arguments related to the output of the
  function. See
  [`get_output_args_default()`](https://norskregnesentral.github.io/shapr/reference/get_output_args_default.md)
  for description of the arguments and their default values.

- extra_computation_args:

  Named list. Specifies extra arguments related to the computation of
  the Shapley values. See
  [`get_extra_comp_args_default()`](https://norskregnesentral.github.io/shapr/reference/get_extra_comp_args_default.md)
  for description of the arguments and their default values.

- model_class:

  Character string. The class of the model object, e.g., "lm", "glm",
  "xgboost", etc. obtained by `class(model)[1]`.

- ...:

  Further arguments passed to specific approaches, see below.

## Value

An internal list containing parameters, info, data, and computations
needed for later steps. The list is expanded and modified in other
functions.
