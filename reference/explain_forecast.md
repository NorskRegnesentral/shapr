# Explain a Forecast from Time Series Models with Dependence-Aware (Conditional/Observational) Shapley Values

Computes dependence-aware Shapley values for observations in
`explain_idx` from the specified `model` by using the method specified
in `approach` to estimate the conditional expectation. See [Aas, et. al
(2021)](https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf)
for a thorough introduction to dependence-aware prediction explanation
with Shapley values. For an overview of the methodology and capabilities
of the `shapr` package, see the software paper [Jullum et al.
(2025)](https://arxiv.org/pdf/2504.01842), or the pkgdown site at
[norskregnesentral.github.io/shapr/](https://norskregnesentral.github.io/shapr/).

## Usage

``` r
explain_forecast(
  model,
  y,
  xreg = NULL,
  train_idx = NULL,
  explain_idx,
  explain_y_lags,
  explain_xreg_lags = explain_y_lags,
  horizon,
  approach,
  phi0,
  max_n_coalitions = NULL,
  iterative = NULL,
  group_lags = TRUE,
  group = NULL,
  n_MC_samples = 1000,
  seed = NULL,
  predict_model = NULL,
  get_model_specs = NULL,
  verbose = "basic",
  extra_computation_args = list(),
  iterative_args = list(),
  output_args = list(),
  ...
)
```

## Arguments

- model:

  Model object. The model whose predictions you want to explain. Run
  [`get_supported_models()`](https://norskregnesentral.github.io/shapr/reference/get_supported_models.md)
  for a table of which models `explain` supports natively. Unsupported
  models can still be explained by passing `predict_model` and
  (optionally) `get_model_specs`, see details for more information.

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

- horizon:

  Numeric. The forecast horizon to explain. Passed to the
  `predict_model` function.

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

- max_n_coalitions:

  Integer. Upper limit on the number of unique feature/group coalitions
  to use in the iterative procedure (if `iterative = TRUE`). If
  `iterative = FALSE`, it represents the number of feature/group
  coalitions to use directly. The quantity refers to the number of
  unique feature coalitions if `group = NULL`, and group coalitions if
  `group != NULL`. `max_n_coalitions = NULL` corresponds to
  `2^n_features`.

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

- group_lags:

  Logical. If `TRUE` all lags of each variable are grouped together and
  explained as a group. If `FALSE` all lags of each variable are
  explained individually.

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

- predict_model:

  Function. Prediction function to use when `model` is not natively
  supported. (Run
  [`get_supported_models()`](https://norskregnesentral.github.io/shapr/reference/get_supported_models.md)
  for a list of natively supported models.) The function must have two
  arguments, `model` and `newdata`, which specify the model and a
  data.frame/data.table to compute predictions for, respectively. The
  function must give the prediction as a numeric vector. `NULL` (the
  default) uses functions specified internally. Can also be used to
  override the default function for natively supported model classes.

- get_model_specs:

  Function. An optional function for checking model/data consistency
  when `model` is not natively supported. (Run
  [`get_supported_models()`](https://norskregnesentral.github.io/shapr/reference/get_supported_models.md)
  for a list of natively supported models.) The function takes `model`
  as an argument and provides a list with 3 elements:

  labels

  :   Character vector with the names of each feature.

  classes

  :   Character vector with the class of each feature.

  factor_levels

  :   Character vector with the levels for any categorical features.

  If `NULL` (the default), internal functions are used for natively
  supported model classes, and checking is disabled for unsupported
  model classes. Can also be used to override the default function for
  natively supported model classes.

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

- extra_computation_args:

  Named list. Specifies extra arguments related to the computation of
  the Shapley values. See
  [`get_extra_comp_args_default()`](https://norskregnesentral.github.io/shapr/reference/get_extra_comp_args_default.md)
  for description of the arguments and their default values.

- iterative_args:

  Named list. Specifies the arguments for the iterative procedure. See
  [`get_iterative_args_default()`](https://norskregnesentral.github.io/shapr/reference/get_iterative_args_default.md)
  for description of the arguments and their default values.

- output_args:

  Named list. Specifies certain arguments related to the output of the
  function. See
  [`get_output_args_default()`](https://norskregnesentral.github.io/shapr/reference/get_output_args_default.md)
  for description of the arguments and their default values.

- ...:

  Arguments passed on to
  [`setup_approach.categorical`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md),
  [`setup_approach.copula`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md),
  [`setup_approach.ctree`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md),
  [`setup_approach.empirical`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md),
  [`setup_approach.gaussian`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md),
  [`setup_approach.independence`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md),
  [`setup_approach.timeseries`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md),
  [`setup_approach.vaeac`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)

  `categorical.joint_prob_dt`

  :   Data.table. (Optional) Containing the joint probability
      distribution for each combination of feature values. `NULL` means
      it is estimated from the `x_train` and `x_explain`.

  `categorical.epsilon`

  :   Numeric value. (Optional) If `categorical.joint_prob_dt` is not
      supplied, probabilities/frequencies are estimated using `x_train`.
      If certain observations occur in `x_explain` and NOT in `x_train`,
      then epsilon is used as the proportion of times that these
      observations occur in the training data. In theory, this
      proportion should be zero, but this causes an error later in the
      Shapley computation.

  `internal`

  :   List. Not used directly, but passed through from
      [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

  `ctree.mincriterion`

  :   Numeric scalar or vector. Either a scalar or vector of length
      equal to the number of features in the model. The value is equal
      to 1 - \\\alpha\\ where \\\alpha\\ is the nominal level of the
      conditional independence tests. If it is a vector, this indicates
      which value to use when conditioning on various numbers of
      features. The default value is 0.95.

  `ctree.minsplit`

  :   Numeric scalar. Determines the minimum value that the sum of the
      left and right daughter nodes must reach for a split. The default
      value is 20.

  `ctree.minbucket`

  :   Numeric scalar. Determines the minimum sum of weights in a
      terminal node required for a split. The default value is 7.

  `ctree.sample`

  :   Boolean. If `TRUE` (default), then the method always samples
      `n_MC_samples` observations from the leaf nodes (with
      replacement). If `FALSE` and the number of observations in the
      leaf node is less than `n_MC_samples`, the method will take all
      observations in the leaf. If `FALSE` and the number of
      observations in the leaf node is more than `n_MC_samples`, the
      method will sample `n_MC_samples` observations (with replacement).
      This means that there will always be sampling in the leaf unless
      `sample = FALSE` *and* the number of obs in the node is less than
      `n_MC_samples`.

  `empirical.type`

  :   Character. (default = `"fixed_sigma"`) Must be one of
      `"independence"`, `"fixed_sigma"`, `"AICc_each_k"`, or
      `"AICc_full"`. Note: `"empirical.type = independence"` is
      deprecated; use `approach = "independence"` instead.
      `"fixed_sigma"` uses a fixed bandwidth (set through
      `empirical.fixed_sigma`) in the kernel density estimation.
      `"AICc_each_k"` and `"AICc_full"` optimize the bandwidth using the
      AICc criterion, with respectively one bandwidth per coalition size
      and one bandwidth for all coalition sizes.

  `empirical.eta`

  :   Numeric scalar. Needs to be `0 < eta <= 1`. The default value is
      0.95. Represents the minimum proportion of the total empirical
      weight that data samples should use. For example, if `eta = .8`,
      we choose the `K` samples with the largest weights so that the sum
      of the weights accounts for 80\\ `eta` is the \\\eta\\ parameter
      in equation (15) of [Aas et al.
      (2021)](https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf).

  `empirical.fixed_sigma`

  :   Positive numeric scalar. The default value is 0.1. Represents the
      kernel bandwidth in the distance computation used when
      conditioning on all different coalitions. Only used when
      `empirical.type = "fixed_sigma"`

  `empirical.n_samples_aicc`

  :   Positive integer. Number of samples to consider in AICc
      optimization. The default value is 1000. Only used when
      `empirical.type` is either `"AICc_each_k"` or `"AICc_full"`.

  `empirical.eval_max_aicc`

  :   Positive integer. Maximum number of iterations when optimizing the
      AICc. The default value is 20. Only used when `empirical.type` is
      either `"AICc_each_k"` or `"AICc_full"`.

  `empirical.start_aicc`

  :   Numeric. Start value of the `sigma` parameter when optimizing the
      AICc. The default value is 0.1. Only used when `empirical.type` is
      either `"AICc_each_k"` or `"AICc_full"`.

  `empirical.cov_mat`

  :   Numeric matrix. (Optional) The covariance matrix of the data
      generating distribution used to define the Mahalanobis distance.
      `NULL` means it is estimated from `x_train`.

  `gaussian.mu`

  :   Numeric vector. (Optional) Containing the mean of the data
      generating distribution. `NULL` means it is estimated from the
      `x_train`.

  `gaussian.cov_mat`

  :   Numeric matrix. (Optional) Containing the covariance matrix of the
      data generating distribution. `NULL` means it is estimated from
      the `x_train`.

  `timeseries.fixed_sigma`

  :   Positive numeric scalar. Represents the kernel bandwidth in the
      distance computation. The default value is 2.

  `timeseries.bounds`

  :   Numeric vector of length two. Specifies the lower and upper bounds
      of the timeseries. The default is `c(NULL, NULL)`, i.e. no bounds.
      If one or both of these bounds are not `NULL`, we restrict the
      sampled time series to be between these bounds. This is useful if
      the underlying time series are scaled between 0 and 1, for
      example.

  `vaeac.depth`

  :   Positive integer (default is `3`). The number of hidden layers in
      the neural networks of the masked encoder, full encoder, and
      decoder.

  `vaeac.width`

  :   Positive integer (default is `32`). The number of neurons in each
      hidden layer in the neural networks of the masked encoder, full
      encoder, and decoder.

  `vaeac.latent_dim`

  :   Positive integer (default is `8`). The number of dimensions in the
      latent space.

  `vaeac.lr`

  :   Positive numeric (default is `0.001`). The learning rate used in
      the
      [`torch::optim_adam()`](https://torch.mlverse.org/docs/reference/optim_adam.html)
      optimizer.

  `vaeac.activation_function`

  :   An
      [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html)
      representing an activation function such as, e.g.,
      [`torch::nn_relu()`](https://torch.mlverse.org/docs/reference/nn_relu.html)
      (default),
      [`torch::nn_leaky_relu()`](https://torch.mlverse.org/docs/reference/nn_leaky_relu.html),
      [`torch::nn_selu()`](https://torch.mlverse.org/docs/reference/nn_selu.html),
      or
      [`torch::nn_sigmoid()`](https://torch.mlverse.org/docs/reference/nn_sigmoid.html).

  `vaeac.n_vaeacs_initialize`

  :   Positive integer (default is `4`). The number of different vaeac
      models to initiate in the start. Pick the best performing one
      after `vaeac.extra_parameters$epochs_initiation_phase` epochs
      (default is `2`) and continue training that one.

  `vaeac.epochs`

  :   Positive integer (default is `100`). The number of epochs to train
      the final vaeac model. This includes
      `vaeac.extra_parameters$epochs_initiation_phase`, where the
      default is `2`.

  `vaeac.extra_parameters`

  :   Named list with extra parameters to the `vaeac` approach. See
      [`vaeac_get_extra_para_default()`](https://norskregnesentral.github.io/shapr/reference/vaeac_get_extra_para_default.md)
      for description of possible additional parameters and their
      default values.

## Value

Object of class `c("shapr", "list")`. Contains the following items:

- `shapley_values_est`:

  data.table with the estimated Shapley values with explained
  observation in the rows and features along the columns. The column
  `none` is the prediction not devoted to any of the features (given by
  the argument `phi0`)

- `shapley_values_sd`:

  data.table with the standard deviation of the Shapley values
  reflecting the uncertainty in the coalition sampling part of the
  kernelSHAP procedure. These are, by definition, 0 when all coalitions
  are used. Only present when `extra_computation_args$compute_sd=TRUE`,
  which is the default when `iterative = TRUE`.

- `internal`:

  List with the different parameters, data, functions and other output
  used internally.

- `pred_explain`:

  Numeric vector with the predictions for the explained observations.

- `MSEv`:

  List with the values of the MSEv evaluation criterion for the
  approach. See the [MSEv evaluation section in the general usage
  vignette for
  details](https://norskregnesentral.github.io/shapr/articles/general_usage.html#msev-evaluation-criterion%0A).

- `timing`:

  List containing timing information for the different parts of the
  computation. `summary` contains the time stamps for the start and end
  time in addition to the total execution time. `overall_timing_secs`
  gives the time spent on different parts of the explanation
  computation. `main_computation_timing_secs` further decomposes the
  main computation time into different parts of the computation for each
  iteration of the iterative estimation routine, if used.

## Details

This function explains a forecast of length `horizon`. The argument
`train_idx` is analogous to x_train in
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md),
however, it just contains the time indices of where in the data the
forecast should start for each training sample. In the same way
`explain_idx` defines the time index (indices) which will precede a
forecast to be explained.

As any autoregressive forecast model will require a set of lags to make
a forecast at an arbitrary point in time, `explain_y_lags` and
`explain_xreg_lags` define how many lags are required to "refit" the
model at any given time index. This allows the different approaches to
work in the same way they do for time-invariant models.

See the [forecasting section of the general usage
vignette](https://norskregnesentral.github.io/shapr/articles/general_usage.html#forecasting)
for further details. See also the software paper [Jullum et al. (2025,
Sec. 6)](https://arxiv.org/pdf/2504.01842) for a more detailed
introduction to the methodology, and additional examples.

## References

- [Jullum, M., Olsen, L. H. B., Lachmann, J., & Redelmeier, A. (2025).
  shapr: Explaining Machine Learning Models with Conditional Shapley
  Values in R and Python. arXiv preprint
  arXiv:2504.01842.](https://arxiv.org/pdf/2504.01842)

- [Aas, K., Jullum, M., & Løland, A. (2021). Explaining individual
  predictions when features are dependent: More accurate approximations
  to Shapley values. Artificial Intelligence, 298,
  103502](https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf)

- [Frye, C., Rowat, C., & Feige, I. (2020). Asymmetric Shapley values:
  incorporating causal knowledge into model-agnostic explainability.
  Advances in neural information processing systems, 33,
  1229-1239](https://proceedings.neurips.cc/paper_files/paper/2020/file/0d770c496aa3da6d2c3f2bd19e7b9d6b-Paper.pdf)

- [Heskes, T., Sijben, E., Bucur, I. G., & Claassen, T. (2020). Causal
  shapley values: Exploiting causal knowledge to explain individual
  predictions of complex models. Advances in neural information
  processing systems, 33,
  4778-4789](https://proceedings.neurips.cc/paper/2020/file/32e54441e6382a7fbacbbbaf3c450059-Paper.pdf)

- [Jullum, M., Redelmeier, A. & Aas, K. (2021). Efficient and simple
  prediction explanations with groupShapley: A practical perspective.
  Italian Workshop on Explainable Artificial Intelligence
  2021.](https://martinjullum.com/publication/jullum-2021-efficient/jullum-2021-efficient.pdf)

- [Redelmeier, A., Jullum, M., & Aas, K. (2020). Explaining predictive
  models with mixed features using Shapley values and conditional
  inference trees. In Machine Learning and Knowledge Extraction:
  International Cross-Domain Conference, CD-MAKE 2020, Dublin, Ireland,
  August 25-28, 2020, Proceedings 4 (pp. 117-137). Springer
  International
  Publishing.](https://martinjullum.com/publication/redelmeier-2020-explaining/redelmeier-2020-explaining.pdf)

- [Sellereite N., & Jullum, M. (2019). shapr: An R-package for
  explaining machine learning models with dependence-aware Shapley
  values. Journal of Open Source Software, 5(46),
  2027](https://www.theoj.org/joss-papers/joss.02027/10.21105.joss.02027.pdf)

- [Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using
  Shapley values and variational autoencoders to explain predictive
  models with dependent mixed features. Journal of machine learning
  research, 23(213),
  1-51](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf)

- [Olsen, L. H. B., Glad, I. K., Jullum, M., & Aas, K. (2024). A
  comparative study of methods for estimating model-agnostic Shapley
  value explanations. Data Mining and Knowledge Discovery,
  1-48](https://link.springer.com/content/pdf/10.1007/s10618-024-01016-z.pdf)

- [Olsen, L. H. B., & Jullum, M. (2024). Improving the Sampling Strategy
  in KernelSHAP. arXiv e-prints,
  arXiv-2410](https://arxiv.org/pdf/2410.04883)

## Author

Jon Lachmann, Martin Jullum

## Examples

``` r
# \donttest{
# Load example data
data("airquality")
data <- data.table::as.data.table(airquality)

# Fit an AR(2) model.
model_ar_temp <- ar(data$Temp, order = 2)

# Calculate the zero prediction values for a three step forecast.
p0_ar <- rep(mean(data$Temp), 3)

# Empirical approach, explaining forecasts starting at T = 152 and T = 153.
explain_forecast(
  model = model_ar_temp,
  y = data[, "Temp"],
  train_idx = 2:151,
  explain_idx = 152:153,
  explain_y_lags = 2,
  horizon = 3,
  approach = "empirical",
  phi0 = p0_ar,
  group_lags = FALSE
)
#> 
#> ── Starting `shapr::explain_forecast()` at 2025-12-22 17:30:47 ─────────────────
#> ℹ Feature names extracted from the model contain `NA`.
#>   Consistency checks between model and data are therefore disabled.
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 4`, and is
#>   therefore set to `2^n_features = 4`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <ar>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: empirical
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 1000
#> • Number of feature-wise Shapley values: 2
#> • Number of observations to explain: 2
#> • Computations (temporary) saved at: /tmp/RtmpcgR2vK/shapr_obj_23451f40e31c.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 4 of 4 coalitions. 
#>    explain_idx horizon  none Temp.1 Temp.2
#>          <int>   <int> <num>  <num>  <num>
#> 1:         152       1  77.9 -0.397 -1.391
#> 2:         153       1  77.9 -6.618 -0.184
#> 3:         152       2  77.9 -0.329 -1.203
#> 4:         153       2  77.9 -6.021 -0.337
#> 5:         152       3  77.9 -0.291 -1.055
#> 6:         153       3  77.9 -5.212 -0.255
# }
```
