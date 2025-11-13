# Set up the framework for the chosen approach

Different choices of `approach` take different (optional) parameters,
which are forwarded from
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).
See the [general usage
vignette](https://norskregnesentral.github.io/shapr/articles/general_usage.html)
for more information about the different approaches.

## Usage

``` r
setup_approach(internal, ...)

# S3 method for class 'combined'
setup_approach(internal, ...)

# S3 method for class 'categorical'
setup_approach(
  internal,
  categorical.joint_prob_dt = NULL,
  categorical.epsilon = 0.001,
  ...
)

# S3 method for class 'copula'
setup_approach(internal, ...)

# S3 method for class 'ctree'
setup_approach(
  internal,
  ctree.mincriterion = 0.95,
  ctree.minsplit = 20,
  ctree.minbucket = 7,
  ctree.sample = TRUE,
  ...
)

# S3 method for class 'empirical'
setup_approach(
  internal,
  empirical.type = "fixed_sigma",
  empirical.eta = 0.95,
  empirical.fixed_sigma = 0.1,
  empirical.n_samples_aicc = 1000,
  empirical.eval_max_aicc = 20,
  empirical.start_aicc = 0.1,
  empirical.cov_mat = NULL,
  model = NULL,
  predict_model = NULL,
  ...
)

# S3 method for class 'gaussian'
setup_approach(internal, gaussian.mu = NULL, gaussian.cov_mat = NULL, ...)

# S3 method for class 'independence'
setup_approach(internal, ...)

# S3 method for class 'regression_separate'
setup_approach(
  internal,
  regression.model = parsnip::linear_reg(),
  regression.tune_values = NULL,
  regression.vfold_cv_para = NULL,
  regression.recipe_func = NULL,
  ...
)

# S3 method for class 'regression_surrogate'
setup_approach(
  internal,
  regression.model = parsnip::linear_reg(),
  regression.tune_values = NULL,
  regression.vfold_cv_para = NULL,
  regression.recipe_func = NULL,
  regression.surrogate_n_comb =
    internal$iter_list[[length(internal$iter_list)]]$n_coalitions - 2,
  ...
)

# S3 method for class 'timeseries'
setup_approach(
  internal,
  timeseries.fixed_sigma = 2,
  timeseries.bounds = c(NULL, NULL),
  ...
)

# S3 method for class 'vaeac'
setup_approach(
  internal,
  vaeac.depth = 3,
  vaeac.width = 32,
  vaeac.latent_dim = 8,
  vaeac.activation_function = torch::nn_relu,
  vaeac.lr = 0.001,
  vaeac.n_vaeacs_initialize = 4,
  vaeac.epochs = 100,
  vaeac.extra_parameters = list(),
  ...
)
```

## Arguments

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- ...:

  Arguments passed to specific classes. See below.

- categorical.joint_prob_dt:

  Data.table. (Optional) Containing the joint probability distribution
  for each combination of feature values. `NULL` means it is estimated
  from the `x_train` and `x_explain`.

- categorical.epsilon:

  Numeric value. (Optional) If `categorical.joint_prob_dt` is not
  supplied, probabilities/frequencies are estimated using `x_train`. If
  certain observations occur in `x_explain` and NOT in `x_train`, then
  epsilon is used as the proportion of times that these observations
  occur in the training data. In theory, this proportion should be zero,
  but this causes an error later in the Shapley computation.

- ctree.mincriterion:

  Numeric scalar or vector. Either a scalar or vector of length equal to
  the number of features in the model. The value is equal to 1 -
  \\\alpha\\ where \\\alpha\\ is the nominal level of the conditional
  independence tests. If it is a vector, this indicates which value to
  use when conditioning on various numbers of features. The default
  value is 0.95.

- ctree.minsplit:

  Numeric scalar. Determines the minimum value that the sum of the left
  and right daughter nodes must reach for a split. The default value is
  20.

- ctree.minbucket:

  Numeric scalar. Determines the minimum sum of weights in a terminal
  node required for a split. The default value is 7.

- ctree.sample:

  Boolean. If `TRUE` (default), then the method always samples
  `n_MC_samples` observations from the leaf nodes (with replacement). If
  `FALSE` and the number of observations in the leaf node is less than
  `n_MC_samples`, the method will take all observations in the leaf. If
  `FALSE` and the number of observations in the leaf node is more than
  `n_MC_samples`, the method will sample `n_MC_samples` observations
  (with replacement). This means that there will always be sampling in
  the leaf unless `sample = FALSE` *and* the number of obs in the node
  is less than `n_MC_samples`.

- empirical.type:

  Character. (default = `"fixed_sigma"`) Must be one of
  `"independence"`, `"fixed_sigma"`, `"AICc_each_k"`, or `"AICc_full"`.
  Note: `"empirical.type = independence"` is deprecated; use
  `approach = "independence"` instead. `"fixed_sigma"` uses a fixed
  bandwidth (set through `empirical.fixed_sigma`) in the kernel density
  estimation. `"AICc_each_k"` and `"AICc_full"` optimize the bandwidth
  using the AICc criterion, with respectively one bandwidth per
  coalition size and one bandwidth for all coalition sizes.

- empirical.eta:

  Numeric scalar. Needs to be `0 < eta <= 1`. The default value is 0.95.
  Represents the minimum proportion of the total empirical weight that
  data samples should use. For example, if `eta = .8`, we choose the `K`
  samples with the largest weights so that the sum of the weights
  accounts for 80\\ `eta` is the \\\eta\\ parameter in equation (15) of
  [Aas et al.
  (2021)](https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf).

- empirical.fixed_sigma:

  Positive numeric scalar. The default value is 0.1. Represents the
  kernel bandwidth in the distance computation used when conditioning on
  all different coalitions. Only used when
  `empirical.type = "fixed_sigma"`

- empirical.n_samples_aicc:

  Positive integer. Number of samples to consider in AICc optimization.
  The default value is 1000. Only used when `empirical.type` is either
  `"AICc_each_k"` or `"AICc_full"`.

- empirical.eval_max_aicc:

  Positive integer. Maximum number of iterations when optimizing the
  AICc. The default value is 20. Only used when `empirical.type` is
  either `"AICc_each_k"` or `"AICc_full"`.

- empirical.start_aicc:

  Numeric. Start value of the `sigma` parameter when optimizing the
  AICc. The default value is 0.1. Only used when `empirical.type` is
  either `"AICc_each_k"` or `"AICc_full"`.

- empirical.cov_mat:

  Numeric matrix. (Optional) The covariance matrix of the data
  generating distribution used to define the Mahalanobis distance.
  `NULL` means it is estimated from `x_train`.

- model:

  Objects. The model object that ought to be explained. See the
  documentation of
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  for details.

- predict_model:

  Function. The prediction function used when `model` is not natively
  supported. See the documentation of
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  for details.

- gaussian.mu:

  Numeric vector. (Optional) Containing the mean of the data generating
  distribution. `NULL` means it is estimated from the `x_train`.

- gaussian.cov_mat:

  Numeric matrix. (Optional) Containing the covariance matrix of the
  data generating distribution. `NULL` means it is estimated from the
  `x_train`.

- regression.model:

  A `tidymodels` object of class `model_specs`. Default is a linear
  regression model, i.e.,
  [`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html).
  See [tidymodels](https://www.tidymodels.org/find/parsnip/) for all
  possible models, and see the vignette for how to add new/own models.
  Note, to make it easier to call
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  from Python, the `regression.model` parameter can also be a string
  specifying the model which will be parsed and evaluated. For example,
  `"parsnip::rand_forest(mtry = hardhat::tune(), trees = 100, engine = "ranger", mode = "regression")"`
  is also a valid input. It is essential to include the package prefix
  if the package is not loaded.

- regression.tune_values:

  Either `NULL` (default), a data.frame/data.table/tibble, or a
  function. The data.frame must contain the possible hyperparameter
  value combinations to try. The column names must match the names of
  the tunable parameters specified in `regression.model`. If
  `regression.tune_values` is a function, then it should take one
  argument `x` which is the training data for the current coalition and
  returns a data.frame/data.table/tibble with the properties described
  above. Using a function allows the hyperparameter values to change
  based on the size of the coalition See the regression vignette for
  several examples. Note, to make it easier to call
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  from Python, the `regression.tune_values` can also be a string
  containing an R function. For example,
  `"function(x) return(dials::grid_regular(dials::mtry(c(1, ncol(x)))), levels = 3))"`
  is also a valid input. It is essential to include the package prefix
  if the package is not loaded.

- regression.vfold_cv_para:

  Either `NULL` (default) or a named list containing the parameters to
  be sent to
  [`rsample::vfold_cv()`](https://rsample.tidymodels.org/reference/vfold_cv.html).
  See the regression vignette for several examples.

- regression.recipe_func:

  Either `NULL` (default) or a function that that takes in a
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
  object and returns a modified
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
  with potentially additional recipe steps. See the regression vignette
  for several examples. Note, to make it easier to call
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  from Python, the `regression.recipe_func` can also be a string
  containing an R function. For example,
  `"function(recipe) return(recipes::step_ns(recipe, recipes::all_numeric_predictors(), deg_free = 2))"`
  is also a valid input. It is essential to include the package prefix
  if the package is not loaded.

- regression.surrogate_n_comb:

  Positive integer. Specifies the number of unique coalitions to apply
  to each training observation. The default is the number of sampled
  coalitions in the present iteration. Any integer between 1 and the
  default is allowed. Larger values requires more memory, but may
  improve the surrogate model. If the user sets a value lower than the
  maximum, we sample this amount of unique coalitions separately for
  each training observations. That is, on average, all coalitions should
  be equally trained.

- timeseries.fixed_sigma:

  Positive numeric scalar. Represents the kernel bandwidth in the
  distance computation. The default value is 2.

- timeseries.bounds:

  Numeric vector of length two. Specifies the lower and upper bounds of
  the timeseries. The default is `c(NULL, NULL)`, i.e. no bounds. If one
  or both of these bounds are not `NULL`, we restrict the sampled time
  series to be between these bounds. This is useful if the underlying
  time series are scaled between 0 and 1, for example.

- vaeac.depth:

  Positive integer (default is `3`). The number of hidden layers in the
  neural networks of the masked encoder, full encoder, and decoder.

- vaeac.width:

  Positive integer (default is `32`). The number of neurons in each
  hidden layer in the neural networks of the masked encoder, full
  encoder, and decoder.

- vaeac.latent_dim:

  Positive integer (default is `8`). The number of dimensions in the
  latent space.

- vaeac.activation_function:

  An
  [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html)
  representing an activation function such as, e.g.,
  [`torch::nn_relu()`](https://torch.mlverse.org/docs/reference/nn_relu.html)
  (default),
  [`torch::nn_leaky_relu()`](https://torch.mlverse.org/docs/reference/nn_leaky_relu.html),
  [`torch::nn_selu()`](https://torch.mlverse.org/docs/reference/nn_selu.html),
  or
  [`torch::nn_sigmoid()`](https://torch.mlverse.org/docs/reference/nn_sigmoid.html).

- vaeac.lr:

  Positive numeric (default is `0.001`). The learning rate used in the
  [`torch::optim_adam()`](https://torch.mlverse.org/docs/reference/optim_adam.html)
  optimizer.

- vaeac.n_vaeacs_initialize:

  Positive integer (default is `4`). The number of different vaeac
  models to initiate in the start. Pick the best performing one after
  `vaeac.extra_parameters$epochs_initiation_phase` epochs (default is
  `2`) and continue training that one.

- vaeac.epochs:

  Positive integer (default is `100`). The number of epochs to train the
  final vaeac model. This includes
  `vaeac.extra_parameters$epochs_initiation_phase`, where the default is
  `2`.

- vaeac.extra_parameters:

  Named list with extra parameters to the `vaeac` approach. See
  [`vaeac_get_extra_para_default()`](https://norskregnesentral.github.io/shapr/reference/vaeac_get_extra_para_default.md)
  for description of possible additional parameters and their default
  values.

## Value

Updated internal object with the approach set up.

## References

- [Aas, K., Jullum, M., & Løland, A. (2021). Explaining individual
  predictions when features are dependent: More accurate approximations
  to Shapley values. Artificial Intelligence, 298,
  103502](https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf)

## Author

Martin Jullum

Lars Henry Berge Olsen
