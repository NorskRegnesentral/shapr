# Train a Tidymodels Model via Workflows

Function that trains a `tidymodels` model via `workflows` based on the
provided input parameters. This function allows for cross validating the
hyperparameters of the model.

## Usage

``` r
regression.train_model(
  x,
  seed = 1,
  verbose = NULL,
  regression.model = parsnip::linear_reg(),
  regression.tune = FALSE,
  regression.tune_values = NULL,
  regression.vfold_cv_para = NULL,
  regression.recipe_func = NULL,
  regression.response_var = "y_hat",
  regression.surrogate_n_comb = NULL,
  current_comb = NULL
)
```

## Arguments

- x:

  Data.table containing the training data.

- seed:

  Positive integer. Specifies the seed before any code involving
  randomness is run. If `NULL` (default), no seed is set in the calling
  environment.

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

- regression.tune:

  Logical (default is `FALSE`). If `TRUE`, then we are to tune the
  hyperparemeters based on the values provided in
  `regression.tune_values`. Note that no checks are conducted as this is
  checked earlier in `setup_approach.regression_separate` and
  `setup_approach.regression_surrogate`.

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

- regression.response_var:

  String (default is `y_hat`) containing the name of the response
  variable.

- regression.surrogate_n_comb:

  Integer (default is `NULL`). The number of times each training
  observations has been augmented. If `NULL`, then we assume that we are
  doing separate regression.

- current_comb:

  Integer vector. The current combination of features, passed to
  verbosity printing function.

## Value

A trained `tidymodels` model based on the provided input parameters.

## Author

Lars Henry Berge Olsen
