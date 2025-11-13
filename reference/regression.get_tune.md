# Get if model is to be tuned

That is, if the regression model contains hyperparameters we are to tune
using cross validation. See
[tidymodels](https://www.tidymodels.org/find/parsnip/#model-args) for
default model hyperparameters.

## Usage

``` r
regression.get_tune(regression.model, regression.tune_values, x_train)
```

## Arguments

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

- x_train:

  Data.table with training data.

## Value

A boolean variable indicating if the regression model is to be tuned.

## Author

Lars Henry Berge Olsen
