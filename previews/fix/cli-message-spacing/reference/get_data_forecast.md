# Set up data for explain_forecast

Set up data for explain_forecast

## Usage

``` r
get_data_forecast(
  y,
  xreg,
  train_idx,
  explain_idx,
  explain_y_lags,
  explain_xreg_lags,
  horizon
)
```

## Arguments

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

## Value

A list containing

- The data.frames x_train and x_explain which holds the lagged data
  examples.

- A numeric, n_endo denoting how many columns are endogenous in x_train
  and x_explain.

- A list, group with groupings of each variable to explain per variable
  and not per variable and lag.
