# Set up exogenous regressors for explanation in a forecast model.

Set up exogenous regressors for explanation in a forecast model.

## Usage

``` r
reg_forecast_setup(x, horizon, group)
```

## Arguments

- x:

  A matrix with the exogenous variables.

- horizon:

  Numeric. The forecast horizon to explain. Passed to the
  `predict_model` function.

- group:

  The list of endogenous groups, to append exogenous groups to.

## Value

A list containing

- fcast A matrix containing the exogenous observations needed for each
  observation.

- group The list group with the exogenous groups appended.
