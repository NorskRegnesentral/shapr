# Generate predictions for input data with specified model

Performs prediction of response
[`stats::lm()`](https://rdrr.io/r/stats/lm.html),
[`stats::glm()`](https://rdrr.io/r/stats/glm.html),
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md),
[`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html),
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
(i.e., `tidymodels` models), and
[`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)/[`xgboost::xgboost()`](https://rdrr.io/pkg/xgboost/man/xgboost.html)
with binary or continuous response. For time series models
[`explain_forecast()`](https://norskregnesentral.github.io/shapr/reference/explain_forecast.md),
supports [`stats::ar()`](https://rdrr.io/r/stats/ar.html) and
[`forecast::Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html).
See details for more information.

## Usage

``` r
predict_model(x, newdata, ...)

# Default S3 method
predict_model(x, newdata, ...)

# S3 method for class 'ar'
predict_model(x, newdata, newreg, horizon, ...)

# S3 method for class 'Arima'
predict_model(
  x,
  newdata,
  newreg,
  horizon,
  explain_idx,
  explain_lags,
  y,
  xreg,
  ...
)

# S3 method for class 'forecast_ARIMA'
predict_model(x, newdata, newreg, horizon, ...)

# S3 method for class 'glm'
predict_model(x, newdata, ...)

# S3 method for class 'lm'
predict_model(x, newdata, ...)

# S3 method for class 'gam'
predict_model(x, newdata, ...)

# S3 method for class 'ranger'
predict_model(x, newdata, ...)

# S3 method for class 'workflow'
predict_model(x, newdata, ...)

# S3 method for class 'xgboost'
predict_model(x, newdata, ...)

# S3 method for class 'xgb.Booster'
predict_model(x, newdata, ...)
```

## Arguments

- x:

  Model object for the model to be explained.

- newdata:

  A data.frame/data.table with the features to predict from.

- ...:

  `newreg` and `horizon` parameters used in models passed to
  `[explain_forecast()]`

- horizon:

  Numeric. The forecast horizon to explain. Passed to the
  `predict_model` function.

- explain_idx:

  Numeric vector. The row indices in data and reg denoting points in
  time to explain.

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

## Value

Numeric. Vector of size equal to the number of rows in `newdata`.

## Details

The following models are currently supported:

- [`stats::lm()`](https://rdrr.io/r/stats/lm.html)

- [`stats::glm()`](https://rdrr.io/r/stats/glm.html)

- [`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md)

- [`mgcv::gam()`](https://rdrr.io/pkg/mgcv/man/gam.html)

- [`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)

- [`xgboost::xgb.train()`](https://rdrr.io/pkg/xgboost/man/xgb.train.html)

- [`xgboost::xgboost()`](https://rdrr.io/pkg/xgboost/man/xgboost.html)

- [`stats::ar()`](https://rdrr.io/r/stats/ar.html) (for
  [`explain_forecast()`](https://norskregnesentral.github.io/shapr/reference/explain_forecast.md))

- [`forecast::Arima()`](https://pkg.robjhyndman.com/forecast/reference/Arima.html)
  (for
  [`explain_forecast()`](https://norskregnesentral.github.io/shapr/reference/explain_forecast.md))

If you have a binary classification model we'll always return the
probability prediction for a single class.

If you are explaining a model not supported natively, you need to create
the `[predict_model()]` function yourself, and pass it on to as an
argument to `[explain()]`.

For more details on how to explain such non-supported models (i.e.
custom models), see the Advanced usage section of the general usage:  
From R:
[`vignette("general_usage", package = "shapr")`](https://norskregnesentral.github.io/shapr/articles/general_usage.md)  
Web:
<https://norskregnesentral.github.io/shapr/articles/general_usage.html#explain-custom-models>

## Author

Martin Jullum

## Examples

``` r
# Load example data
data("airquality")
airquality <- airquality[complete.cases(airquality), ]
# Split data into test- and training data
x_train <- head(airquality, -3)
x_explain <- tail(airquality, 3)
# Fit a linear model
model <- lm(Ozone ~ Solar.R + Wind + Temp + Month, data = x_train)

# Predicting for a model with a standardized format
predict_model(x = model, newdata = x_explain)
#>      151      152      153 
#> 17.75241 37.75649 15.67266 
```
