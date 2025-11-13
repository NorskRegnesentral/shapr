# Check that the type of model is supported by the native implementation of the model class

The function checks whether the model given by `x` is supported. If `x`
is not a supported model the function will return an error message,
otherwise it return NULL (meaning all types of models with this class is
supported)

## Usage

``` r
model_checker(x)

# Default S3 method
model_checker(x)

# S3 method for class 'ar'
model_checker(x)

# S3 method for class 'Arima'
model_checker(x)

# S3 method for class 'forecast_ARIMA'
model_checker(x)

# S3 method for class 'glm'
model_checker(x)

# S3 method for class 'lm'
model_checker(x)

# S3 method for class 'gam'
model_checker(x)

# S3 method for class 'ranger'
model_checker(x)

# S3 method for class 'workflow'
model_checker(x)

# S3 method for class 'xgb.Booster'
model_checker(x)
```

## Arguments

- x:

  Model object for the model to be explained.

## Value

Error or NULL

## See also

See
[`predict_model()`](https://norskregnesentral.github.io/shapr/reference/predict_model.md)
for more information about what type of models `shapr` currently
support.
