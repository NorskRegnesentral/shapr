# Fetches feature information from natively supported models

This function is used to extract the feature information from the model
to be checked against the corresponding feature information in the data
passed to
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

NOTE: You should never need to call this function explicitly. It is
exported just to be easier accessible for users, see details.

## Usage

``` r
get_model_specs(x)

# Default S3 method
get_model_specs(x)

# S3 method for class 'ar'
get_model_specs(x)

# S3 method for class 'Arima'
get_model_specs(x)

# S3 method for class 'fc_model'
get_model_specs(x)

# S3 method for class 'glm'
get_model_specs(x)

# S3 method for class 'lm'
get_model_specs(x)

# S3 method for class 'gam'
get_model_specs(x)

# S3 method for class 'ranger'
get_model_specs(x)

# S3 method for class 'workflow'
get_model_specs(x)

# S3 method for class 'xgboost'
get_model_specs(x)

# S3 method for class 'xgb.Booster'
get_model_specs(x)
```

## Arguments

- x:

  Model object for the model to be explained.

## Value

A list with the following elements:

- labels:

  character vector with the feature names to compute Shapley values for

- classes:

  a named character vector with the labels as names and the class type
  as elements

- factor_levels:

  a named list with the labels as names and character vectors with the
  factor levels as elements (NULL if the feature is not a factor)

## Details

If you are explaining a model not supported natively, you may
(optionally) enable such checking by creating this function yourself and
passing it on to
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

## See also

For model classes not supported natively, you NEED to create an analogue
to
[`predict_model()`](https://norskregnesentral.github.io/shapr/reference/predict_model.md).
See it's help file for details.

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
get_model_specs(model)
#> $labels
#> [1] "Solar.R" "Wind"    "Temp"    "Month"  
#> 
#> $classes
#>   Solar.R      Wind      Temp     Month 
#> "numeric" "numeric" "numeric" "numeric" 
#> 
#> $factor_levels
#> $factor_levels$Solar.R
#> NULL
#> 
#> $factor_levels$Wind
#> NULL
#> 
#> $factor_levels$Temp
#> NULL
#> 
#> $factor_levels$Month
#> NULL
#> 
#> 
```
