# Get feature specifications from the model

Get feature specifications from the model

## Usage

``` r
get_feature_specs(get_model_specs, model)
```

## Arguments

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

- model:

  Model object. The model whose predictions you want to explain. Run
  [`get_supported_models()`](https://norskregnesentral.github.io/shapr/reference/get_supported_models.md)
  for a table of which models `explain` supports natively. Unsupported
  models can still be explained by passing `predict_model` and
  (optionally) `get_model_specs`, see details for more information.
