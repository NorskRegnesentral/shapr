# Model testing function

Model testing function

## Usage

``` r
test_predict_model(x_test, predict_model, model, internal)
```

## Arguments

- predict_model:

  Function. The prediction function used when `model` is not natively
  supported. See the documentation of
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  for details.

- model:

  Objects. The model object that ought to be explained. See the
  documentation of
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  for details.

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.
