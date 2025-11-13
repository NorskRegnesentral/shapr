# Get the predicted responses

Get the predicted responses

## Usage

``` r
regression.get_y_hat(internal, model, predict_model)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

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

## Value

The same `internal` list, but added vectors
`internal$data$x_train_y_hat` and `internal$data$x_explain_y_hat`
containing the predicted response of the training and explain data.

## Author

Lars Henry Berge Olsen
