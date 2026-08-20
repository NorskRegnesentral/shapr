# Unexported documentation helper function.

Unexported documentation helper function.

## Usage

``` r
default_doc_internal(
  internal,
  model,
  predict_model,
  x_explain,
  x_train,
  n_features,
  W_kernel,
  S,
  dt_vS,
  model_class,
  output_size,
  ...
)
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

- x_explain:

  Data.table with the features of the observation whose predictions
  ought to be explained (test data).

- x_train:

  Data.table with training data.

- n_features:

  Positive integer. The number of features.

- W_kernel:

  Numeric matrix. Contains all non-scaled weights between training and
  test observations for all coalitions. The dimension equals
  `n_train x m`.

- S:

  Integer matrix of dimension `n_coalitions x m`, where `n_coalitions`
  and `m` equals the total number of sampled/non-sampled coalitions and
  the total number of unique features, respectively. Note that
  `m = ncol(x_train)`.

- dt_vS:

  Data.table of dimension `n_coalitions` times `n_explain + 1`
  containing the contribution function estimates. The first column is
  assumed to be named `id_coalition` and containing the ids of the
  coalitions. The last row is assumed to be the full coalition, i.e., it
  contains the predicted responses for the observations which are to be
  explained.

- model_class:

  Character string. The class of the model object, e.g., "lm", "glm",
  "xgboost", etc. obtained by `class(model)[1]`.

- output_size:

  Scalar integer. Specifies the dimension of the output from the
  prediction model for every observation.

- ...:

  Further arguments passed to `approach`-specific functions.

## Value

The `internal` list. It holds all parameters, data, and computed objects
used within
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).
