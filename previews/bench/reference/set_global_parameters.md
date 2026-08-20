# Set the global (SAGE) parameters in `internal`

Set the global (SAGE) parameters in `internal`

## Usage

``` r
set_global_parameters(internal)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

## Value

The (updated) `internal` list.

## Details

Reads the loss function from `extra_computation_args$global_loss_func`,
resolves the default loss function (logistic loss for binary responses,
mean squared error otherwise) when none is supplied, and stores both the
loss function and the baseline loss
`zero_loss = -loss_func(y_explain, phi0)` in `internal`.

## Author

Martin Jullum
