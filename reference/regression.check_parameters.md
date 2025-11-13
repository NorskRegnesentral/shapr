# Check regression parameters

Check regression parameters

## Usage

``` r
regression.check_parameters(internal)
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

The same `internal` list, but added logical indicator
`internal$parameters$regression.tune` if we are to tune the regression
model/models.

## Author

Lars Henry Berge Olsen
