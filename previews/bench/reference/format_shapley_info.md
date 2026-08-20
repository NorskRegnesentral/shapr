# Internal function to extract the formatted Shapley value table

To be used by both
[`print_iter()`](https://norskregnesentral.github.io/shapr/reference/print_iter.md)
and
[`summary.shapr()`](https://norskregnesentral.github.io/shapr/reference/summary.shapr.md)

## Usage

``` r
format_shapley_info(internal, iter, digits = 2L)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

- iter:

  Integer. The iteration number. Only used internally.

- digits:

  Integer. (Maximum) number of digits to be displayed after the decimal
  point. Defaults to 2.
