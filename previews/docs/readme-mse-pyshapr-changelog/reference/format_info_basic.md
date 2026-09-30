# Internal function to extract a vector with formatted info about the shapr call

To be used by both
[`cli_startup()`](https://norskregnesentral.github.io/shapr/reference/cli_startup.md)
and
[`summary.shapr()`](https://norskregnesentral.github.io/shapr/reference/summary.shapr.md)

## Usage

``` r
format_info_basic(internal)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.
