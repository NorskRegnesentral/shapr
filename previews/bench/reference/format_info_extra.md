# Internal function to extract some extra formatted info about the shapr call

To be used in
[`summary.shapr()`](https://norskregnesentral.github.io/shapr/reference/summary.shapr.md)

## Usage

``` r
format_info_extra(internal)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.
