# Internal function to extract formatted info about the (current) convergence state of the shapr call

To be used by both
[`print_iter()`](https://norskregnesentral.github.io/shapr/reference/print_iter.md)
and
[summary.shapr](https://norskregnesentral.github.io/shapr/reference/summary.shapr.md)

## Usage

``` r
format_convergence_info(internal, iter)
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
