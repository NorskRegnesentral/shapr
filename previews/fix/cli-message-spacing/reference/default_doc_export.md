# Exported documentation helper function.

Exported documentation helper function.

## Usage

``` r
default_doc_export(internal, iter, index_features, digits)
```

## Arguments

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- iter:

  Integer. The iteration number. Only used internally.

- index_features:

  Positive integer vector. Specifies the id_coalition to apply to the
  present method. `NULL` means all coalitions. Only used internally.

- digits:

  Integer. (Maximum) number of digits to be displayed after the decimal
  point. Defaults to 2.
