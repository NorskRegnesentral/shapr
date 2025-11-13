# Function to extend the explicands and apply all relevant masks/coalitions

Function to extend the explicands and apply all relevant
masks/coalitions

## Usage

``` r
vaeac_get_x_explain_extended(x_explain, S, index_features)
```

## Arguments

- x_explain:

  Matrix or data.frame/data.table. Features for which predictions should
  be explained.

- S:

  The `internal$objects$S` matrix containing the possible coalitions.

- index_features:

  Positive integer vector. Specifies the id_coalition to apply to the
  present method. `NULL` means all coalitions. Only used internally.

## Value

The extended version of `x_explain` where the masks from `S` with
indices `index_features` have been applied.

## Author

Lars Henry Berge Olsen
