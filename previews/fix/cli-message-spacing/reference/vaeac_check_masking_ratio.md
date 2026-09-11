# Function that checks that the masking ratio argument is valid

Function that checks that the masking ratio argument is valid

## Usage

``` r
vaeac_check_masking_ratio(masking_ratio, n_features)
```

## Arguments

- masking_ratio:

  Numeric (default is `0.5`). Probability of masking a feature in the
  [`mcar_mask_generator()`](https://norskregnesentral.github.io/shapr/reference/mcar_mask_generator.md)
  (MCAR = Missing Completely At Random). The MCAR masking scheme ensures
  that `vaeac` model can do arbitrary conditioning as all coalitions
  will be trained. `masking_ratio` will be overruled if
  `mask_gen_coalitions` is specified.

- n_features:

  The number of features, i.e., the number of columns in the training
  data.

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
