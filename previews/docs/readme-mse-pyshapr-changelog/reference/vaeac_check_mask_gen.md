# Function that checks the specified masking scheme

Function that checks the specified masking scheme

## Usage

``` r
vaeac_check_mask_gen(mask_gen_coalitions, mask_gen_coalitions_prob, x_train)
```

## Arguments

- mask_gen_coalitions:

  Matrix (default is `NULL`). Matrix containing the coalitions that the
  `vaeac` model will be trained on, see
  [`specified_masks_mask_generator()`](https://norskregnesentral.github.io/shapr/reference/specified_masks_mask_generator.md).
  This parameter is used internally in `shapr` when we only consider a
  subset of coalitions, i.e., when `n_coalitions` \\\<
  2^{n\_{\text{features}}}\\, and for group Shapley, i.e., when `group`
  is specified in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- mask_gen_coalitions_prob:

  Numeric array (default is `NULL`). Array of length equal to the height
  of `mask_gen_coalitions` containing the probabilities of sampling the
  corresponding coalitions in `mask_gen_coalitions`.

- x_train:

  A data.table containing the training data. Categorical data must have
  class names \\1,2,\dots,K\\.

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
