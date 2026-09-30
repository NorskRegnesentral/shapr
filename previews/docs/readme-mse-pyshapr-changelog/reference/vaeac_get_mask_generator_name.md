# Function that determines which mask generator to use

Function that determines which mask generator to use

## Usage

``` r
vaeac_get_mask_generator_name(
  mask_gen_coalitions,
  mask_gen_coalitions_prob,
  masking_ratio,
  verbose
)
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

- masking_ratio:

  Numeric (default is `0.5`). Probability of masking a feature in the
  [`mcar_mask_generator()`](https://norskregnesentral.github.io/shapr/reference/mcar_mask_generator.md)
  (MCAR = Missing Completely At Random). The MCAR masking scheme ensures
  that `vaeac` model can do arbitrary conditioning as all coalitions
  will be trained. `masking_ratio` will be overruled if
  `mask_gen_coalitions` is specified.

- verbose:

  String vector or NULL. Controls verbosity (printout detail level) via
  one or more of `"basic"`, `"progress"`, `"convergence"`, `"shapley"`
  and `"vS_details"`. `"basic"` (default) displays basic information
  about the computation and messages about parameters/checks.
  `"progress"` displays where in the calculation process the function
  currently is. `"convergence"` displays how close the Shapley value
  estimates are to convergence (only when `iterative = TRUE`).
  `"shapley"` displays intermediate Shapley value estimates and standard
  deviations (only when `iterative = TRUE`), and the final estimates.
  `"vS_details"` displays information about the v(S) estimates, most
  relevant for
  `approach %in% c("regression_separate", "regression_surrogate", "vaeac")`.
  `NULL` means no printout. Any combination can be used, e.g.,
  `verbose = c("basic", "vS_details")`.

## Value

The function does not return anything.

## Author

Lars Henry Berge Olsen
