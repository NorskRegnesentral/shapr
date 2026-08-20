# Check that all explicands has at least one valid MC sample in causal Shapley values

Check that all explicands has at least one valid MC sample in causal
Shapley values

## Usage

``` r
check_categorical_valid_MCsamp(dt, n_explain, n_MC_samples, joint_prob_dt)
```

## Arguments

- dt:

  Data.table containing the generated MC samples (and conditional
  values) after each sampling step

- n_MC_samples:

  Positive integer. For most approaches, it indicates the maximum number
  of samples to use in the Monte Carlo integration of every conditional
  expectation. For `approach="ctree"`, `n_MC_samples` corresponds to the
  number of samples from the leaf node (see an exception related to the
  `ctree.sample` argument in
  [`setup_approach.ctree()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)).
  For `approach="empirical"`, `n_MC_samples` is the \\K\\ parameter in
  equations (14-15) of [Aas et al.
  (2021)](https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf),
  i.e. the maximum number of observations (with largest weights) that is
  used, see also the `empirical.eta` argument
  [`setup_approach.empirical()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md).

## Details

For undocumented arguments, see
[`setup_approach.categorical()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md).

## Author

Lars Henry Berge Olsen
