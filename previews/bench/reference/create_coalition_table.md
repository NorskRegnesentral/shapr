# Define coalitions, and fetch additional information about each unique coalition

Define coalitions, and fetch additional information about each unique
coalition

## Usage

``` r
create_coalition_table(
  m,
  exact = TRUE,
  n_coalitions = 200,
  n_coal_each_size = choose(m, seq(m - 1)),
  weight_zero_m = 10^6,
  paired_shap_sampling = TRUE,
  prev_X = NULL,
  n_samps_scale = 10,
  coal_feature_list = as.list(seq_len(m)),
  approach0 = "gaussian",
  kernelSHAP_reweighting = "none",
  semi_deterministic_sampling = FALSE,
  dt_coal_samp_info = NULL,
  dt_valid_causal_coalitions = NULL
)
```

## Arguments

- m:

  Positive integer. Total number of features/groups.

- exact:

  Logical. If `TRUE` all `2^m` coalitions are generated, otherwise a
  subsample of the coalitions is used.

- n_coalitions:

  Positive integer. Note that if `exact = TRUE`, `n_coalitions` is
  ignored.

- n_coal_each_size:

  Vector of integers of length `m-1`. The number of valid coalitions of
  each coalition size 1, 2,..., m-1. For symmetric Shapley values, this
  is `choose(m, seq(m-1))` (default). While for asymmetric Shapley
  values, this is the number of valid coalitions of each size in the
  causal ordering. Used to correctly normalize the Shapley weights.

- weight_zero_m:

  Numeric. The value to use as a replacement for infinite coalition
  weights when doing numerical operations.

- paired_shap_sampling:

  Logical. Whether to do paired sampling of coalitions.

- prev_X:

  data.table. The X data.table from the previous iteration.

- n_samps_scale:

  Positive integer. Integer that scales the number of coalitions
  `n_coalitions` to sample as sampling is cheap, while checking for
  `n_coalitions` unique coalitions is expensive, thus we over sample the
  number of coalitions by a factor of `n_samps_scale` and determine when
  we have `n_coalitions` unique coalitions and only use the coalitions
  up to this point and throw away the remaining coalitions.

- coal_feature_list:

  List. A list mapping each coalition to the features it contains.

- approach0:

  Character vector. Contains the approach to be used for estimation of
  each coalition size. Same as `approach` in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- kernelSHAP_reweighting:

  String. How to reweight the sampling frequency weights in the
  kernelSHAP solution after sampling. The aim of this is to reduce the
  randomness and thereby the variance of the Shapley value estimates.
  The options are one of `'none'`, `'on_N'`, `'on_all'`, `'on_all_cond'`
  (default). `'none'` means no reweighting, i.e. the sampling frequency
  weights are used as is. `'on_N'` means the sampling frequencies are
  averaged over all coalitions with the same original sampling
  probabilities. `'on_all'` means the original sampling probabilities
  are used for all coalitions. `'on_all_cond'` means the original
  sampling probabilities are used for all coalitions, while adjusting
  for the probability that they are sampled at least once.
  `'on_all_cond'` is preferred as it performs the best in simulation
  studies, see [Olsen & Jullum
  (2025)](https://link.springer.com/content/pdf/10.1007/978-3-032-08324-1_9.pdf).

- semi_deterministic_sampling:

  Logical. If `FALSE` (default), then we sample from all coalitions. If
  `TRUE`, the sampling of coalitions is semi-deterministic, i.e. the
  sampling is done in a way that ensures that coalitions that are
  expected to be sampled based on the number of coalitions are
  deterministically included such that we sample among fewer coalitions.
  This is done to reduce the variance of the Shapley value estimates,
  and corresponds to the PySHAP\* strategy in the paper [Olsen & Jullum
  (2025)](https://link.springer.com/content/pdf/10.1007/978-3-032-08324-1_9.pdf).

- dt_coal_samp_info:

  data.table. The data.table contains information about which coalitions
  should be deterministically included and which can be sampled, in
  addition to the sampling probabilities of each available coalition
  size, and the weight given to the sampled and deterministically
  included coalitions (excluding empty and grand coalitions which are
  given the `weight_zero_m` weight).

- dt_valid_causal_coalitions:

  data.table. Only applicable for asymmetric Shapley value explanations,
  and is `NULL` for symmetric Shapley values. The data.table contains
  information about the coalitions that respects the causal ordering.

## Value

A data.table with info about the coalitions to use

## Author

Nikolai Sellereite, Martin Jullum, Lars Henry Berge Olsen
