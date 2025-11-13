# Get the Default Values for the Extra Computation Arguments

Get the Default Values for the Extra Computation Arguments

## Usage

``` r
get_extra_comp_args_default(
  internal,
  paired_shap_sampling = isFALSE(internal$parameters$asymmetric),
  semi_deterministic_sampling = FALSE,
  kernelSHAP_reweighting = "on_all_cond",
  compute_sd = isFALSE(internal$parameters$exact),
  n_boot_samps = 100,
  vS_batching_method = "future",
  max_batch_size = 10,
  min_n_batches = 10
)
```

## Arguments

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- paired_shap_sampling:

  Logical. If `TRUE` paired versions of all sampled coalitions are also
  included in the computation. That is, if there are 5 features and e.g.
  coalitions (1,3,5) are sampled, then also coalition (2,4) is used for
  computing the Shapley values. This is done to reduce the variance of
  the Shapley value estimates. `TRUE` is the default and is recommended
  for highest accuracy. For asymmetric, `FALSE` is the default and the
  only legal value.

- semi_deterministic_sampling:

  Logical. If `FALSE` (default), then we sample from all coalitions. If
  `TRUE`, the sampling of coalitions is semi-deterministic, i.e. the
  sampling is done in a way that ensures that coalitions that are
  expected to be sampled based on the number of coalitions are
  deterministically included such that we sample among fewer coalitions.
  This is done to reduce the variance of the Shapley value estimates,
  and corresponds to the PySHAP\* strategy in the paper [Olsen & Jullum
  (2024)](https://arxiv.org/pdf/2410.04883).

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
  (2024)](https://arxiv.org/pdf/2410.04883).

- compute_sd:

  Logical. Whether to estimate the standard deviations of the Shapley
  value estimates. This is TRUE whenever sampling based kernelSHAP is
  applied (either iteratively or with a fixed number of coalitions).

- n_boot_samps:

  Integer. The number of bootstrapped samples (i.e. samples with
  replacement) from the set of all coalitions used to estimate the
  standard deviations of the Shapley value estimates.

- vS_batching_method:

  String. The method used to perform batch computing of vS. `"future"`
  (default), utilizes
  [future.apply::future_apply](https://future.apply.futureverse.org/reference/future_apply.html)
  (via the
  [future::future](https://future.futureverse.org/reference/future.html)
  package), enabling parallelized computation and progress updates via
  [progressr::progressr](https://progressr.futureverse.org/reference/progressr.html).
  Alternatively, `"forloop"` can be used for straightforward sequential
  computation, which is mainly useful for package development and
  debugging purposes.

- max_batch_size:

  Integer. The maximum number of coalitions to estimate simultaneously
  within each iteration. A larger number requires more memory, but may
  have a slight computational advantage.

- min_n_batches:

  Integer. The minimum number of batches to split the computation into
  within each iteration. Larger numbers give more frequent progress
  updates. If parallelization is applied, this should be set no smaller
  than the number of parallel workers.

## Value

A list with the default values for the extra computation arguments.

## References

- [Olsen, L. H. B., & Jullum, M. (2024). Improving the Sampling Strategy
  in KernelSHAP. arXiv preprint
  arXiv:2410.04883.](https://arxiv.org/pdf/2410.04883)

## Author

Martin Jullum
