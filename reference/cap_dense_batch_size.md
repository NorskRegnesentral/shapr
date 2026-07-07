# Cap the batch size to keep the dense per-batch sampling array within limits

Some approaches build a dense per-batch array (an `RcppArmadillo` cube)
whose number of elements grows with the number of coalitions in the
batch. For the `gaussian` and `copula` approaches this array has a total
of `n_MC_samples * n_explain * coalitions_per_batch * n_features`
elements, while the `empirical` approach builds a distance array with
`n_train * n_explain * coalitions_per_batch` elements. With many
features, explicands, training observations or coalitions, this can
exceed the 32-bit indexing limit of the underlying `RcppArmadillo`
arrays (failing with `Cube::init(): requested size is too large`) or
simply demand excessive memory. This helper reduces `max_batch_size`
(i.e. uses more batches) so that no single batch exceeds
`max_batch_cube_size` elements. Note that parallelization
(`workers > 1`) increases total memory because several batches are held
at once, but each individual array still fits, so the limit is enforced
per batch and is unaffected by the number of workers.

## Usage

``` r
cap_dense_batch_size(internal, per_coalition_size)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

- per_coalition_size:

  Numeric. The number of dense array elements contributed by a single
  coalition for the current approach (i.e. the per-batch array size
  divided by `coalitions_per_batch`).

## Value

The (possibly modified) `internal` list.
