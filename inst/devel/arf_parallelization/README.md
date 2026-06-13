# ARF Parallelization Study

This folder contains reproducible timing studies for the `approach = "arf"` integration in `shapr`.

Run from the repository root:

```sh
Rscript inst/devel/arf_parallelization/benchmark_arf_parallelization.R
```

Useful options:

```sh
Rscript inst/devel/arf_parallelization/benchmark_arf_parallelization.R \
  --workers=4 \
  --reps=3 \
  --n-train=600 \
  --n-explain=3 \
  --features=6 \
  --trees=30 \
  --max-iters=2 \
  --mc-samples=20 \
  --coalitions=20
```

The main script writes results to `inst/devel/arf_parallelization/results/`:

- `arf_parallel_benchmark_results.csv`: one row per repetition and scenario.
- `arf_parallel_benchmark_summary.csv`: summary by scenario.
- `arf_parallel_benchmark_summary.md`: human-readable summary.
- `arf_parallel_benchmark_config.txt`: `sessionInfo()` for reproducibility.

Additional scripts in this folder:

- `benchmark_arf_batch_sweep.R`: varies the number of shapr `v(S)` batches for the ARF-internal strategy.
- `benchmark_arf_future_forde_hybrid.R`: compares shapr `future` batching with and without parallel `forde()`.

When the scripts are run locally, they write generated result folders such as:

- `results/` and `results_larger/`: initial moderate-size comparisons of the natural parallelization layers.
- `results_worker_sweep/`: larger worker sweep for 2, 4, 8, 16, 24, and 32 workers.
- `results_batch_sweep/`: batch-count sweep for the best ARF-internal strategy.
- `results_future_forde_hybrid/`: targeted comparison of shapr `future` batching with and without parallel `forde()`.

These result folders are generated artifacts and do not need to be committed with the scripts.

## Parallel Layers

There are four relevant layers:

1. `arf::adversarial_rf()` training.
   - With `parallel = FALSE`, `arf` passes `num.threads = 1L` to `ranger`.
   - With `parallel = TRUE`, `arf` passes `num.threads = NULL` to `ranger`.
   - `ranger` then uses its own precedence: explicit `num.threads`, `R_RANGER_NUM_THREADS`, `options(ranger.num.threads)`, `options(Ncpus)`, then default.
   - Because `arf` already passes `num.threads`, passing `num.threads` through `...` causes a duplicate-argument error. Use `options(ranger.num.threads = n)` or `R_RANGER_NUM_THREADS` instead.

2. `arf::forde()` parameter extraction.
   - `parallel = TRUE` uses `foreach %dopar%` across trees.
   - It only uses multiple cores if a foreach backend is registered.

3. `arf::forge()` conditional sample generation.
   - `parallel = TRUE` uses `foreach %dopar%` across evidence chunks.
   - It only uses multiple cores if a foreach backend is registered.

4. `shapr` v(S) batching.
   - Controlled by `extra_computation_args$vS_batching_method`.
   - `"future"` parallelizes batches of coalitions through `future.apply::future_lapply()`.
   - `"forloop"` keeps v(S) batching sequential.

## Results Summary

The studies compare four parallelization layers: `ranger` threads in `arf::adversarial_rf()`, `foreach` work in
`arf::forde()`, `foreach` work in `arf::forge()`, and shapr's outer `future` batching over `v(S)` batches.

The saved local runs in `results/` and `results_larger/` found that parallelizing only ARF setup was fastest, with fully
sequential execution close behind. In those runs, ARF sampling and shapr future overhead dominated any benefit from
more aggressive parallelism.

For the larger workload (`n_explain = 50`, `arf.num_trees = 500`, `n_MC_samples = 500`, `max_n_coalitions = 100`), the
picture changed: ARF-internal parallelization was best. In `results_worker_sweep/`, `inner_arf_only` was fastest for
every worker budget, with the best result at 16 workers and 8 workers almost tied. This means parallelizing both
`forde()` and `forge()` through `foreach`, while keeping shapr future batching off.

The batch sweep in `results_batch_sweep/` showed that fewer shapr batches are much faster for this ARF-internal setup.
The best observed result was 24 workers with one batch, and 16 workers with one batch was close. This improves runtime
because shapr makes fewer calls to `prepare_data.arf()`/`arf::forge()`, giving ARF fewer and larger generation jobs.

The hybrid study in `results_future_forde_hybrid/` showed that parallelizing `forde()` is still useful when using shapr
`future` batching: `future_plus_forde` was much faster than pure `outer_future_only`. It was still slower than the
one-batch ARF-internal strategy because `forge()` remained sequential inside each future worker.

## Recommendation

The package defaults should favor composability with the usual shapr batching setup:

- parallelize ARF training and parameter extraction: `arf.parallel_train = TRUE`,
- keep ARF sample generation sequential by default: `arf.parallel_gen = FALSE`,
- let users keep the usual `future`-based shapr batching if desired.

This is the least surprising default because ARF training uses `ranger` threads, while ARF parameter extraction and
sample generation use `foreach`. In particular, `arf::forge()` parallelization can conflict with shapr's outer
`future` batching by creating nested parallel work.

This default also preserves the usual shapr workflow: users can keep standard batch sizes and `future` batching, while
still getting parallel ARF training/`forde()` when their `ranger` and `foreach` settings allow it.

For ARF-heavy explanations, the faster advanced setup was:

```r
doParallel::registerDoParallel(cores = 16)

explain(
   ...,
   approach = "arf",
   arf.parallel_train = TRUE,
   arf.parallel_gen = TRUE,
   extra_computation_args = list(
      vS_batching_method = "forloop",
      min_n_batches = 1,
      max_batch_size = Inf
   )
)
```

This gives ARF fewer, larger calls to `arf::forge()` and lets ARF's `foreach` layer do the inner parallel work. It can
be faster, but may use more memory for large `n_explain`, `n_MC_samples`, or coalition counts.

The benchmark script compares this recommendation with natural alternatives:

- all sequential,
- outer `future` v(S) batching only,
- inner `arf::forge()` foreach only,
- ARF setup parallel only,
- ARF setup plus `forge()` parallel, but no shapr outer future batching.

The script caps each scenario to the same worker budget where possible.
