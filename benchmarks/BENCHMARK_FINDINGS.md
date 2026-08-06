# Compute and memory benchmark findings

This note summarizes the complete curated benchmark set. The main questions are
what drives elapsed time and peak RAM, and which settings users should change
when either resource is constrained.

## Scope and interpretation

- The approach-specific grids contain 2,353 successful runs. Most configurations
  use three replicates; VAEAC and the expensive ARF/timeseries realistic blocks
  use two. Treat small timing differences as ties.
- All results come from one Linux host and synthetic numeric, mixed, or
  categorical data. Absolute seconds and RAM are machine-specific; relative
  effects are the useful result.
- Tables below use whole-process elapsed time (`bash_wall_secs`) unless they are
  explicitly labelled as `explain()` time. RAM is the median process-tree or
  cgroup peak, not merely the parent R process.
- All retained experiments are blocks in the ordinary approach-specific YAML
  files and are included by `bin/run_week.sh`.

## Main conclusions across approaches

1. **Total sampling work drives time.** More coalitions, Monte Carlo samples,
   explicands, and costly conditional/prediction methods all add work. The
   accuracy experiment confirms that coalition count and Monte Carlo count are
   different accuracy levers, so neither should be increased mechanically.
2. **The live batch and worker count drive RAM.** Splitting coalitions into more
   batches often reduces sequential peak RAM by several-fold. Parallel workers
   then multiply the live state: 16 workers routinely used several GB and the
   most demanding ARF/timeseries cases used tens of GB.
3. **Parallelism pays only when each batch contains enough work.** Four workers
   are a robust first step. Eight often improves throughput further. Sixteen is
   mainly a latency option for CTree, heavy empirical/ARF, and expensive model
   prediction; it is usually a poor default because memory grows faster than
   speed.
4. **More batches are not free.** They are an effective memory control for
   Gaussian, empirical, CTree, and ARF, but timeseries and VAEAC incur large
   per-batch overheads. For those approaches, overly fine batching can increase
   elapsed time by multiples.
5. **The default dense-array cap is conservative for good reason.** In the
   Gaussian calibration, `max_batch_cube_size = 1e6` was both lowest-memory and
   as fast as, or faster than, larger caps. Disabling the cap should be an
   informed latency-for-memory trade, not a general optimization.
6. **Real prediction cost changes the parallel optimum.** Four workers captured
   most of the benefit for linear and standard XGBoost models. For a real
   500-tree, depth-6 XGBoost model, `explain()` speedup rose to 2.79x with four
   workers and 3.80x with 16. Advice about workers therefore has to account for
   the model, not just the shapr approach and dimensions.

## 1. Iterative-pair integrity

The curated snapshot contains 36 dependent rows whose stored fixed budget does
not match the source's currently recorded achieved budget. There are six such
rows for each of ARF, copula, independence, regression separate, regression
surrogate, and timeseries. Gaussian, empirical, CTree, categorical, and VAEAC
pairs are valid.

These 36 rows remain valid standalone fixed-budget timings, and all ordinary
non-pair rows remain comparable. They are not valid iterative-versus-fixed
pairs. The aggregator now records `source_used_n_coalitions` and
`pair_budget_matches`, and excludes only mismatched dependents from newly
generated summaries. On resume, a dependent is re-used only when its override
still matches its source's recorded coalition count. This check intentionally
does not invalidate results because the Git SHA or installed package changed.

**Guidance:** paired comparisons require equal realized coalition budgets. For
all other comparisons, do not discard otherwise compatible runs merely because
metadata such as SHA differs.

## 2. Realistic parallel workloads

The following table selects representative heavy settings and compares equal
batch counts where possible. Times are median total elapsed seconds and RAM is
median MB.

| Approach / batches | 1 worker | 4 workers | 8 workers | 16 workers |
|---|---:|---:|---:|---:|
| Gaussian / 32 | 36.5 s, 592 MB | 17.3 s, 2,236 MB | 14.6 s, 4,116 MB | 13.6 s, 7,263 MB |
| Empirical / 8 | 79.6 s, 1,548 MB | 28.2 s, 5,133 MB | 20.2 s, 8,892 MB | 20.3 s, 9,391 MB |
| CTree / 32 | 254.3 s, 747 MB | 75.2 s, 2,860 MB | 46.9 s, 5,241 MB | 32.5 s, 9,989 MB |
| ARF / 32 | 365.9 s, 3,793 MB | 120.1 s, 12,981 MB | 77.7 s, 24,452 MB | 65.7 s, 44,733 MB |
| Timeseries / 8 | 253.5 s, 11,810 MB | 83.1 s, 47,148 MB | 53.5 s, 71,477 MB | 53.7 s, 72,050 MB |
| VAEAC, explanation-heavy / 16 | 294.6 s, 3,376 MB | 212.8 s, 10,488 MB | not run | 194.2 s, 17,475 MB |

Important approach-specific findings:

- **Gaussian:** four workers roughly halved elapsed time. Eight workers gave a
  smaller second gain; 16 offered little beyond eight. With one worker, moving
  from one to 32 batches cut heavy-case RAM from 8.6 GB to 0.6 GB and was also
  slightly faster than one batch.
- **Empirical:** parallelism remained worthwhile through eight workers. The
  medium workload saturated there, while the heavier workload still gained at
  16. A 32-batch parallel layout used substantially less RAM than eight batches,
  at a modest time cost in most cases.
- **CTree:** this was the clearest CPU-parallel case; the heavy 32-batch run
  improved from 254.3 seconds sequentially to 32.5 seconds at 16 workers. RAM
  rose from 0.7 GB to 10.0 GB.
- **ARF:** heavy runs parallelized strongly but retained large worker-local
  state. A practical compromise is four workers and 32 batches (120.1 seconds,
  13.0 GB); 16 workers saved another 54 seconds but peaked at 44.7 GB.
- **Timeseries:** coarse batches mattered at least as much as workers. The heavy
  sequential case took 100.8 seconds with two batches but 900.4 seconds with 32.
  Four workers/eight batches was faster but needed 47.1 GB. Eight workers
  matched 16 workers almost exactly in both time (53.5 versus 53.7 seconds) and
  RAM (71.5 versus 72.1 GB), so it did not reveal a safer intermediate point.
- **VAEAC:** training-dominated work had no useful parallel payoff. The best
  sequential setup took 409.9 seconds and 0.8 GB; 16 workers took 398.5 seconds
  and 7.0 GB. Explanation-heavy work gained modestly, but even there four
  workers should precede 16. Fine 64-batch sequential runs were slow even after
  replication: 738.7 seconds for the training-dominated workload and 544.2
  seconds for the explanation-heavy workload.

**User guidance:** start at one worker when RAM is uncertain, then try four.
Use eight only for a material workload and adequate RAM. Reserve 16 for measured
latency needs. Prefer 32 batches as a memory-oriented starting point for
Gaussian/empirical/CTree/ARF, but use coarse batching for timeseries and VAEAC.

## 3. Accuracy versus cost and parameter interactions

The Gaussian experiment used eight features, three replicates, and a reference
mean from three exact-coalition runs with 2,000 Monte Carlo samples. Reference
noise RMSE was 0.00634. Selected results for 50 explained observations are:

| Coalitions | MC samples | `explain()` time | Peak RAM | Shapley RMSE | Replicate instability RMSE |
|---:|---:|---:|---:|---:|---:|
| 32 | 25 | 1.36 s | 234 MB | 0.214 | 0.302 |
| 32 | 100 | 1.43 s | 247 MB | 0.081 | 0.140 |
| 32 | 400 | 1.73 s | 263 MB | 0.068 | 0.101 |
| 64 | 100 | 1.62 s | 259 MB | 0.054 | 0.084 |
| 128 | 100 | 2.14 s | 262 MB | 0.047 | 0.060 |
| 128 | 400 | 4.05 s | 309 MB | 0.023 | 0.033 |
| 256 | 100 | 1.70 s | 273 MB | 0.023 | 0.034 |
| 256 | 400 | 6.21 s | 359 MB | 0.014 | 0.017 |

The two levers interact. At only 32 coalitions, increasing MC samples from 100
to 400 had diminishing returns because coalition approximation remained. At
128 or 256 coalitions, the same MC increase reduced both error and replicate
variation much more effectively. Conversely, increasing coalitions with only 25
MC samples left substantial Monte Carlo noise.

With eight features, 256 is the exact set of coalitions and follows a different
execution path. Its unexpectedly low runtime relative to 128 coalitions is
therefore a path-specific discontinuity, not evidence that more coalitions are
generally cheaper.

**User guidance:** increase the visibly limiting dimension. If repeated runs
vary materially, raise `n_MC_samples`; if they are stable but biased relative to
a higher-budget check, raise `max_n_coalitions`. For this example, 64-128
coalitions and 100 MC samples form a sensible middle region; 256/400 is a
high-accuracy option at roughly four times the `explain()` time of 64/100.

## 4. Dense-batch memory budget

For the 12-feature, 1,024-coalition Gaussian workload:

| Cube-size cap | Actual batches | 1-worker elapsed / RAM | 4-worker elapsed / RAM |
|---:|---:|---:|---:|
| 1 million | 342 | 33.1 s / 367 MB | 16.5 s / 1,568 MB |
| 4 million | 79 | 38.6 s / 425 MB | 18.2 s / 2,057 MB |
| 16 million | 20 | 36.1 s / 748 MB | 18.5 s / 2,728 MB |
| 64 million | 8 | 34.3 s / 1,473 MB | 18.0 s / 5,260 MB |
| unlimited | 8 | 34.1 s / 1,462 MB | 17.8 s / 5,233 MB |

The 20-feature calibration showed the same conclusion: the 1-million cap used
128 batches and 343 MB, compared with about 706-710 MB at the largest settings,
without a speed penalty. The many small Gaussian batches did not exhibit the
severe overhead seen for timeseries or VAEAC.

**User guidance:** keep the 1-million default under an unknown memory budget.
Relax it only after measuring a representative job, and expect the memory
effect to be multiplied by parallel workers. `used_n_batches` and
`effective_max_batch_size` in the result make the cap's actual consequence
observable.

## 5. Prediction-cost sensitivity

### Real model complexity

The real-model study kept the Gaussian workload fixed and changed only its
pre-built prediction model. Training remained outside the measured run. The
models were a basic linear regression, the standard XGBoost configuration (50
trees, depth 3), and a large XGBoost configuration (500 trees, depth 6).

| Prediction model | 1 worker | 4 workers | 16 workers |
|---|---:|---:|---:|
| Linear | 8.28 s, 300 MB | 4.54 s (1.82x), 1,237 MB | 4.18 s (1.98x), 3,580 MB |
| XGBoost | 10.59 s, 369 MB | 6.36 s (1.66x), 1,756 MB | 5.75 s (1.84x), 5,269 MB |
| XGBoost large | 50.36 s, 373 MB | 18.03 s (2.79x), 1,742 MB | 13.24 s (3.80x), 5,336 MB |

Times are median `explain()` seconds. The linear model is a useful cheap lower
bound, but it does not make parallelism free: whole-process elapsed time fell
only from 9.39 seconds sequentially to 6.78 seconds with four workers, and did
not improve at 16. The standard XGBoost model likewise saturated at four
workers for practical purposes. Only the large real model retained a
meaningful 4-to-16-worker gain, at the cost of tripling parallel RAM.

**User guidance:** use one worker for small jobs or strict RAM limits; four is
the normal parallel ceiling for cheap/moderate prediction models. Consider more
workers only when representative measurements show that prediction is a large
fraction of the workload.

## Retained benchmark components

- **Essential:** the pair-integrity labels and resume check. They protect the
  interpretation of every iterative comparison and add no benchmark workload.
- **High-value:** accuracy/cost interactions and the memory-cap calibration.
  They directly turn two important user knobs into actionable guidance.
- **High-value:** the real linear/standard/large prediction-model study. It
  directly demonstrates when additional workers become worthwhile.
- **High-value for resource guidance:** Gaussian, CTree, ARF, timeseries, and
  VAEAC parallel studies. Together they show why a universal worker/batch rule
  would be unsafe.
- **Useful corroboration:** empirical parallelism strengthens the common pattern
  and is retained in the empirical approach grid.

## Small, bounded follow-ups worth considering

1. **Accuracy generalization:** repeat a reduced 2 x 2 coalition/MC grid for one
   nonlinear model and one mixed dataset. About 12 candidate runs plus two or
   three references would test whether the Gaussian conclusion generalizes.
2. **VAEAC amortization:** separately report one-time training/setup and
   explanation phases for two `n_explain` values. Existing timing fields may be
   enough; if not, a small four-point run would clarify when training dominates.
3. **Second-machine check:** run a compact Gaussian/CTree subset on a smaller
   RAM or core-count machine. This is the most useful validation of transfer,
   but is not necessary before presenting the current single-host results with
   an explicit limitation.
