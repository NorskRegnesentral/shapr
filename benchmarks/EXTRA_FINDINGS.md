# Follow-up benchmark findings

This note records the optional follow-up experiments in the `extra_*` configs.
It complements the core benchmark snapshot; it does not replace or re-run it.
The main questions are what drives elapsed time and peak RAM, and which settings
users should change when either resource is constrained.

## Scope and interpretation

- The follow-ups planned 343 runs. Of these, 341 completed normally, one skipped
  a duplicate known to exceed the resource limit, and one was killed by that
  limit.
- Parallel and memory studies use two replicates per point. Accuracy uses three.
  Treat small timing differences as ties; the large effects are consistent
  enough to support operational guidance.
- All results come from one Linux host and synthetic numeric data. Absolute
  seconds and RAM are machine-specific; relative effects are the useful result.
- Tables below use whole-process elapsed time (`bash_wall_secs`) unless they are
  explicitly labelled as `explain()` time. RAM is the median process-tree or
  cgroup peak, not merely the parent R process.
- Every extension is removable as one `config/extra_*.yml` file plus its matching
  `results/extra_*` directory. None is called by `bin/run_week.sh`.

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
   most of the benefit for linear and small/current XGBoost models. For a real
   500-tree, depth-6 XGBoost model, `explain()` speedup rose to 2.76x with four
   workers and 3.28x with 16. Advice about workers therefore has to account for
   the model, not just the shapr approach and dimensions.
7. **Controlled prediction repeats confirm the mechanism.** Repeating otherwise
   identical model predictions increased 16-worker `explain()` speedup from
   1.83x to 5.63x, independently of changes to the prediction values.

## 1. Iterative-pair integrity

The historical core snapshot contains 72 dependent rows whose stored fixed
budget is 4,096 coalitions while the source currently records another achieved
budget. There are eight such rows for each of ARF, copula, CTree, empirical,
Gaussian, independence, regression separate, regression surrogate, and
timeseries. Categorical and VAEAC pairs are valid.

These 72 rows remain valid standalone fixed-budget timings, and all ordinary
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
| Gaussian / 32 | 36.7 s, 591 MB | 17.3 s, 2,233 MB | 14.8 s, 4,098 MB | 13.7 s, 7,262 MB |
| Empirical / 8 | 79.2 s, 1,577 MB | 28.3 s, 5,227 MB | 20.2 s, 8,893 MB | 20.3 s, 9,397 MB |
| CTree / 32 | 252.6 s, 796 MB | 74.9 s, 2,868 MB | 46.7 s, 5,212 MB | 32.4 s, 9,966 MB |
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
  improved from 252.6 seconds sequentially to 32.4 seconds at 16 workers. RAM
  rose from 0.8 GB to 10.0 GB.
- **ARF:** heavy runs parallelized strongly but retained large worker-local
  state. A practical compromise is four workers and 32 batches (120.1 seconds,
  13.0 GB); 16 workers saved another 54 seconds but peaked at 44.7 GB.
- **Timeseries:** coarse batches mattered at least as much as workers. The heavy
  sequential case took 100.8 seconds with two batches but 900.4 seconds with 32.
  Four workers/eight batches was faster but needed 47.1 GB. Eight workers
  matched 16 workers almost exactly in both time (53.5 versus 53.7 seconds) and
  RAM (71.5 versus 72.1 GB), so it did not reveal a safer intermediate point. A
  16-worker, 32-batch attempt reached 132.8 GB (123.7 GiB) and was
  resource-killed; its duplicate was not launched.
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
| 1 million | 342 | 33.2 s / 372 MB | 16.6 s / 1,569 MB |
| 4 million | 79 | 38.4 s / 425 MB | 18.2 s / 2,079 MB |
| 16 million | 20 | 36.2 s / 752 MB | 18.5 s / 2,710 MB |
| 64 million | 8 | 34.5 s / 1,477 MB | 18.1 s / 5,086 MB |
| unlimited | 8 | 34.1 s / 1,466 MB | 18.2 s / 4,975 MB |

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
models were a basic linear regression and XGBoost with small (10 trees, depth
2), current (50 trees, depth 3), and large (500 trees, depth 6) configurations.

| Prediction model | 1 worker | 4 workers | 16 workers |
|---|---:|---:|---:|
| Linear | 8.26 s, 300 MB | 4.59 s (1.80x), 1,242 MB | 4.28 s (1.93x), 3,717 MB |
| XGBoost small | 9.45 s, 369 MB | 5.88 s (1.61x), 1,682 MB | 5.62 s (1.68x), 5,270 MB |
| XGBoost current | 10.52 s, 370 MB | 6.37 s (1.65x), 1,763 MB | 5.76 s (1.83x), 5,271 MB |
| XGBoost large | 50.02 s, 374 MB | 18.15 s (2.76x), 1,743 MB | 15.27 s (3.28x), 5,337 MB |

Times are median `explain()` seconds. The linear model is a useful cheap lower
bound, but it does not make parallelism free: whole-process elapsed time fell
only from 9.40 seconds sequentially to 6.83 seconds with four workers, and did
not improve at 16. The small and current XGBoost models likewise saturated at
four workers for practical purposes. Only the large real model retained a
meaningful 4-to-16-worker gain, at the cost of tripling parallel RAM.

**User guidance:** use one worker for small jobs or strict RAM limits; four is
the normal parallel ceiling for cheap/moderate prediction models. Consider more
workers only when representative measurements show that prediction is a large
fraction of the workload.

### Controlled repeated predictions

The benchmark-only wrapper repeated the native model prediction 1, 4, or 16
times while returning identical predictions. This isolates model evaluation
cost without changing the explanation.

| Prediction work | 1 worker | 4 workers | 16 workers |
|---:|---:|---:|---:|
| 1x | 10.52 s | 6.37 s (1.65x) | 5.76 s (1.83x) |
| 4x | 18.05 s | 8.45 s (2.14x) | 6.17 s (2.92x) |
| 16x | 52.66 s | 17.09 s (3.08x) | 9.36 s (5.63x) |

These are `explain()` times so fixed R startup does not dilute the causal
effect. Peak RAM was nearly unchanged by repeated prediction work at a fixed
worker count: roughly 0.35 GB, 1.6-1.8 GB, and 5.3-5.5 GB for one, four, and 16
workers respectively.

This experiment supports the mechanism independently of model-output changes,
but is artificial. It can live in an appendix or be dropped from a lean public
benchmark set without weakening the directly observed real-model results.

## What to retain in a presented benchmark set

- **Essential:** the pair-integrity labels and resume check. They protect the
  interpretation of every iterative comparison and add no benchmark workload.
- **High-value:** accuracy/cost interactions and the memory-cap calibration.
  They directly turn two important user knobs into actionable guidance.
- **High-value:** the real linear/small/current/large prediction-model study. It
  directly demonstrates when additional workers become worthwhile.
- **High-value for resource guidance:** Gaussian, CTree, ARF, timeseries, and
  VAEAC parallel studies. Together they show why a universal worker/batch rule
  would be unsafe.
- **Useful corroboration:** empirical parallelism. It strengthens the common
  pattern but could be omitted if the presentation needs fewer approach panels.
- **Mechanistic/optional:** prediction-cost repeats. Keep it to explain why
  users should benchmark their model; omit it if only real workloads should be
  shown.

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
