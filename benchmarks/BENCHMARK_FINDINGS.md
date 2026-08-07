# Compute and memory benchmark findings

This report summarizes only the experiments retained in the approach-specific
configuration files and their committed result snapshot. Its purpose is to
explain what drives elapsed time and peak RAM, and to turn those observations
into practical guidance for users.

## Executive summary

- There is no single fastest or most memory-efficient setting across all
  approaches. Approach internals and the prediction model often matter more
  than any one generic shapr parameter.
- Coalition count and the number of explained observations are the most
  consistent generic time drivers. Training-row count matters little for some
  approaches, but strongly affects methods that fit, search, or store richer
  conditional models.
- Monte Carlo sample count is important when conditional sampling or prediction
  is expensive, but it can be secondary to training, fitting, or data-search
  costs. It should be tuned for accuracy rather than assumed to be the main
  runtime control.
- Batching controls how much intermediate state is live at once. More batches
  can reduce sequential RAM substantially, but each batch adds overhead.
  Gaussian, CTree, and ARF tolerate fine batching well; timeseries and VAEAC do
  not.
- Parallel workers normally multiply live state. They are worthwhile only when
  individual batches contain enough computation. Cheap approaches should
  normally use one worker. Four workers are a sensible first parallel trial for
  material workloads; eight or sixteen should be selected only from a measured
  time-versus-RAM trade-off.
- Prediction cost changes that trade-off. A large XGBoost model continued to
  benefit beyond four workers, while linear and standard XGBoost models were
  already close to saturation.
- The separate Gaussian accuracy study shows that coalition count and Monte
  Carlo count address different error sources. Increasing either in isolation
  can have diminishing returns.

## Scope, completeness, and interpretation

This report covers two distinct bodies of work. The **cost studies** measure
elapsed time and peak RAM across eleven approaches, and supply every finding
below except one. The **accuracy study** ([`config/accuracy.yml`](config/accuracy.yml))
measures error instead of cost, is currently Gaussian-only, and is reported
separately in [Accuracy versus cost](#accuracy-versus-cost). Its runs are not
part of the cost-snapshot counts.

The curated cost snapshot contains 2,278 successful runs representing 789
distinct configurations. There are 700 configurations with three replicates and
89 with two. Two replicates are used only for VAEAC and the expensive realistic
ARF and timeseries blocks; all other configurations use the common
three-replicate default. The accuracy study adds a further 75 successful runs
(72 candidates and 3 references) at three replicates. Warm-up runs are not part
of the functionality, configuration, or results.

All iterative source/dependent pairs are valid: the fixed dependent uses the
coalition budget achieved by its corresponding iterative source. This was
verified across every retained result after refreshing the previously invalid
dependents.

The measurements were collected on one Linux host using synthetic numeric,
mixed, and categorical datasets. Absolute time and RAM will change with
hardware, operating system, backend, model implementation, and data. Relative
effects within a controlled block are consequently more transferable than the
absolute values.

Unless explicitly labelled as `explain()` time, elapsed time means median
whole-process wall time. Peak RAM is the median peak for the measured process
tree or cgroup, rather than only the parent R process. Small timing differences
should be treated as ties: the median relative timing IQR is 0.73% for
three-replicate configurations and 0.33% for two-replicate configurations; the
90th percentiles are 1.96% and 1.33%, respectively. The apparently lower
variation for two replicates is not evidence that two are more precise—their
tails are simply estimated less robustly.

## Comparable reference workload

This reference holds the generic numeric workload at 12 features, 128 maximum
coalitions, 250 Monte Carlo samples, 10 batches, 1 worker, 1,000 training rows,
and 25 explained rows. The categorical approach uses its corresponding
categorical dataset. Approach defaults otherwise remain in force, so this is a
practical cost comparison, not a claim that the methods are statistically
equivalent.

| Approach | Whole process | `explain()` | Peak RAM |
|---|---:|---:|---:|
| Gaussian | 4.123 s | 2.302 s | 273.1 MB |
| Independence | 4.967 s | 3.184 s | 258.8 MB |
| Copula | 4.967 s | 3.168 s | 283.3 MB |
| Categorical | 4.979 s | 4.038 s | 278.2 MB |
| Regression surrogate | 6.084 s | 3.311 s | 462.1 MB |
| Empirical | 7.217 s | 5.417 s | 246.1 MB |
| Regression separate | 8.188 s | 5.412 s | 308.6 MB |
| CTree | 19.188 s | 17.337 s | 357.3 MB |
| ARF | 19.232 s | 17.127 s | 775.2 MB |
| Timeseries | 60.856 s | 58.983 s | 1,301.2 MB |
| VAEAC | 1,163.646 s | 1,159.353 s | 542.7 MB |

The ordering is workload-specific. In particular, VAEAC includes its neural
training cost, and the reference does not amortize a trained model over repeated
explanation calls.

## What drives computation and memory across approaches

### Problem dimensions

More coalitions consistently add work and commonly add RAM. More explained
observations also increase work, although the effect is weak when a large
one-time fit dominates. Feature count affects both the possible coalition space
and approach-specific internal representations, so it becomes especially
important for CTree, ARF, timeseries, and grouped Gaussian runs.

Training-row count separates the approaches most clearly:

- It has little effect for Gaussian and independence at the tested Monte Carlo
  budget.
- It has a measurable effect for empirical, copula, categorical, ARF,
  timeseries, VAEAC, and regression surrogate.
- It is especially consequential when the method fits or retains complex
  conditional structures, as in CTree, ARF, timeseries, and VAEAC.
- Regression separate is comparatively insensitive to generic Monte Carlo and
  training-size changes, but is highly sensitive to its model-fitting variant.

Monte Carlo count should not be interpreted in isolation. In timeseries, for
example, training and batch overhead can dominate it; in Gaussian and ARF it is
more visible. Its most defensible role is an accuracy/stability control, tested
together with coalition count.

### Data and method complexity

The number of factor levels and mixed-data structure can materially increase
CTree, ARF, and VAEAC costs. Regression-separate tuning and learner choice can
dominate all ordinary dimension changes. These results support exposing
approach choice, approach arguments, and data structure in any estimate of
resource needs; rows and columns alone are insufficient.

### Batches and workers

Batching trades live memory against repeated setup and scheduling. Parallelism
trades elapsed time against multiple worker-local copies. Representative heavy
workloads illustrate the scale of that trade:

| Approach / batches | 1 worker | 4 workers | 8 workers | 16 workers |
|---|---:|---:|---:|---:|
| Gaussian / 32 | 36.5 s, 592 MB | 17.3 s, 2,236 MB | 14.6 s, 4,116 MB | 13.6 s, 7,263 MB |
| Empirical / 8 | 79.6 s, 1,548 MB | 28.2 s, 5,133 MB | 20.2 s, 8,892 MB | 20.3 s, 9,391 MB |
| CTree / 32 | 254.3 s, 747 MB | 75.2 s, 2,860 MB | 46.9 s, 5,241 MB | 32.5 s, 9,989 MB |
| ARF / 32 | 365.9 s, 3,793 MB | 120.1 s, 12,981 MB | 77.7 s, 24,452 MB | 65.7 s, 44,733 MB |
| Timeseries / 8 | 253.5 s, 11,810 MB | 83.1 s, 47,148 MB | 53.5 s, 71,477 MB | 53.7 s, 72,050 MB |
| VAEAC explanation-heavy / 16 | 294.6 s, 3,376 MB | 212.8 s, 10,488 MB | not run | 194.2 s, 17,475 MB |

These are deliberately heavy cases where parallel work has a chance to pay
off. They must not be generalized to cheap work. The retained core parallel
blocks show that independence, copula, categorical, and regression separate
do not benefit at their tested sizes. Regression surrogate is the clearest
counterexample: adding workers made it progressively slower while multiplying
RAM. For those cases, one worker is the correct default.

## Findings by approach

### Gaussian

Gaussian is inexpensive at the reference workload. Coalition count, explained
rows, and Monte Carlo samples increase cost, while training rows have little
effect in the tested range. Fine batching controls memory effectively and has
low overhead. Feature grouping helps only when it reduces the effective problem
substantially; small groups can add organization without reducing work.

Heavy Gaussian work benefits from four workers, with diminishing time gains at
eight and sixteen. The Linux multicore backend was substantially lighter than
multisession in the tested small parallel block, but this is platform- and
backend-specific and should not be presented as portable behavior.

### Empirical

Empirical cost grows with training rows, coalitions, and explained rows. Very
fine batching lowers RAM but can add substantial repeated-search overhead.
Parallelism is worthwhile for the retained heavy workloads through four or
eight workers, but the benefit saturates.

The empirical bandwidth-selection mode is a first-order cost decision: repeated
AICc selection is orders of magnitude more expensive than fixed bandwidth in
the tested block. Users should choose it for statistical reasons and budget for
it explicitly.

### CTree

CTree responds strongly to feature count, coalitions, explained rows, training
rows, and mixed/factor complexity. Batching reduces peak memory with little
penalty in the tested ordinary range. It also has the strongest sustained CPU
parallel scaling among the retained approaches, although RAM rises nearly with
the worker-local state.

### ARF

ARF time and RAM grow strongly with the main problem dimensions and with complex
mixed data. More batches are an effective memory control, with a modest time
cost. Heavy workloads parallelize well, but their worker-local model and sample
state make the RAM price unusually large. Four workers are a practical first
trial; higher counts require an explicit memory budget.

### Timeseries

Timeseries is sensitive to features, coalitions, explained rows, training rows,
and data structure. Its per-batch overhead is large: splitting work too finely
can increase elapsed time by multiples even while RAM falls. Coarse batching is
therefore preferred unless memory forces a compromise.

Material timeseries workloads parallelize, but their absolute RAM use can be
very high. In the representative case, eight and sixteen workers were
effectively tied in elapsed time and RAM, so sixteen had no justification.

### VAEAC

VAEAC is dominated by neural-model training in the default reference. Training
rows and epoch count are primary time drivers; network depth, width, latent
size, factor complexity, and explanation dimensions affect either time or RAM
to varying degrees. More batches reduce memory but add expensive repeated
overhead.

Training-dominated work showed almost no useful parallel payoff. The
explanation-heavy block improved at four workers and only modestly thereafter.
Users should first consider whether training can be amortized or reused, then
consider parallel explanation.

### Independence

Independence is cheap and largely insensitive to training-row count. Coalition
count, explained rows, and Monte Carlo samples are the relevant generic
dimensions. More workers do not help at the retained sizes and multiply RAM, so
one worker is preferred.

### Copula

Copula is also inexpensive at the reference workload, but unlike independence
it becomes more sensitive to training size. Coalition and explanation sizes
remain important. Its retained core workload does not justify parallel workers.

### Categorical

Categorical cost increases with training and explanation size, while Monte
Carlo count is weak in the tested block. Parallel overhead outweighs useful work
at the retained size, making one worker the appropriate default.

### Regression separate

Regression separate is relatively insensitive to generic Monte Carlo,
explanation, batch, and data.table-thread changes in the retained blocks.
Coalition count matters, but learner and tuning configuration dominate: smooth
or XGBoost cross-validation is far more expensive than the untuned variants.
The retained workload does not benefit from shapr worker parallelism; users
should instead budget around the selected fitting strategy.

### Regression surrogate

Regression surrogate grows with training size, coalition count, and surrogate
combination count; explained rows and Monte Carlo count are weaker drivers in
the tested range. Its retained parallel experiment is actively unfavorable:
worker startup and duplicated state make both elapsed time and RAM worse. Use
one shapr worker unless a materially different workload is measured.

## Accuracy versus cost

This section is the only part of the report that measures error rather than
cost. It comes from the separate accuracy study
([`config/accuracy.yml`](config/accuracy.yml)), whose runs are excluded from the
cost snapshot above. The study is currently Gaussian-only; the accuracy/budget
trade-off may behave differently for other approaches.

The study used eight features and three replicates. Its reference mean came
from three exact-coalition runs with 2,000 Monte Carlo samples; reference noise
RMSE was 0.00634, roughly two orders of magnitude below the candidate errors.
Because the reference is itself a Gaussian run, these numbers measure
convergence toward the approach's own high-budget answer, not accuracy against
the true conditional distribution. Selected results for 50 explained
observations are:

| Coalitions | MC samples | `explain()` time | Peak RAM | Shapley RMSE | Replicate instability RMSE |
|---:|---:|---:|---:|---:|---:|
| 32 | 25 | 1.46 s | 230 MB | 0.214 | 0.302 |
| 32 | 100 | 1.53 s | 245 MB | 0.081 | 0.140 |
| 32 | 400 | 1.84 s | 250 MB | 0.068 | 0.101 |
| 64 | 100 | 1.73 s | 247 MB | 0.054 | 0.084 |
| 128 | 100 | 2.25 s | 252 MB | 0.047 | 0.060 |
| 128 | 400 | 4.06 s | 305 MB | 0.023 | 0.033 |
| 256 | 100 | 1.78 s | 261 MB | 0.023 | 0.034 |
| 256 | 400 | 6.30 s | 342 MB | 0.014 | 0.017 |

At 32 coalitions, increasing Monte Carlo samples has diminishing returns because
coalition approximation remains. At 128 or 256 coalitions, the same increase
reduces both error and replicate instability more effectively. Conversely,
adding coalitions while leaving Monte Carlo noise high is insufficient.

With eight features, 256 is the exact coalition set and follows a different
execution path. Its low time at 100 Monte Carlo samples relative to 128 is a
path-specific discontinuity, not evidence that more coalitions are generally
cheaper.

Practical guidance is to increase the limiting dimension. If repeated runs vary
materially, raise `n_MC_samples`. If results are stable but differ from a
higher-coalition check, raise `max_n_coalitions`. For this example, 64–128
coalitions and 100 Monte Carlo samples form a useful middle region; this is an
illustration rather than a universal accuracy prescription.

## Dense-batch memory cap

The Gaussian memory calibration used 12 features, 1,024 coalitions, and 100
explained observations:

| Cube-size cap | Actual batches | 1-worker elapsed / RAM | 4-worker elapsed / RAM |
|---:|---:|---:|---:|
| 1 million | 342 | 33.1 s / 367 MB | 16.5 s / 1,568 MB |
| 4 million | 79 | 38.6 s / 425 MB | 18.2 s / 2,057 MB |
| 16 million | 20 | 36.1 s / 748 MB | 18.5 s / 2,728 MB |
| 64 million | 8 | 34.3 s / 1,473 MB | 18.0 s / 5,260 MB |
| Unlimited | 8 | 34.1 s / 1,462 MB | 17.8 s / 5,233 MB |

The one-million default delivered the lowest RAM and no speed penalty in this
Gaussian experiment. It should remain the default under an unknown memory
budget. Relaxing it is a measured memory-for-latency decision, and the memory
effect becomes much larger with multiple workers. This conclusion is specific
to the dense Gaussian path; it does not override the observed batch overhead
for timeseries and VAEAC.

## Real prediction-model cost

The prediction experiment keeps the Gaussian explanation problem fixed and
changes only a pre-built model. Model training is outside the measured run. The
standard XGBoost model has 50 trees of depth 3; the large model has 500 trees of
depth 6.

| Model | 1 worker | 4 workers | 16 workers |
|---|---:|---:|---:|
| Linear | 8.28 s, 300 MB | 4.54 s (1.82x), 1,236 MB | 4.18 s (1.98x), 3,580 MB |
| XGBoost | 10.59 s, 369 MB | 6.36 s (1.67x), 1,756 MB | 5.75 s (1.84x), 5,269 MB |
| XGBoost large | 50.36 s, 373 MB | 18.03 s (2.79x), 1,742 MB | 13.24 s (3.80x), 5,336 MB |

Times are median `explain()` seconds. For the linear model, whole-process time
falls from 9.39 seconds with one worker to 6.78 with four and 6.75 with sixteen;
the extra workers therefore add RAM without a meaningful end-to-end gain. The
standard XGBoost model is also close to practical saturation at four workers.
Only the large model retains a material four-to-sixteen-worker gain, with a
large RAM increase.

## User guidance

1. Start with one worker when the workload is small or memory is uncertain.
2. If runtime is material, test four workers on a representative case and
   record both whole-process time and peak RAM. Test eight or sixteen only when
   the measured gain can justify the additional memory.
3. Use batching as a memory control, but distinguish approaches: finer batches
   are generally safe for Gaussian, CTree, and ARF; avoid very fine batches for
   timeseries and VAEAC unless required by memory.
4. Tune coalitions and Monte Carlo samples together. Use replicate instability
   and a higher-budget comparison to decide which error source is limiting.
5. Include prediction-model cost, training rows, feature types, and
   approach-specific tuning in resource estimates. Generic dimensions alone do
   not predict cost reliably.
6. For iterative comparisons, require matching realized coalition budgets. The
   final curated snapshot satisfies this requirement for every pair.

## Limitations and bounded follow-ups

The current results support operational guidance, but not hardware-independent
runtime formulas. They use one host, synthetic datasets, and one detailed
accuracy study based on Gaussian explanations. Two-replicate expensive blocks
are adequate for large effects but less reliable for close timing differences.

If more evidence is wanted, the highest-value bounded additions are:

1. Add a reduced 2-by-2 coalition/Monte Carlo grid for one nonlinear model and
   one mixed dataset, with a small reference set, as further blocks in
   [`config/accuracy.yml`](config/accuracy.yml).
2. Separate reusable VAEAC training/setup time from explanation time at two
   explanation sizes, if the existing timing fields cannot already do so.
3. Run a compact Gaussian/CTree subset on a second, smaller machine to test how
   well the worker and RAM guidance transfers.

These are extensions, not prerequisites for reporting the present curated
benchmark. The numerical tables and snapshot invariants in this report are
reproduced by `Rscript benchmarks/R/audit_findings.R`.
