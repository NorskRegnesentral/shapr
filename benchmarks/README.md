# shapr compute & memory benchmark study

> **Reading guide:** This README primarily documents how to use, rerun, and
> extend the benchmark framework. Its detailed technical reference is intended
> mainly for AI assistants working with this folder. For human readers seeking
> the basic study design, findings, and practical conclusions from the completed
> benchmarks, we recommend the
> [computational cost benchmark article on the pkgdown site](https://norskregnesentral.github.io/shapr/articles/benchmarks.html).

A self-contained framework to measure **wall time** and **peak RAM** of
`shapr::explain()` across the package's many settings, on a single machine.

The completed study contains 2,278 successful runs across 789 configurations
and all 11 built-in estimation approaches. It covers synthetic numeric, mixed,
and categorical data while varying the main problem dimensions, batching and
parallelism controls, prediction models, and approach-specific settings. The
[computational cost benchmark article](https://norskregnesentral.github.io/shapr/articles/benchmarks.html)
on the `shapr` website is the canonical summary of its results, practical
guidance, scope, and limitations.

The goal is **cost**: how fast and memory-hungry each approach is, and how that
scales with user-controlled arguments. Approximation error is not measured by
the shipped studies, but the machinery to add such a study is included — see
[Accuracy studies](#accuracy-studies).

This README documents both the reusable benchmark framework and the precise
design, configuration, and outputs of the completed study. It also explains
how to rerun the study or extend it with new approaches, configurations, and
experiments. Everything is driven by editable YAML config files in
[`config/`](config/).

---

## TL;DR

Commands below assume Linux and an installed `shapr`. Before rerunning the
curated snapshot, read [Re-running / extending](#re-running--extending): the
launcher rewrites grids and aggregates, and resume is safe only with compatible
local outputs.

```bash
cd benchmarks

# run ONE approach's study (grid -> prebuild -> timed runs -> aggregate)
bin/orchestrate.sh config/gaussian.yml

# run the WHOLE suite, one approach at a time (vaeac last)
bin/run_suite.sh

# run just a few approaches
bin/run_suite.sh gaussian empirical ctree

# retry timeouts and skipped dependents, and run any unfinished configurations
# (raise timeout_sec in common.yml first to give them more time)
bin/run_suite.sh --retry-timeouts
bin/orchestrate.sh config/vaeac.yml --retry-timeouts
```

Results land in `results/<approach>/results.csv` (one row per run) and
`results/<approach>/summary.csv` (wall-time median/IQR and peak-RAM median/max
per configuration, plus diagnostic summaries). The compact
`grid.csv`, `results.csv`, and `summary.csv` files are committed as the study
record. Per-run JSON artefacts remain local and git-ignored. Runs are
**resumable with an unchanged grid** — re-running skips configs that already
have a result file, and each study stops launching new runs once its
`time_budget_sec` execution window is used up. This window starts after
prebuilding; an in-flight run may finish after it expires. See
[Re-running / extending](#re-running--extending)
before running over the curated snapshot or changing a completed study's grid.

The orchestrator first builds the grid, then **pre-builds every dataset pool
and prediction model** (`R/prebuild.R`) so that model fitting is excluded from
the timed runs, then executes each run under a wall-clock **timeout** with
bash-level timing.

---

## What gets measured

Per run (one `explain()` call in a fresh R process):

| Metric | Source | Notes |
|---|---|---|
| Fresh-process wall time | launcher `date +%s.%N` interval | diagnostic; includes R startup, data loading, `explain()`, post-processing, and launcher/monitoring overhead |
| Data-load time | `Sys.time()` around data preparation and cached model loading | includes reading pools and subsetting rows/columns, not just disk I/O |
| **explain() wall time** | `Sys.time()` around `explain()` only | **headline** runtime reported in the findings and vignette |
| CPU time | `proc.time()` (self + child) | child time covers `multicore` forks only |
| Phase breakdown | shapr's own `$timing` | where time goes (setup vs `compute_vS` …) |
| Iterations | `length(internal$iter_list)` | 1 for non-iterative; iterative runs may also stop after 1 |
| Batches used | `length(iter_list[[i]]$S_batch)` | `used_n_batches` (final iter) + `used_n_batches_max`; `effective_max_batch_size` shows the post-cap batch size |
| Peak RAM (poll) | external `/proc` sampler | sums RSS for session members (or cgroup members in cgroup mode); may miss spikes and count shared pages more than once |
| Peak RAM (cgroup) | cgroup-v2 `memory.peak` | kernel-recorded high-water mark; sampler falls back to sampled `memory.current` if unavailable |
| gc peak | `gc()` max in the parent | in-process cross-check (sequential runs) |

The **`explain()` wall time** is the headline because it measures the package
call consistently without conflating it with fresh-process startup, cached-input
loading, benchmark bookkeeping, or process shutdown. Fresh-process wall time,
`data_load_secs`, CPU time, and the internal phase breakdown remain available as
diagnostics. Prediction-model fitting is pre-built and excluded from both
wall-time metrics when using the orchestrator with valid caches.

Here, pre-built model fitting means training the **prediction model being
explained**. Fitting performed inside `explain()` by an estimation approach
(such as VAEAC training or coalition regressions) is included in `wall_secs`.
Unlike that timer, the external peak-RAM measurement covers the full isolated
run, including R startup, loaded data/models, workers, and post-processing.
`peak_ram_mb` uses bytes divided by `1024^2` (MiB despite the column name),
preferring a positive cgroup measurement and otherwise falling back to RSS.
All retained published runs use cgroup-v2 `memory.peak`.

Plus grid dimensions and approach arguments, actual coalitions used, iterations,
status (`ok` / `error` /
`skipped_*` / `timeout` / `killed_resource`), and metadata (R/shapr version,
git SHA, host, timestamp). A run exceeding `timeout_sec` is recorded as
`timeout`; exit code 137 is labelled `killed_resource` (a SIGKILL indicator,
not proof of an out-of-memory kill). Launcher-generated failure markers may
lack R-side metadata; aggregation fills their configuration fields from the grid.

For iterative pairs, `source_used_n_coalitions` and `pair_budget_matches` make
the dependency check explicit in `results.csv`. A successful dependent is only
included in `summary.csv` when its override and actual coalition count both
match the source's currently recorded count. This is deliberately independent
of Git SHA and package version. Pair identity is retained in `summary.csv` so
separate iterative budgets cannot be combined into one aggregate row.

## What gets varied

Each **approach** is its own study: `config/<approach>.yml` lists a set of named
**blocks**, and each block is a small mini-design that varies a few dimensions
around the shared `baseline` (in `common.yml`) while everything else is held
fixed. Because cost behaviour differs enormously per approach, every approach
runs its own copy of the core battery (sized to its cost — coarser for the slow
ones), and `gaussian` is the broadest, most detailed testbed.

A block can combine:

- **`grid:`** — a cross-product over standard run dimensions. A single entry is
  a 1-D sweep; two or three entries form a 2-D/3-D grid. Dimensions:
  `n_train`, `n_MC_samples`, `max_n_coalitions`, `n_features` (numeric only),
  `n_explain`, `min_n_batches`, `max_batch_size`, `max_batch_cube_size`,
  `workers`, `backend`, `dt_threads`, `group`, `group_size`, `dataset`,
  `model_variant`, `save_explanations`, `approach`, `iterative`.
- **`approach_args:`** — a cross-product over approach-specific arguments
  (`empirical.type`, `vaeac.depth/width/epochs/latent_dim/n_vaeacs_initialize`,
  `regression.surrogate_n_comb`, or a named regression `variant` from
  [`R/registry.R`](R/registry.R)). Encoded into the `approach_args` column.
- **`pair: iterative`** — emits a dependent **pair** per grid point: a `source`
  run (`iterative = TRUE`, which records the number of iterations and the
  coalitions actually used) and a `dependent` run (`iterative = FALSE`) then run
  at *exactly* that coalition count, so iterative vs fixed compare at an equal
  budget.

When both `grid:` and `approach_args:` are present, their combinations are
crossed. Each block can override `replicates`. Duplicate configurations are
removed before replicates are expanded, retaining the first block's sweep
label and replicate count; iterative pairs remain distinct by pair identity.

The core battery present in (almost) every approach: `scale_train_mc`
(n_train × n_MC), `features` (numeric only), `coalitions`, `explain`,
`iterative_budget`, `dt_threads`, `parallel` (workers × batching, up to 32
workers), `batches` (the `min_n_batches` lever), and — for factor-supporting
approaches other than `categorical` — a `dataset` sweep over the four mixed
settings + categorical. Gaussian calls its core parallel block
`parallel_batching`.
`gaussian` additionally carries the `grouping` / `group_size` studies, the
`parallel_backend` (multisession vs multicore) study, memory-cap calibration,
and prediction-model comparisons. The `highdim_cap` cube-size study is present
in `gaussian`, `copula`, and `empirical`. Retained realistic-workload blocks
extend `gaussian`, `empirical`, `ctree`, `arf`, `timeseries`, and `vaeac`.
Approach-specific blocks add `empirical.type`, the regression
`variants` described below,
`regression.surrogate_n_comb`, and the five vaeac hyperparameters.

The default is three replicates per configuration. VAEAC uses two throughout;
the expensive ARF and timeseries realistic-workload blocks also use two.

### Regression variants

The `variant=...` entries in the results' `approach_args` column identify registered
estimator and tuning recipes; the article displays these as `regression_variant=...`.
The `smooth_*` and `xgb_*` variants are used by
`regression_separate`; `surrogate_none` is used by `regression_surrogate`.
Their exact executable definitions are in
[`regression_variants()`](R/registry.R#L25-L102).

| Variant | Regression approach | Estimator and tuning recipe |
|---|---|---|
| [`smooth_none`](R/registry.R#L28-L34) | `regression_separate` | Natural-spline preprocessing (3 degrees of freedom) followed by linear regression, with no tuning. |
| [`smooth_light`](R/registry.R#L35-L45) | `regression_separate` | Natural splines followed by ridge regression; tunes 3 penalty values using 2-fold cross-validation. |
| [`smooth_cv`](R/registry.R#L46-L58) | `regression_separate` | Natural splines followed by elastic-net regression; tunes 5 penalties and 3 mixture values using 5-fold cross-validation. |
| [`xgb_none`](R/registry.R#L60-L67) | `regression_separate` | XGBoost with 50 trees and maximum tree depth 3, with no tuning. |
| [`xgb_light`](R/registry.R#L68-L78) | `regression_separate` | XGBoost; tunes 2 tree counts and 2 maximum depths using 2-fold cross-validation. |
| [`xgb_cv`](R/registry.R#L79-L91) | `regression_separate` | XGBoost; tunes 3 tree counts, 3 maximum depths, and 2 learning rates using 5-fold cross-validation. |
| [`surrogate_none`](R/registry.R#L93-L100) | `regression_surrogate` | A single XGBoost surrogate model with 50 trees and maximum tree depth 3, with no tuning. |

The suffixes therefore describe the tuning budget: `_none` uses fixed
hyperparameters, `_light` uses a small grid with 2-fold cross-validation, and
`_cv` uses a larger grid with 5-fold cross-validation. These are benchmark
recipes rather than additional `shapr` approaches.

Approaches: `independence`, `gaussian`, `copula`, `empirical`, `timeseries`,
`ctree`, `arf`, `categorical`, `vaeac`, `regression_separate`,
`regression_surrogate`. Incompatible approach/dataset pairs are removed when
building the grid. Missing registered approach/variant dependencies are checked
per run (see [`R/capability.R`](R/capability.R) and the requirements below).
Only `gaussian`/`copula`/`empirical` use the dense-array cube-size cap.

### Datasets (numeric, four mixed, categorical)

- `numeric` — all numeric features (AR(1)-correlated), up to 30 columns. Works
  with every approach except `categorical`.
- `mixed_fc_fl`, `mixed_fc_ml`, `mixed_mc_fl`, `mixed_mc_ml` — numeric + factor
  features spanning **f**ew/**m**any factor **c**olumns x **f**ew/**m**any
  **l**evels. All belong to the `mixed` family. For factor-supporting approaches.
- `categorical` — all factor features. Required by the `categorical` approach.

Runs use the first `n_train` and `n_explain` rows of separately generated
training and explanation pools; replicates reuse these data. Only numeric runs
subset columns using `n_features`. Mixed datasets always have 4 numeric plus
2 or 8 factor columns, and the categorical dataset has 8 factor columns,
regardless of the nominal `n_features` value in the grid. Keep requested row
counts and numeric feature counts within the pool sizes in `common.yml`.

Models (keyed by dataset *family*): **xgboost** for `numeric`; **ranger** for
`mixed`/`categorical` (ranger handles factors natively). Models are pre-built
and cached by `R/prebuild.R` and **excluded** from the measured time. Gaussian's
`prediction_model` block also compares a linear model and a larger XGBoost
model with the baseline XGBoost model.

---

## Design

Every retained study is **one approach** described by a list of `blocks` (see above).
Each block expresses a one-dimensional sweep, a cross-product of several
dimensions, or an iterative/fixed-budget pair. Slow approaches use coarser
levels and fewer or lighter blocks. All retained experiments are defined in
the 11 approach configs; optional accuracy studies use the same block format.

---

## Configuration

- [`config/common.yml`](config/common.yml) — machine-wide defaults: seed,
  replicates, RAM method, models, the four `mixed_*` dataset specs, the
  `baseline` configuration, thread controls, the per-run `timeout_sec` (12 h)
  and the per-approach `time_budget_sec` (96 h). **Every study inherits from
  this.**
- `config/<approach>.yml` — one file per approach (`gaussian.yml`, `vaeac.yml`,
  …), each a list of `blocks`.

A study file is deep-merged on top of a sibling `common.yml` (study wins).
Place new study configs in `config/` with unique filenames. To vary supported
dimensions and approach arguments, edit the YAML; extending the harness itself
may also require R changes.

The workload designs are documented blocks in the relevant approach file.
This keeps the presented configuration and result set aligned:
every reported experiment is generated by `common.yml` plus one approach YAML.

Example block config:

```yaml
approach: gaussian
dataset: numeric
replicates: 3
blocks:
  - name: scale_train_mc                 # a 2-D grid
    grid: {n_train: [500, 5000, 20000], n_MC_samples: [50, 250, 1000]}
  - name: highdim_cap                    # 3-D: cube-size cap ON vs OFF
    grid:
      n_features: [12, 20, 30]
      max_n_coalitions: [128, 512]
      max_batch_cube_size: [1e6, Inf]
  - name: iterative_budget               # source/dependent pair
    pair: iterative
    grid: {max_n_coalitions: [256, 1024]}
```

Set `grid: {max_batch_cube_size: [Inf]}` in a block to disable shapr's
dense-array cap and control batching via `min_n_batches` / `max_batch_size`.
The actual count also depends on the available coalitions and is recorded as
`used_n_batches`; `min_n_batches` is not an exact batch-count request.

Machine-wide knobs in `common.yml`: `timeout_sec` (per-run wall-clock kill, 12 h),
`time_budget_sec` (per-approach budget, 96 h), the four `mixed_*` dataset specs,
and the `baseline` (which carries every run dimension, incl. `dt_threads`,
`group`, `group_size` and `max_batch_cube_size`).

The current shell launcher fixes output directories at `results/<study>/` and
`logs/<study>/`; do not override `paths.results_dir` or `paths.logs_dir` when
using it. BLAS environment limits are also hard-coded in the launcher, not read
from YAML. Requested data.table thread counts come from `baseline.dt_threads`
or the block `grid.dt_threads` dimension; effective counts are subject to the
OpenMP limit described below.

---

## Why a fresh process per run

To make the numbers trustworthy:

- **Fresh R state** — no reused R heap or fitted estimation-approach state.
  Dataset/model caches are deliberately reused; the operating system's file
  cache is not flushed between runs.
- **Attributable RAM** — peak memory belongs to exactly one config or dedicated
  process session.
- **Clean parallelism** — fresh `future` workers each time.

`orchestrate.sh` pins `OPENBLAS_NUM_THREADS=MKL_NUM_THREADS=
R_DATATABLE_NUM_THREADS=1` and sets `OMP_NUM_THREADS` to each run's requested
`dt_threads` before R starts. The R runner calls `data.table::setDTthreads()`
and records `dt_threads_effective_before` and `dt_threads_effective_after`
around the workload, failing if either differs from the request. These fields
measure configured effective limits, not observed simultaneous thread usage.
The data.table version and OpenMP/BLAS environment limits are recorded too.
Other OpenMP libraries can also respond to `OMP_NUM_THREADS`, so this is not
a guarantee that only data.table changes its parallelism.

Runs are executed in dependency-aware randomised order with a short cooldown to reduce
thermal drift (this box uses the `schedutil` governor) correlating with any one
dimension.

---

## File map

```
benchmarks/
  config/        editable YAML studies (+ common.yml)
  R/
    config.R       load + deep-merge YAML
    capability.R   approach x dataset(family) matrix + dependency checks
    registry.R     named regression "variant" recipes (model specs + tuning)
    data.R         synthetic datasets (4 mixed) + cached xgboost/ranger models
    grid.R         expand a config -> results/<study>/grid.csv (blocks, pairs)
    prebuild.R     pre-generate all pools + pre-fit all models (excluded from timing)
    measure.R      timing / gc / iterations / batches / metadata helpers
    run_one.R      run ONE config in isolation -> results/<study>/<id>.json
    sampler.R      external peak-RAM sampler (poll + cgroup)
    aggregate.R    merge results (+ *.time.json, *.mem.json) -> results.csv + summary.csv
    accuracy.R     score saved explanations against a high-budget reference
                   (opt-in; run manually after orchestrate.sh)
    audit_findings.R verify the committed snapshot and published numerical tables
  bin/
    orchestrate.sh run ONE approach (grid -> prebuild -> timed runs -> aggregate)
    run_suite.sh   run the whole suite, one approach at a time
    status.sh      report progress from local per-run artefacts
  data/                   generated datasets/models (git-ignored)
  results/<study>/
    grid.csv              generated study grid (committed)
    results.csv           generated per-run aggregate (committed)
    summary.csv           generated configuration summary (committed)
    accuracy_*.csv        optional accuracy-study metrics (not in this snapshot)
    *.shapley.rds         optional saved Shapley values (not in this snapshot)
    *.json                generated per-run artefacts (git-ignored)
  logs/                   generated run logs (git-ignored)
```

Per run the orchestrator writes `results/<study>/<id>.json` (R-side result),
`<id>.time.json` (bash wall time + exit code + timed-out flag), `<id>.mem.json`
(sampler peak RAM), and `logs/<study>/<id>.log`.

## Requirements

The supplied shell workflow requires Linux, Bash, GNU `date` and `timeout`,
`setsid`, standard process utilities, and `/proc` for session polling. Switching
to `ram.method: poll` removes the systemd requirement, not the Linux requirement.
Git is used to record the checkout SHA. Commands in this README run from
`benchmarks/` unless stated otherwise.

R packages: `shapr` (installed), `yaml`, `jsonlite`, `data.table`, `future`,
`future.apply`, `ps`, `xgboost`, `ranger`, and the per-approach deps
(`arf`, `partykit`, `torch`, `parsnip`, `recipes`, `hardhat`, `glmnet` for the
smooth/penalised regression variants, …). Approaches/variants whose deps are
missing are recorded as `skipped_missing_dep` when the per-run dependency check
detects them. This is not a general dependency installer or validator: missing
framework or prediction-model packages can fail grid generation or prebuilding
before runs begin. VAEAC also requires an installed torch backend.

The cgroup RAM method needs Linux with cgroup-v2 and a responsive
`systemd-run --user`; otherwise set `ram.method: poll` in `common.yml` (the
framework also falls back to session polling automatically).

For VAEAC, use `backend: multicore` when `workers > 1`, as the shipped config
does. Trained torch objects cannot be exported to multisession workers. With
one worker the harness uses a sequential future plan regardless of `backend`.

---

## Re-running / extending

The committed CSVs are a historical snapshot, not a resume checkpoint for a
changed design. The Gaussian grid retains IDs 481–507 for the prediction-model
block after excluded experiments were removed. Regenerating it assigns those
runs IDs 406–432 instead. All run settings and iterative pairings match, but
`run_one.R` seeds each run with `seed + id`, so those 27 runs also get different
seeds on a fresh run. The other ten grids regenerate with identical IDs.

Before rerunning the curated Gaussian study, or changing a completed study's
blocks, seed, datasets, prediction models, or software environment, move its
existing `results/<study>/` and `logs/<study>/` directories to an archive outside
this folder. Start with empty output directories; do not mix old per-run JSON
files with a changed experiment. Resume checks existing run IDs and paired
coalition budgets, not all configuration values or software versions. Rerunning
writes new CSV aggregates and does not reproduce the original measurements
exactly. A fresh checkout contains no per-run JSON files, so launching a study
reruns it rather than resuming from its committed CSVs.

- **Audit the published snapshot**: `Rscript R/audit_findings.R` verifies the
  retained counts, paired budgets, and numerical tables without running benchmarks.
- **Monitor local runs**: `bin/status.sh` reads per-run JSON artefacts; it does
  not display progress from the committed CSVs alone.
- **Whole suite**: `bin/run_suite.sh` runs every approach in the configured
  order. Add approach names to run only some: `bin/run_suite.sh gaussian ctree`.
- **Resume**: just re-run `orchestrate.sh` / `run_suite.sh`; existing
  `results/<study>/<id>.json` files are skipped when the grid is unchanged.
  Each invocation starts a new `time_budget_sec` window per study.
- **Retry timeouts**: `bin/orchestrate.sh config/<approach>.yml --retry-timeouts`
  (or `bin/run_suite.sh --retry-timeouts`) clears `timeout` and
  `skipped_missing_dep` markers, plus earlier dependent-sentinel errors. It then
  resumes the full grid, including unfinished runs; it is not a timeout-only
  filter. Ordinary errors and resource-kill markers are not cleared.
- **Re-aggregate only**: `Rscript R/aggregate.R --config config/<approach>.yml`
  requires the original per-run JSON files and sidecars with their matching
  grid. The committed CSVs alone are not enough.
- **New sweep / grid point**: add or extend a block's `grid:` (or
  `approach_args:`) in the approach's config.
- **New block**: append `{name, grid|approach_args, [pair: iterative]}` to the
  approach's `blocks:` list. The optional `replicates` field overrides the
  study default; `grid` and `approach_args` can be used together.
- **New approach**: add `config/<approach>.yml` with `approach:` + `blocks:`
  for an approach supported by the installed `shapr`. Register its dataset
  families in `approach_capability()` and any extra packages in
  `approach_dependencies()` in [`R/capability.R`](R/capability.R); unregistered
  approaches are filtered out of the grid.
- **New regression variant**: add a named recipe to `regression_variants()` in
  [`R/registry.R`](R/registry.R), then reference it via
  `approach_args: {variant: […]}` in a block.

Note: changing a dataset spec (e.g. `n_features_max`) or the seed in
`common.yml` automatically regenerates the affected `data/pool_*.rds` cache
(the cache key includes the spec and seed). **Trained-model caches do not hash
the dataset spec or actual data values.** Their keys include the dataset name,
model settings, column names, training-row count, and seed. After changing a
dataset spec or data-generation code, clear `data/model_*.rds` before prebuilding
to avoid reusing models fitted to old data. After changing data-generation code,
clear `data/pool_*.rds` as well. Archive caches first if they are needed to
reproduce the earlier study.

## Accuracy studies

The results presented in the
[published benchmark article](https://norskregnesentral.github.io/shapr/articles/benchmarks.html)
measure computational cost only: they do not include approximation-error or
accuracy results. No accuracy study is part of the current committed snapshot.

To weigh a coalition or Monte Carlo budget against approximation quality, the
framework can run an optional **accuracy study**: a grid of candidate budgets
scored against a high-budget reference. The machinery is in place for such
studies, but their results are not currently published on the `shapr` website.

Write a config with two blocks whose names `R/accuracy.R` looks for, and set
`save_explanations: [true]` under each block's `grid` so each run writes its
Shapley matrix to
`results/<study>/<id>.shapley.rds` (saving happens outside the timed region, so
it does not contribute to `wall_secs`, but can affect fresh-process time and
peak RAM):

```yaml
approach: gaussian
dataset: numeric

blocks:
  - name: accuracy_cost          # candidate budgets
    grid:
      n_features: [8]
      max_n_coalitions: [32, 64, 128, 256]
      n_MC_samples: [25, 100, 400]
      n_explain: [10, 50]
      save_explanations: [true]

  - name: accuracy_reference     # near-converged target, >= 2 replicates
    grid:
      n_features: [8]
      max_n_coalitions: [256]
      n_MC_samples: [2000]
      n_explain: [50]
      save_explanations: [true]
```

```bash
bin/orchestrate.sh config/<study>.yml
Rscript R/accuracy.R --config config/<study>.yml
```

`R/accuracy.R` averages the reference runs into a target, then reports RMSE,
MAE, and max absolute deviation over feature contributions (excluding the
baseline `none` column) per candidate, plus two noise measures:
`reference_noise_rmse` is the median RMSE of each reference run against their
mean; `replicate_stability_rmse` is the mean pairwise RMSE among a candidate
configuration's replicates (or `NA` for a single replicate). These are
variability diagnostics, not confidence intervals or bounds on reference bias.
Reference noise should be small relative to candidate errors. Output goes to
`accuracy_results.csv` and `accuracy_summary.csv`.

Points to respect when designing one:

- The reference must be a materially larger budget than every candidate, and
  needs at least two replicates so its own noise can be quantified.
- Candidates are only ever compared with a reference of the **same approach**;
  blocks may override `approach:` in their `grid:` to cover several.
- The reference is itself an approximation, so the result measures convergence
  toward the approach's own high-budget answer — not accuracy against the true
  conditional distribution.
- Within each approach, keep the dataset, training data, feature columns,
  prediction model, grouping, and approach settings fixed across candidates and
  references. The scorer pools references by approach, not by these settings.
- Candidate `n_explain` must not exceed the reference's; the first rows are
  compared.
