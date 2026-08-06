# shapr compute & memory benchmark study

A self-contained framework to measure **CPU time** and **peak RAM** of
`shapr::explain()` across the package's many settings, on a single machine.

The primary goal is **cost**: how fast and memory-hungry each approach is, and
how that scales with user-controlled arguments. A bounded Gaussian accuracy
surface is also included so coalition and Monte Carlo cost can be interpreted
against approximation quality rather than in isolation.

Everything is driven by editable YAML config files in [`config/`](config/).
Findings from the complete study are summarized in
[`BENCHMARK_FINDINGS.md`](BENCHMARK_FINDINGS.md).

---

## TL;DR

```bash
cd benchmarks

# run ONE approach's study (grid -> prebuild -> timed runs -> aggregate)
bin/orchestrate.sh config/gaussian.yml

# run the WHOLE suite, one approach at a time (cheapest first, vaeac last)
bin/run_week.sh

# run just a few approaches
bin/run_week.sh gaussian empirical ctree

# re-attempt only the runs previously killed by the per-run timeout
# (raise timeout_sec in common.yml first to give them more time)
bin/run_week.sh --retry-timeouts
bin/orchestrate.sh config/vaeac.yml --retry-timeouts
```

Results land in `results/<approach>/results.csv` (one row per run) and
`results/<approach>/summary.csv` (median/IQR per configuration). The compact
`grid.csv`, `results.csv`, and `summary.csv` files are committed as the study
record. Per-run JSON artefacts remain local and git-ignored. Runs are
**resumable** — re-running skips configs that already have a result file, and
each study stops launching new runs once its `time_budget_sec` is used up.

The orchestrator first builds the grid, then **pre-builds every dataset pool
and prediction model** (`R/prebuild.R`) so that model fitting is excluded from
the timed runs, then executes each run under a wall-clock **timeout** with
bash-level timing.

---

## What gets measured

Per run (one `explain()` call in a fresh R process):

| Metric | Source | Notes |
|---|---|---|
| **Bash wall time** | `date +%s.%N` around the whole `Rscript` | **headline** number; includes R startup + data load + `explain()` |
| Data-load time | `Sys.time()` around the cached data/model read | lets you subtract I/O from the bash wall |
| explain() wall time | `Sys.time()` around `explain()` only | the pure compute portion |
| CPU time | `proc.time()` (self + child) | child time covers `multicore` forks only |
| Phase breakdown | shapr's own `$timing` | where time goes (setup vs `compute_vS` …) |
| Iterations | `length(internal$iter_list)` | 1 for non-iterative; >1 for iterative |
| Batches used | `length(iter_list[[i]]$S_batch)` | `used_n_batches` (final iter) + `used_n_batches_max`; `effective_max_batch_size` shows the post-cap batch size |
| Peak RAM (poll) | external `/proc` sampler | sums RSS for a dedicated process session incl. detached workers |
| Peak RAM (cgroup) | cgroup-v2 `memory.peak` | exact, catches transient spikes (Linux + systemd) |
| gc peak | `gc()` max in the parent | in-process cross-check (sequential runs) |

The **bash wall time** is the headline because it captures the *true* cost of
producing one explanation on this machine (model fitting excluded, since that is
pre-built). The internal `explain()` wall and `data_load_secs` let you decompose
it.

Plus full config, actual coalitions used, iterations, status (`ok` / `error` /
`skipped_*` / `timeout` / `killed_resource`), and metadata (R/shapr version,
git SHA, host, timestamp). A run exceeding `timeout_sec` is recorded as
`timeout`; a separate signal-based resource kill is retained distinctly.

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

A block is one of:

- **`grid:`** — a cross-product over standard run dimensions. A single entry is
  a 1-D sweep; two or three entries form a 2-D/3-D grid. Dimensions:
  `n_train`, `n_MC_samples`, `max_n_coalitions`, `n_features` (numeric only),
  `n_explain`, `min_n_batches`, `max_batch_size`, `max_batch_cube_size`,
  `workers`, `backend`, `dt_threads`, `group`, `group_size`, `dataset`.
- **`approach_args:`** — a cross-product over approach-specific arguments
  (`empirical.type`, `vaeac.depth/width/epochs/latent_dim/n_vaeacs_initialize`,
  `regression.surrogate_n_comb`, or a named regression `variant` from
  [`R/registry.R`](R/registry.R)). Encoded into the `approach_args` column.
- **`pair: iterative`** — emits a dependent **pair** per grid point: a `source`
  run (`iterative = TRUE`, which records the number of iterations and the
  coalitions actually used) and a `dependent` run (`iterative = FALSE`) then run
  at *exactly* that coalition count, so iterative vs fixed compare at an equal
  budget.

The core battery present in (almost) every approach: `scale_train_mc`
(n_train × n_MC), `features` (numeric only), `coalitions`, `explain`,
`iterative_budget`, `dt_threads`, `parallel` (workers × batching, up to 32
cores), `batches` (the `min_n_batches` lever), and — for factor-supporting
approaches — a `dataset` sweep over the four mixed settings + categorical.
`gaussian` additionally carries the `grouping` / `group_size` studies, the
`highdim_cap` cube-size study, and a `parallel_backend` (multisession vs
multicore) study. Approach-specific blocks add `empirical.type`, the six
regression `variants` (GAM-like & xgboost × none/light/cv),
`regression.surrogate_n_comb`, and the five vaeac hyperparameters.

Approaches: `independence`, `gaussian`, `copula`, `empirical`, `timeseries`,
`ctree`, `arf`, `categorical`, `vaeac`, `regression_separate`,
`regression_surrogate`. Incompatible approach/dataset pairs and approaches with
missing deps are skipped automatically (see [`R/capability.R`](R/capability.R)).
Only `gaussian`/`copula`/`empirical` use the dense-array cube-size cap.

### Datasets (numeric, four mixed, categorical)

- `numeric` — all numeric features (AR(1)-correlated), up to 30 columns. Works
  with every approach.
- `mixed_fc_fl`, `mixed_fc_ml`, `mixed_mc_fl`, `mixed_mc_ml` — numeric + factor
  features spanning **f**ew/**m**any factor **c**olumns x **f**ew/**m**any
  **l**evels. All belong to the `mixed` family. For factor-supporting approaches.
- `categorical` — all factor features. Required by the `categorical` approach.

Models (keyed by dataset *family*): **xgboost** for `numeric`; **ranger** for
`mixed`/`categorical` (ranger handles factors natively). Models are pre-built
and cached by `R/prebuild.R` and **excluded** from the measured time.

---

## Design

Every study is **one approach** described by a list of `blocks` (see above).
There is a single design — no separate OAT vs factorial files — because a block
is flexible enough to express both a 1-D one-at-a-time sweep and a small
factorial grid. Slow approaches simply use coarser block levels, and set
fewer/lighter blocks.

---

## Configuration

- [`config/common.yml`](config/common.yml) — machine-wide defaults: seed,
  replicates, RAM method, models, the four `mixed_*` dataset specs, the
  `baseline` configuration, thread controls, the per-run `timeout_sec` (12 h)
  and the per-approach `time_budget_sec` (96 h). **Every study inherits from
  this.**
- `config/<approach>.yml` — one file per approach (`gaussian.yml`, `vaeac.yml`,
  …), each a list of `blocks`.

A study file is deep-merged on top of `common.yml` (study wins). To tweak a
study, just edit its YAML — no code changes needed.

The retained follow-up designs are ordinary, documented blocks in the relevant
approach file. This keeps the presented configuration and result set aligned:
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
  - name: empirical_type                 # an approach-argument sweep
    approach_args: {empirical.type: [fixed_sigma, AICc_each_k, AICc_full]}
  - name: iterative_budget               # source/dependent pair
    pair: iterative
    grid: {max_n_coalitions: [256, 1024]}
```

Set `max_batch_cube_size: Inf` in a block to disable shapr's dense-array cap and
control the batch count precisely via `min_n_batches` / `max_batch_size`; the
batch count actually used is recorded as `used_n_batches`.

Machine-wide knobs in `common.yml`: `timeout_sec` (per-run wall-clock kill, 12 h),
`time_budget_sec` (per-approach budget, 96 h), the four `mixed_*` dataset specs,
and the `baseline` (which carries every run dimension, incl. `dt_threads`,
`group`, `group_size` and `max_batch_cube_size`).

---

## Why a fresh process per run

To make the numbers trustworthy:

- **No cross-run caching / warm heap** — each config starts cold.
- **Attributable RAM** — peak memory belongs to exactly one config or dedicated
  process session.
- **Clean parallelism** — fresh `future` workers each time.

`orchestrate.sh` also pins `OMP_NUM_THREADS=OPENBLAS_NUM_THREADS=
MKL_NUM_THREADS=R_DATATABLE_NUM_THREADS=1` by default, so the **only**
parallelism is the swept `workers` count and the swept `dt_threads` value
(`data.table::setDTthreads()` per run); otherwise multi-threaded BLAS would
confound the threading dimensions, especially for `gaussian`/`copula`). Runs are
executed in dependency-aware randomised order with a short cooldown to avoid
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
  bin/
    orchestrate.sh run ONE approach (grid -> prebuild -> timed runs -> aggregate)
    run_week.sh    run the whole suite, one approach at a time (cheapest first)
  data/                   generated datasets/models (git-ignored)
  results/<study>/
    grid.csv              generated study grid (committed)
    results.csv           generated per-run aggregate (committed)
    summary.csv           generated configuration summary (committed)
    *.json                generated per-run artefacts (git-ignored)
  logs/                   generated run logs (git-ignored)
  BENCHMARK_FINDINGS.md   cross-study conclusions and user guidance
```

Per run the orchestrator writes `results/<study>/<id>.json` (R-side result),
`<id>.time.json` (bash wall time + exit code + timed-out flag), `<id>.mem.json`
(sampler peak RAM), and `logs/<study>/<id>.log`.

## Requirements

R packages: `shapr` (installed), `yaml`, `jsonlite`, `data.table`, `future`,
`future.apply`, `ps`, `xgboost`, `ranger`, and the per-approach deps
(`arf`, `partykit`, `torch`, `parsnip`, `recipes`, `hardhat`, `glmnet` for the
smooth/penalised regression variants, …). Approaches/variants whose deps are
missing are recorded as `skipped_missing_dep` instead of failing the study.

The cgroup RAM method needs Linux with cgroup-v2 and a responsive
`systemd-run --user`; otherwise set `ram.method: poll` in `common.yml` (the
framework also falls back to session polling automatically).

---

## Re-running / extending

- **Whole suite**: `bin/run_week.sh` runs every approach in turn (cheapest
  first). Add approach names to run only some: `bin/run_week.sh gaussian ctree`.
- **Resume**: just re-run `orchestrate.sh` / `run_week.sh`; existing
  `results/<study>/<id>.json` files are skipped. Delete a study's
  `results/<study>/` folder to start it over.
- **Retry timeouts**: `bin/orchestrate.sh config/<approach>.yml --retry-timeouts`
  (or `bin/run_week.sh --retry-timeouts`) deletes previous `timeout` markers and
  re-attempts only those runs — typically after raising `timeout_sec`.
- **Re-aggregate only**: `Rscript R/aggregate.R --config config/<approach>.yml`.
- **New sweep / grid point**: add or extend a block's `grid:` (or
  `approach_args:`) in the approach's config.
- **New block**: append `{name, grid|approach_args, [pair: iterative]}` to the
  approach's `blocks:` list.
- **New approach**: add `config/<approach>.yml` with `approach:` + `blocks:`
  (and, if it needs extra packages, an entry in `approach_dependencies()`).
- **New regression variant**: add a named recipe to `regression_variants()` in
  [`R/registry.R`](R/registry.R), then reference it via
  `approach_args: {variant: […]}` in a block.

Note: changing a dataset spec (e.g. `n_features_max`) or the seed in
`common.yml` automatically regenerates the affected `data/pool_*.rds` cache
(the cache key includes the spec); trained models are keyed by their inputs and
regenerate as needed too.
