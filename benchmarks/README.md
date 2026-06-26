# shapr compute & memory benchmark study

A self-contained framework to measure **CPU time** and **peak RAM** of
`shapr::explain()` across the package's many settings, on a single machine.

The goal is **not** accuracy (that depends on the data) but **cost**: how
fast / memory-hungry each approach is, and how that scales with the arguments
you can change — so end users know which knob to turn when compute is limited.

Everything is driven by editable YAML config files in [`config/`](config/).

---

## TL;DR

```bash
cd benchmarks

# smoke run, per-approach one-factor-at-a-time design
bin/orchestrate.sh config/oat_quick.yml

# the big weekend run (all approaches, dense sweeps, 3 replicates)
bin/orchestrate.sh config/oat_weekend.yml

# partial-factorial designs (interactions)
bin/orchestrate.sh config/factorial_quick.yml
bin/orchestrate.sh config/factorial_weekend.yml

# re-attempt only the runs that previously hit the wall-clock timeout
bin/orchestrate.sh config/oat_weekend.yml --retry-timeouts
```

Results land in `results/<study>/results.csv` (one row per run) and
`results/<study>/summary.csv` (median/IQR per configuration). Runs are
**resumable** — re-running skips configs that already have a result file.

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
| Peak RAM (tree) | external `/proc` sampler | sums RSS of the whole process tree incl. workers |
| Peak RAM (cgroup) | cgroup-v2 `memory.peak` | exact, catches transient spikes (Linux + systemd) |
| gc peak | `gc()` max in the parent | in-process cross-check (sequential runs) |

The **bash wall time** is the headline because it captures the *true* cost of
producing one explanation on this machine (model fitting excluded, since that is
pre-built). The internal `explain()` wall and `data_load_secs` let you decompose
it.

Plus full config, actual coalitions used, iterations, status (`ok` / `error` /
`skipped_missing_dep` / `timeout`), and metadata (R/shapr version, git SHA,
host, timestamp). A run exceeding `timeout_sec` is killed and recorded as
`timeout`.

## What gets varied

All driven from the config files. The OAT design is **tiered** — most sweeps are
repeated *per approach* (because cost behaviour differs a lot per approach),
while approach-agnostic infrastructure levers are swept once at the baseline.

**Per-approach sweeps** (each repeated for every approach in `approaches`, on
that approach's primary dataset):

- **dataset** — the approach across every dataset family it supports
- **max_n_coalitions**, **n_MC_samples**, **n_train**, **n_explain**
- **n_features** (numeric family only; the largest point, 20, triggers a
  high-dimensional coalition cap via `high_dim:`)
- **group** — features chunked into groups (`group_size`) instead of individual
- **iterative** — emits a dependent **pair**: a `source` run with
  `iterative = TRUE` and a `dependent` run with `iterative = FALSE` whose
  `max_n_coalitions` is set to *exactly* the number of coalitions the source
  consumed, so iterative vs non-iterative are compared at equal budget.

**Baseline-only (infrastructure) sweeps:**

- **min_n_batches** (the batching lever; `max_batch_size = Inf` keeps it clean)
- **dt_threads** (`data.table::setDTthreads()`)
- the **workers x backend** parallel grid (up to 32 workers, `multisession` vs
  `multicore`), combined with heavier batching so workers have work.

**Approach-specific sweeps:**

- scalar params (`empirical.type`, `vaeac.*`, `regression.surrogate_n_comb`)
- named **regression variants** (smoothing / penalisation / tuning recipes) —
  see [`R/registry.R`](R/registry.R).

Approaches: `independence`, `gaussian`, `copula`, `empirical`, `timeseries`,
`ctree`, `arf`, `categorical`, `vaeac`, `regression_separate`,
`regression_surrogate`. Incompatible approach/dataset pairs and approaches with
missing deps are skipped automatically (see [`R/capability.R`](R/capability.R)).

### Datasets (numeric, four mixed, categorical)

- `numeric` — all numeric features (AR(1)-correlated), up to 20 columns. Works
  with every approach.
- `mixed_fc_fl`, `mixed_fc_ml`, `mixed_mc_fl`, `mixed_mc_ml` — numeric + factor
  features spanning **f**ew/**m**any factor **c**olumns x **f**ew/**m**any
  **l**evels. All belong to the `mixed` family. For factor-supporting approaches.
- `categorical` — all factor features. Required by the `categorical` approach.

Models (keyed by dataset *family*): **xgboost** for `numeric`; **ranger** for
`mixed`/`categorical` (ranger handles factors natively). Models are pre-built
and cached by `R/prebuild.R` and **excluded** from the measured time.

---

## Designs

Two interchangeable designs share the same runner and measurement layer:

- **OAT** (`oat_*.yml`) — start from a `baseline` and vary **one dimension at a
  time**, tiered per-approach (see above). Best for clean "change this lever →
  this happens" guidance.
- **Factorial** (`factorial_*.yml`) — full cross-product over a chosen subset of
  dimensions (everything else fixed). Captures interactions OAT misses; cost is
  the **product** of the factor lengths, so keep it small.

Two scales per design: **quick** (smoke) and **weekend** (full).

---

## Configuration

- [`config/common.yml`](config/common.yml) — machine-wide defaults: seed,
  replicates, warm-up, RAM method, models, datasets, the `baseline`
  configuration, and thread controls. **Every study inherits from this.**
- `config/oat_quick.yml`, `config/oat_weekend.yml` — OAT sweeps.
- `config/factorial_quick.yml`, `config/factorial_weekend.yml` — factorial grids.

A study file is deep-merged on top of `common.yml` (study wins). To tweak the
study, just edit the YAML — no code changes needed.

Key OAT study keys:

```yaml
approaches: [gaussian, empirical, ctree, regression_separate, vaeac]
per_approach_sweeps:        # repeated for every approach above
  dataset: true
  max_n_coalitions: [32, 128, 256]
  n_MC_samples: [50, 250, 1000]
  n_train: [100, 1000, 5000]
  n_explain: [1, 25, 100]
  n_features: [5, 10, 20]   # numeric only; 20 -> high_dim cap
  iterative: true           # source/dependent pair
  group: true
high_dim: {n_features: 20, max_n_coalitions: 4000}
infra_sweeps:               # baseline-only
  min_n_batches: [1, 10, 50]
  dt_threads: [1, 2, 4]
parallel_grid:              # baseline-only
  workers: [1, 2, 4, 8, 16, 32]
  backend: [multisession, multicore]
  min_n_batches: 32
approach_param_sweeps:      # scalar approach args; optional `variant`
  - {approach: empirical, dataset: numeric, param: empirical.type,
     values: [fixed_sigma, AICc_full, independence]}
regression_variant_sweeps:  # named recipes from R/registry.R
  - {approach: regression_separate, dataset: numeric,
     variants: [smooth_none, smooth_light, xgb_none]}
```

Machine-wide knobs in `common.yml`: `timeout_sec` (wall-clock kill),
`iterative_cap` (budget for the iterative source), `mixed_default` (fallback
dataset for factor-only approaches), `group_size`, the four `mixed_*` dataset
specs, and the `baseline` (which now also carries `dt_threads` and `group`).

---

## Why a fresh process per run

To make the numbers trustworthy:

- **No cross-run caching / warm heap** — each config starts cold.
- **Attributable RAM** — peak memory belongs to exactly one config.
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
    grid.R         expand a config -> results/<study>/grid.csv (tiered OAT, pairs)
    prebuild.R     pre-generate all pools + pre-fit all models (excluded from timing)
    measure.R      timing / gc / iterations / metadata helpers
    run_one.R      run ONE config in isolation -> results/<study>/<id>.json
    sampler.R      external peak-RAM sampler (poll + cgroup)
    aggregate.R    merge results (+ *.time.json, *.mem.json) -> results.csv + summary.csv
  bin/
    orchestrate.sh main driver (grid -> prebuild -> timed runs -> aggregate)
  data/  results/  logs/   (generated; git-ignored)
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

The cgroup RAM method needs Linux with cgroup-v2 and `systemd-run --user`;
otherwise set `ram.method: poll` in `common.yml` (the framework also falls back
to `poll` automatically if `systemd-run` is absent).

---

## Re-running / extending

- **Resume**: just re-run `orchestrate.sh`; existing `results/<study>/<id>.json`
  files are skipped. Delete a study's `results/<study>/` folder to start over.
- **Retry timeouts**: `bin/orchestrate.sh config/<study>.yml --retry-timeouts`
  deletes previous `timeout` markers and re-attempts only those runs (e.g. after
  raising `timeout_sec`).
- **Re-aggregate only**: `Rscript R/aggregate.R --config config/<study>.yml`.
- **New per-approach sweep point**: add a value under `per_approach_sweeps`.
- **New approach-specific sweep**: add an entry under `approach_param_sweeps`
  — `{approach, dataset, param, values}` (optionally `variant`).
- **New regression variant**: add a named recipe to `regression_variants()` in
  [`R/registry.R`](R/registry.R), then reference it under
  `regression_variant_sweeps` or as a `variant:` on an `approach_param_sweeps`
  entry.

Note: changing dataset specs (e.g. `n_features_max`) or model settings in
`common.yml` invalidates the `data/` caches — delete `data/pool_*.rds` and
`data/model_*.rds` so they regenerate.
