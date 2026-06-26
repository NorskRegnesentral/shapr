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

# ~1 hour smoke run, one-factor-at-a-time design
bin/orchestrate.sh config/oat_quick.yml

# the big weekend run (all 11 approaches, dense sweeps, 3 replicates)
bin/orchestrate.sh config/oat_weekend.yml

# partial-factorial designs (interactions)
bin/orchestrate.sh config/factorial_quick.yml
bin/orchestrate.sh config/factorial_weekend.yml
```

Results land in `results/<study>/results.csv` (one row per run) and
`results/<study>/summary.csv` (median/IQR per configuration). Runs are
**resumable** — re-running skips configs that already have a result file.

---

## What gets measured

Per run (one `explain()` call in a fresh R process):

| Metric | Source | Notes |
|---|---|---|
| Wall time | `Sys.time()` around `explain()` | headline speed number |
| CPU time | `proc.time()` (self + child) | child time covers `multicore` forks only |
| Phase breakdown | shapr's own `$timing` | where time goes (setup vs `compute_vS` …) |
| Peak RAM (tree) | external `/proc` sampler | sums RSS of the whole process tree incl. workers |
| Peak RAM (cgroup) | cgroup-v2 `memory.peak` | exact, catches transient spikes (Linux + systemd) |
| gc peak | `gc()` max in the parent | in-process cross-check (sequential runs) |

Plus full config, actual coalitions used, status (`ok` / `error` /
`skipped_missing_dep`), and metadata (R/shapr version, git SHA, host, timestamp).

## What gets varied

All driven from the config files. Out of the box:

- **approach** — all 11 (`independence`, `gaussian`, `copula`, `empirical`,
  `ctree`, `arf`, `categorical`, `vaeac`, `regression_separate`,
  `regression_surrogate`; `timeseries` is supported by the capability matrix but
  not in the default grids). Incompatible approach/dataset pairs are skipped
  automatically (see [`R/capability.R`](R/capability.R)).
- **n_features**, **max_n_coalitions**, **n_MC_samples**
- **min_n_batches** (the batching lever; `max_batch_size = Inf` keeps it clean)
- **workers** and **backend** (`multisession` vs `multicore`)
- **n_train**, **n_explain**
- a few **approach-specific** params (e.g. `empirical.type`, `ctree.sample`,
  `arf.num_trees`).

### Two datasets (three generators)

- `numeric` — all numeric features (AR(1)-correlated). Works with every approach.
- `mixed` — numeric + factor features. For factor-supporting approaches.
- `categorical` — all factor features. Required by the `categorical` approach.

Models: **xgboost** for `numeric`; **ranger** for `mixed`/`categorical` (ranger
handles factors natively). Model training is cached and **excluded** from the
measured time.

---

## Designs

Two interchangeable designs share the same runner and measurement layer:

- **OAT** (`oat_*.yml`) — start from a `baseline` and vary **one dimension at a
  time**, plus a few targeted 2-D interaction grids. Best for clean
  "change this lever → this happens" guidance.
- **Factorial** (`factorial_*.yml`) — full cross-product over a chosen subset of
  dimensions (everything else fixed). Captures interactions OAT misses; cost is
  the **product** of the factor lengths, so keep it small.

Two scales per design: **quick** (~1 h smoke) and **weekend** (full).

---

## Configuration

- [`config/common.yml`](config/common.yml) — machine-wide defaults: seed,
  replicates, warm-up, RAM method, models, datasets, the `baseline`
  configuration, and thread controls. **Every study inherits from this.**
- `config/oat_quick.yml`, `config/oat_weekend.yml` — OAT sweeps.
- `config/factorial_quick.yml`, `config/factorial_weekend.yml` — factorial grids.

A study file is deep-merged on top of `common.yml` (study wins). To tweak the
study, just edit the YAML — no code changes needed. For example, to add a
sweep point, add a value to a list under `sweeps:`; to change the baseline for
all OAT runs, edit `baseline:` in `common.yml`.

---

## Why a fresh process per run

To make the numbers trustworthy:

- **No cross-run caching / warm heap** — each config starts cold.
- **Attributable RAM** — peak memory belongs to exactly one config.
- **Clean parallelism** — fresh `future` workers each time.

`orchestrate.sh` also pins `OMP_NUM_THREADS=OPENBLAS_NUM_THREADS=
MKL_NUM_THREADS=R_DATATABLE_NUM_THREADS=1`, so the **only** parallelism is the
swept `workers` count (otherwise multi-threaded BLAS would confound the
"threads" dimension, especially for `gaussian`/`copula`). Runs are executed in
randomised order with a short cooldown to avoid thermal drift (this box uses the
`schedutil` governor) correlating with any one dimension.

---

## File map

```
benchmarks/
  config/        editable YAML studies (+ common.yml)
  R/
    config.R       load + deep-merge YAML
    capability.R   approach x dataset matrix + dependency checks
    data.R         synthetic datasets + cached xgboost/ranger models
    grid.R         expand a config -> results/<study>/grid.csv
    measure.R      timing / gc / metadata helpers
    run_one.R      run ONE config in isolation -> results/<study>/<id>.json
    sampler.R      external peak-RAM sampler (poll + cgroup)
    aggregate.R    merge results -> results.csv + summary.csv
  bin/
    orchestrate.sh main driver
  data/  results/  logs/   (generated; git-ignored)
```

## Requirements

R packages: `shapr` (installed), `yaml`, `jsonlite`, `data.table`, `future`,
`future.apply`, `ps`, `xgboost`, `ranger`, and the per-approach deps
(`arf`, `partykit`, `torch`, `parsnip`, …). Approaches whose deps are missing
are recorded as `skipped_missing_dep` instead of failing the study.

The cgroup RAM method needs Linux with cgroup-v2 and `systemd-run --user`;
otherwise set `ram.method: poll` in `common.yml` (the framework also falls back
to `poll` automatically if `systemd-run` is absent).

---

## Re-running / extending

- **Resume**: just re-run `orchestrate.sh`; existing `results/<study>/<id>.json`
  files are skipped. Delete a study's `results/<study>/` folder to start over.
- **Re-aggregate only**: `Rscript R/aggregate.R --config config/<study>.yml`.
- **New dimension/value**: edit the relevant YAML list and re-run.
- **New approach-specific sweep**: add an entry under `approach_param_sweeps`
  (OAT) — `{approach, dataset, param, values}`.
