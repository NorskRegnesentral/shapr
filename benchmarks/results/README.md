# Benchmark result snapshot

This directory contains the compact, committed outputs from the benchmark
study. Each current approach has:

- `grid.csv`: the planned configurations;
- `results.csv`: one row per completed run, including status and metadata;
- `summary.csv`: median and IQR summaries over successful measured runs.

An accuracy study would additionally carry `accuracy_results.csv`,
`accuracy_summary.csv`, and the `*.shapley.rds` matrices those metrics are
computed from. None is currently part of the snapshot.

The per-run JSON artefacts, datasets, trained models, and logs remain local and
git-ignored because they are generated, granular, and substantially larger.

## Status

The curated snapshot contains all 2,278 planned runs, all successful. It uses
three replicates by default, two for VAEAC, and two for the added expensive ARF
and timeseries realistic-workload blocks. Warm-up runs are not part of the
configuration or result set.

Every retained peak RAM value uses cgroup-v2 `memory.peak`; the published
snapshot does not contain process-tree RSS fallback measurements.

All iterative-pair dependents use the coalition budget currently recorded by
their source. The final validation found no mismatched pairs, so every retained
pair is included in its approach summary.

Obsolete `oat_*` and `factorial_*` experiments from an earlier study design are
not part of the committed snapshot.

The approach grids include the retained realistic parallel workloads,
dense-batch memory calibration, and prediction models (`linear`, `xgb`, and
`xgb_large`). Superseded `extra_*` studies, and the Gaussian accuracy/cost
interaction surface, are not part of the active configuration or result tree.

See the published
[computational cost benchmark article](https://norskregnesentral.github.io/shapr/articles/benchmarks.html)
for cross-study findings, user guidance, and limitations.

Run `Rscript benchmarks/R/audit_findings.R` from the repository root to verify
the snapshot invariants and reproduce the numerical tables used in the article.
