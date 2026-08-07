# Benchmark result snapshot

This directory contains the compact, committed outputs from the benchmark
study. Each current approach has:

- `grid.csv`: the planned configurations;
- `results.csv`: one row per completed run, including status and metadata;
- `summary.csv`: median and IQR summaries over successful measured runs.

The `accuracy/` directory is the separate accuracy study. In addition to the
three files above it carries `accuracy_results.csv` and `accuracy_summary.csv`
(error metrics against the high-budget reference) and the `*.shapley.rds`
matrices those metrics are computed from, so the analysis can be reproduced
with `Rscript benchmarks/R/accuracy.R --config config/accuracy.yml`.

The per-run JSON artefacts, datasets, trained models, and logs remain local and
git-ignored because they are generated, granular, and substantially larger.

## Status

The curated cost snapshot contains all 2,278 planned runs, all successful. It
uses three replicates by default, two for VAEAC, and two for the added
expensive ARF and timeseries realistic-workload blocks. The accuracy study adds
75 successful runs (72 candidates and 3 references). Warm-up runs are not part
of the configuration or result set.

All iterative-pair dependents use the coalition budget currently recorded by
their source. The final validation found no mismatched pairs, so every retained
pair is included in its approach summary.

Obsolete `oat_*` and `factorial_*` experiments from an earlier study design are
not part of the committed snapshot.

The approach grids include the retained realistic parallel workloads,
dense-batch memory calibration, and prediction models (`linear`, `xgb`, and
`xgb_large`). The Gaussian accuracy/cost interaction surface now lives in the
separate `accuracy/` study rather than in `gaussian/`. Superseded `extra_*`
studies are not part of the active configuration or result tree.

See [`../BENCHMARK_FINDINGS.md`](../BENCHMARK_FINDINGS.md) for cross-study
findings, user guidance, and limitations.

Run `Rscript benchmarks/R/audit_findings.R` from the repository root to verify
the snapshot invariants and reproduce the numerical tables used in the report.
