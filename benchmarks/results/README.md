# Benchmark result snapshot

This directory contains the compact, committed outputs from the benchmark
study. Each current approach has:

- `grid.csv`: the planned configurations;
- `results.csv`: one row per completed run, including status and metadata;
- `summary.csv`: runtime median/IQR and peak-RAM median/max over successful
	measured runs, plus diagnostic summaries.

An accuracy study would additionally carry `accuracy_results.csv`,
`accuracy_summary.csv`, and the `*.shapley.rds` matrices those metrics are
computed from. None is currently part of the snapshot.

The per-run JSON artefacts, datasets, trained models, and logs remain local and
git-ignored because they are generated, granular, and substantially larger.

## Status

The curated snapshot contains all 2,278 planned runs, all successful. It uses
three replicates by default, two for VAEAC, and two for the expensive ARF
and timeseries realistic-workload blocks.

Every retained peak RAM value uses cgroup-v2 `memory.peak`; the published
snapshot does not contain process-tree RSS fallback measurements.

All iterative-pair dependents use the coalition budget currently recorded by
their source. The final validation found no mismatched pairs, so every retained
pair is included in its approach summary.

The approach grids include the retained realistic parallel workloads,
dense-batch memory calibration, and prediction models (`linear`, `xgb`, and
`xgb_large`). Only the 11 retained approach studies belong in this snapshot.
Optional accuracy-study support remains in `R/accuracy.R` and the benchmark
README, separate from these published cost results.

Gaussian's prediction-model runs retain their original IDs 481–507. The gap
marks excluded experiments, not missing results. The current config regenerates
the same settings with IDs 406–432, which also changes their `seed + id` random
seeds. Preserve this snapshot and start in empty output directories when
rerunning that study; see the benchmark README for resume precautions.

See the published
[computational cost benchmark article](https://norskregnesentral.github.io/shapr/articles/benchmarks.html)
for cross-study findings, user guidance, and limitations.

Run `Rscript benchmarks/R/audit_findings.R` from the repository root to verify
the snapshot invariants and reproduce the numerical tables used in the article.
