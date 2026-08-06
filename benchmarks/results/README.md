# Benchmark result snapshot

This directory contains the compact, committed outputs from the benchmark
study. Each current approach has:

- `grid.csv`: the planned configurations;
- `results.csv`: one row per completed run, including status and metadata;
- `summary.csv`: median and IQR summaries over successful measured runs.

The per-run JSON artefacts, datasets, trained models, and logs remain local and
git-ignored because they are generated, granular, and substantially larger.

## Core status

The current snapshot contains all 2,582 planned runs, all successful. Ten
previously failed VAEAC runs were replaced after their fixes became available:
eight parallel runs now use the non-serializing `multicore` future plan, and two
all-categorical runs use the corrected one-hot encoding.

A dependency audit found 72 historical iterative-pair dependents whose stored
fixed budget (4,096 coalitions) does not match the coalition count now recorded
by their source. They are retained as successful standalone fixed-budget runs,
but must not be used for iterative-versus-fixed comparisons. The affected
studies have eight rows each: ARF, copula, CTree, empirical, Gaussian,
independence, regression separate, regression surrogate, and timeseries.
Categorical and VAEAC pairs are valid; all non-dependent rows remain comparable.
The aggregator now labels this condition and excludes such dependents from new
summaries without using Git/package-version invalidation.

Obsolete `oat_*` and `factorial_*` experiments from an earlier study design are
not part of the committed snapshot.

## Optional follow-up status

Twelve self-contained `extra_*` studies add realistic parallel workloads,
Gaussian accuracy/cost interactions, dense-batch memory calibration, and model
prediction-cost sensitivity. They planned 343 runs: 338 completed normally,
three duplicate long-duration runs were skipped, one duplicate known to exceed
the resource limit was skipped, and one timeseries run was resource-killed.

See [`../EXTRA_FINDINGS.md`](../EXTRA_FINDINGS.md) for the cross-study findings,
user guidance, limitations, and a recommendation about which optional studies
are most useful to retain. Any extension can be discarded independently by
removing its `config/extra_*.yml` and matching `results/extra_*` directory.
