# Benchmark result snapshot

This directory contains the compact, committed outputs from the benchmark
study. Each current approach has:

- `grid.csv`: the planned configurations;
- `results.csv`: one row per completed run, including status and metadata;
- `summary.csv`: median and IQR summaries over successful measured runs.

The per-run JSON artefacts, datasets, trained models, and logs remain local and
git-ignored because they are generated, granular, and substantially larger.

## Current status

The current snapshot contains all 2,582 planned runs: 2,572 succeeded and 10
VAEAC runs failed. Of the failures, eight used a serializing `multisession`
future plan with non-serializable torch pointers, and two used an
all-categorical dataset affected by the one-hot encoding bug. These runs are
retained to record the study state and should be replaced after their fixes are
available on the benchmark branch.

Obsolete `oat_*` and `factorial_*` experiments from an earlier study design are
not part of the committed snapshot.
