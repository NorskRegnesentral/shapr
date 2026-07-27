# Benchmark result snapshot

This directory contains the compact, committed outputs from the benchmark
study. Each current approach has:

- `grid.csv`: the planned configurations;
- `results.csv`: one row per completed run, including status and metadata;
- `summary.csv`: median and IQR summaries over successful measured runs.

The per-run JSON artefacts, datasets, trained models, and logs remain local and
git-ignored because they are generated, granular, and substantially larger.

## Current status

The current snapshot contains all 2,582 planned runs, all successful. Ten
previously failed VAEAC runs were replaced after their fixes became available:
eight parallel runs now use the non-serializing `multicore` future plan, and two
all-categorical runs use the corrected one-hot encoding.

Obsolete `oat_*` and `factorial_*` experiments from an earlier study design are
not part of the committed snapshot.
