# Computational cost benchmarks

## What this study measures

This benchmark study measures the **computational cost** of
[`shapr::explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md):
elapsed time and peak RAM. It covers 2,278 successful runs across 789
configurations and all 11 built-in estimation approaches. Each approach
is studied through controlled blocks that vary one or a few workload
dimensions around a shared baseline.

The main workload dimensions are the number of features, coalitions,
Monte Carlo samples, training observations, observations to explain,
batches, and parallel workers. The study also includes relevant
approach-specific choices, such as factor complexity, empirical
bandwidth selection, regression learners, and VAEAC architecture and
training settings.

**This is a cost study, not an accuracy study.** Coalition and Monte
Carlo budgets affect runtime and memory, but these results do not
establish which budgets are accurate enough for a particular explanation
problem.

All measurements were collected on one Linux host running Ubuntu 24.04,
with an AMD Ryzen Threadripper 1950X processor (16 physical cores, 32
threads) and 128 GiB RAM. The study used synthetic numeric, mixed, or
categorical data. Absolute time and RAM depend on the machine, operating
system, prediction model, and data. The most transferable evidence is
therefore the relative change within a controlled benchmark block,
rather than a direct prediction of runtime on another machine.

**Treat these results as rough planning guidelines.** We provide no
guarantee that the reported timings, memory use, approach ordering, or
scaling patterns will generalize to another machine, dataset, model, or
package version. We recommend benchmarking a representative subset of
the intended workload before committing substantial compute resources.

The headline elapsed-time measure is whole-process wall time, which
includes R startup, loading the pre-built model and data, and
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).
The table also contains
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
wall time separately. Peak RAM covers the measured process tree or
cgroup, including workers. Most configurations have three replicates;
the expensive VAEAC and realistic ARF and timeseries blocks have two.

The full design, execution framework, committed CSV files, and numerical
audit are available in the [benchmark
directory](https://github.com/NorskRegnesentral/shapr/tree/master/benchmarks).
at GitHub.

## Key findings and practical guidance

- **`approach`: choose for the data, then consider cost.** Gaussian is
  the fastest dependence-aware method for purely numeric data,
  regression surrogate for the mixed-data settings, and the categorical
  approach for fully categorical data. These are cost comparisons, not
  rankings of explanation quality.

- **`x_explain` and `x_train`: their sizes can change the preferred
  approach.** Low-setup approaches such as Gaussian, copula, and
  empirical are attractive when there are few observations to explain
  (few rows in `x_explain`). Regression surrogate, regression separate,
  and VAEAC have reusable setup costs and add little runtime as
  `x_explain` grows; this makes the regression approaches more
  competitive, although VAEAC remains expensive. Ctree and ARF also
  perform setup work, but their runtime still grows strongly with
  `x_explain`; hence, using a sample of the original `x_explain` may
  reduce their runtime. Large `x_train` favors methods with weak
  training-size sensitivity (Gaussian, regression separate, and
  independence, (not recommended)).

- **`workers`^([^1]): one worker is often fine; try four for expensive
  runs.** Parallel overhead commonly outweighs useful work for small
  explanation tasks. Heavy Gaussian, empirical, Ctree, ARF, timeseries,
  and explanation-heavy VAEAC workloads can benefit. Independence,
  copula, categorical, regression separate, and regression surrogate do
  not benefit at the tested sizes. Additional workers increase memory
  use, and runtime gains often diminish beyond four workers. Parallel
  VAEAC requires
  [`future::multicore`](https://future.futureverse.org/reference/multicore.html).

- **`min_n_batches`, `max_batch_size`, and `max_batch_cube_size`^([^2]):
  use batching to manage memory.** Increase the number of batches when
  memory is limiting; reduce it when repeated batch overhead dominates
  runtime. Fine batching is comparatively inexpensive for Gaussian,
  Ctree, and ARF, but can be costly for empirical, timeseries, and
  VAEAC. For Gaussian, copula, and empirical, `max_batch_cube_size`
  provides an additional cap on dense per-batch arrays.

- **`iterative`: use when no reasonable coalition budget is known.** At
  the same final coalition budget, iterative execution costs about the
  same as a fixed run if it stops after one iteration and more if it
  needs several. It is most useful when not even a rough fixed budget is
  known; otherwise, prefer a fixed run.

- **`max_n_coalitions`, `n_MC_samples`, and `x_explain`: control the
  main generic costs.** Feature count, training size, and factor
  complexity can also be important, but their effects vary more by
  approach (see below).

- **`model` and `n_MC_samples`: prediction cost can amplify sampling
  cost.** Model prediction is repeated across coalitions and conditional
  samples. For slow prediction models, reducing `n_MC_samples` may
  therefore shorten runtime considerably; for fast models, other work
  may dominate.

- **Approach-specific arguments can dominate generic settings.**
  Examples include empirical AICc bandwidth selection,
  regression-separate learner tuning, VAEAC training, fine timeseries or
  VAEAC batches, and parallel ARF.

## Additional guidance by approach

The controls described above, including `max_n_coalitions`,
`n_MC_samples`, `x_explain`, `x_train`, workers, and batching, apply
across approaches. This section highlights only additional
approach-specific controls and important constraints that affect how
those controls should be used. Reducing statistical budgets or model
complexity can change the explanation, so choose them based on the
required quality rather than cost alone. Pass approach-specific
arguments to
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
through `...`; see the
[manual](https://norskregnesentral.github.io/shapr/reference/explain.html#arg--).

[TABLE]

## Explore all results

The table below combines every committed approach-level `summary.csv`.
Each row is one configuration aggregated over its replicates. The
retained experiments cover 2, 4, 6, 8, 10, 12, 16, 20, 25, 30 features,
with available values varying by approach. Use the controls above each
column to filter: categorical controls accept one or more values, and
sliders for experiment inputs snap to values present in the results.
Click headings to sort, and use **Column visibility** to focus on
relevant fields. The headline time and RAM metrics remain fixed on the
right while scrolling horizontally. **CSV** downloads the currently
displayed table.

The default view keeps the main workload inputs, approach-specific
settings, replicate count, and two headline outcomes. Less frequently
needed execution details and secondary outcomes remain available under
**Column visibility**. Hover over any column name for its description.

Column descriptions

Direct
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
arguments link to the function manual. Batching controls link to
[`get_extra_comp_args_default()`](https://norskregnesentral.github.io/shapr/reference/get_extra_comp_args_default.md)
and are passed through `extra_computation_args`.

| Column | Description |
|:---|:---|
| `dataset` | Benchmark dataset type. |
| [`approach`](https://norskregnesentral.github.io/shapr/reference/explain.md) | Conditional-distribution approach passed to explain(). |
| `n_features` | Number of features in x_train and x_explain. |
| [`max_n_coalitions`](https://norskregnesentral.github.io/shapr/reference/explain.md) | Maximum number of coalitions passed to explain(). |
| [`n_MC_samples`](https://norskregnesentral.github.io/shapr/reference/explain.md) | Number of Monte Carlo samples passed to explain(). |
| [`min_n_batches`](https://norskregnesentral.github.io/shapr/reference/get_extra_comp_args_default.md) | Minimum batches per iteration, passed through explain(extra_computation_args = list(…)). |
| [`max_batch_size`](https://norskregnesentral.github.io/shapr/reference/get_extra_comp_args_default.md) | Maximum coalitions per batch, passed through explain(extra_computation_args = list(…)). |
| [`max_batch_cube_size`](https://norskregnesentral.github.io/shapr/reference/get_extra_comp_args_default.md) | Dense-array size limit, passed through explain(extra_computation_args = list(…)). |
| `workers` | Parallel workers configured with future::plan() before calling explain(). |
| `backend` | future backend used for parallel execution. |
| `dt_threads` | Number of data.table threads. |
| `n_train` | Number of rows in x_train. |
| `n_explain` | Number of rows in x_explain. |
| [`iterative`](https://norskregnesentral.github.io/shapr/reference/explain.md) | Whether iterative estimation was requested in explain(). |
| `Iterations` | Median number of Shapley estimation iterations. |
| [`group`](https://norskregnesentral.github.io/shapr/reference/explain.md) | Whether group-wise explanations were requested in explain(). |
| `group_size` | Number of consecutive features per generated group. |
| `model_variant` | Prediction model being explained: xgb, xgb_large, ranger, or linear. |
| `sweep` | Benchmark block that generated the configuration. |
| [`approach_args`](https://norskregnesentral.github.io/shapr/reference/explain.md) | Arguments passed through explain(…); regression_variant selects a registered regression estimator and tuning recipe. |
| `pair_key` | Identifier for a matched iterative/fixed-budget comparison. |
| `pair_role` | Role in a matched iterative/fixed-budget comparison. |
| `n` | Number of successful measured replicates. |
| `Explain time (s)` | Median runtime of explain() in seconds. |
| `Explain time IQR (s)` | Interquartile range of explain() runtime in seconds. |
| `CPU time (s)` | Median user CPU time for the parent R process in seconds. |
| `Max RAM (MB)` | Maximum peak RAM in MB across successful replicates. |
| `Load time (s)` | Median time to load the cached data and prediction model in seconds. |
| `Time (s)` | Median total runtime in seconds across successful replicates. |
| `RAM (MB)` | Median peak RAM in MB across successful replicates. |

[^1]: `workers``shapr``future::plan()``explain()``future::plan("multisession", workers = 4)``future::plan("sequential")``future::plan()`

[^2]: `min_n_batches``max_batch_size``max_batch_cube_size``extra_computation_args``list(min_n_batches = 20)`
