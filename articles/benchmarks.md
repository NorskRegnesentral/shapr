# Computational cost benchmarks

## What this study measures

This benchmark study measures the **computational cost** of
[`shapr::explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md):
runtime and peak RAM. It covers 2,278 successful runs across 789
configurations and all 11 built-in estimation approaches. Each approach
is studied through controlled blocks that vary one or a few workload
dimensions around a shared baseline.

The main workload dimensions are the number of features, coalitions,
Monte Carlo samples, training observations, observations to explain,
batches, and parallel workers. The study also includes relevant
approach-specific choices, such as factor complexity, empirical
bandwidth selection, regression models, and VAEAC architecture and
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

Runtime is measured directly around
[`shapr::explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).
This timing excludes preparing the benchmark data and fitting the
models. Peak RAM is the highest memory used during the full benchmark
run, including loaded data, the fitted prediction model, and workers.
Every retained peak RAM measurement uses cgroup-v2 `memory.peak`, which
captures the full process group. To limit compute time, some of the most
expensive VAEAC, ARF, and timeseries configurations were run twice; all
other configurations were run three times.

The full design, execution framework, committed CSV files, and numerical
audit are available in the [benchmark
directory](https://github.com/NorskRegnesentral/shapr/tree/master/benchmarks)
at GitHub.

## Key findings and practical guidance

- **`approach`: choose for the data, then consider cost.** Gaussian is
  typically the fastest dependence-aware method for purely numeric data,
  although regression surrogate is faster at large `x_explain` (`100+`).
  Regression surrogate is fastest for the mixed-data settings. For fully
  categorical data, it has similar runtime to the categorical approach.
  These are cost comparisons, not rankings of explanation quality.

- **`x_explain`: approaches differ in setup cost and cost per explained
  observation.** The approaches fall into three broad cost patterns.

  1.  **Low setup, increasing cost per observation:** Gaussian, copula,
      empirical, categorical, and independence (not recommended) are
      attractive when explaining only a few observations, but their
      runtime increases as `x_explain` grows.
  2.  **High setup, low cost per observation:** For regression
      surrogate, regression separate, and VAEAC, setup costs typically
      dominate while each additional observation adds relatively little
      runtime. They can therefore become more competitive for larger
      `x_explain` (although VAEAC remains costly).
  3.  **High setup, increasing cost per observation:** Ctree, ARF, and
      timeseries combine setup costs with strong runtime growth as
      `x_explain` increases.

  When appropriate, explaining a representative sample can reduce
  runtime for methods whose cost increases with `x_explain` (category 1
  and 3).

- **`x_train`: reducing the training set can lower runtime for most
  approaches.** Runtime changes little with `x_train` for Gaussian,
  regression separate, and independence (not recommended). It increases
  more clearly for empirical, copula, categorical, Ctree, ARF,
  timeseries, VAEAC, and regression surrogate. Subsampling `x_train` can
  therefore reduce runtime for these approaches (when statistically
  appropriate).

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
  balance peak RAM against runtime.**

  - **RAM:** Smaller batches generally reduce peak RAM by limiting how
    much intermediate data is held at once. For Gaussian, copula, and
    empirical, `max_batch_cube_size` only affects large dense workloads,
    where it can automatically create smaller batches; relaxing the
    default limit can sharply increase RAM.
  - **Runtime:** The cost of smaller batches depends on the approach.
    Using smaller batches adds little to modest overhead for Gaussian,
    Ctree, and ARF, but can be costly for empirical, timeseries, and
    VAEAC. Parallel runs also need enough batches to keep workers busy.

  Start with the defaults, then use smaller batches when memory is
  limiting or larger batches when repeated batch overhead dominates.

- **`iterative`: use when no reasonable coalition budget is known.** At
  the same final coalition budget, iterative execution costs about the
  same as a fixed run if it stops after one iteration and more if it
  needs several. It is most useful when not even a rough fixed budget is
  known; otherwise, prefer a fixed run.

- **`max_n_coalitions` and `x_explain`: the most consistent generic cost
  controls.** `n_MC_samples` can also matter, but training, fitting, or
  data-search costs dominate it for some approaches. Feature count,
  training size, and factor complexity can also be important, but their
  effects vary more by approach (see below).

- **`model` and `n_MC_samples`: prediction cost can amplify sampling
  cost.** Model prediction is repeated across coalitions and conditional
  samples. For slow prediction models, reducing `n_MC_samples` may
  therefore shorten runtime considerably; for fast models, other parts
  of the computations may dominate.

- **Approach-specific choices and execution settings can dominate
  generic controls.** Examples include empirical AICc bandwidth
  selection, tuning of the regression model in regression separate,
  VAEAC training, many small batches for timeseries or VAEAC, and
  parallel ARF.

## Additional guidance by approach

The controls described above, including `max_n_coalitions`,
`n_MC_samples`, `x_explain`, `x_train`, workers, and batching, apply
relatively broadly across approaches. This section highlights
*additional* approach-specific controls and important constraints that
affect how those controls should be used. Reducing `max_n_coalitions`,
`n_MC_samples`, `x_train`, or model complexity can affect the resulting
explanations, so verify that the explanation accuracy and quality remain
adequate. Reducing `x_explain` is not always an option because every
observation may need to be explained. Nevertheless, knowing which
approaches are sensitive to `x_explain` is useful for runtime planning.
Pass approach-specific arguments to
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
through `...`; see the
[manual](https://norskregnesentral.github.io/shapr/reference/explain.html#arg--).

[TABLE]

## Explore all results

The table below combines the benchmark results for the various
approaches. Each row is one configuration aggregated over its
replicates. The retained experiments cover 2, 4, 6, 8, 10, 12, 16, 20,
25, 30 features, with available values varying by approach. Use the
controls above each column to filter: categorical controls accept one or
more values, and sliders for experiment inputs snap to values present in
the results. Click headings to sort, and use **Column visibility** to
focus on relevant fields. Runtime and peak RAM remain fixed on the right
while scrolling horizontally; runtime IQR is available under **Column
visibility**. **CSV** downloads the currently displayed table.

The default view shows the main workload inputs, approach-specific
settings, replicate count, runtime, and peak RAM. Less frequently needed
experiment details remain available under **Column visibility**. Hover
over any column name for its description.

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
| [`approach_args`](https://norskregnesentral.github.io/shapr/reference/explain.md) | Arguments passed through explain(…); regression_variant selects a registered regression estimator and tuning recipe. [See the regression variant reference.](https://github.com/NorskRegnesentral/shapr/blob/master/benchmarks/README.md#regression-variants) |
| `pair_key` | Identifier for a matched iterative/fixed-budget comparison. |
| `pair_role` | Role in a matched iterative/fixed-budget comparison. |
| `n` | Number of successful measured replicates. |
| `Runtime IQR (s)` | Interquartile range of explain() runtimes across replicates, in seconds. |
| `Runtime (s)` | Median runtime of explain() across replicates, in seconds. |
| `Peak RAM (MiB)` | Median peak RAM used during the full benchmark run, including loaded data, the fitted prediction model, and workers; 1 MiB = 1.0486 MB. |

[^1]: `workers``shapr``future::plan()``explain()``future::plan("multisession", workers = 4)``future::plan("sequential")``future::plan()`

[^2]: `min_n_batches``max_batch_size``max_batch_cube_size``extra_computation_args``list(min_n_batches = 20)`
