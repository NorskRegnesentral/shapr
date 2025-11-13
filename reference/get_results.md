# Extract Components from a Shapr Object

Extract Components from a Shapr Object

## Usage

``` r
get_results(
  x,
  what = c("calling_function", "proglang", "approach", "shapley_est", "shapley_sd",
    "pred_explain", "MSEv", "MSEv_explicand", "MSEv_coalition", "iterative_info",
    "iterative_shapley_est", "iterative_shapley_sd", "saving_path", "timing_summary",
    "timing_details", "parameters", "x_train", "x_explain", "dt_vS", "dt_samp_for_vS",
    "dt_used_coalitions", "dt_valid_causal_coalitions", "dt_coal_samp_info"),
  ...
)
```

## Arguments

- x:

  A `shapr` object

- what:

  Character vector specifying one or more components to extract.
  Options: "calling_function", "proglang", "approach", "shapley_est",
  "shapley_sd", "pred_explain", "MSEv", "MSEv_explicand",
  "MSEv_coalition", "iterative_info", "iterative_shapley_est",
  "iterative_shapley_sd", "saving_path", "timing_summary",
  "timing_details", "parameters", "x_train", "x_explain", "dt_vS",
  "dt_samp_for_vS", "dt_used_coalitions", "dt_valid_causal_coalitions",
  "dt_coal_samp_info". The default is to return all components. See
  details for what each component contains.

- ...:

  Not used

## Value

If a single component is requested, returns that object. If multiple are
requested, returns a named list.

## Details

The function extracts a full suite of information related to the
computation of the Shapley values from a `shapr` object. The allowed
characters in `what` provides information as follows:

- `calling_function`:

  Name of function called to create the `shapr` object,
  ([`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  or
  [`explain_forecast()`](https://norskregnesentral.github.io/shapr/reference/explain_forecast.md)).

- `proglang`:

  Programming language used to initiate the computations (`R` or
  `Python`).

- `approach`:

  Approach used to estimate the conditional expectations.

- `shapley_est`:

  data.table with the estimated Shapley values.

- `shapley_sd`:

  data.table with the standard deviation of the Shapley values
  reflecting the uncertainty in the coalition sampling part of the
  kernelSHAP procedure.

- `pred_explain`:

  Numeric vector with the predictions for the explained observations.

- `MSEv/MSEv_explicand/MSEv_coalition`:

  Data.tables with MSEv evaluation criterion values overall/ per
  explicand/per coalition. Smaller values indicate better estimates of
  `v(S)`. See the [MSEv evaluation section in the general usage vignette
  for
  details](https://norskregnesentral.github.io/shapr/articles/general_usage.html#msev-evaluation-criterion%0A).

- `iterative_info`:

  Data.table with information about the iterative estimation procedure.

- `iterative_shapley_est/iterative_shapley_sd`:

  Data.tables with the estimated Shapley values/their standard deviation
  for each iteration (when using the iterative estimation procedure).

- `saving_path`:

  Character string with the path where the (temporary) results are
  saved.

- `timing_summary`:

  Data.table with one row and three columns: `init_time` and `end_time`
  give the time stamps for the start and end of the computation,
  respectively, while `total_time_secs` gives the total time in seconds
  for the full computation.

- `timing_details`:

  List containing timing information for the different parts of the
  computation. `summary` contains the information from `timing_summary`.
  `overall_timing_secs` gives the time spent on the different parts of
  the explanation computation. `main_computation_timing_secs` further
  decomposes the main computation time into the different parts of the
  computation for each iteration of the iterative estimation routine, if
  used.

- `parameters`:

  List with the parameters used in the computation.

- `x_train/x_explain`:

  Data.tables with the training data used in the
  computation/observations to explain.

- `dt_vS`:

  Data.table with the contribution function (`v(S)`) estimates for each
  coalition.

- `dt_samp_for_vS`:

  Data.table with the samples used in the Monte Carlo estimation of the
  contribution function (`v(S)`). This is only available if
  `output_args_default$keep_samp_for_vS = TRUE` (defaults to FALSE) in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- `dt_used_coalitions`:

  Data.table with an overview of the coalitions used in the computation.

- `dt_valid_causal_coalitions`:

  Data.table with the valid causal coalitions used in the computation.

- `dt_coal_samp_info`:

  Data.table with information related to the coalition sampling
  procedure being used.

Note that the
[`summary.shapr()`](https://norskregnesentral.github.io/shapr/reference/summary.shapr.md)
function provides a nicely formatted printout with the most important
information, to then invisibly return the output of the present
function. The
[`print.shapr()`](https://norskregnesentral.github.io/shapr/reference/print.shapr.md)
allows direct printing of the main results.
