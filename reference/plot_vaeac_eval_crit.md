# Plot the training VLB and validation IWAE for `vaeac` models

This function makes
([`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html))
figures of the training VLB and the validation IWAE for a list of
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
objects with `approach = "vaeac"`. See
[`setup_approach()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)
for more information about the `vaeac` approach. Two figures are
returned by the function. In the figure, each object in
`explanation_list` gets its own facet, while in the second figure, we
plot the criteria in each facet for all objects.

## Usage

``` r
plot_vaeac_eval_crit(
  explanation_list,
  plot_from_nth_epoch = 1,
  plot_every_nth_epoch = 1,
  criteria = c("VLB", "IWAE"),
  plot_type = c("method", "criterion"),
  facet_wrap_scales = "fixed",
  facet_wrap_ncol = NULL
)
```

## Arguments

- explanation_list:

  A list of
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  objects applied to the same data, model, and `vaeac` must be the used
  approach. If the entries in the list is named, then the function use
  these names. Otherwise, it defaults to the approach names (with
  integer suffix for duplicates) for the explanation objects in
  `explanation_list`.

- plot_from_nth_epoch:

  Integer. If we are only plot the results form the nth epoch and so
  forth. The first epochs can be large in absolute value and make the
  rest of the plot difficult to interpret.

- plot_every_nth_epoch:

  Integer. If we are only to plot every nth epoch. Usefully to
  illustrate the overall trend, as there can be a lot of fluctuation and
  oscillation in the values between each epoch.

- criteria:

  Character vector. The possible options are "VLB", "IWAE",
  "IWAE_running". Default is the first two.

- plot_type:

  Character vector. The possible options are "method" and "criterion".
  Default is to plot both.

- facet_wrap_scales:

  String. Should the scales be fixed ("`fixed`", the default), free
  ("`free`"), or free in one dimension ("`free_x`", "`free_y`").

- facet_wrap_ncol:

  Integer. Number of columns in the facet wrap.

## Value

Either a single
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object or a list of
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
objects based on the `plot_type` parameter.

## Details

See [Olsen et al.
(2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf) or the
[blog post](https://borea17.github.io/paper_summaries/iwae/) for a
summary of the VLB and IWAE.

## References

- [Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using
  Shapley values and variational autoencoders to explain predictive
  models with dependent mixed features. Journal of machine learning
  research, 23(213),
  1-51](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf)

## Author

Lars Henry Berge Olsen

## Examples

``` r
# \donttest{

if (requireNamespace("xgboost", quietly = TRUE) &&
  requireNamespace("torch", quietly = TRUE) &&
  torch::torch_is_installed()) {
  data("airquality")
  data <- data.table::as.data.table(airquality)
  data <- data[complete.cases(data), ]

  x_var <- c("Solar.R", "Wind", "Temp", "Month")
  y_var <- "Ozone"

  ind_x_explain <- 1:6
  x_train <- data[-ind_x_explain, ..x_var]
  y_train <- data[-ind_x_explain, get(y_var)]
  x_explain <- data[ind_x_explain, ..x_var]

  # Fitting a basic xgboost model to the training data
  model <- xgboost::xgboost(
    x = x_train,
    y = y_train,
    nround = 100,
    verbosity = 0
  )

  # Specifying the phi_0, i.e. the expected prediction without any features
  p0 <- mean(y_train)

  # Train vaeac with and without paired sampling
  explanation_paired <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "vaeac",
    phi0 = p0,
    n_MC_samples = 1, # As we are only interested in the training of the vaeac
    vaeac.epochs = 10, # Should be higher in applications.
    vaeac.n_vaeacs_initialize = 1,
    vaeac.width = 16,
    vaeac.depth = 2,
    vaeac.extra_parameters = list(vaeac.paired_sampling = TRUE)
  )

  explanation_regular <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "vaeac",
    phi0 = p0,
    n_MC_samples = 1, # As we are only interested in the training of the vaeac
    vaeac.epochs = 10, # Should be higher in applications.
    vaeac.width = 16,
    vaeac.depth = 2,
    vaeac.n_vaeacs_initialize = 1,
    vaeac.extra_parameters = list(vaeac.paired_sampling = FALSE)
  )

  # Collect the explanation objects in an named list
  explanation_list <- list(
    "Regular sampling" = explanation_regular,
    "Paired sampling" = explanation_paired
  )

  # Call the function with the named list, will use the provided names
  plot_vaeac_eval_crit(explanation_list = explanation_list)

  # The function also works if we have only one method,
  # but then one should only look at the method plot.
  plot_vaeac_eval_crit(
    explanation_list = explanation_list[2],
    plot_type = "method"
  )

  # Can alter the plot
  plot_vaeac_eval_crit(
    explanation_list = explanation_list,
    plot_from_nth_epoch = 2,
    plot_every_nth_epoch = 2,
    facet_wrap_scales = "free"
  )

  # If we only want the VLB
  plot_vaeac_eval_crit(
    explanation_list = explanation_list,
    criteria = "VLB",
    plot_type = "criterion"
  )

  # If we want only want the criterion version
  tmp_fig_criterion <-
    plot_vaeac_eval_crit(explanation_list = explanation_list, plot_type = "criterion")

  # Since tmp_fig_criterion is a ggplot2 object, we can alter it
  # by, e.g,. adding points or smooths with se bands
  tmp_fig_criterion + ggplot2::geom_point(shape = "circle", size = 1, ggplot2::aes(col = Method))
  tmp_fig_criterion$layers[[1]] <- NULL
  tmp_fig_criterion + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
    ggplot2::scale_color_brewer(palette = "Set1") +
    ggplot2::theme_minimal()
}
#> 
#> ── Starting `shapr::explain()` at 2026-01-07 20:20:52 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <xgboost>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: vaeac
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 1
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 6
#> • Computations (temporary) saved at: /tmp/RtmppfCLKb/shapr_obj_24c74c2d1c3c.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 
#> 
#> ── Starting `shapr::explain()` at 2026-01-07 20:21:04 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <xgboost>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: vaeac
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 1
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 6
#> • Computations (temporary) saved at: /tmp/RtmppfCLKb/shapr_obj_24c7662893ab.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 

# }
```
