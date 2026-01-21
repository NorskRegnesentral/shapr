# Plots of the MSEv Evaluation Criterion

Make plots to visualize and compare the MSEv evaluation criterion for a
list of
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
objects applied to the same data and model. The function creates bar
plots and line plots with points to illustrate the overall MSEv
evaluation criterion, but also for each observation/explicand and
coalition by only averaging over the coalitions and
observations/explicands, respectively.

## Usage

``` r
plot_MSEv_eval_crit(
  explanation_list,
  index_x_explain = 1:10,
  id_coalition = NULL,
  CI_level = if (length(explanation_list[[1]]$pred_explain) < 20) NULL else 0.95,
  geom_col_width = 0.9,
  plot_type = "overall"
)
```

## Arguments

- explanation_list:

  A list of
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  objects applied to the same data and model. If the entries in the list
  are named, then the function use these names. Otherwise, they default
  to the approach names (with integer suffix for duplicates) for the
  explanation objects in `explanation_list`.

- index_x_explain:

  Integer vector. Which of the test observations to plot. For example,
  if you have explained 10 observations using
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md),
  you can generate a plot for the first five observations by setting
  `index_x_explain = 1:5`. Defaults to the first 10 observations for
  `plot_type = "bar"` and `"waterfall"`, and to all observations for
  `plot_type = "scatter"` and `"beeswarm"`.

- id_coalition:

  Integer vector. Which of the coalitions to plot. E.g. if you used
  `n_coalitions = 16` in
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md),
  you can generate a plot for the first 5 coalitions and the 10th by
  setting `id_coalition = c(1:5, 10)`.

- CI_level:

  Positive numeric between zero and one. Default is `0.95` if the number
  of observations to explain is larger than 20, otherwise
  `CI_level = NULL`, which removes the confidence intervals. The level
  of the approximate confidence intervals for the overall MSEv and the
  MSEv_coalition. The confidence intervals are based on that the MSEv
  scores are means over the observations/explicands, and that means are
  approximation normal. Since the standard deviations are estimated, we
  use the quantile t from the T distribution with N_explicands - 1
  degrees of freedom corresponding to the provided level. Here,
  N_explicands is the number of observations/explicands. MSEv +/-
  t*SD(MSEv)/sqrt(N_explicands). Note that the
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  function already scales the standard deviation by sqrt(N_explicands),
  thus, the CI are MSEv \\- t*MSEv_sd, where the values MSEv and MSEv_sd
  are extracted from the MSEv data.tables in the objects in the
  `explanation_list`.

- geom_col_width:

  Numeric. Bar width. By default, set to 90% of the
  [`ggplot2::resolution()`](https://ggplot2.tidyverse.org/reference/resolution.html)
  of the data.

- plot_type:

  Character vector. The possible options are "overall" (default),
  "coalition", and "explicand". If `plot_type = "overall"`, then the
  plot (one bar plot) associated with the overall MSEv evaluation
  criterion for each method is created, i.e., when averaging over both
  the coalitions and observations/explicands. If
  `plot_type = "coalition"`, then the plots (one line plot and one bar
  plot) associated with the MSEv evaluation criterion for each coalition
  are created, i.e., when we only average over the
  observations/explicands. If `plot_type = "explicand"`, then the plots
  (one line plot and one bar plot) associated with the MSEv evaluation
  criterion for each observations/explicands are created, i.e., when we
  only average over the coalitions. If `plot_type` is a vector of one or
  several of "overall", "coalition", and "explicand", then the
  associated plots are created.

## Value

Either a single
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object of the MSEv criterion when `plot_type = "overall"`, or a list of
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
objects based on the `plot_type` parameter.

## Details

Note that in contrast to
[`plot.shapr()`](https://norskregnesentral.github.io/shapr/reference/plot.shapr.md),
`plot_MSEv_eval_crit()` always just returns the ggplot objects, i.e. no
force displaying through
[`ggplot2::print.ggplot()`](https://ggplot2.tidyverse.org/reference/print.ggplot.html).

## Author

Lars Henry Berge Olsen

## Examples

``` r
# \donttest{
if (requireNamespace("xgboost", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  # Get the data
  data("airquality")
  data <- data.table::as.data.table(airquality)
  data <- data[complete.cases(data), ]

  #' Define the features and the response
  x_var <- c("Solar.R", "Wind", "Temp", "Month")
  y_var <- "Ozone"

  # Split data into test and training data set
  ind_x_explain <- 1:25
  x_train <- data[-ind_x_explain, ..x_var]
  y_train <- data[-ind_x_explain, get(y_var)]
  x_explain <- data[ind_x_explain, ..x_var]

  # Fitting a basic xgboost model to the training data
  model <- xgboost::xgboost(
    x = x_train,
    y = y_train,
    nround = 20,
    verbosity = 0
  )

  # Specifying the phi_0, i.e. the expected prediction without any features
  phi0 <- mean(y_train)

  # Independence approach
  explanation_independence <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "independence",
    phi0 = phi0,
    n_MC_samples = 1e2
  )

  # Gaussian 1e1 approach
  explanation_gaussian_1e1 <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",
    phi0 = phi0,
    n_MC_samples = 1e1
  )

  # Gaussian 1e2 approach
  explanation_gaussian_1e2 <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "gaussian",
    phi0 = phi0,
    n_MC_samples = 1e2
  )

  # ctree approach
  explanation_ctree <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "ctree",
    phi0 = phi0,
    n_MC_samples = 1e2
  )

  # Combined approach
  explanation_combined <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = c("gaussian", "independence", "ctree"),
    phi0 = phi0,
    n_MC_samples = 1e2
  )

  # Create a list of explanations with names
  explanation_list_named <- list(
    "Ind." = explanation_independence,
    "Gaus. 1e1" = explanation_gaussian_1e1,
    "Gaus. 1e2" = explanation_gaussian_1e2,
    "Ctree" = explanation_ctree,
    "Combined" = explanation_combined
  )

  # Create the default MSEv plot where we average over both the coalitions and observations
  # with approximate 95% confidence intervals
  plot_MSEv_eval_crit(explanation_list_named, CI_level = 0.95, plot_type = "overall")

  # Can also create plots of the MSEv criterion averaged only over the coalitions or observations.
  MSEv_figures <- plot_MSEv_eval_crit(explanation_list_named,
    CI_level = 0.95,
    plot_type = c("overall", "coalition", "explicand")
  )
  MSEv_figures$MSEv_bar
  MSEv_figures$MSEv_coalition_bar
  MSEv_figures$MSEv_explicand_bar

  # When there are many coalitions or observations, then it can be easier to look at line plots
  MSEv_figures$MSEv_coalition_line_point
  MSEv_figures$MSEv_explicand_line_point

  # We can specify which observations or coalitions to plot
  plot_MSEv_eval_crit(explanation_list_named,
    plot_type = "explicand",
    index_x_explain = c(1, 3:4, 6),
    CI_level = 0.95
  )$MSEv_explicand_bar
  plot_MSEv_eval_crit(explanation_list_named,
    plot_type = "coalition",
    id_coalition = c(3, 4, 9, 13:15),
    CI_level = 0.95
  )$MSEv_coalition_bar

  # We can alter the figures if other palette schemes or design is wanted
  bar_text_n_decimals <- 1
  MSEv_figures$MSEv_bar +
    ggplot2::scale_x_discrete(limits = rev(levels(MSEv_figures$MSEv_bar$data$Method))) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_discrete() + #' Default ggplot2 palette
    ggplot2::theme_minimal() + #' This must be set before the other theme call
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 10),
      legend.position = "bottom"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1, ncol = 6)) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf(
        paste("%.", sprintf("%d", bar_text_n_decimals), "f", sep = ""),
        round(MSEv, bar_text_n_decimals)
      )),
      vjust = -1.1, # This value must be altered based on the plot dimension
      hjust = 1.1, # This value must be altered based on the plot dimension
      color = "black",
      position = ggplot2::position_dodge(0.9),
      size = 5
    )
}
#> 
#> ── Starting `shapr::explain()` at 2026-01-21 09:26:32 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <xgboost>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: independence
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 100
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 25
#> • Computations (temporary) saved at: /tmp/Rtmp3qBi8q/shapr_obj_1b4c73a5e4c5.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 
#> 
#> ── Starting `shapr::explain()` at 2026-01-21 09:26:32 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <xgboost>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: gaussian
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 10
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 25
#> • Computations (temporary) saved at: /tmp/Rtmp3qBi8q/shapr_obj_1b4c6508efaa.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 
#> 
#> ── Starting `shapr::explain()` at 2026-01-21 09:26:33 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <xgboost>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: gaussian
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 100
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 25
#> • Computations (temporary) saved at: /tmp/Rtmp3qBi8q/shapr_obj_1b4c4c575c18.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 
#> 
#> ── Starting `shapr::explain()` at 2026-01-21 09:26:33 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <xgboost>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: ctree
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 100
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 25
#> • Computations (temporary) saved at: /tmp/Rtmp3qBi8q/shapr_obj_1b4c35ff9151.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 
#> 
#> ── Starting `shapr::explain()` at 2026-01-21 09:26:35 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <xgboost>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: gaussian, independence, and ctree
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 100
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 25
#> • Computations (temporary) saved at: /tmp/Rtmp3qBi8q/shapr_obj_1b4c40e31556.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 
#> ℹ Showing 10 of 25 observations.
#> ℹ Showing 10 of 25 observations.
#> ℹ Showing 4 of 25 observations.
#> ℹ Showing 10 of 25 observations.

# }
```
