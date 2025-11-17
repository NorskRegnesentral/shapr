# Shapley Value Bar Plots for Several Explanation Objects

Make plots to visualize and compare the estimated Shapley values for a
list of
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
objects applied to the same data and model. For group-wise Shapley
values, the features values plotted are the mean feature values for all
features in each group.

## Usage

``` r
plot_SV_several_approaches(
  explanation_list,
  index_explicands = NULL,
  index_explicands_sort = FALSE,
  only_these_features = NULL,
  plot_phi0 = FALSE,
  digits = 4,
  print_ggplot = TRUE,
  add_zero_line = FALSE,
  axis_labels_n_dodge = NULL,
  axis_labels_rotate_angle = NULL,
  horizontal_bars = TRUE,
  facet_scales = "free",
  facet_ncol = 2,
  geom_col_width = 0.85,
  brewer_palette = NULL,
  include_group_feature_means = FALSE
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

- index_explicands:

  Integer vector. Which of the explicands (test observations) to plot.
  E.g. if you have explained 10 observations using
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md),
  you can generate a plot for the first 5 observations/explicands and
  the 10th by setting `index_x_explain = c(1:5, 10)`. The argument
  `index_explicands_sort` must be `FALSE` to plot the explicand in the
  order specified in `index_x_explain`.

- index_explicands_sort:

  Boolean. If `FALSE` (default), then `shapr` plots the explicands in
  the order specified in `index_explicands`. If `TRUE`, then `shapr`
  sort the indices in increasing order based on their id.

- only_these_features:

  String vector. Containing the names of the features which are to be
  included in the bar plots.

- plot_phi0:

  Boolean. If we are to include the \\\phi_0\\ in the bar plots or not.

- digits:

  Integer. Number of significant digits to use in the feature
  description. Applicable for `plot_type` `"bar"` and `"waterfall"`

- print_ggplot:

  Logical. Whether to print the created `ggplot` object once it is
  returned. The default is `TRUE` which ensures the plot is always
  displayed also in loops, functions, when sourcing a script, and when
  assigning the output to a variable like `p <- plot.shapr(...)`. See
  [`ggplot2::print.ggplot()`](https://ggplot2.tidyverse.org/reference/print.ggplot.html)
  for more details. If you wish to further modify the returned `ggplot`
  object outside of `plot.shapr`, we recommend setting
  `print_ggplot = FALSE` to avoid force printing. See the examples for a
  practical use case.

- add_zero_line:

  Boolean. If we are to add a black line for a feature contribution of
  0.

- axis_labels_n_dodge:

  Integer. The number of rows that should be used to render the labels.
  This is useful for displaying labels that would otherwise overlap.

- axis_labels_rotate_angle:

  Numeric. The angle of the axis label, where 0 means horizontal, 45
  means tilted, and 90 means vertical. Compared to setting the angle in
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html)
  /
  [`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
  this also uses some heuristics to automatically pick the `hjust` and
  `vjust` that you probably want.

- horizontal_bars:

  Boolean. Flip Cartesian coordinates so that horizontal becomes
  vertical, and vertical, horizontal. This is primarily useful for
  converting geoms and statistics which display y conditional on x, to x
  conditional on y. See
  [`ggplot2::coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html).

- facet_scales:

  Should scales be free ("`free`", the default), fixed ("`fixed`"), or
  free in one dimension ("`free_x`", "`free_y`")? The user has to change
  the latter manually depending on the value of `horizontal_bars`.

- facet_ncol:

  Integer. The number of columns in the facet grid. Default is
  `facet_ncol = 2`.

- geom_col_width:

  Numeric. Bar width. By default, set to 85% of the
  [`ggplot2::resolution()`](https://ggplot2.tidyverse.org/reference/resolution.html)
  of the data.

- brewer_palette:

  String. Name of one of the color palettes from
  [`RColorBrewer::RColorBrewer()`](https://rdrr.io/pkg/RColorBrewer/man/ColorBrewer.html).
  If `NULL`, then the function uses the default
  [`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
  color scheme. The following palettes are available for use with these
  scales:

  Diverging

  :   BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral

  Qualitative

  :   Accent, Dark2, Paired, Pastel1, Pastel2, Set1, Set2, Set3

  Sequential

  :   Blues, BuGn, BuPu, GnBu, Greens, Greys, Oranges, OrRd, PuBu,
      PuBuGn, PuRd, Purples, RdPu, Reds, YlGn, YlGnBu, YlOrBr, YlOrRd

- include_group_feature_means:

  Logical. Whether to include the average feature value in a group on
  the y-axis or not. If `FALSE` (default), then no value is shown for
  the groups. If `TRUE`, then `shapr` includes the mean of the features
  in each group.

## Value

A
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object.

## Author

Lars Henry Berge Olsen

## Examples

``` r
if (FALSE) { # \dontrun{
if (requireNamespace("xgboost", quietly = TRUE) && requireNamespace("ggplot2", quietly = TRUE)) {
  # Get the data
  data("airquality")
  data <- data.table::as.data.table(airquality)
  data <- data[complete.cases(data), ]

  # Define the features and the response
  x_var <- c("Solar.R", "Wind", "Temp", "Month")
  y_var <- "Ozone"

  # Split data into test and training data set
  ind_x_explain <- 1:12
  x_train <- data[-ind_x_explain, ..x_var]
  y_train <- data[-ind_x_explain, get(y_var)]
  x_explain <- data[ind_x_explain, ..x_var]

  # Fitting a basic xgboost model to the training data
  model <- xgboost::xgboost(
    data = as.matrix(x_train),
    label = y_train,
    nround = 20,
    verbose = FALSE
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

  # Empirical approach
  explanation_empirical <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = "empirical",
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

  # Combined approach
  explanation_combined <- explain(
    model = model,
    x_explain = x_explain,
    x_train = x_train,
    approach = c("gaussian", "ctree", "empirical"),
    phi0 = phi0,
    n_MC_samples = 1e2
  )

  # Create a list of explanations with names
  explanation_list <- list(
    "Ind." = explanation_independence,
    "Emp." = explanation_empirical,
    "Gaus. 1e1" = explanation_gaussian_1e1,
    "Gaus. 1e2" = explanation_gaussian_1e2,
    "Combined" = explanation_combined
  )

  # The function uses the provided names.
  plot_SV_several_approaches(explanation_list)

  # We can change the number of columns in the grid of plots and add other visual alterations
  # Set `print_ggplot = FALSE` to avoid force displaying the ggplot object before the modifications
  # outside plot_SV_several_approaches()

  plot_SV_several_approaches(explanation_list,
    facet_ncol = 3,
    facet_scales = "free_y",
    add_zero_line = TRUE,
    digits = 2,
    brewer_palette = "Paired",
    geom_col_width = 0.6,
    print_ggplot = FALSE
  ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 10))


  # We can specify which explicands to plot to get less chaotic plots and make the bars vertical
  plot_SV_several_approaches(explanation_list,
    index_explicands = c(1:2, 5, 10),
    horizontal_bars = FALSE,
    axis_labels_rotate_angle = 45
  )

  # We can change the order of the features by specifying the
  # order using the `only_these_features` parameter.
  plot_SV_several_approaches(explanation_list,
    index_explicands = c(1:2, 5, 10),
    only_these_features = c("Temp", "Solar.R", "Month", "Wind")
  )

  # We can also remove certain features if we are not interested in them
  # or want to focus on, e.g., two features. The function will give a
  # message to if the user specifies non-valid feature names.
  plot_SV_several_approaches(explanation_list,
    index_explicands = c(1:2, 5, 10),
    only_these_features = c("Temp", "Solar.R"),
    plot_phi0 = TRUE
  )

}
} # }
```
