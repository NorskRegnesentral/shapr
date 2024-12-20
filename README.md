
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shapr <img src="man/figures/NR-logo_utvidet_r32g60b136_small.png" align="right" height="50px"/>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/shapr)](https://cran.r-project.org/package=shapr)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/grand-total/shapr)](https://cran.r-project.org/package=shapr)
[![R build
status](https://github.com/NorskRegnesentral/shapr/workflows/R-CMD-check/badge.svg)](https://github.com/NorskRegnesentral/shapr/actions?query=workflow%3AR-CMD-check)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.02027/status.svg)](https://doi.org/10.21105/joss.02027)
<!-- badges: end -->

## Brief NEWS

This is `shapr` version 1.0.0 (Released on GitHub Nov 2024), which
provides a full restructuring of the code based, and provides a full
suit of new functionality, including:

- A long list of approaches for estimating the contribution/value
  function $v(S)$, including Variational Autoencoders, and
  regression-based methods
- Iterative Shapley value estimation with convergence detection
- Parallelized computations with progress updates
- Reweighted Kernel SHAP for faster convergence
- New function `explain_forecast()` for explaining forecasts
- Several other methodological, computational and user-experience
  improvements
- Python wrapper making the core functionality of `shapr` available in
  Python

Below we provide a brief overview of the breaking changes. See the
[NEWS](https://github.com/NorskRegnesentral/shapr/blob/master/NEWS.md)
for the full list of details.

### Breaking changes

The new syntax for explaining models essentially amounts to using a
single function (`explain()`) instead of two functions (`shapr()` and
`explain()`). In addition, custom models are now explained by passing
the prediction function directly to `explain()`, some input arguments
got new names, and a few functions for edge cases was removed to
simplify the code base.

Note that the CRAN version of `shapr` (v0.2.2) still uses the old
syntax. The examples below uses the new syntax.
[Here](https://github.com/NorskRegnesentral/shapr/blob/cranversion_0.2.2/README.md)
is a version of this README with the syntax of the CRAN version
(v0.2.2).

### Python wrapper

We now also provide a Python wrapper (`shaprpy`) which allows explaining
python models with the methodology implemented in `shapr`, directly from
Python. The wrapper is available
[here](https://github.com/NorskRegnesentral/shapr/tree/master/python).

## The package

The `shapr` R package implements an enhanced version of the Kernel SHAP
method, for approximating Shapley values, with a strong focus on
conditional Shapley values. The core idea is to remain completely
model-agnostic while offering a variety of methods for estimating
contribution functions, enabling accurate computation of conditional
Shapley values across different feature types, dependencies, and
distributions. The package also includes evaluation metrics to compare
various approaches. With features like parallelized computations,
convergence detection, progress updates, and extensive plotting options,
shapr is as a highly efficient and user-friendly tool, delivering
precise estimates of conditional Shapley values, which are critical for
understanding how features truly contribute to predictions.

A basic example is provided below. Otherwise we refer to the [pkgdown
website](https://norskregnesentral.github.io/shapr/) and the vignettes
there  
for details and further examples.

## Installation

We highly recommend to install the development version of shapr (with
the new explanation syntax and all functionality),

``` r
remotes::install_github("NorskRegnesentral/shapr")
```

To also install all dependencies, use

``` r
remotes::install_github("NorskRegnesentral/shapr", dependencies = TRUE)
```

**The CRAN version of `shapr` (NOT RECOMMENDED) can be installed with**

``` r
install.packages("shapr")
```

## Example

`shapr` supports computation of Shapley values with any predictive model
which takes a set of numeric features and produces a numeric outcome.

The following example shows how a simple `xgboost` model is trained
using the *airquality* dataset, and how `shapr` explains the individual
predictions.

We first enable parallel computation and progress updates with the
following code chunk. These are optional, but recommended for improved
performance and user friendliness, particularly for problems with many
features.

``` r
# Enable parallel computation
# Requires the future and future_lapply packages
future::plan("multisession", workers = 2) # Increase the number of workers for increased performance with many features

# Enable progress updates of the v(S)-computations
# Requires the progressr package
progressr::handlers(global = TRUE)
handlers("cli") # Using the cli package as backend (recommended for the estimates of the remaining time)
```

Here comes the actual example

``` r
library(xgboost)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Looking at the dependence between the features
cor(x_train)
#>            Solar.R       Wind       Temp      Month
#> Solar.R  1.0000000 -0.1243826  0.3333554 -0.0710397
#> Wind    -0.1243826  1.0000000 -0.5152133 -0.2013740
#> Temp     0.3333554 -0.5152133  1.0000000  0.3400084
#> Month   -0.0710397 -0.2013740  0.3400084  1.0000000

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the actual Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  phi0 = p0
)
#> Note: Feature classes extracted from the model contains NA.
#> Assuming feature classes from the data are correct.
#> Success with message:
#> max_n_coalitions is NULL or larger than or 2^n_features = 16, 
#> and is therefore set to 2^n_features = 16.
#> 
#> ── Starting `shapr::explain()` at 2024-11-20 12:23:18 ──────────────────────────
#> • Model class: <xgb.Booster>
#> • Approach: empirical
#> • Iterative estimation: FALSE
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 6
#> • Computations (temporary) saved at:
#> '/tmp/Rtmp4yBCHY/shapr_obj_17459f7fdc4b8f.rds'
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions.

# Printing the Shapley values for the test data.
# For more information about the interpretation of the values in the table, see ?shapr::explain.
print(explanation$shapley_values_est)
#>    explain_id     none    Solar.R      Wind      Temp      Month
#>         <int>    <num>      <num>     <num>     <num>      <num>
#> 1:          1 43.08571 13.2117337  4.785645 -25.57222  -5.599230
#> 2:          2 43.08571 -9.9727747  5.830694 -11.03873  -7.829954
#> 3:          3 43.08571 -2.2916185 -7.053393 -10.15035  -4.452481
#> 4:          4 43.08571  3.3254595 -3.240879 -10.22492  -6.663488
#> 5:          5 43.08571  4.3039571 -2.627764 -14.15166 -12.266855
#> 6:          6 43.08571  0.4786417 -5.248686 -12.55344  -6.645738

# Finally we plot the resulting explanations
plot(explanation)
```

<img src="man/figures/README-basic_example-1.png" width="100%" />

See the
[vignette](https://norskregnesentral.github.io/shapr/articles/understanding_shapr.html)
for further basic usage examples.

## Contribution

All feedback and suggestions are very welcome. Details on how to
contribute can be found
[here](https://norskregnesentral.github.io/shapr/CONTRIBUTING.html). If
you have any questions or comments, feel free to open an issue
[here](https://github.com/NorskRegnesentral/shapr/issues).

Please note that the ‘shapr’ project is released with a [Contributor
Code of
Conduct](https://norskregnesentral.github.io/shapr/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## References
