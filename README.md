
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shapr <img src="man/figures/nrlogo_new.jpg" align="right" height="139"/>

<!-- badges: start -->

[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version-last-release/shapr)](https://cran.r-project.org/package=shapr)
[![CRAN_Downloads_Badge](https://cranlogs.r-pkg.org/badges/grand-total/shapr)](https://cran.r-project.org/package=shapr)
[![R build
status](https://github.com/NorskRegnesentral/shapr/workflows/R-CMD-check/badge.svg)](https://github.com/NorskRegnesentral/shapr/actions?query=workflow%3AR-CMD-check)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/license/mit)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.02027/status.svg)](https://doi.org/10.21105/joss.02027)

See the pkgdown site at
[norskregnesentral.github.io/shapr/](https://norskregnesentral.github.io/shapr/)
for a complete introduction with examples and documentation of the
package. <!-- badges: end -->

## NEWS

With `shapr` version 1.0.0 (GitHub only, Nov 2024) and version 1.0.1
(CRAN, Jan 2025), the package was subject to a major update, providing a
full restructuring of the code based, and a full suit of new
functionality, including:

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

See the
[NEWS](https://github.com/NorskRegnesentral/shapr/blob/master/NEWS.md)
for a complete list.

### Coming from shapr \< 1.0.0?

`shapr` version \>= 1.0.0 comes with a number of breaking changes. Most
notably, we moved from using two function (`shapr()` and `explain()`) to
a single function (`explain()`). In addition, custom models are now
explained by passing the prediction function directly to `explain()`,
quite a few input arguments got new names, and a few functions for edge
cases was removed to simplify the code base.

Click
[here](https://github.com/NorskRegnesentral/shapr/blob/cranversion_0.2.2/README.md)
to view a version of this README with old syntax (v0.2.2).

### Python wrapper

We provide an (experimental) Python wrapper (`shaprpy`) which allows
explaining Python models with the methodology implemented in `shapr`,
directly from Python. The wrapper calls `R` internally, and therefore
requires an installation of `R`. See
[here](https://norskregnesentral.github.io/shapr/shaprpy.html) for
installation instructions and examples.

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
website](https://norskregnesentral.github.io/shapr/) and the different
vignettes there for details and further examples.

## Installation

`shapr` is available on [CRAN](https://cran.r-project.org/package=shapr)
and can be installed in R as:

``` r
install.packages("shapr")
```

To install the development version of `shapr`, available on GitHub, use

``` r
remotes::install_github("NorskRegnesentral/shapr")
```

To also install all dependencies, use

``` r
remotes::install_github("NorskRegnesentral/shapr", dependencies = TRUE)
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
progressr::handlers("cli") # Using the cli package as backend (recommended for the estimates of the remaining time)
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

# Computing the Shapley values with kernelSHAP accounting for feature dependence using
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
#> ── Starting `shapr::explain()` at 2025-03-26 06:47:04 ──────────────────────────
#> • Model class: <xgb.Booster>
#> • Approach: empirical
#> • Iterative estimation: FALSE
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 6
#> • Computations (temporary) saved at:
#> '/tmp/RtmpwFUqgs/shapr_obj_150f03bfd7b8.rds'
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

See the [general usage
vignette](https://norskregnesentral.github.io/shapr/articles/general_usage.html)
for further basic usage examples and brief introductions to the
methodology. For more thorough information about the underlying
methodology, see Aas, Jullum, and Løland (2021), Redelmeier, Jullum, and
Aas (2020), Jullum, Redelmeier, and Aas (2021), Olsen et al. (2022),
Olsen et al. (2024) . See also Sellereite and Jullum (2019) for a brief
paper about the previous (\< 1.0.0) version of the package.

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

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-aas2019explaining" class="csl-entry">

Aas, Kjersti, Martin Jullum, and Anders Løland. 2021. “Explaining
Individual Predictions When Features Are Dependent: More Accurate
Approximations to Shapley Values.” *Artificial Intelligence* 298.

</div>

<div id="ref-jullum2021efficient" class="csl-entry">

Jullum, Martin, Annabelle Redelmeier, and Kjersti Aas. 2021. “Efficient
and Simple Prediction Explanations with groupShapley: A Practical
Perspective.” In *Proceedings of the 2nd Italian Workshop on Explainable
Artificial Intelligence*, 28–43. CEUR Workshop Proceedings.

</div>

<div id="ref-olsen2022using" class="csl-entry">

Olsen, Lars Henry Berge, Ingrid Kristine Glad, Martin Jullum, and
Kjersti Aas. 2022. “Using Shapley Values and Variational Autoencoders to
Explain Predictive Models with Dependent Mixed Features.” *Journal of
Machine Learning Research* 23 (213): 1–51.

</div>

<div id="ref-olsen2024comparative" class="csl-entry">

———. 2024. “A Comparative Study of Methods for Estimating Model-Agnostic
Shapley Value Explanations.” *Data Mining and Knowledge Discovery*,
1–48.

</div>

<div id="ref-redelmeier2020explaining" class="csl-entry">

Redelmeier, Annabelle, Martin Jullum, and Kjersti Aas. 2020. “Explaining
Predictive Models with Mixed Features Using Shapley Values and
Conditional Inference Trees.” In *International Cross-Domain Conference
for Machine Learning and Knowledge Extraction*, 117–37. Springer.

</div>

<div id="ref-sellereite2019shapr" class="csl-entry">

Sellereite, N., and M. Jullum. 2019. “Shapr: An r-Package for Explaining
Machine Learning Models with Dependence-Aware Shapley Values.” *Journal
of Open Source Software* 5 (46): 2027.
<https://doi.org/10.21105/joss.02027>.

</div>

</div>
