# Summary Method for Shapr Objects

Provides a formatted summary of a shapr object and returns an object of
class `summary.shapr` containing the same information as returned by
[`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md).

## Usage

``` r
# S3 method for class 'shapr'
summary(object, digits = 2L, ...)
```

## Arguments

- object:

  A shapr object.

- digits:

  Integer. (Maximum) number of digits to be displayed after the decimal
  point. Defaults to 2.

- ...:

  Currently unused.

## Value

An object of class `summary.shapr`, which is a named list with the same
accessible components as returned by
[`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md).
See
[`get_results()`](https://norskregnesentral.github.io/shapr/reference/get_results.md)
for details about each component.

## Examples

``` r
# \donttest{
# Load example data
data("airquality")
airquality <- airquality[complete.cases(airquality), ]
x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

# Split data into test and training data
data_train <- head(airquality, -3)
data_explain <- tail(airquality, 3)

x_train <- data_train[, x_var]
x_explain <- data_explain[, x_var]

# Fit a linear model
lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
model <- lm(lm_formula, data = data_train)

# Explain predictions
p <- mean(data_train[, y_var])

explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  phi0 = p,
  n_MC_samples = 1e2
)
#> 
#> ── Starting `shapr::explain()` at 2026-01-21 09:09:09 ──────────────────────────
#> ℹ `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is
#>   therefore set to `2^n_features = 16`.
#> 
#> ── Explanation overview ──
#> 
#> • Model class: <lm>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: gaussian
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 100
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 3
#> • Computations (temporary) saved at: /tmp/RtmpTOyEbq/shapr_obj_1b4473640d4b.rds
#> 
#> ── Main computation started ──
#> 
#> ℹ Using 16 of 16 coalitions. 

# Call summary without assignment - prints formatted output to console
summary(explanation)
#> 
#> ── Summary of Shapley value explanation ────────────────────────────────────────
#> • Computed with `shapr::explain()` in 0.4 seconds, started 2026-01-21 09:09:09
#> • Model class: <lm>
#> • v(S) estimation class: Monte Carlo integration
#> • Approach: gaussian
#> • Procedure: Non-iterative
#> • Number of Monte Carlo integration samples: 100
#> • Number of feature-wise Shapley values: 4
#> • Number of observations to explain: 3
#> • Number of coalitions used: 16 (of total 16)
#> • Computations (temporary) saved at: /tmp/RtmpTOyEbq/shapr_obj_1b4473640d4b.rds
#> 
#> ── Estimated Shapley values 
#>    explain_id   none Solar.R   Wind   Temp  Month
#>         <int> <char>  <char> <char> <char> <char>
#> 1:          1  42.79    0.88 -20.18  -5.51  -0.23
#> 2:          2  42.79   -4.68   7.90  -8.16  -0.10
#> 3:          3  42.79    4.11  -5.62 -24.44  -1.16
#> 
#> ── Estimated MSEv 
#> Estimated MSE of v(S) = 240 (with sd = 90)

# Assign to variable - returns shapr.summary with summary information for later use
expl_summary <- summary(explanation) # print(expl_summary) provides the formatted output

# Access components from the summary object
expl_summary$shapley_est # Estimated Shapley values
#>    explain_id     none    Solar.R       Wind       Temp       Month
#>         <int>    <num>      <num>      <num>      <num>       <num>
#> 1:          1 42.78704  0.8824554 -20.181070  -5.509617 -0.22639602
#> 2:          2 42.78704 -4.6755745   7.898933  -8.157233 -0.09667692
#> 3:          3 42.78704  4.1090449  -5.619535 -24.442822 -1.16106366
expl_summary$timing_summary$total_time_secs # Total computation time
#> [1] 0.4466596
expl_summary$approach # Approach used
#> [1] "gaussian"
# }
```
