# Function to specify arguments of the iterative estimation procedure

Function to specify arguments of the iterative estimation procedure

## Usage

``` r
get_iterative_args_default(
  internal,
  initial_n_coalitions = ceiling(min(200, max(5, internal$parameters$n_features,
    (2^internal$parameters$n_features)/10), internal$parameters$max_n_coalitions)),
  fixed_n_coalitions_per_iter = NULL,
  max_iter = 20,
  convergence_tol = 0.02,
  n_coal_next_iter_factor_vec = c(seq(0.1, 1, by = 0.1), rep(1, max_iter - 10))
)
```

## Arguments

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- initial_n_coalitions:

  Integer. Number of coalitions to use in the first estimation
  iteration.

- fixed_n_coalitions_per_iter:

  Integer. Number of `n_coalitions` to use in each iteration. `NULL`
  (default) means setting it based on estimates based on a set
  convergence threshold.

- max_iter:

  Integer. Maximum number of estimation iterations

- convergence_tol:

  Numeric. The t variable in the convergence threshold formula on page 6
  in the paper Covert and Lee (2021), 'Improving KernelSHAP: Practical
  Shapley Value Estimation via Linear Regression'
  https://arxiv.org/pdf/2012.01536. Smaller values requires more
  coalitions before convergence is reached.

- n_coal_next_iter_factor_vec:

  Numeric vector. The number of `n_coalitions` that must be used to
  reach convergence in the next iteration is estimated. The number of
  `n_coalitions` actually used in the next iteration is set to this
  estimate multiplied by `n_coal_next_iter_factor_vec[i]` for iteration
  `i`. It is wise to start with smaller numbers to avoid using too many
  `n_coalitions` due to uncertain estimates in the first iterations.

## Value

A list with the default values for the iterative estimation procedure

## Details

The functions sets default values for the iterative estimation
procedure, according to the function defaults. If the argument
`iterative` of
[`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
is FALSE, it sets parameters corresponding to the use of a non-iterative
estimation procedure

## Author

Martin Jullum
