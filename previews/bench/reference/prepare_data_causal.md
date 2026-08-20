# Generate Data Used for Predictions and Monte Carlo Integration for Causal Shapley Values

This function loops over the given coalitions, and for each coalition it
extracts the chain of relevant sampling steps provided in
`internal$object$S_causal`. This chain can contain sampling from
marginal and conditional distributions. We use the approach given by
`internal$parameters$approach` to generate the samples from the
conditional distributions, and we iteratively call
[`prepare_data()`](https://norskregnesentral.github.io/shapr/reference/prepare_data.md)
with a modified `internal_copy` list to reuse code. However, this also
means that chains with the same conditional distributions will retrain a
model of said conditional distributions several times. For the marginal
distribution, we sample from the Gaussian marginals when the approach is
`gaussian` and from the marginals of the training data for all other
approaches. Note that we could extend the code to sample from the
marginal (gaussian) copula, too, when `approach` is `copula`.

## Usage

``` r
prepare_data_causal(internal, index_features = NULL, ...)
```

## Arguments

- internal:

  List. Not used directly, but passed through from
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md).

- index_features:

  Positive integer vector. Specifies the id_coalition to apply to the
  present method. `NULL` means all coalitions. Only used internally.

- ...:

  Currently not used.

## Value

A data.table containing simulated data that respects the (partial)
causal ordering and the the confounding assumptions. The data is used to
estimate the contribution function by Monte Carlo integration.

## Author

Lars Henry Berge Olsen
