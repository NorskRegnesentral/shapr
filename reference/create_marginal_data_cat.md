# Create marginal categorical data for causal Shapley values

This function is used when we generate marginal data for the categorical
approach when we have several sampling steps. We need to treat this
separately, as we here in the marginal step CANNOT make feature values
such that the combination of those and the feature values we condition
in S are NOT in `categorical.joint_prob_dt`. If we do this, then we
cannot progress further in the chain of sampling steps. E.g., X1 in
(1,2,3), X2 in (1,2,3), and X3 in (1,2,3). We know X2 = 2, and let
causal structure be X1 -\> X2 -\> X3. Assume that P(X1 = 1, X2 = 2, X =
3) = P(X1 = 2, X2 = 2, X = 3) = 1/2. Then there is no point generating
X1 = 3, as we then cannot generate X3. The solution is only to generate
the values which can proceed through the whole chain of sampling steps.
To do that, we have to ensure the marginal sampling respects the valid
feature coalitions for all sets of conditional features, i.e., the
features in `features_steps_cond_on`. We sample from the valid
coalitions using the MARGINAL probabilities.

## Usage

``` r
create_marginal_data_cat(
  n_MC_samples,
  x_explain,
  Sbar_features,
  S_original,
  joint_prob_dt
)
```

## Arguments

- n_MC_samples:

  Positive integer. For most approaches, it indicates the maximum number
  of samples to use in the Monte Carlo integration of every conditional
  expectation. For `approach="ctree"`, `n_MC_samples` corresponds to the
  number of samples from the leaf node (see an exception related to the
  `ctree.sample` argument in
  [`setup_approach.ctree()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)).
  For `approach="empirical"`, `n_MC_samples` is the \\K\\ parameter in
  equations (14-15) of [Aas et al.
  (2021)](https://martinjullum.com/publication/aas-2021-explaining/aas-2021-explaining.pdf),
  i.e. the maximum number of observations (with largest weights) that is
  used, see also the `empirical.eta` argument
  [`setup_approach.empirical()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md).

- x_explain:

  Matrix or data.frame/data.table. Features for which predictions should
  be explained.

- Sbar_features:

  Vector of integers containing the features indices to generate
  marginal observations for. That is, if `Sbar_features` is `c(1,4)`,
  then we sample `n_MC_samples` observations from \\P(X_1, X_4)\\. That
  is, we sample the first and fourth feature values from the same valid
  feature coalition using the marginal probability, so we do not break
  the dependence between them.

- S_original:

  Vector of integers containing the features indices of the original
  coalition `S`. I.e., not the features in the current sampling step,
  but the features are known to us before starting the chain of sampling
  steps.

## Value

Data table of dimension \\(\`n_MC_samples\` \* \`nrow(x_explain)\`)
\times \`length(Sbar_features)\`\\ with the sampled observations.

## Details

For undocumented arguments, see
[`setup_approach.categorical()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md).

## Author

Lars Henry Berge Olsen
