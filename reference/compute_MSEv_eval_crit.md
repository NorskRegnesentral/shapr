# Mean squared error of the contribution function `v(S)`

Compute the mean squared error (MSEv) of the contribution function v(S)
as proposed by [Frye et al. (2019)](https://arxiv.org/pdf/2006.01272)
and used by [Olsen et al.
(2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

## Usage

``` r
compute_MSEv_eval_crit(
  internal,
  dt_vS,
  MSEv_uniform_comb_weights,
  MSEv_skip_empty_full_comb = TRUE
)
```

## Arguments

- internal:

  List. Holds all parameters, data, functions and computed objects used
  within
  [`explain()`](https://norskregnesentral.github.io/shapr/reference/explain.md)
  The list contains one or more of the elements `parameters`, `data`,
  `objects`, `iter_list`, `timing_list`, `main_timing_list`, `output`,
  and `iter_timing_list`.

- dt_vS:

  Data.table of dimension `n_coalitions` times `n_explain + 1`
  containing the contribution function estimates. The first column is
  assumed to be named `id_coalition` and containing the ids of the
  coalitions. The last row is assumed to be the full coalition, i.e., it
  contains the predicted responses for the observations which are to be
  explained.

- MSEv_uniform_comb_weights:

  Logical. If `TRUE` (default), then the function weights the coalitions
  uniformly when computing the MSEv criterion. If `FALSE`, then the
  function use the Shapley kernel weights to weight the coalitions when
  computing the MSEv criterion. Note that the Shapley kernel weights are
  replaced by the sampling frequency when not all coalitions are
  considered.

- MSEv_skip_empty_full_comb:

  Logical. If `TRUE` (default), exclude the empty and grand coalitions
  when computing the MSEv evaluation criterion. This is reasonable as
  they are identical for all methods, i.e., their contribution function
  is independent of the method used (special cases not affected by the
  approach). If `FALSE`, include the empty and grand coalitions. In that
  case, we recommend setting `MSEv_uniform_comb_weights = TRUE`;
  otherwise the large weights for the empty and grand coalitions will
  outweigh all others and make the MSEv criterion uninformative.

## Value

List containing:

- `MSEv`:

  A
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with the overall MSEv evaluation criterion averaged over both the
  coalitions and observations/explicands. The
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  also contains the standard deviation of the MSEv values for each
  explicand (only averaged over the coalitions) divided by the square
  root of the number of explicands.

- `MSEv_explicand`:

  A
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with the mean squared error for each explicand, i.e., only averaged
  over the coalitions.

- `MSEv_coalition`:

  A
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  with the mean squared error for each coalition, i.e., only averaged
  over the explicands/observations. The
  [`data.table`](https://rdatatable.gitlab.io/data.table/reference/data.table.html)
  also contains the standard deviation of the MSEv values for each
  coalition divided by the square root of the number of explicands.

## Details

The MSEv evaluation criterion does not rely on access to the true
contribution functions or the true Shapley values. A lower value
indicates better approximations; however, the scale and magnitude of
MSEv are not directly interpretable regarding the precision of the final
estimated Shapley values. [Olsen et al.
(2024)](https://link.springer.com/content/pdf/10.1007/s10618-024-01016-z.pdf)
illustrates (Figure 11) a fairly strong linear relationship between MSEv
and the MAE between the estimated and true Shapley values in a
simulation study. Note: explicands are the observations whose
predictions we explain.

## References

- [Frye, C., de Mijolla, D., Begley, T., Cowton, L., Stanley, M., &
  Feige, I. (2021). Shapley explainability on the data manifold. In
  International Conference on Learning
  Representations.](https://arxiv.org/pdf/2006.01272)

- [Olsen, L. H., Glad, I. K., Jullum, M., & Aas, K. (2022). Using
  Shapley values and variational autoencoders to explain predictive
  models with dependent mixed features. Journal of machine learning
  research, 23(213),
  1-51](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf)

- [Olsen, L. H. B., Glad, I. K., Jullum, M., & Aas, K. (2024). A
  comparative study of methods for estimating model-agnostic Shapley
  value explanations. Data Mining and Knowledge Discovery,
  1-48](https://link.springer.com/content/pdf/10.1007/s10618-024-01016-z.pdf)

## Author

Lars Henry Berge Olsen
