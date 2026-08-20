# Missing Completely at Random (MCAR) Mask Generator

A mask generator which masks the entries in the input completely at
random.

## Usage

``` r
mcar_mask_generator(masking_ratio = 0.5, paired_sampling = FALSE)
```

## Arguments

- masking_ratio:

  Numeric between 0 and 1. The probability for an entry in the generated
  mask to be 1 (masked).

- paired_sampling:

  Boolean. If we are doing paired sampling. So include both S and
  \\\bar{S}\\. If `TRUE`, then `batch` must be sampled using
  [`paired_sampler()`](https://norskregnesentral.github.io/shapr/reference/paired_sampler.md)
  which ensures that the `batch` contains two instances for each
  original observation. That is, `batch` \\= \[X_1, X_1, X_2, X_2, X_3,
  X_3, ...\]\\, where each entry \\X_j\\ is a row of dimension \\p\\
  (i.e., the number of features).

## Details

The mask generator mask each element in the `batch` (N x p) using a
component-wise independent Bernoulli distribution with probability
`masking_ratio`. Default values for `masking_ratio` is 0.5, so all masks
are equally likely to be generated, including the empty and full masks.
The function returns a mask of the same shape as the input `batch`, and
the `batch` can contain missing values, indicated by the "NaN" token,
which will always be masked.

## Shape

- Input: \\(N, p)\\ where N is the number of observations in the `batch`
  and \\p\\ is the number of features.

- Output: \\(N, p)\\, same shape as the input

## Author

Lars Henry Berge Olsen
