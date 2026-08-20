# A [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html) Representing a specified_prob_mask_generator

A mask generator which masks the entries based on specified
probabilities.

## Usage

``` r
specified_prob_mask_generator(masking_probs, paired_sampling = FALSE)
```

## Arguments

- masking_probs:

  An M+1 numerics containing the probabilities masking 'd' of the
  (0,...M) entries for each observation.

- paired_sampling:

  Boolean. If we are doing paired sampling. So include both S and
  \\\bar{S}\\. If TRUE, then batch must be sampled using
  'paired_sampler' which creates batches where the first half and second
  half of the rows are duplicates of each other. That is,
  `batch = [row1, row1, row2, row2, row3, row3, ...]`.

## Details

A class that takes in the probabilities of having d masked observations.
I.e., for M dimensional data, masking_probs is of length M+1, where the
d'th entry is the probability of having d-1 masked values.

A mask generator that first samples the number of entries 'd' to be
masked in the 'M'-dimensional observation 'x' in the batch based on the
given M+1 probabilities. The 'd' masked are uniformly sampled from the
'M' possible feature indices. The d'th entry of the probability of
having d-1 masked values.

Note that mcar_mask_generator with p = 0.5 is the same as using
`specified_prob_mask_generator()` with `masking_ratio` = choose(M, 0:M),
where M is the number of features. This function was initially created
to check if increasing the probability of having a masks with many
masked features improved vaeac's performance by focusing more on these
situations during training.
