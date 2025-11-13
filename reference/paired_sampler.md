# Sampling Paired Observations

A sampler used to samples the batches where each instances is sampled
twice

## Usage

``` r
paired_sampler(vaeac_dataset_object, shuffle = FALSE)
```

## Arguments

- vaeac_dataset_object:

  A
  [`vaeac_dataset()`](https://norskregnesentral.github.io/shapr/reference/vaeac_dataset.md)
  object containing the data.

- shuffle:

  Boolean. If `TRUE`, then the data is shuffled. If `FALSE`, then the
  data is returned in chronological order.

## Details

A sampler object that allows for paired sampling by always including
each observation from the
[`vaeac_dataset()`](https://norskregnesentral.github.io/shapr/reference/vaeac_dataset.md)
twice. A
[`torch::sampler()`](https://torch.mlverse.org/docs/reference/sampler.html)
object can be used with
[`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html)
when creating batches from a torch dataset
[`torch::dataset()`](https://torch.mlverse.org/docs/reference/dataset.html).
See <https://rdrr.io/cran/torch/src/R/utils-data-sampler.R> for more
information. This function does not use batch iterators, which might
increase the speed.

## Author

Lars Henry Berge Olsen
