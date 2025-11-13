# Extends Incomplete Batches by Sampling Extra Data from Dataloader

If the height of the `batch` is less than `batch_size`, this function
extends the `batch` with data from the
[`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html)
until the `batch` reaches the required size. Note that `batch` is a
tensor.

## Usage

``` r
vaeac_extend_batch(batch, dataloader, batch_size)
```

## Arguments

- batch:

  The batch we want to check if has the right size, and if not extend it
  until it has the right size.

- dataloader:

  A
  [`torch::dataloader()`](https://torch.mlverse.org/docs/reference/dataloader.html)
  object from which we can create an iterator object and load data to
  extend the batch.

- batch_size:

  Integer. The number of samples to include in each batch.

## Value

Returns the extended batch with the correct batch_size.

## Author

Lars Henry Berge Olsen
