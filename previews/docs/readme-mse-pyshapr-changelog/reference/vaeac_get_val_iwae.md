# Compute the Importance Sampling Estimator (Validation Error)

Compute the Importance Sampling Estimator which the vaeac model uses to
evaluate its performance on the validation data.

## Usage

``` r
vaeac_get_val_iwae(
  val_dataloader,
  mask_generator,
  batch_size,
  vaeac_model,
  val_iwae_n_samples
)
```

## Arguments

- val_dataloader:

  A torch dataloader which loads the validation data.

- mask_generator:

  A mask generator object that generates the masks.

- batch_size:

  Integer. The number of samples to include in each batch.

- vaeac_model:

  The vaeac model.

- val_iwae_n_samples:

  Number of samples to generate for computing the IWAE for each
  validation sample.

## Value

The average iwae over all instances in the validation dataset.

## Details

Compute mean IWAE log likelihood estimation of the validation set. IWAE
is an abbreviation for Importance Sampling Estimator \$\$\log
p\_{\theta, \psi}(x\|y) \approx \log {\frac{1}{S}\sum\_{i=1}^S
p\_\theta(x\|z_i, y) p\_\psi(z_i\|y) \big/ q\_\phi(z_i\|x,y),}\$\$ where
\\z_i \sim q\_\phi(z\|x,y)\\. For more details, see [Olsen et al.
(2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

## Author

Lars Henry Berge Olsen
