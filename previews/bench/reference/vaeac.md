# Initializing a vaeac model

Class that represents a vaeac model, i.e., the class creates the neural
networks in the vaeac model and necessary training utilities. For more
details, see [Olsen et al.
(2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

## Usage

``` r
vaeac(
  one_hot_max_sizes,
  width = 32,
  depth = 3,
  latent_dim = 8,
  activation_function = torch::nn_relu,
  skip_conn_layer = FALSE,
  skip_conn_masked_enc_dec = FALSE,
  batch_normalization = FALSE,
  paired_sampling = FALSE,
  mask_generator_name = c("mcar_mask_generator", "specified_prob_mask_generator",
    "specified_masks_mask_generator"),
  masking_ratio = 0.5,
  mask_gen_coalitions = NULL,
  mask_gen_coalitions_prob = NULL,
  sigma_mu = 10000,
  sigma_sigma = 1e-04
)
```

## Arguments

- one_hot_max_sizes:

  A torch tensor of dimension `n_features` containing the one hot sizes
  of the `n_features` features. That is, if the `i`th feature is a
  categorical feature with 5 levels, then `one_hot_max_sizes[i] = 5`.
  While the size for continuous features can either be `0` or `1`.

- width:

  Integer. The number of neurons in each hidden layer in the neural
  networks of the masked encoder, full encoder, and decoder.

- depth:

  Integer. The number of hidden layers in the neural networks of the
  masked encoder, full encoder, and decoder.

- latent_dim:

  Integer. The number of dimensions in the latent space.

- activation_function:

  A
  [`torch::nn_module()`](https://torch.mlverse.org/docs/reference/nn_module.html)
  representing an activation function such as, e.g.,
  [`torch::nn_relu()`](https://torch.mlverse.org/docs/reference/nn_relu.html),
  [`torch::nn_leaky_relu()`](https://torch.mlverse.org/docs/reference/nn_leaky_relu.html),
  [`torch::nn_selu()`](https://torch.mlverse.org/docs/reference/nn_selu.html),
  [`torch::nn_sigmoid()`](https://torch.mlverse.org/docs/reference/nn_sigmoid.html).

- skip_conn_layer:

  Boolean. If we are to use skip connections in each layer, see
  [`skip_connection()`](https://norskregnesentral.github.io/shapr/reference/skip_connection.md).
  If `TRUE`, then we add the input to the outcome of each hidden layer,
  so the output becomes \\X + \operatorname{activation}(WX + b)\\. I.e.,
  the identity skip connection.

- skip_conn_masked_enc_dec:

  Boolean. If we are to apply concatenating skip connections between the
  layers in the masked encoder and decoder. The first layer of the
  masked encoder will be linked to the last layer of the decoder. The
  second layer of the masked encoder will be linked to the second to
  last layer of the decoder, and so on.

- batch_normalization:

  Boolean. If we are to use batch normalization after the activation
  function. Note that if `skip_conn_layer` is TRUE, then the
  normalization is done after the adding from the skip connection. I.e,
  we batch normalize the whole quantity X + activation(WX + b).

- paired_sampling:

  Boolean. If we are doing paired sampling. I.e., if we are to include
  both coalition S and \\\bar{S}\\ when we sample coalitions during
  training for each batch.

- mask_generator_name:

  String specifying the type of mask generator to use. Need to be one of
  'mcar_mask_generator', 'specified_prob_mask_generator', and
  'specified_masks_mask_generator'.

- masking_ratio:

  Scalar. The probability for an entry in the generated mask to be 1
  (masked). Not used if `mask_gen_coalitions` is given.

- mask_gen_coalitions:

  Matrix containing the different coalitions to learn. Must be given if
  `mask_generator_name = 'specified_masks_mask_generator'`.

- mask_gen_coalitions_prob:

  Numerics containing the probabilities for sampling each mask in
  `mask_gen_coalitions`. Array containing the probabilities for sampling
  the coalitions in `mask_gen_coalitions`.

- sigma_mu:

  Numeric representing a hyperparameter in the normal-gamma prior used
  on the masked encoder, see Section 3.3.1 in [Olsen et al.
  (2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

- sigma_sigma:

  Numeric representing a hyperparameter in the normal-gamma prior used
  on the masked encoder, see Section 3.3.1 in [Olsen et al.
  (2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).

## Value

Returns a list with the neural networks of the masked encoder, full
encoder, and decoder together with reconstruction log probability
function, optimizer constructor, sampler from the decoder output, mask
generator, batch size, and scale factor for the stability of the
variational lower bound optimization.

## Details

This function builds neural networks (masked encoder, full encoder,
decoder) given the list of one-hot max sizes of the features in the
dataset we use to train the vaeac model, and the provided parameters for
the networks. It also creates, e.g., reconstruction log probability
function, methods for sampling from the decoder output, and then use
these to create the vaeac model.

## make_observed

Apply Mask to Batch to Create Observed Batch

Compute the parameters for the latent normal distributions inferred by
the encoders. If `only_masked_encoder = TRUE`, then we only compute the
latent normal distributions inferred by the masked encoder. This is used
in the deployment phase when we do not have access to the full
observation.

## make_latent_distributions

Compute the Latent Distributions Inferred by the Encoders

Compute the parameters for the latent normal distributions inferred by
the encoders. If `only_masked_encoder = TRUE`, then we only compute the
latent normal distributions inferred by the masked encoder. This is used
in the deployment phase when we do not have access to the full
observation.

## masked_encoder_regularization

Compute the Regularizes for the Latent Distribution Inferred by the
Masked Encoder.

The masked encoder (prior) distribution regularization in the latent
space. This is used to compute the extended variational lower bound used
to train vaeac, see Section 3.3.1 in [Olsen et al.
(2022)](https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf).
Though regularizing prevents the masked encoder distribution parameters
from going to infinity, the model usually doesn't diverge even without
this regularization. It almost doesn't affect learning process near zero
with default regularization parameters which are recommended to be used.

## batch_vlb

Compute the Variational Lower Bound for the Observations in the Batch

Compute differentiable lower bound for the given batch of objects and
mask. Used as the (negative) loss function for training the vaeac model.

## batch_iwae

Compute IWAE log likelihood estimate with K samples per object.

Technically, it is differentiable, but it is recommended to use it for
evaluation purposes inside torch.no_grad in order to save memory. With
[`torch::with_no_grad()`](https://torch.mlverse.org/docs/reference/with_no_grad.html)
the method almost doesn't require extra memory for very large K. The
method makes K independent passes through decoder network, so the batch
size is the same as for training with batch_vlb. IWAE is an abbreviation
for Importance Sampling Estimator: \$\$ \log p\_{\theta, \psi}(x\|y)
\approx \log {\frac{1}{K} \sum\_{i=1}^K \[p\_\theta(x\|z_i, y) \*
p\_\psi(z_i\|y) / q\_\phi(z_i\|x,y)\]} \newline = \log {\sum\_{i=1}^K
\exp(\log\[p\_\theta(x\|z_i, y) \* p\_\psi(z_i\|y) /
q\_\phi(z_i\|x,y)\])} - \log(K) \newline = \log {\sum\_{i=1}^K
\exp(\log\[p\_\theta(x\|z_i, y)\] + \log\[p\_\psi(z_i\|y)\] -
\log\[q\_\phi(z_i\|x,y)\])} - \log(K) \newline =
\operatorname{logsumexp}(\log\[p\_\theta(x\|z_i, y)\] +
\log\[p\_\psi(z_i\|y)\] - \log\[q\_\phi(z_i\|x,y)\]) - \log(K) \newline
= \operatorname{logsumexp}(\text{rec}\\\text{loss} +
\text{prior}\\\text{log}\\\text{prob} -
\text{proposal}\\\text{log}\\\text{prob}) - \log(K),\$\$ where \\z_i
\sim q\_\phi(z\|x,y)\\.

## generate_samples_params

Generate the parameters of the generative distributions for samples from
the batch.

The function makes K latent representation for each object from the
batch, send these latent representations through the decoder to obtain
the parameters for the generative distributions. I.e., means and
variances for the normal distributions (continuous features) and
probabilities for the categorical distribution (categorical features).
The second axis is used to index samples for an object, i.e. if the
batch shape is \[n x D1 x D2\], then the result shape is \[n x K x D1 x
D2\]. It is better to use it inside
[`torch::with_no_grad()`](https://torch.mlverse.org/docs/reference/with_no_grad.html)
in order to save memory. With
[`torch::with_no_grad()`](https://torch.mlverse.org/docs/reference/with_no_grad.html)
the method doesn't require extra memory except the memory for the
result.

## Author

Lars Henry Berge Olsen
