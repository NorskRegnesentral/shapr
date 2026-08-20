# Creates Categorical Distributions

Function that takes in a tensor containing the logits for each of the K
classes. Each row corresponds to an observations. Send each row through
the softmax function to convert from logits to probabilities that sum 1
one. The function also clamps the probabilities between a minimum and
maximum probability. Note that we still normalize them afterward, so the
final probabilities can be marginally below or above the thresholds.

## Usage

``` r
vaeac_categorical_parse_params(params, min_prob = 0, max_prob = 1)
```

## Arguments

- params:

  Tensor of dimension `batch_size` x `K` containing the logits for each
  of the `K` classes and `batch_size` observations.

- min_prob:

  For stability it might be desirable that the minimal probability is
  not too close to zero.

- max_prob:

  For stability it might be desirable that the maximal probability is
  not too close to one.

## Value

A
[torch::distr_categorical](https://torch.mlverse.org/docs/reference/distr_categorical.html)
distributions with the provided probabilities for each class.

## Details

Take a Tensor (e. g. a part of neural network output) and return
[`torch::distr_categorical()`](https://torch.mlverse.org/docs/reference/distr_categorical.html)
distribution. The input tensor after applying softmax over the last axis
contains a batch of the categorical probabilities. So there are no
restrictions on the input tensor. Technically, this function treats the
last axis as the categorical probabilities, but Categorical takes only
2D input where the first axis is the batch axis and the second one
corresponds to the probabilities, so practically the function requires
2D input with the batch of probabilities for one categorical feature.
`min_prob` is the minimal probability for each class. After clipping the
probabilities from below and above they are renormalized in order to be
a valid distribution. This regularization is required for the numerical
stability and may be considered as a neural network architecture choice
without any change to the probabilistic model.Note that the softmax
function is given by \\\operatorname{Softmax}(x_i) =
(\exp(x_i))/(\sum\_{j} \exp(x_j))\\, where \\x_i\\ are the logits and
can take on any value, negative and positive. The output
\\\operatorname{Softmax}(x_i) \in \[0,1\]\\ and \\\sum\_{j} Softmax(x_i)
= 1\\.

## Author

Lars Henry Berge Olsen
