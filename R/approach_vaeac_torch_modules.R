# VAEAC Model =========================================================================================================
## vaeac --------------------------------------------------------------------------------------------------------------
#' Initializing a vaeac model
#'
#' @description Class that represents a vaeac model, i.e., the class creates the neural networks in the vaeac
#' model and necessary training utilities.
#' For more details, see \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#'
#' @details This function builds neural networks (masked encoder, full encoder, decoder) given
#' the list of one-hot max sizes of the features in the dataset we use to train the vaeac model,
#' and the provided parameters for the networks. It also creates, e.g., reconstruction log probability function,
#' methods for sampling from the decoder output, and then use these to create the vaeac model.
#'
#' @param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the `M` features.
#' The sizes for the continuous features can either be `0` or `1`.
#' @param width Integer. The number of neurons in each hidden layer in the neural networks
#' of the masked encoder, full encoder, and decoder.
#' @param depth Integer. The number of hidden layers in the neural networks of the
#' masked encoder, full encoder, and decoder.
#' @param latent_dim Integer. The number of dimensions in the latent space.
#' @param activation_function A [torch::nn_module()] representing an activation function such as, e.g.,
#' [torch::nn_relu()], [torch::nn_leaky_relu()], [torch::nn_selu()],
#' [torch::nn_sigmoid()].
#' @param use_skip_connections Boolean. If we are to use skip connections in each layer, see [shapr::SkipConnection()].
#' If `TRUE`, then we add the input to the outcome of each hidden layer, so the output becomes
#' \eqn{X + \operatorname{activation}(WX + b)}. I.e., the identity skip connection.
#' @param skip_connection_masked_enc_dec Boolean. If we are to apply concatenating skip
#' connections between the layers in the masked encoder and decoder. The first layer of the masked encoder will be
#' linked to the last layer of the decoder. The second layer of the masked encoder will be
#' linked to the second to last layer of the decoder, and so on.
#' @param use_batch_normalization Boolean. If we are to use batch normalization after the activation function.
#' Note that if `use_skip_connections` is TRUE, then the normalization is
#' done after the adding from the skip connection. I.e, we batch normalize the whole quantity X + activation(WX + b).
#' @param paired_sampling Boolean. If we are doing paired sampling. I.e., if we are to include both coalition S
#' and \eqn{\bar{S}} when we sample coalitions during training for each batch.
#' @param mask_generator_name String specifying the type of mask generator to use. Need to be one of
#' 'MCAR_mask_generator', 'Specified_prob_mask_generator', and 'Specified_masks_mask_generator'.
#' @param masking_ratio Scalar. The probability for an entry in the generated mask to be 1 (masked).
#' Not used if `mask_gen_these_coalitions` is given.
#' @param mask_gen_these_coalitions Matrix containing the different coalitions to learn.
#' Must be given if `mask_generator_name = 'Specified_masks_mask_generator'`.
#' @param mask_gen_these_coalitions_prob Numerics containing the probabilities
#' for sampling each mask in `mask_gen_these_coalitions`.
#' Array containing the probabilities for sampling the coalitions in `mask_gen_these_coalitions`.
#' @param sigma_mu Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder,
#' see Section 3.3.1 in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' @param sigma_sigma Numeric representing a hyperparameter in the normal-gamma prior used on the masked encoder,
#' see Section 3.3.1 in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#'
#' @return Returns a list with the neural networks of the masked encoder, full encoder, and decoder together
#' with reconstruction log probability function, optimizer constructor, sampler from the decoder output,
#' mask generator, batch size, and scale factor for the stability of the variational lower bound optimization.
#'
#' @section make_observed:
#' Apply Mask to Batch to Create Observed Batch
#'
#' Compute the parameters for the latent normal distributions inferred by the encoders.
#' If `only_masked_encoder = TRUE`, then we only compute the latent normal distributions inferred by the
#' masked encoder. This is used in the deployment phase when we do not have access to the full observation.
#'
#' @section make_latent_distributions:
#' Compute the Latent Distributions Inferred by the Encoders
#'
#' Compute the parameters for the latent normal distributions inferred by the encoders.
#' If `only_masked_encoder = TRUE`, then we only compute the latent normal distributions inferred by the
#' masked encoder. This is used in the deployment phase when we do not have access to the full observation.
#'
#' @section masked_encoder_regularization:
#' Compute the Regularizes for the Latent Distribution Inferred by the Masked Encoder.
#'
#' The masked encoder (prior) distribution regularization in the latent space.
#' This is used to compute the extended variational lower bound used to train vaeac, see
#' Section 3.3.1 in \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#' Though regularizing prevents the masked encoder distribution parameters from going to infinity,
#' the model usually doesn't diverge even without this regularization. It almost doesn't affect
#' learning process near zero with default regularization parameters which are recommended to be used.
#'
#' @section batch_vlb:
#' Compute the Variational Lower Bound for the Observations in the Batch
#'
#' Compute differentiable lower bound for the given batch of objects and mask.
#' Used as the (negative) loss function for training the vaeac model.
#'
#' @section batch_iwae:
#' Compute IWAE log likelihood estimate with K samples per object.
#'
#' Technically, it is differentiable, but it is recommended to use it for
#' evaluation purposes inside torch.no_grad in order to save memory. With [torch::with_no_grad()]
#' the method almost doesn't require extra memory for very large K. The method makes K independent
#' passes through decoder network, so the batch size is the same as for training with batch_vlb.
#' IWAE is an abbreviation for Importance Sampling Estimator:
#' \deqn{
#' \log p_{\theta, \psi}(x|y) \approx
#' \log {\frac{1}{K} \sum_{i=1}^K [p_\theta(x|z_i, y) * p_\psi(z_i|y) / q_\phi(z_i|x,y)]} \newline
#' =
#' \log {\sum_{i=1}^K \exp(\log[p_\theta(x|z_i, y) * p_\psi(z_i|y) / q_\phi(z_i|x,y)])} - \log(K) \newline
#' =
#' \log {\sum_{i=1}^K \exp(\log[p_\theta(x|z_i, y)] + \log[p_\psi(z_i|y)] - \log[q_\phi(z_i|x,y)])} - \log(K) \newline
#' =
#' \operatorname{logsumexp}(\log[p_\theta(x|z_i, y)] + \log[p_\psi(z_i|y)] - \log[q_\phi(z_i|x,y)]) - \log(K) \newline
#' =
#' \operatorname{logsumexp}(\text{rec}\_\text{loss} + \text{prior}\_\text{log}\_\text{prob} -
#'  \text{proposal}\_\text{log}\_\text{prob}) - \log(K),}
#' where \eqn{z_i \sim q_\phi(z|x,y)}.
#'
#' @section generate_samples_params:
#' Generate the parameters of the generative distributions for samples from the batch.
#'
#' The function makes K latent representation for each object from the batch, send these
#' latent representations through the decoder to obtain the parameters for the generative distributions.
#' I.e., means and variances for the normal distributions (continuous features) and probabilities
#' for the categorical distribution (categorical features).
#' The second axis is used to index samples for an object, i.e. if the batch shape is \[n x D1 x D2\], then
#' the result shape is \[n x K x D1 x D2\]. It is better to use it inside [torch::with_no_grad()] in order to save
#' memory. With [torch::with_no_grad()] the method doesn't require extra memory except the memory for the result.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac <- torch::nn_module(

  # Name of the torch::nn_module object
  classname = "vaeac",

  # Initializing a vaeac model
  initialize = function(one_hot_max_sizes,
                        width = 32,
                        depth = 3,
                        latent_dim = 8,
                        activation_function = torch::nn_relu,
                        use_skip_connections = FALSE,
                        skip_connection_masked_enc_dec = FALSE,
                        use_batch_normalization = FALSE,
                        paired_sampling = FALSE,
                        mask_generator_name = c("MCAR_mask_generator",
                                                "Specified_prob_mask_generator",
                                                "Specified_masks_mask_generator"),
                        masking_ratio = 0.5,
                        mask_gen_these_coalitions = NULL,
                        mask_gen_these_coalitions_prob = NULL,
                        sigma_mu = 1e4,
                        sigma_sigma = 1e-4) {
    # Check that a valid mask_generator was provided.
    mask_generator_name <- match.arg(mask_generator_name)

    # Get the number of features
    num_features <- length(one_hot_max_sizes)

    # Extra strings to add to names of layers depending on if we use memory layers and/or batch normalization.
    # If FALSE, they are just an empty string and do not effect the names.
    name_extra_memory_layer <- ifelse(skip_connection_masked_enc_dec, "_and_memory", "")
    name_extra_batch_normalize <- ifelse(use_batch_normalization, "_and_batch_norm", "")

    # Save some of the initializing hyperparameters to the vaeac object. Others are saved later.
    self$one_hot_max_sizes <- one_hot_max_sizes
    self$depth <- depth
    self$width <- width
    self$latent_dim <- latent_dim
    self$activation_function <- activation_function
    self$use_skip_connections <- use_skip_connections
    self$skip_connection_masked_enc_dec <- skip_connection_masked_enc_dec
    self$use_batch_normalization <- use_batch_normalization
    self$sigma_mu <- sigma_mu
    self$sigma_sigma <- sigma_sigma
    self$paired_sampling <- paired_sampling

    # Save the how to compute the loss and how to sample from the vaeac model.
    self$reconstruction_log_prob <- GaussCatLoss(one_hot_max_sizes)
    self$sampler_most_likely <- GaussCatSamplerMostLikely(one_hot_max_sizes)
    self$sampler_random <- GaussCatSamplerRandom(one_hot_max_sizes)
    self$generative_parameters <- GaussCatParameters(one_hot_max_sizes)
    self$num_features <- num_features
    self$vlb_scale_factor <- 1 / num_features

    ##### Generate the mask generator
    if (mask_generator_name == "MCAR_mask_generator") {
      # Create a MCAR_mask_generator and attach it to the vaeac object. Note that masking_ratio is a singleton here.
      self$mask_generator <- MCAR_mask_generator(
        masking_ratio = masking_ratio,
        paired_sampling = paired_sampling
      )

      # Attach the masking ratio to the vaeac object.
      self$masking_ratio <- masking_ratio
    } else if (mask_generator_name == "Specified_prob_mask_generator") {
      # Create a Specified_prob_mask_generator and attach it to the vaeac object.
      # Note that masking_ratio is an array here.
      self$mask_generator <- Specified_prob_mask_generator(
        masking_probs = masking_ratio,
        paired_sampling = paired_sampling
      )

      # Attach the masking probabilities to the vaeac object.
      self$masking_probs <- masking_ratio
    } else if (mask_generator_name == "Specified_masks_mask_generator") {
      # Small check that they have been provided.
      if (is.null(mask_gen_these_coalitions) | is.null(mask_gen_these_coalitions_prob)) {
        stop("Both 'mask_gen_these_coalitions' and 'mask_gen_these_coalitions_prob'
must be provided when using 'Specified_masks_mask_generator'.\n")
      }

      # Create a Specified_masks_mask_generator and attach it to the vaeac object.
      self$mask_generator <- Specified_masks_mask_generator(
        masks = mask_gen_these_coalitions,
        masks_probs = mask_gen_these_coalitions_prob,
        paired_sampling = paired_sampling
      )

      # Save the possible masks and corresponding probabilities to the vaeac object.
      self$masks <- mask_gen_these_coalitions
      self$masks_probs <- mask_gen_these_coalitions_prob
    } else {
      # Print error to user.
      stop(sprintf("Maske geneartor '%s' is not supported.
Chose one of 'MCAR_mask_generator', 'Specified_prob_mask_generator', and 'Specified_masks_mask_generator'.\n",
                   mask_generator))
    }

    ##### Full Encoder
    full_encoder_network <- torch::nn_sequential()

    # Full Encoder: Input layer
    full_encoder_network$add_module(
      module = CategoricalToOneHotLayer(c(one_hot_max_sizes, rep(0, num_features)), seq(num_features)),
      name = "input_layer_cat_to_one_hot"
    )
    full_encoder_network$add_module(
      module = torch::nn_linear(in_features = sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) +
                                  num_features * 2,
                                out_features = width),
      name = "input_layer_linear"
    )
    full_encoder_network$add_module(
      module = activation_function(),
      name = "input_layer_layer_activation"
    )
    if (use_batch_normalization) {
      full_encoder_network$add_module(
        module = torch::nn_batch_norm1d(num_features = width),
        name = "input_layer_layer_batch_norm"
      )
    }

    # Full Encoder: Hidden layers
    for (i in seq(depth)) {
      if (use_skip_connections) {
        # Add identity skip connection. Such that the input is added to the output of the linear layer
        # and activation function: output = X + activation(WX + b).
        full_encoder_network$add_module(
          module = SkipConnection(
            torch::nn_linear(width, width),
            activation_function(),
            if (use_batch_normalization) torch::nn_batch_norm1d(num_features = width)
          ),
          name = paste0("hidden_layer_", i, "_skip_connection_with_linear_and_activation", name_extra_batch_normalize)
        )
      } else {
        # Do not use skip connections and do not add the input to the output.
        full_encoder_network$add_module(
          module = torch::nn_linear(width, width),
          name = paste0("hidden_layer_", i, "_linear")
        )
        full_encoder_network$add_module(
          module = activation_function(),
          name = paste0("hidden_layer_", i, "_activation")
        )
        if (use_batch_normalization) {
          full_encoder_network$add_module(
            module = torch::nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm")
          )
        }
      }
    }

    # Full Encoder: Go to latent space
    full_encoder_network$add_module(
      module = torch::nn_linear(width, latent_dim * 2),
      name = "latent_space_layer_linear"
    )

    ##### Masked Encoder
    masked_encoder_network <- torch::nn_sequential()

    # Masked Encoder: Input layer
    masked_encoder_network$add_module(
      module = CategoricalToOneHotLayer(c(one_hot_max_sizes, rep(0, num_features))),
      name = "input_layer_cat_to_one_hot"
    )
    if (skip_connection_masked_enc_dec) {
      masked_encoder_network$add_module(
        module = MemoryLayer("#input"),
        name = "input_layer_memory"
      )
    }
    masked_encoder_network$add_module(
      module = torch::nn_linear(in_features = sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) +
                                  num_features,
                                out_features = width),
      name = "input_layer_linear"
    )
    masked_encoder_network$add_module(
      module = activation_function(),
      name = "input_layer_activation"
    )
    if (use_batch_normalization) {
      masked_encoder_network$add_module(
        module = torch::nn_batch_norm1d(num_features = width),
        name = "input_layer_batch_norm"
      )
    }

    # Masked Encoder: Hidden layers
    for (i in seq(depth)) {
      if (use_skip_connections) {
        # Add identity skip connection. Such that the input is added to the output of the linear layer
        # and activation function: output = X + activation(WX + b).
        # Also check inside SkipConnection if we are to use MemoryLayer. I.e., skip connection with
        # concatenation from masked encoder to decoder.
        masked_encoder_network$add_module(
          module = SkipConnection(
            if (skip_connection_masked_enc_dec) MemoryLayer(paste0("#", i)),
            torch::nn_linear(width, width),
            activation_function()
          ),
          name = paste0("hidden_layer_", i, "_skip_connection_with_linear_and_activation", name_extra_memory_layer)
        )
        if (use_batch_normalization) {
          masked_encoder_network$add_module(
            module = torch::nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm")
          )
        }
      } else {
        # Do not use skip connections and do not add the input to the output.
        if (skip_connection_masked_enc_dec) {
          masked_encoder_network$add_module(
            module = MemoryLayer(paste0("#", i)),
            name = paste0("hidden_layer_", i, "_memory")
          )
        }
        masked_encoder_network$add_module(
          module = torch::nn_linear(width, width),
          name = paste0("hidden_layer_", i, "_linear")
        )
        masked_encoder_network$add_module(
          module = activation_function(),
          name = paste0("hidden_layer_", i, "_activation")
        )
        if (use_batch_normalization) {
          masked_encoder_network$add_module(
            module = torch::nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm")
          )
        }
      }
    }

    # Masked Encoder: Go to latent space
    if (skip_connection_masked_enc_dec) {
      masked_encoder_network$add_module(
        module = MemoryLayer(paste0("#", depth + 1)),
        name = "latent_space_layer_memory"
      )
    }
    masked_encoder_network$add_module(
      module = torch::nn_linear(width, 2 * latent_dim),
      name = "latent_space_layer_linear"
    )

    ##### Decoder
    decoder_network <- torch::nn_sequential()

    # Decoder: Go from latent space
    decoder_network$add_module(
      module = torch::nn_linear(latent_dim, width),
      name = "latent_space_layer_linear"
    )
    decoder_network$add_module(
      module = activation_function(),
      name = "latent_space_layer_activation"
    )
    if (use_batch_normalization) {
      decoder_network$add_module(
        module = torch::nn_batch_norm1d(num_features = width),
        name = "latent_space_layer_batch_norm"
      )
    }

    # Get the width of the hidden layers in the decoder. Needs to be multiplied with two if
    # we use skip connections between masked encoder and decoder as we concatenate the tensors.
    width_decoder <- ifelse(skip_connection_masked_enc_dec, 2 * width, width)

    # Same for the input dimension to the last layer in decoder that yields the distribution params.
    extra_params_skip_con_mask_enc <-
      ifelse(test = skip_connection_masked_enc_dec,
        yes = sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) + num_features,
        no = 0
      )

    # Will need an extra hidden layer if we use skip connection from masked encoder to decoder
    # as we send the full input layer of the masked encoder to the last layer in the decoder.
    depth_decoder <- ifelse(skip_connection_masked_enc_dec, depth + 1, depth)

    # Decoder: Hidden layers
    for (i in seq(depth_decoder)) {
      if (use_skip_connections) {
        # Add identity skip connection. Such that the input is added to the output of the linear layer
        # and activation function: output = X + activation(WX + b).
        # Also check inside SkipConnection if we are to use MemoryLayer. I.e., skip connection with
        # concatenation from masked encoder to decoder.
        # If TRUE, then the memory layers extracts the corresponding input used in the masked encoder
        # and concatenate them with the current input.
        # Note that we add the memory layers in the opposite direction from how they were created.
        # So, we get a classical U-net with latent
        # space at the bottom and a connection between the layers on the same height of the U-shape.
        decoder_network$add_module(
          module = torch::nn_sequential(
            SkipConnection(
              if (skip_connection_masked_enc_dec) {
                MemoryLayer(paste0("#", depth - i + 2), TRUE)
              },
              torch::nn_linear(width_decoder, width),
              activation_function()
            )
          ),
          name = paste0("hidden_layer_", i, "_skip_connection_with_linear_and_activation", name_extra_memory_layer)
        )
        if (use_batch_normalization) {
          decoder_network$add_module(
            module = torch::nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm")
          )
        }
      } else {
        # Do not use skip connections and do not add the input to the output.
        if (skip_connection_masked_enc_dec) {
          decoder_network$add_module(
            module = MemoryLayer(paste0("#", depth - i + 2), TRUE),
            name = paste0("hidden_layer_", i, "_memory")
          )
        }
        decoder_network$add_module(
          module = torch::nn_linear(width_decoder, width),
          name = paste0("hidden_layer_", i, "_linear")
        )
        decoder_network$add_module(
          module = activation_function(),
          name = paste0("hidden_layer_", i, "_activation")
        )
        if (use_batch_normalization) {
          decoder_network$add_module(
            module = torch::nn_batch_norm1d(num_features = width),
            name = paste0("hidden_layer_", i, "_batch_norm")
          )
        }
      }
    }

    # Decoder: Go the parameter space of the generative distributions
    # Concatenate the input to the first layer of the masked encoder to the last layer of the decoder network.
    if (skip_connection_masked_enc_dec) {
      decoder_network$add_module(
        module = MemoryLayer("#input", TRUE),
        name = "output_layer_memory"
      )
    }
    # Linear layer to the parameters of the generative distributions Gaussian and Categorical.
    # Note that sum(apply(rbind(one_hot_max_sizes, rep(1, num_features)), 2, max)) is the number of
    # one hot variables to the masked encoder and num_features represents the binary variables if
    # the features was masked/missing or not when they entered the masked encoder.
    # The output dimension is 2 for the continuous features and K_i for categorical feature X_i,
    # where K_i is the number of classes the i'th categorical feature can take on.
    decoder_network$add_module(
      module = torch::nn_linear(
        in_features = width + extra_params_skip_con_mask_enc,
        out_features = sum(apply(rbind(one_hot_max_sizes, rep(2, num_features)), 2, max))
      ),
      name = "output_layer_linear"
    )

    # Save the networks to the vaeac object
    self$full_encoder_network <- full_encoder_network
    self$masked_encoder_network <- masked_encoder_network
    self$decoder_network <- decoder_network

    # Compute the number of trainable parameters in the different networks
    num_train_param_full_encoder <- sum(sapply(full_encoder_network$parameters, function(p) prod(p$size())))
    num_train_param_masked_encoder <- sum(sapply(masked_encoder_network$parameters, function(p) prod(p$size())))
    num_train_param_decoder <- sum(sapply(decoder_network$parameters, function(p) prod(p$size())))
    num_train_param_total <- num_train_param_full_encoder +
      num_train_param_masked_encoder +
      num_train_param_decoder
    num_train_param <- rbind(
      num_train_param_total,
      num_train_param_full_encoder,
      num_train_param_masked_encoder,
      num_train_param_decoder
    )

    # Save the number of parameters to the vaeac object
    self$num_train_param <- num_train_param
  },

  # Forward functions are required in torch::nn_modules,
  # but is it not needed in the way we have implemented vaeac.
  forward = function(...) {
    warning("NO FORWARD FUNCTION IMPLEMENTED FOR VAEAC.")
    return("NO FORWARD FUNCTION IMPLEMENTED FOR VAEAC.")
  },

  # Apply Mask to Batch to Create Observed Batch
  #
  # @description Clones the batch and applies the mask to set masked entries to 0 to create the observed batch.
  #
  # @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  # @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
  make_observed = function(batch, mask) {
    # Clone and detach the batch from the graph (removes the gradient element for the tensor).
    observed <- batch$clone()$detach()

    # Apply the mask by masking every entry in batch where 'mask' is 1.
    observed[mask == 1] <- 0

    # Return the observed batch where masked entries are set to 0.
    return(observed)
  },

  # Compute the Latent Distributions Inferred by the Encoders
  #
  # @description Compute the parameters for the latent normal distributions inferred by the encoders.
  # If `only_masked_encoder = TRUE`, then we only compute the latent normal distributions inferred by the
  # masked encoder. This is used in the deployment phase when we do not have access to the full observation.
  #
  # @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  # @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
  # @param only_masked_encoder Boolean. If we are only to compute the latent distributions for the masked encoder.
  # Used in deployment phase when we do not have access to the full data. Always FALSE in the training phase.
  make_latent_distributions = function(batch, mask, only_masked_encoder = FALSE) {
    # Artificially mask the observations where mask == 1 to create the observed batch values.
    observed <- self$make_observed(batch = batch, mask = mask)

    # Check if we are in training or deployment phase
    if (only_masked_encoder) {
      # In deployment phase and only use the masked encoder.
      full_encoder <- NULL
    } else {
      # In the training phase where we need to use both masked and full encoder.

      # Column bind the batch and the mask to create the full information sent to the full encoder.
      full_info <- torch::torch_cat(c(batch, mask), dim = 2)

      # Send the full_information through the full encoder. It needs the full information to know if a
      # value is missing or just masked. The output tensor is of shape batch_size x (2 x latent_dim)
      # In each row, i.e., each observation in the batch, the first latent_dim entries are the means mu
      # while the last latent_dim entries are the softplus of the sigmas, so they can take on any
      # negative or positive value. Recall that softplus(x) = ln(1+e^{x}).
      full_encoder_params <- self$full_encoder_network(full_info)

      # Takes the full_encoder_parameters and returns a normal distribution, which is component-wise
      # independent. If sigma (after softmax transform) is less than 1e-3, then we set sigma to 0.001.
      full_encoder <- normal_parse_params(params = full_encoder_params, min_sigma = 1e-3)
    }

    # Column bind the batch and the mask to create the observed information sent to the masked encoder.
    observed_info <- torch::torch_cat(c(observed, mask), dim = -1)

    # Compute the latent normal dist parameters (mu, sigma) for the masked
    # encoder by sending the observed values and the mask to the masked encoder.
    masked_encoder_params <- self$masked_encoder_network(observed_info)

    # Create the latent normal distributions based on the parameters (mu, sigma) from the masked encoder
    masked_encoder <- normal_parse_params(params = masked_encoder_params, min_sigma = 1e-3)

    # Return the full and masked encoders
    return(list(
      full_encoder = full_encoder,
      masked_encoder = masked_encoder
    ))
  },

  # Compute the Regularizes for the Latent Distribution Inferred by the Masked Encoder.
  #
  # @description The masked encoder (prior) distribution regularization in the latent space.
  # This is used to compute the extended variational lower bound used to train vaeac, see
  # Section 3.3.1 in Olsen et al. (2022).
  # Though regularizing prevents the masked encoder distribution parameters from going to infinity,
  # the model usually doesn't diverge even without this regularization. It almost doesn't affect
  # learning process near zero with default regularization parameters which are recommended to be used.
  #
  # @param masked_encoder The torch_Normal object returned when calling the masked encoder.
  masked_encoder_regularization = function(masked_encoder) {
    # Extract the number of observations. Same as batch_size.
    num_observations <- masked_encoder$mean$shape[1]

    # Extract the number of dimension in the latent space.
    num_latent_dimensions <- masked_encoder$mean$shape[2]

    # Extract means and ensure correct shape (batch_size x latent_dim).
    mu <- masked_encoder$mean$view(c(num_observations, num_latent_dimensions))

    # Extract the sigmas and ensure correct shape (batch_size x latent_dim).
    sigma <- masked_encoder$scale$view(c(num_observations, num_latent_dimensions))

    # Note that sum(-1) indicates that we sum together the columns.
    # mu_regularizer is then a tensor of length num_observations
    mu_regularizer <- -(mu^2)$sum(-1) / (2 * self$sigma_mu^2)

    # sigma_regularizer is then also a tensor of length num_observations.
    sigma_regularizer <- (sigma$log() - sigma)$sum(-1) * self$sigma_sigma

    # Add the regularization terms together and return them.
    return(mu_regularizer + sigma_regularizer)
  },

  # Compute the Variational Lower Bound for the Observations in the Batch
  #
  # @description Compute differentiable lower bound for the given batch of objects and mask.
  # Used as the (negative) loss function for training the vaeac model.
  #
  # @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  # @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
  batch_vlb = function(batch, mask) {
    # Compute the latent normal distributions obtained from the full and masked encoder
    encoders_list <- self$make_latent_distributions(batch = batch, mask = mask)

    # Extract the masked and full encoders. These are torch_Normal objects.
    masked_encoder <- encoders_list$masked_encoder
    full_encoder <- encoders_list$full_encoder

    # Apply the regularization on the mus and sigmas of the normal dist obtained from the masked encoder
    # such that they don't blow up. Regularized according to their normal gamma prior, see Olsen et al. (2022).
    masked_encoder_regularization <- self$masked_encoder_regularization(masked_encoder)

    # To use the reparameterization trick to train vaeac, we need to use 'rsample'
    # and not 'sample', which allows backpropagation through the mean and standard deviation layers,
    # see https://pytorch.org/docs/stable/distributions.html#pathwise-derivative.
    # For each training instance in the batch we sample values for each of the latent variables,
    # i.e.,  we get a tensor of dimension batch_size x latent_dim.
    latent <- full_encoder$rsample()

    # Send the latent samples through the decoder and get the batch_size x 2*num_features (in cont case)
    # where we for each row have a normal dist on each feature The form will be (mu_1, sigma_1, ..., mu_p, sigma_p)
    reconstruction_params <- self$decoder_network(latent)

    # Compute the reconstruction loss, i.e., the log likelihood of only the masked values in
    # the batch (true values) given the current reconstruction parameters from the decoder.
    # We do not consider the log likelihood of observed or missing/nan values.
    reconstruction_loss <- self$reconstruction_log_prob(batch, reconstruction_params, mask)

    # Compute the KL divergence between the two latent normal distributions obtained from the full encoder
    # and masked encoder. Since the networks create MVN with diagonal covariance matrices, that is, the same as
    # a product of individual Gaussian distributions, we can compute KL analytically very easily:
    # KL(p, q) = \int p(x) log(p(x)/q(x)) dx
    #          = 0.5 * { (sigma_p/sigma_q)^2 + (mu_q - mu_p)^2/sigma_q^2 - 1 + 2 ln (sigma_q/sigma_p)}
    # when both p and q are torch_Normal objects.
    kl <- kl_normal_normal(full_encoder, masked_encoder)$view(c(batch$shape[1], -1))$sum(-1)

    # Return the variational lower bound with the prior regularization. See Section 3.3.1 in Olsen et al. (2022)
    return(reconstruction_loss - kl + masked_encoder_regularization)
  },

  # Compute the Importance Sampling Estimator for the Observations in the Batch
  #
  # @description Compute IWAE log likelihood estimate with K samples per object.
  #
  # @details Technically, it is differentiable, but it is recommended to use it for
  # evaluation purposes inside torch.no_grad in order to save memory. With torch::with_no_grad
  # the method almost doesn't require extra memory for very large K. The method makes K independent
  # passes through decoder network, so the batch size is the same as for training with batch_vlb.
  # IWAE is an abbreviation for Importance Sampling Estimator
  # log p_{theta, psi}(x|y) approx
  # log {1/K * sum_{i=1}^K [p_theta(x|z_i, y) * p_psi(z_i|y) / q_phi(z_i|x,y)]} =
  # log {sum_{i=1}^K exp(log[p_theta(x|z_i, y) * p_psi(z_i|y) / q_phi(z_i|x,y)])} - log(K) =
  # log {sum_{i=1}^K exp(log[p_theta(x|z_i, y)] + log[p_psi(z_i|y)] - log[q_phi(z_i|x,y)])} - log(K) =
  # logsumexp(log[p_theta(x|z_i, y)] + log[p_psi(z_i|y)] - log[q_phi(z_i|x,y)]) - log(K) =
  # logsumexp(rec_loss + prior_log_prob - proposal_log_prob) - log(K),
  # where z_i ~ q_phi(z|x,y).
  #
  # @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  # @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
  # @param K Integer. The number of samples generated to compute the IWAE for each observation in `batch`.
  batch_iwae = function(batch, mask, K) {
    # Compute the latent normal distributions obtained from the full and masked encoder
    encoders_list <- self$make_latent_distributions(batch = batch, mask = mask)

    # Extract the masked and full encoders. These are torch_Normal objects.
    masked_encoder <- encoders_list$masked_encoder
    full_encoder <- encoders_list$full_encoder

    # List to store the estimates.
    estimates <- list()

    # Iterate over the number of samples/passes through the decoder for each validation observation.
    for (i in seq(K)) {
      # See equation 18 on page 18 in Ivanov et al. (2019). Create samples from the
      # full encoder; z_i ~ q_phi(z|x,y). We get a tensor of dimension batch_size x latent_dim.
      latent <- full_encoder$rsample()

      # Send the latent samples through the decoder and get the batch_size x 2*num_features (in cont case)
      # where we for each row have a normal dist on each feature The form will be (mu_1, sigma_1, ..., mu_p, sigma_p)
      reconstruction_params <- self$decoder_network(latent)

      # Compute the reconstruction loss, i.e., the log likelihood of only the masked values in
      # the batch (true values) given the current reconstruction parameters from the decoder.
      # We do not consider the log likelihood of observed or missing/nan values.
      reconstruction_loss <- self$reconstruction_log_prob(batch, reconstruction_params, mask)

      # Compute the log likelihood of observing the sampled latent representations from
      # the full_encoder when using the normal distribution estimated by the masked_encoder.
      masked_encoder_log_prob <- masked_encoder$log_prob(latent)

      # Ensure dimensions batch$shape[1] x something.
      masked_encoder_log_prob <- masked_encoder_log_prob$view(c(batch$shape[1], -1))

      # Sum over the rows (last dimension), i.e., add the log-likelihood for each instance.
      masked_encoder_log_prob <- masked_encoder_log_prob$sum(-1)

      # Same explanations here as above, but now for the full_encoder.
      full_encoder_log_prob <- full_encoder$log_prob(latent)
      full_encoder_log_prob <- full_encoder_log_prob$view(c(batch$shape[1], -1))
      full_encoder_log_prob <- full_encoder_log_prob$sum(-1)

      # Combine the estimated loss based on the formula from equation 18 on page 18 in Ivanov et al. (2019).
      # Consists of batch.shape[0] number of values
      estimate <- reconstruction_loss + masked_encoder_log_prob - full_encoder_log_prob

      # Make sure that the results are a column vector of height batch_size.
      estimate <- estimate$unsqueeze(-1)

      # Add the results to the estimates list
      estimates <- append(estimates, estimate)
    }

    # Convert from list of tensors to a single tensor using colum bind
    estimates <- torch::torch_cat(estimates, -1)

    # Use the stabilizing trick logsumexp.
    # We have worked on log-scale above, hence plus and minus and not multiplication and division,
    # while Eq. 18 in Ivanov et al. (2019) work on regular scale with multiplication and division.
    # We take the exp of the values to get back to original scale, then sum it and convert back to
    # log scale. Note that we add -log(K) instead of dividing each term by K.
    # Take the log sum exp along the rows (validation samples) then subtract log(K).
    return(torch::torch_logsumexp(estimates, -1) - log(K))
  },

  # Generate the Parameters of the Generative Distributions
  #
  # @description Generate the parameters of the generative distributions for samples from the batch.
  #
  # @details The function makes K latent representation for each object from the batch, send these
  # latent representations through the decoder to obtain the parameters for the generative distributions.
  # I.e., means and variances for the normal distributions (continuous features) and probabilities
  # for the categorical distribution (categorical features).
  # The second axis is used to index samples for an object, i.e. if the batch shape is [n x D1 x D2], then
  # the result shape is [n x K x D1 x D2]. It is better to use it inside torch::with_no_grad in order to save
  # memory. With torch::with_no_grad the method doesn't require extra memory except the memory for the result.
  #
  # @param batch Tensor of dimension batch_size x num_features containing a batch of observations.
  # @param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
  # @param K Integer. The number of imputations to be done for each observation in batch.
  generate_samples_params = function(batch, mask, K = 1) {
    # Compute the latent normal distributions obtained from only the masked encoder.
    encoders_list <- self$make_latent_distributions(batch = batch, mask = mask, only_masked_encoder = TRUE)

    # Only extract the masked encoder (torch_Normal object) as we are in the deployment phase.
    masked_encoder <- encoders_list$masked_encoder

    # Create a list to keep the sampled parameters.
    samples_params <- list()

    # Iterate over the number of imputations for each observation in the batch.
    for (i in seq(K)) {
      # Generate latent representations by using the masked encoder.
      latent <- masked_encoder$rsample()

      # Send the latent representations through the decoder.
      sample_params <- self$decoder_network(latent)

      # Collect the parameters of the induced Gaussian distributions.
      samples_params <- append(samples_params, sample_params$unsqueeze(2))
    }

    # Concatenate the list to a 3d-tensor. 2nd dimensions is the imputations.
    return(torch::torch_cat(samples_params, 2))
  }
)

# Dataset Utility Functions ===========================================================================================
#' Compute Featurewise Means and Standard Deviations
#'
#' @description Returns the means and standard deviations for all continuous features in the data set.
#' Categorical features get mean=0 and sd=1 by default.
#'
#' @param data A torch_tensor of dimension N x p containing the data.
#' @param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
#' The sizes for the continuous features can either be '0' or '1'.
#'
#' @return List containing the means and the standard deviations of the different features.
#' @author Lars Henry Berge Olsen
#' @keywords internal
compute_normalization <- function(data,
                                  one_hot_max_sizes) {
  # Create vectors of zeros that will store the means and sd for each feature.
  norm_vector_mean <- torch::torch_zeros(length(one_hot_max_sizes))
  norm_vector_std <- torch::torch_ones(length(one_hot_max_sizes))

  # Iterate over the features
  for (variable_j in seq_along(one_hot_max_sizes)) {
    # Number of one hot encoded dummy features for the j'th variable
    size_j <- one_hot_max_sizes[variable_j]

    # Check if categorical or continuous feature
    if (size_j >= 2) {
      # Categorical feature
      # Do not do anything when the feature is categorical
      next
    } else {
      # Continuous feature

      # Get the values of the i'th features
      variable_j_values <- data[, variable_j]

      # Only keep the non-missing values
      variable_j_values <- variable_j_values[variable_j_values$isnan()$logical_not()]

      # Compute the mean of the values
      variable_j_values_mean <- variable_j_values$mean()

      # Compute the sd of the values
      variable_j_values_sd <- variable_j_values$std()

      # Save the mean and sd in the right place of the vectors
      norm_vector_mean[variable_j] <- variable_j_values_mean
      norm_vector_std[variable_j] <- variable_j_values_sd
    }
  }

  # return the vectors of means and standards deviations
  return(list(
    norm_vector_mean = norm_vector_mean,
    norm_vector_std = norm_vector_std
  ))
}



#' Preprocess Data for the vaeac approach
#'
#' @description vaeac only supports numerical values. This function converts categorical features
#' to numerics with class labels 1,2,...,K, and keeps track of the map between the original and
#' new class labels. It also computes the one_hot_max_sizes.
#'
#' @param data matrix/data.frame/data.table containing the training data. Only the features and
#' not the response.
#' @param transform_all_cont_features Boolean. If we are to log transform all continuous
#' features before sending the data to vaeac. vaeac creates unbounded values, so if the continuous
#' features are strictly positive, as for Burr and Abalone data, it can be advantageous to log-transform
#' the data to unbounded form before using vaeac. If TRUE, then `vaeac_postprocess_data` will
#' take the exp of the results to get back to strictly positive values.
#'
#' @return list containing data which can be used in vaeac, maps between original and new class
#' names for categorical features, one_hot_max_sizes, and list of information about the data.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_preprocess_data <- function(data, transform_all_cont_features = FALSE) {
  # Ensure that data is data.table object
  data <- data.table::copy(data.table::as.data.table(data))

  # Create feature list which contains information about the features
  feature_list <- list()
  feature_list$labels <- colnames(data)
  feature_list$classes <- sapply(data, class)
  feature_list$factor_levels <- sapply(data, levels)
  feature_list

  # Create an return_list object to store information about the data
  return_list <- list()
  return_list$feature_list <- feature_list
  return_list$n_features <- ncol(return_list$x_train)

  # Compute the one_hot_max_sizes for the features
  one_hot_max_sizes <- unname(sapply(return_list$feature_list$factor_levels, length))
  one_hot_max_sizes[one_hot_max_sizes == 0] <- 1
  return_list$one_hot_max_sizes <- as.integer(one_hot_max_sizes)

  # col_cat = unname(return_list$feature_list$classes == "factor")
  col_cat <- sapply(data, is.factor)
  col_cont <- sapply(data, is.numeric)
  cat_in_dataset <- sum(col_cat) > 0

  # Extract the names of the categorical and continuous features
  col_cat_names <- names(col_cat[col_cat])
  col_cont_names <- names(col_cont[col_cont])

  if (cat_in_dataset) {
    # Data table contains one or several categorical features.
    # For vaeac to work, these must have levels 1,2, ..., K,
    # so we transform the levels to the desired form.

    # # Get the indices of the columns that are categorical
    # col_cat_indices = seq_along(col_cat)[col_cat]
    # col_cont_indices = seq_along(col_cont)[col_cont]

    # Lists that will store maps between the original and new class names for categorical features
    map_original_to_new_names <- list()
    map_new_to_original_names <- list()

    # Iterate over the categorical features
    for (col_cat_name in col_cat_names) {
      # Create a map from the original class names to the new class names
      map_original_to_new_names[[col_cat_name]] <- as.list(seq_along(levels(data[[col_cat_name]])))
      names(map_original_to_new_names[[col_cat_name]]) <- levels(data[[col_cat_name]])
      map_original_to_new_names[[col_cat_name]]

      # Create a map from the new class names to the original class names
      map_new_to_original_names[[col_cat_name]] <- as.list(levels(data[[col_cat_name]]))
      names(map_new_to_original_names[[col_cat_name]]) <- seq_along(levels(data[[col_cat_name]]))
      map_new_to_original_names[[col_cat_name]]

      # Update the names of the levels. Faster to use the method with as.numeric bellow.
      # levels(data[[col_cat_name]]) = unlist(map_original_to_new_names[[col_cat_name]])
    }

    # Convert the categorical features to numeric. Automatically gets class levels 1,2,...,K.
    data[, (col_cat_names) := lapply(.SD, as.numeric), .SDcols = col_cat_names]

    # Add the maps to the return_list object
    return_list$map_new_to_original_names <- map_new_to_original_names
    return_list$map_original_to_new_names <- map_original_to_new_names
  }

  # Check if we are to log transform all continuous features.
  if (transform_all_cont_features) {
    # This is not the best way. We only give an error when all features are known, i.e., during training.
    # During imputations we do not worry, as we are going to impute the NA values.
    if (!is.na(suppressWarnings(any(data[, col_cont_names, with = FALSE] <= 0)))) {
      # Small check that all continues features are strictly positive
      if (suppressWarnings(any(data[, col_cont_names, with = FALSE] <= 0))) {
        stop("The continuous features in data is not strictly positive. Cannot log-transform them.")
      }
    }

    # Log-transform the continuous features.
    data[, (col_cont_names) := lapply(.SD, log), .SDcols = col_cont_names]
  }

  # Add the numerical data table to the return_list object, and some other variables.
  return_list$transform_all_cont_features <- transform_all_cont_features
  return_list$data_preprocessed <- as.matrix(data)
  return_list$col_cat <- col_cat
  return_list$col_cat_names <- col_cat_names
  return_list$col_cont <- col_cont
  return_list$col_cont_names <- col_cont_names
  return_list$cat_in_dataset <- cat_in_dataset

  # return the return_list object
  return(return_list)
}

#' Postprocess Data Generated by a vaeac Model
#'
#' @description vaeac generates numerical values. This function converts categorical features
#' to from numerics with class labels 1,2,...,K, to factors with the original and class labels.
#'
#' @param data Matrix/data.frame/data.table containing the data generated by a vaeac model
#' @param vaeac_model_state_list List. The returned list from the `vaeac_preprocess_data` function.
#'
#' @return data.table with the generated data from a vaeac model where the categorical features
#' now have the original class names.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' # preprocessed <- vaeac_preprocess_data(abalone[, -1])
#' # preprocessed$data_preprocessed
#' # postprocessed <- vaeac_postprocess_data(preprocessed$data_preprocessed, preprocessed)
#' # postprocessed
#' # all.equal(abalone[, -1], postprocessed)
vaeac_postprocess_data <- function(data, vaeac_model_state_list) {
  # Go from vaeac type data back to data.table used in shapr
  data_dt <- as.data.table(data)
  colnames(data_dt) <- vaeac_model_state_list$feature_list$labels

  # Extract the column names for the categorical and continuous features
  col_cat_names <- vaeac_model_state_list$col_cat_names
  col_cont_names <- vaeac_model_state_list$col_cont_names

  # Extract the map from
  map_new_to_original_names <- vaeac_model_state_list$map_new_to_original_names

  # Convert all categorical features (if there are any) from numeric back to factors
  if (length(col_cat_names) > 0) data_dt[, (col_cat_names) := lapply(.SD, factor), .SDcols = col_cat_names]

  # Iterate over the categorical features
  for (col_cat_name in col_cat_names) {
    # Map the class labels names back to the original names
    levels(data_dt[[col_cat_name]]) <- unlist(map_new_to_original_names[[col_cat_name]])
  }

  # If we log transformed the continuous features in the pre-processing, we need to
  # undo the transformation by exp-transforming the features back to strictly positives.
  if (vaeac_model_state_list$transform_all_cont_features) {
    # Exp-transform the continuous features.
    data_dt[, (col_cont_names) := lapply(.SD, exp), .SDcols = col_cont_names]
  }

  # return the data table
  return(data_dt)
}



#' Transform Strictly Positive Features to Unbounded by Log Function
#'
#' @description Function that converts all continuous features by applying the log function.
#' This ensures that strictly positive values are unbounded after the transformation.
#'
#' @details Used for, e.g., Burr data and Abalone data.
#'
#' @param data Matrix/data.frame. Original data set where all continuous features are strictly positive.
#' @param one_hot_max_sizes An array of numeric of dimension p containing the one hot sizes of the p features.
#' The sizes for the continuous features can either be '0' or '1'.
#'
#' @return The input data where the continuous features have been log-transformed.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' # data <- data.frame(matrix(rgamma(1000 * 3, 2), ncol = 3)) # Simulate positive data
#' # data$X2 <- factor(data$X2 >= 2) # Create a factor, larger than mean
#' # one_hot_max_sizes <- c(1, 2, 1)
#' # print(data)
#' # data_unbounded <- log_trans_cont_features_func(data, one_hot_max_sizes)
#' # print(data_unbounded)
#' # data <- as.data.table(data) # convert to data table. Same functions works then too
#' # print(data)
#' # data_unbounded <- log_trans_cont_features_func(data, one_hot_max_sizes)
#' # print(data_unbounded)
log_trans_cont_features_func <- function(data, one_hot_max_sizes) {
  # Check if data is data.table or matrix/data.frame.
  if (any(class(data) == "data.table")) {
    # Extract which columns that are continuous.
    cont_cols <- seq_along(one_hot_max_sizes)[one_hot_max_sizes < 2]

    # # Small check that all continues features are strictly positive
    # if (suppressWarnings(any(data[,..cont_cols] <= 0))) {
    #   stop("The continuous features in data is not strictly positive. Cannot log-transform them.")
    # }

    # Make a deep copy of the data table. Otherwise, we alter the input data outside this function.
    data <- copy(data)

    # Log-transform the continuous features.
    data[, (cont_cols) := lapply(.SD, log), .SDcols = cont_cols]
  } else {
    # Extract which columns that are continuous.
    cont_cols <- one_hot_max_sizes < 2

    # # # Small check that all continues features are strictly positive
    # if (suppressWarnings(any(data[,cont_cols] <= 0))) {
    #   stop("The continuous features in data is not strictly positive. Cannot log-transform them.")
    # }

    # Log-transform the continuous features.
    data[, cont_cols] <- log(data[, cont_cols, drop = FALSE])
  }

  # Return the data
  return(data)
}


#' Transform Unbounded Features to Strictly Positive by Exp Function
#'
#' @description Function that converts all continuous features by applying the exp function.
#' This ensures that unbounded features are strictly positive after the transformation.
#'
#' @details Used for, e.g., Burr data and Abalone data.
#'
#' @param data Matrix/data.frame. Original data set where all continuous features are strictly positive.
#' @param one_hot_max_sizes An array of numeric of dimension p containing the one hot sizes of the p features.
#' The sizes for the continuous features can either be '0' or '1'.
#'
#' @return The input data where the continuous features have been exp-transformed.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' # data <- data.frame(matrix(rgamma(500 * 3, 2), ncol = 3)) # Simulate positive data
#' # data$X2 <- factor(data$X2 >= 2) # Create a factor, larger than mean
#' # one_hot_max_sizes <- c(1, 2, 1)
#' # print(data)
#' # data_unbounded <- log_trans_cont_features_func(data, one_hot_max_sizes)
#' # print(data_unbounded)
#' # data_bounded <- exp_trans_cont_features_func(data_unbounded, one_hot_max_sizes)
#' # print(data_bounded)
#' # all.equal(data, data_bounded)
#' # data <- as.data.table(data) # convert to data table. Same functions works then too
#' # print(data)
#' # data_unbounded <- log_trans_cont_features_func(data, one_hot_max_sizes)
#' # print(data_unbounded)
#' # data_bounded <- exp_trans_cont_features_func(data_unbounded, one_hot_max_sizes)
#' # print(data_bounded)
#' # all.equal(data, data_bounded)
exp_trans_cont_features_func <- function(data, one_hot_max_sizes) {
  # Check if data is data.table or matrix/data.frame.
  if (any(class(data) == "data.table")) {
    # Extract which columns that are continuous.
    cont_cols <- seq_along(one_hot_max_sizes)[one_hot_max_sizes < 2]

    # Make a deep copy of the data table. Otherwise, we alter the input data outside this function.
    data <- copy(data)

    # Exp-transform the continuous features.
    data[, (cont_cols) := lapply(.SD, exp), .SDcols = cont_cols]
  } else {
    # Extract which columns that are continuous.
    cont_cols <- one_hot_max_sizes < 2

    # Exp-transform the continuous features.
    data[, cont_cols] <- exp(data[, cont_cols, drop = FALSE])
  }

  # Return the data
  return(data)
}


## vaeac_dataset ------------------------------------------------------------------------------------------------------

#' Dataset Used by the Vaeac Model
#'
#' @description
#' Convert a the data into a [torch::dataset()] which the vaeac model creates batches from.
#'
#' @details
#' This function creates a [torch::dataset()] object that represent a map from keys to data samples.
#' It is used by the [torch::dataloader()] to load data which should be used to extract the
#' batches for all epochs in the training phase of the neural network. Note that a dataset object
#' is an R6 instanc, see \url{https://r6.r-lib.org/articles/Introduction.html}, which is classical
#' object-oriented programming, with self reference. I.e, [shapr::vaeac_dataset()] is a subclass
#' of type [torch::dataset()].
#'
#' @param X A torch_tensor contain the data of shape N x p, where N and p are the number
#' of observations and features, respectively.
#' @param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of
#' the p features. The sizes for the continuous features can either be 0 or 1.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' # p <- 5
#' # N <- 14
#' # batch_size <- 10
#' # one_hot_max_sizes <- rep(1, p)
#' # vaeac_ds <- vaeac_dataset(
#' #   torch_tensor(matrix(rnorm(p * N), ncol = p),
#' #                dtype = torch_float()),
#' #   one_hot_max_sizes)
#' # vaeac_ds
#'
#' # vaeac_dl <- torch::dataloader(
#' #   vaeac_ds,
#' #   batch_size = batch_size,
#' #   shuffle = TRUE,
#' #   drop_last = FALSE)
#' # vaeac_dl$.length()
#' # vaeac_dl$.iter()
#'
#' # vaeac_iterator <- vaeac_dl$.iter()
#' # vaeac_iterator$.next() # batch1
#' # vaeac_iterator$.next() # batch2
#' # vaeac_iterator$.next() # Empty
vaeac_dataset <- torch::dataset(

  # @field name The name of the `torch::dataset`.
  name = "vaeac_dataset",

  # @description Create a new vaeac_dataset object.
  # @param X A torch_tensor contain the data
  # @param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
  # The sizes for the continuous features can either be '0' or '1'.
  initialize = function(X,
                        one_hot_max_sizes) {
    # Save the number of observations in X
    self$N <- nrow(X)

    # Save the number of features in X
    self$p <- ncol(X)

    # Save the number of one hot dummy features for each features
    self$one_hot_max_sizes <- one_hot_max_sizes

    # Save the dataset
    self$X <- X
  },
  # @description How to fetch a data sample for a given key/index.
  .getitem = function(index) {
    X <- self$X[index, ]
  },
  # @description Return the size of the dataset
  .length = function() {
    nrow(self$X)
  }
)


## Paired Sampler  ----------------------------------------------------------------------------------------------------
#' Sampling Paired Observations
#'
#' @description
#' A sampler used to samples the batches where each instances is sampled twice
#'
#' @details
#' A sampler object that allows for paired sampling by always including each observation from the
#' [shapr::vaeac_dataset()] twice.
#' A [torch::sampler()] object can be used with [torch::dataloader()]
#' when creating batches from a torch dataset [torch::dataset()]. See more on
#' \url{https://rdrr.io/cran/torch/src/R/utils-data-sampler.R}.
#' This function does not use batch iterators, which might increase the speed.
#'
#' @param vaeac_dataset_object A [shapr::vaeac_dataset()] object containing the data.
#' @param shuffle Boolean. If `TRUE`, then the data is shuffled. If `FALSE`,
#' then the data is returned in chronological order.
#'
#' @examples
#' # Example how to use it combined with mask generators with paired sampling activated
#' # batch_size <- 4
#' # if (batch_size %% 2 == 1) batch_size <- batch_size - 1 # Make sure that batch size is even
#' # num_featuers <- 3
#' # num_observations <- 5
#' # shuffle <- TRUE
#' # data <- torch_tensor(matrix(rep(seq(num_observations), each = num_featuers),
#' #                             ncol = num_featuers, byrow = TRUE))
#' # data
#' # dataset <- vaeac_dataset(data, rep(1, num_featuers))
#' # dataload <- torch::dataloader(dataset,
#' #   batch_size = batch_size,
#' #   sampler = paired_sampler(dataset,
#' #     shuffle = shuffle
#' #   )
#' # )
#' # dataload$.length() # Number of batches, same as ceiling((2 * num_observation) / batch_size)
#' # mask_generator <- MCAR_mask_generator(paired = TRUE)
#' # coro::loop(for (batch in dataload) {
#' #   mask <- mask_generator(batch)
#' #   obs <- mask * batch
#' #   print(torch::torch_cat(c(batch, mask, obs), -1))
#' # })
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
paired_sampler <- torch::sampler(
  # @field Name of the object
  classname = "paired_sampler",
  # @description Initialize the paired_sampler object
  initialize = function(vaeac_dataset_object, shuffle = FALSE) {
    self$vaeac_dataset_object <- vaeac_dataset_object
    self$shuffle <- shuffle
  },
  # @description Get the number of observations in the datasaet
  .length = function() {
    # Multiply by two do to the sampling
    length(self$vaeac_dataset_object) * 2
  },
  # @description Function to iterate over the data
  .iter = function() {
    # Get the number of observations in the data
    n <- length(self$vaeac_dataset_object)

    # Check if the indices are to be shuffled
    if (self$shuffle) {
      # Sample a random order for the indices
      indices <- sample.int(n)
    } else {
      # Take the indices in increasing order
      indices <- seq_len(n)
    }

    # Duplicate each index and return an iterator
    coro::as_iterator(rep(indices, each = 2))
  }
)




# Neural Network Utility Functions ====================================================================================
##  MemoryLayer -------------------------------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a Memory Layer
#'
#' @description
#' The layer is used to make skip-connections inside a [torch::nn_sequential] network
#' or between several [torch::nn_sequential] networks without unnecessary code complication.
#'
#' @details
#' If `output = FALSE`, this layer stores its input in a static list
#' `storage` with the key `id`` and then passes the input to the next layer.
#' I.e., when memory layer is used in the masked encoder.
#' If `output = TRUE`, this layer takes stored tensor from the storage.
#' I.e., when memory layer is used in the decoder.
#' If `add = TRUE`, it returns sum of the stored vector and an `input`,
#' otherwise it returns their concatenation. If the tensor with specified `id`
#' is not in storage when the layer with `output = TRUE` is called, it would cause an exception.
#'
#' @param id A unique id to use as a key in the storage list.
#' @param output Boolean variable indicating if the memory layer is to store input in storage or extract from storage.
#' @param add Boolean variable indicating if the extracted value are to be added or concatenated to the input.
#' Only applicable when `output = TRUE`.
#' @param verbose Boolean variable indicating if we want to give printouts to the user.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
#' @examples
#' # net1 <- torch::nn_sequential(
#' #   MemoryLayer("#1"),
#' #   MemoryLayer("#0.1"),
#' #   torch::nn_linear(512, 256),
#' #   torch::nn_leaky_relu(),
#' #   # here add cannot be TRUE because the dimensions mismatch
#' #   MemoryLayer("#0.1", output = TRUE, add = FALSE),
#' #   torch::nn_linear(768, 256),
#' #   # the dimension after the concatenation with skip-connection is 512 + 256 = 768
#' # )
#' # net2 <- torch::nn_equential(
#' #   torch::nn_linear(512, 512),
#' #   MemoryLayer("#1", output = TRUE, add = TRUE),
#' #   ...
#' # )
#' # b <- net1(a)
#' # d <- net2(c)
#' # # net2 must be called after net1, otherwise tensor '#1' will not be in storage.
MemoryLayer <- torch::nn_module(
  # @field classname Name of the of torch::nn_module object.
  classname = "MemoryLayer",

  # @field shared_env A shared environment for all instances of MemoryLayers.
  shared_env = new.env(),

  # @description Create a new MemoryLayer object.
  # @param id A unique id to use as a key in the storage list.
  # @param output Boolean variable indicating if the memory layer is to store input in storage or extract from storage.
  # @param add Boolean variable indicating if the extracted value are to be added or concatenated to the input.
  # Only applicable when `output = TRUE`.
  # @param verbose Boolean variable indicating if we want to give printouts to the user.
  initialize = function(id,
                        output = FALSE,
                        add = FALSE,
                        verbose = FALSE) {
    self$id <- id
    self$output <- output
    self$add <- add
    self$verbose <- verbose
  },
  forward = function(input) {
    # Check if we are going to insert input into the storage or extract data from the storage.
    if (!self$output) {
      # We are to insert input into the storage list.

      # Small printout to the user
      if (self$verbose) message(sprintf("Inserting data to memory layer 'self$id = %s'.\n", self$id))

      # Save the input in the shared environment of the MemoryLayer class in the storage list.
      # Note that we do not check if self$id is unique.
      self$shared_env$storage[[self$id]] <- input

      # Return/send the input to the next layer in the network.
      return(input)
    } else {
      # We are to extract data from the storage list.

      # Small printout to the user
      if (self$verbose) message(sprintf("Extracting data from memory layer 'self$id = %s'. ", self$id))

      # Check that the memory layer has data is stored in it. If not, then thorw error.
      if (!self$id %in% names(self$shared_env$storage)) {
        stop(sprintf(
          "ValueError: Looking for memory layer 'self$id = %s', but only available memory layers are: %s.",
          self$id, paste(names(self$shared_env$storage), collapse = ", ")
        ))
      }

      # Extract the stored data for the given memory layer
      stored <- self$shared_env$storage[[self$id]]

      # If we are concatenate the input to the extracted data or add it
      if (!self$add) {
        # We are to concatenate the tensors.

        # Small printout to the user
        if (self$verbose) message(sprintf("Concatenates the tensors.\n"))

        # Concatenate the columns of the tensors.
        data <- torch::torch_cat(c(input, stored), -1)
      } else {
        # We are to add the tensors.

        # Small printout to the user
        if (self$verbose) message(sprintf("Adds the tensors.\n"))

        # Add the tensors together.
        data <- input + stored
      }

      # Return the data
      return(data)
    }
  }
)

## SkipConnection -----------------------------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a skip connection
#'
#' @description
#' Skip connection over the sequence of layers in the constructor. The module passes
#' input data sequentially through these layers and then adds original data to the result.
#'
#' @param ... network modules such as, e.g., [torch::nn_linear()], [torch::nn_relu()],
#' and [shapr::MemoryLayer()] objects. See [shapr::vaeac()] for more information.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
SkipConnection <- torch::nn_module(
  # @field classname Name of the of torch::nn_module object.
  classname = "SkipConnection",

  # @description Initialize a new SkipConnection module
  initialize = function(...) {
    self$inner_net <- torch::nn_sequential(...)
  },
  # @description What to do when a SkipConnection module is called
  forward = function(input) {
    return(input + self$inner_net(input))
  }
)




# Training Utility Functions ==========================================================================================
#' Extends Incomplete Batches by Sampling Extra Data from Dataloader
#'
#' @description If the height of the `batch` is less than `batch_size`, this function extends the `batch` with
#' data from the [torch::dataloader()] until the `batch` reaches the required size.
#' Note that `batch` is a tensor.
#'
#' @param batch The batch we want to check if has the right size, and if not extend it until it has the right size.
#' @param dataloader A [torch::dataloader()] object from which we can create an iterator object
#' and load data to extend the batch.
#' @param batch_size Integer. The number of samples to include in each batch.
#'
#' @return Returns the extended batch with the correct batch_size.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
extend_batch <- function(batch,
                         dataloader,
                         batch_size) {
  # This function could be more streamlined, as I believe that
  # it is sufficient to only call next once. If that is the case,
  # we could have removed the while with and if else, and called
  # next but only taken the first batch_size-batch$shape[1] observations.

  # batch.shape[1] is always smaller or equal to batch_size
  # if smaller we want to add data to the batch from the
  # dataloader until the batch has the correct size.
  while (batch$shape[1] != batch_size) {
    # Not the correct size, so we create an iterator
    # that can iterate over the batches in the dataloader
    dataloader_iterator <- dataloader$.iter()

    # Load the next batch
    extra_batch <- dataloader_iterator$.next()

    # If the the number of instances in extra_batch + batch
    # is larger than batch_size, then we need to remove the
    # appropriate number of instances from the extra_batch.
    if (extra_batch$shape[1] + batch$shape[1] > batch_size) {
      # Keep only the first batch_size - batch.shape[0] instances.
      extra_batch <- extra_batch[seq(batch_size - batch$shape[1]), ]
    }

    # Concatenate the original batch with the extra_batch in a rowbind manner.
    batch <- torch::torch_cat(c(batch, extra_batch), 1)
  }

  # The batch is now of the correct dimension/height
  return(batch)
}


#' Compute the Importance Sampling Estimator (Validation Error)
#'
#' @description Compute the Importance Sampling Estimator which the vaeac model
#' uses to evaluate its performance on the validation data.
#'
#' @details Compute mean IWAE log likelihood estimation of the validation set.
#' Takes validation data loader, mask generator, batch size, model (vaeac)
#' and number of IWAE latent samples per object.Returns one the estimation (float).
#' IWAE is an abbreviation for Importance Sampling Estimator
#' \deqn{\log p_{\theta, \psi}(x|y) \approx
#' \log {\frac{1}{S}\sum_{i=1}^S p_\theta(x|z_i, y) p_\psi(z_i|y) \big/ q_\phi(z_i|x,y),}}
#' where \eqn{z_i \sim q_\phi(z|x,y)}.
#' For more details, see \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#'
#' @param val_dataloader A torch dataloader which loads the validation data.
#' @param mask_generator A mask generator object that generates the masks.
#' @param batch_size Integer. The number of samples to include in each batch.
#' @param model The vaeac model.
#' @param num_samples Number of samples to generate for computing the IWAE for each validation sample.
#' @param verbose Boolean. If we are to print results to the user.
#'
#' @return The average iwae over all instances in the validation dataset.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
get_validation_iwae <- function(val_dataloader,
                                mask_generator,
                                batch_size,
                                model,
                                num_samples,
                                verbose = FALSE) {
  # Set variables to store the number of instances evaluated and avg_iwae
  cum_size <- 0
  avg_iwae <- 0

  # Iterate over all the batches in the validation set
  coro::loop(for (batch in val_dataloader) {
    # Get the number of instances in the current batch
    init_size <- batch$shape[1]

    # Extend the batch if init_size not equal to batch_size
    # returns the batch if appropriate size or adds instances
    # from validation dataloader
    batch <- extend_batch(batch, val_dataloader, batch_size)

    # Create the mask for the current batch.
    # Mask consists of zeros (observed) and ones (missing or masked)
    mask <- mask_generator(batch)

    # If the model.parameters are located on a Nivida GPU, then
    # we send batch and mask to GPU, as it is faster than CPU.
    if (model$parameters[[1]]$is_cuda) {
      batch <- batch$cuda()
      mask <- mask$cuda()
    }

    # use with torch::with_no_grad() as we in this with clause do not
    # want to compute the gradients, as they are not important
    # / not needed for doing backpropagation.
    torch::with_no_grad({
      # Get the iwae for each instance in the current batch
      # but save only the first init_size, as the other are
      # just arbitrary instances we "padded" the batch with
      # to get the appropriate shape.
      iwae <- model$batch_iwae(batch, mask, num_samples)[1:init_size, drop = FALSE]

      # Update the average iwae over all batches (over all instances)
      # This is called recursive/online updating of the mean.
      # I have verified the method. Takes the
      # old average * cum_size to get old sum of iwae
      # adds the sum of newly computed iwae. Then divide the
      # total iwae by the number of instances: cum_size + iwae.shape[0])
      avg_iwae <- (avg_iwae * (cum_size / (cum_size + iwae$shape[1])) + iwae$sum() / (cum_size + iwae$shape[1]))

      # Update the number of instances evaluated
      cum_size <- cum_size + iwae$shape[1]
    }) # End with_no_grad
  }) # End iterating over the validation samples


  # return the average iwae over all instances in the validation set.
  return(avg_iwae$to(dtype = torch::torch_float()))
}


# Probability Utility Functions =======================================================================================
#' Creates Normal Distributions
#'
#' @description Function that takes in the a tensor where the first half of the columns contains the means of the
#' normal distributions, while the latter half of the columns contains the standard deviations. The standard deviations
#' are clamped with `min_sigma` to ensure stable results. If `params` is of dimensions batch_size x 8, the function
#' will create 4 independent normal distributions for each of the observation (`batch_size` observations in total).
#'
#' @details Take a Tensor (e.g. neural network output) and return a [torch::distr_normal()] distribution.
#' This normal distribution is component-wise independent, and its dimensionality depends on the input shape.
#' First half of channels is mean (\eqn{\mu}) of the distribution, the softplus of the second half is
#' std (\eqn{\sigma}), so there is no restrictions on the input tensor. `min_sigma` is the minimal value of
#' \eqn{\sigma}. I.e., if the above softplus is less than `min_sigma`, then \eqn{\sigma} is clipped
#' from below with value `min_sigma`. This regularization is required for the numerical stability and may
#' be considered as a neural network architecture choice without any change to the probabilistic model.
#'
#' @param params Tensor containing the parameters for the normal distributions.
#' @param min_sigma The minimal variance allowed.
#'
#' @return [torch::distr_normal()] distributions with the provided means and standard deviations.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
normal_parse_params <- function(params,
                                min_sigma = 0.001) {
  # Get the number of instances
  n <- params$shape[1]

  # Then get the dimension of the parameters
  d <- params$shape[2]

  # Use double dash to get integer. Do not need it as we by construction always have 2*num_dim_latent_space
  mu <- params[, 1:(d %/% 2)] # Get the first halves which are the means

  # Get the second half which are transformed sigmas
  sigma_params <- params[, (d %/% 2 + 1):d]
  sigma <- torch::nnf_softplus(sigma_params) # ln(1 + exp(sigma_params))
  sigma <- sigma$clamp(min = min_sigma) # Make sure that sigma >= min_sigma

  # Create the normal dist. Multivariate, but with independent dimensions. Correlation = 0. So just Normal
  distr <- torch::distr_normal(loc = mu, scale = sigma)

  # Return the distribution
  return(distr)
}

#' Creates Categorical Distributions
#'
#' @description Function that takes in a tensor containing the logits for each of the K classes. Each row corresponds to
#' an observations. Send each row through the softmax function to convert from logits to probabilities that sum 1 one.
#' The function also clamps the probabilities between a minimum and maximum probability. Note that we still normalize
#' them afterward, so the final probabilities can be marginally below or above the thresholds.
#'
#' @details Take a Tensor (e. g. a part of neural network output) and return [torch::distr_categorical()]
#' distribution. The input tensor after applying softmax over the last axis contains a batch of the categorical
#' probabilities. So there are no restrictions on the input tensor. Technically, this function treats the last axis as
#' the categorical probabilities, but Categorical takes only 2D input where the first axis is the batch axis and the
#' second one corresponds to the probabilities, so practically the function requires 2D input with the batch of
#' probabilities for one categorical feature. `min_prob` is the minimal probability for each class.
#' After clipping the probabilities from below and above they are renormalized in order to be a valid distribution.
#' This regularization is required for the numerical stability and may be considered as a neural network architecture
#' choice without any change to the probabilistic model.Note that the softmax function is given by
#' \eqn{\operatorname{Softmax}(x_i) = (\exp(x_i))/(\sum_{j} \exp(x_j))}, where \eqn{x_i} are the logits and can
#' take on any value, negative and positive. The output \eqn{\operatorname{Softmax}(x_i) \in [0,1]}
#' and \eqn{\sum_{j} Softmax(x_i) = 1}.
#'
#' @param params Tensor of `dimension batch_size` x `K` containing the logits for each
#' of the `K` classes and `batch_size` observations.
#' @param min_prob For stability it might be desirable that the minimal probability is not exactly zero.
#' @param max_prob For stability it might be desirable that the maximal probability is not exactly zero.
#'
#' @return torch::distr_categorical distributions with the provided probabilities for each class.
#' @author Lars Henry Berge Olsen
#' @keywords internal
categorical_parse_params_col <- function(params, min_prob = 0, max_prob = 1) {
  # Send the parameters through the softmax to get normalized probabilities
  params <- torch::nnf_softmax(params, dim = -1)

  # Ensure that the probabilities are between the minimum and maximum allowed probabilities
  params <- torch::torch_clamp(params, min = min_prob, max = max_prob)

  # Make sure that parms sum to 1 after the clamping.
  params <- params / torch::torch_sum(params, dim = -1, keepdim = TRUE)

  # Then create a categorical distribution which will have len(params)
  # number of categories and the probability for each of them is given in params.
  distr <- torch::distr_categorical(probs = params)

  # # Could have directly used that 'dist_categorical' supports logits. But then
  # # we would not be able to clamp the probabilities. This version is 30% faster.
  # distr = torch::distr_categorical(logits = params)

  # Return the distribution
  return(distr)
}

#' Compute the KL Divergence Between Two Gaussian Distributions.
#'
#' @description Computes the KL divergence between univariate normal distributions using the analytical formula,
#' see \url{https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence#Multivariate_normal_distributions}.
#'
#' @param p a [torch::distr_normal()] object.
#' @param q a [torch::distr_normal()] object.
#'
#' @return The KL divergence between the two Gaussian distributions.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
kl_normal_normal <- function(p, q) {
  var_ratio <- (p$scale / q$scale)$pow(2)
  t1 <- ((p$loc - q$loc) / q$scale)$pow(2)
  return(0.5 * (var_ratio + t1 - 1 - var_ratio$log()))
}

# Neural Network Modules ==============================================================================================
## GaussCatSampler -----------------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a GaussCatSampler
#'
#' @description
#' The GaussCatSampler generates a sample from the generative distribution defined by
#' the output of the vaeac. It can either return/generate the most likely values or use random sampling.
#'
#' @param one_hot_max_sizes A vector of integers where the i-th entry is
#' the number of one-hot encoding for the i-th feature.
#' I.e., a categorical feature with 5 levels will have a one_hot_max_size of 5.
#' A feature with a one_hot_max_size of either 0 or 1 will be treated as a continuous feature.
#' @param min_sigma For stability it might be desirable that the minimal sigma is not too close to zero.
#' @param min_prob For stability it might be desirable that the minimal probability is not too close to zero.
#' @param sample_most_probable A boolean indicating if we are to return the mean and most probable class of
#' the Gaussian and categorical distributions, respectively (`TRUE`), or if sampling from the Gaussian and categorical
#' distributions is to be performed (`FALSE`).
#'
#' @return
#' A `GaussCatSampler` object.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
GaussCatSampler <- torch::nn_module(

  #' @field classname Type of torch::nn_module
  classname = "GaussCatSampler",

  # @description Initialize a GaussCatSampler which generates a sample from the generative
  # distribution defined by the output of the neural network.
  initialize = function(one_hot_max_sizes,
                        sample_most_probable = FALSE,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {
    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes <- one_hot_max_sizes

    # We set this to TRUE when we use this class.
    # So just return the mean for Gaussian.
    # This is not what we want in case of SHAPLEY.
    # SO WE SHOULD USE FALSE
    self$sample_most_probable <- sample_most_probable

    # We use the default values, both 1e-4.
    self$min_sigma <- min_sigma
    self$min_prob <- min_prob
  },

  # @param dist_params A matrix of form batch_size x (mu_1, sigma_1, ..., mu_p, sigma_p),
  # when only considering continuous features.
  # For categorical features, we do NOT have mu and sigma for the decoder at the end of the vaeac,
  # but rather logits for the categorical distribution.
  # @return A tensor containing the generated data.
  forward = function(distr_params) {
    # A counter to keep track of which
    cur_distr_col <- 1

    # List to store all the samples sampled from the
    # normal distribution with parameters from distr_params.
    sample <- list()

    # Iterate over the features
    for (i in seq_along(self$one_hot_max_sizes)) {
      size <- self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params <- distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col <- cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr <- normal_parse_params(params, self$min_sigma)

        # If we are going to sample the mean or do a random sampling
        if (self$sample_most_probable) {
          col_sample <- distr$mean
        } else {
          # Use rsample() here as it seems that sample() does not respect manual set seeds. Only for normal.
          # They have fixed the mistake.
          col_sample <- distr$sample()
        }
      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col <- cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_col' function.
        # distr is a "torch::distr_categorical" distribution.
        distr <- categorical_parse_params_col(params, self$min_prob)

        # If we are to return the class with highest probability or sample a
        # class from the distribution based on each class' probabilities.
        if (self$sample_most_probable) {
          # By doing [, NULL], we add an extra dimension such that the tensor is a column vector.
          col_sample <- torch::torch_max(distr$probs, -1)[[2]][, NULL]$to(dtype = torch::torch_float())
        } else {
          # Here we can use $sample() as it respects manual set seeds.
          col_sample <- distr$sample()[, NULL]$to(dtype = torch::torch_float())
        }
      }

      # Add the vector of sampled values for the i´th
      # feature to the sample list.
      sample <- append(sample, col_sample)
    }

    # Create a matrix by column binding the vectors in the list
    return(torch::torch_cat(sample, -1))
  }
)


## GaussCatSamplerMostLikely -------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a GaussCatSamplerMostLikely
#'
#' @description
#' The GaussCatSamplerrMostLikely generates the most likely samples from
#' the generative distribution defined by the output of the vaeac.
#' I.e., the layer will return the mean and most probable class for the Gaussian (continuous features)
#' and categorical (categorical features) distributions, respectively.
#'
#' @param one_hot_max_sizes A vector of integers where the i-th entry is the
#' number of one-hot encoding for the i-th feature.
#' I.e., a categorical feature with 5 levels will have a one_hot_max_size of 5.
#' A feature with a one_hot_max_size of either 0 or 1 will be treated as a continuous feature.
#' @param min_sigma For stability it might be desirable that the minimal sigma is not too close to zero.
#' @param min_prob For stability it might be desirable that the minimal probability is not too close to zero.
#'
#' @return A `GaussCatSamplerMostLikely` object.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
GaussCatSamplerMostLikely <- torch::nn_module(

  # @field classname Type of torch::nn_module
  classname = "GaussCatSamplerMostLikely",

  # @description Initialize a GaussCatSamplerMostLikely which generates the most likely
  # sample from the generative distribution defined by the output of the neural network.
  initialize = function(one_hot_max_sizes,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {
    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes <- one_hot_max_sizes

    # We use the default values, both 1e-4.
    self$min_sigma <- min_sigma
    self$min_prob <- min_prob
  },


  # @param dist_params A matrix of form batch_size x (mu_1, sigma_1, ..., mu_p, sigma_p),
  # when only considering continuous features.
  # For categorical features, we do NOT have mu and sigma for the decoder at the end of the vaeac,
  # but rather logits for the categorical distribution.
  # @return A tensor containing the generated data.
  forward = function(distr_params) {
    # A counter to keep track of which
    cur_distr_col <- 1

    # List to store all the samples sampled from the
    # normal distribution with parameters from distr_params.
    sample <- list()

    # Iterate over the features
    for (i in seq_along(self$one_hot_max_sizes)) {
      size <- self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params <- distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col <- cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr <- normal_parse_params(params, self$min_sigma)

        # We sample the mean (most likely value)
        col_sample <- distr$mean
      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col <- cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_col' function.
        # distr is a "torch::distr_categorical" distribution.
        distr <- categorical_parse_params_col(params, self$min_prob)

        # Return the class with highest probability
        # By doing [, NULL], we add an extra dimension such that the tensor is a column vector.
        col_sample <- torch::torch_max(distr$probs, -1)[[2]][, NULL]$to(dtype = torch::torch_float())
      }

      # Add the vector of sampled values for the i´th
      # feature to the sample list.
      sample <- append(sample, col_sample)
    }

    # Create a matrix by column binding the vectors in the list
    return(torch::torch_cat(sample, -1))
  }
)

## GaussCatSamplerRandom -----------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a GaussCatSamplerRandom
#'
#' @description
#' The GaussCatSamplerRandom generates random samples from the generative
#' distribution defined by the output of the vaeac. The random sample is generated by
#' sampling from the inferred Gaussian and categorical distributions for the
#' continuous and categorical features, respectively.
#'
#' @param one_hot_max_sizes A vector of integers where the i-th entry is the number of one-hot encoding
#' for the i-th feature. I.e., a categorical feature with 5 levels will have a one_hot_max_size of 5.
#' A feature with a one_hot_max_size of either 0 or 1 will be treated as a continuous feature.
#' @param min_sigma For stability it might be desirable that the minimal sigma is not too close to zero.
#' @param min_prob For stability it might be desirable that the minimal probability is not too close to zero.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
GaussCatSamplerRandom <- torch::nn_module(

  # @field classname Type of torch::nn_module
  classname = "GaussCatSamplerRandom",

  # @description
  # Initialize a GaussCatSamplerRandom which generates a sample from the
  # generative distribution defined by the output of the neural network by random sampling.
  # @return A new `GaussCatSamplerRandom` object.
  initialize = function(one_hot_max_sizes,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {
    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes <- one_hot_max_sizes

    # We use the default values, both 1e-4.
    self$min_sigma <- min_sigma
    self$min_prob <- min_prob
  },



  # @param dist_params A matrix of form batch_size x (mu_1, sigma_1, ..., mu_p, sigma_p),
  # when only considering continuous features.
  # For categorical features, we do NOT have mu and sigma for the decoder at the end of the vaeac,
  # but rather logits for the categorical distribution.
  # @return A tensor containing the generated data.
  forward = function(distr_params) {
    # A counter to keep track of which
    cur_distr_col <- 1

    # List to store all the samples sampled from the
    # normal distribution with parameters from distr_params.
    sample <- list()

    # Iterate over the features
    for (i in seq_along(self$one_hot_max_sizes)) {
      size <- self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params <- distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col <- cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr <- normal_parse_params(params, self$min_sigma)

        # Sample from the inferred Gaussian distributions
        # Use rsample() here as it seems that sample() does not respect manual set seeds. Only for normal.
        # They have fixed the mistake.
        col_sample <- distr$sample()
      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col <- cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_col' function.
        # distr is a "torch::distr_categorical" distribution.
        distr <- categorical_parse_params_col(params, self$min_prob)

        # Sample a class from the distribution based on each class' probabilities.
        # By doing [, NULL], we add an extra dimension such that the tensor is a column vector.
        # Here we can use $sample() as it respects manual set seeds.
        col_sample <- distr$sample()[, NULL]$to(dtype = torch::torch_float())
      }

      # Add the vector of sampled values for the i´th
      # feature to the sample list.
      sample <- append(sample, col_sample)
    }

    # Create a matrix by column binding the vectors in the list
    return(torch::torch_cat(sample, -1))
  }
)


## GaussCatParameters --------------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a GaussCatParameters
#'
#' @description
#' The GaussCatParameters module extracts the parameters
#' from the inferred generative Gaussian and categorical distributions for the
#' continuous and categorical features, respectively.
#'
#' If `one_hot_max_sizes` is \eqn{[4, 1, 1, 2]}, then the inferred distribution parameters for one observation is the
#' vector \eqn{[p_{00}, p_{01}, p_{02}, p_{03}, \mu_1, \sigma_1, \mu_2, \sigma_2, p_{30}, p_{31}]}, where
#' \eqn{\operatorname{Softmax}([p_{00}, p_{01}, p_{02}, p_{03}])} and \eqn{\operatorname{Softmax}([p_{30}, p_{31}])}
#' are probabilities of the first and the fourth feature categories respectively in the model generative distribution,
#' and Gaussian(\eqn{\mu_1, \sigma_1^2}) and Gaussian(\eqn{\mu_2, \sigma_2^2}) are the model generative distributions
#' on the second and the third features.
#'
#' @param one_hot_max_sizes A vector of integers where the i-th entry is the number of
#' one-hot encoding for the i-th feature.
#' I.e., a categorical feature with 5 levels will have a one_hot_max_size of 5.
#' A feature with a one_hot_max_size of either 0 or 1 will be treated as a continuous feature.
#' @param min_sigma For stability it might be desirable that the minimal sigma is not too close to zero.
#' @param min_prob For stability it might be desirable that the minimal probability is not too close to zero.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
GaussCatParameters <- torch::nn_module(
  # @field classname Type of torch::nn_module
  classname = "GaussCatParameters",

  # @description
  # Initialize a `GaussCatParameters` which extract the parameters from the
  # generative distribution defined by the output of the neural network.
  # @return A new `GaussCatParameters` object.
  initialize = function(one_hot_max_sizes,
                        min_sigma = 1e-4,
                        min_prob = 1e-4) {
    # one-hot max size of i-th feature, if i-th feature is categorical,
    # and 0 or 1 if i-th feature is real-valued.
    self$one_hot_max_sizes <- one_hot_max_sizes

    # We use the default values, both 1e-4.
    self$min_sigma <- min_sigma
    self$min_prob <- min_prob
  },


  # @param dist_params A matrix of form batch_size x (mu_1, sigma_1, ..., mu_p, sigma_p), when only
  # considering continuous features. For categorical features, we do NOT have mu and sigma for the
  # decoder at the end of the vaeac, but rather logits for the categorical distribution.
  # @return A tensor containing the final parameters of the generative distributions (after transformations).
  forward = function(distr_params) {
    # A counter to keep track of which
    cur_distr_col <- 1

    # List to store all the generative parameters from the normal and categorical distributions
    parameters <- list()

    # Iterate over the features
    for (i in seq_along(self$one_hot_max_sizes)) {
      size <- self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous
        # Gaussian distribution
        # Get the mu and sigma for the current feature, for each instance
        params <- distr_params[, cur_distr_col:(cur_distr_col + 1)]
        cur_distr_col <- cur_distr_col + 2

        # generative model distribution for the feature
        # so create batch_size number of normal distributions.
        distr <- normal_parse_params(params, self$min_sigma)

        # Combine the current parameters
        current_parameters <- torch::torch_cat(c(distr$mean, distr$scale), -1)
      } else {
        # Categorical distribution

        # Extract the logits of the different classes for the ith categorical variable
        params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
        cur_distr_col <- cur_distr_col + size

        # Generate the categorical distribution based on the logits, which are
        # transformed and clamped in the 'categorical_parse_params_col' function.
        # distr is a "torch::distr_categorical" distribution.
        distr <- categorical_parse_params_col(params, self$min_prob)

        # Extract the current probabailities for each classs
        current_parameters <- distr$probs
      }

      # Add the tensor of current parameters for the i´th feature to the parameters list
      parameters <- append(parameters, current_parameters)
    }

    # Create a torch_tensor by column binding the tensors in the list
    return(torch::torch_cat(parameters, -1))
  }
)





## GaussCatLoss --------------------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a GaussCatLoss
#'
#' @description
#' The GaussCatLoss module/layer computes the log probability
#' of the `groundtruth` for each object given the mask and the distribution parameters.
#' That is, the log-likelihoods of the true/full training observations based on the
#' generative distributions parameters `distr_params` inferred by the masked versions of the observations.
#'
#' @details
#' Note that the module works with mixed data represented as 2-dimensional inputs and it
#' works correctly with missing values in `groundtruth` as long as they are represented by NaNs.
#'
#' @param one_hot_max_sizes A vector of integers where the i-th entry is the number of
#' one-hot encoding for the i-th feature.
#' I.e., a categorical feature with 5 levels will have a one_hot_max_size of 5.
#' A feature with a one_hot_max_size of either 0 or 1 will be treated as a continuous feature.
#' @param min_sigma For stability it might be desirable that the minimal sigma is not too close to zero.
#' @param min_prob For stability it might be desirable that the minimal probability is not too close to zero.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
GaussCatLoss <- torch::nn_module(

  # @field classname Type of torch::nn_module
  classname = "GaussCatLoss",

  # @description Initialize a `GaussCatLoss`.
  # @return A new `GaussCatLoss` object.
  initialize = function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
    self$one_hot_max_sizes <- one_hot_max_sizes
    self$min_sigma <- min_sigma
    self$min_prob <- min_prob
  },
  forward = function(groundtruth, distr_params, mask) {
    # Which column in distr_params we now consider.
    # Either increases with 2 in cont case (mu and sigma)
    # or in increases with one-hot encoding size in cat case.
    cur_distr_col <- 1

    # List to store the log probabilities.
    log_prob <- list()

    # Iterate over the features
    for (i in seq_along(self$one_hot_max_sizes)) {
      size <- self$one_hot_max_sizes[i]

      if (size <= 1) {
        # Continuous feature
        # Gaussian distribution

        # select groundtruth, mask and distr_params for i-th feature
        groundtruth_col <- groundtruth[, i, drop = FALSE] # Look at the ith column of the truth
        mask_col <- mask[, i, drop = FALSE] # Get the ith column of the mask

        # These are the mean and sigma for the ith feature,
        # so dimensions batch_size x 2
        params <- distr_params[, cur_distr_col:(cur_distr_col + 1), drop = FALSE]
        cur_distr_col <- cur_distr_col + 2

        # generative model distribution for the feature
        distr <- normal_parse_params(params, self$min_sigma)
        # distr$mean
        # distr$scale
        # log(1 + exp(params[,2]))

        # copy ground truth column, so that zeroing nans will not affect the original data
        gt_col_nansafe <- groundtruth_col$clone()$detach()

        # If groundtruth don't have any nans then this line does not change anything
        nan_mask <- torch::torch_isnan(groundtruth_col)
        gt_col_nansafe[nan_mask] <- 0
        # Everything that was nan is now 0.

        # Mask_col masks both the nan/missing values
        # and the artificially masked values.
        # We want to compute the the log prob
        # only over the artificially missing
        # features, so we omit the missing values.
        # Bottom of page 5.

        # So we remove the masking of the missing values
        # So those ones in mask_col which are there due
        # to missing values are now turned in to zeros.
        mask_col <- mask_col * (torch::torch_logical_not(nan_mask))$to(dtype = torch::torch_float())

        # Get the log-likelihood, but only of the masked values
        # i.e., the ones hat are masked by the masking filter MCARGenerator
        # So this one is batch_size x 1.
        # So this is the log-lik of observing the ground truth
        # given the current parameters, for only the
        # artificially masked features.
        col_log_prob <- distr$log_prob(gt_col_nansafe) * mask_col
      } else {
        # Categorical feature

        # categorical distribution

        # Extract the ground truth and mask
        groundtruth_col <- groundtruth[, i, drop = FALSE] # Look at the ith column of the truth
        mask_col <- mask[, i, drop = FALSE] # Get the ith column of the mask

        # Extract the probabilities for each of the K-classes for the ith feature,
        # The dimension is batch_size x size.
        params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1), drop = FALSE]
        cur_distr_col <- cur_distr_col + size

        # Create a categorical distrbution based on the extracted parameters.
        # Returns a "torch::distr_categorical" distribution.
        # Ensure that the probabbility for each class is at least self$min_prob.
        distr <- categorical_parse_params_col(params, self$min_prob)

        # copy ground truth column, so that zeroing nans will not affect the original data
        gt_col_nansafe <- groundtruth_col$clone()$detach()

        # If groundtruth don't have any nans then this line does not change anything
        nan_mask <- torch::torch_isnan(groundtruth_col)
        gt_col_nansafe[nan_mask] <- 0

        # compute the mask of the values
        # which we consider in the log probability
        # So we remove the masking of the missing values
        # So those ones in mask_col which are there due
        # to missing values are now turned in to zeros.
        mask_col <- mask_col * (torch::torch_logical_not(nan_mask))$to(dtype = torch::torch_float())
        col_log_prob <- distr$log_prob(gt_col_nansafe$squeeze())[, NULL] * mask_col
        # col_log_prob = distr$log_prob(gt_col_nansafe) * mask_col
      }

      # append the column of log probabilities for the i-th feature
      # (for those instances that are masked) into log_prob list
      # log_prob.append(col_log_prob)
      log_prob <- append(log_prob, col_log_prob)
      # log_prob is now a list of length num_features, where each
      # element is a tensor batch_size x 1 containing the log-lik
      # of the parameters of masked values.
    }

    # concatenate the list so we get a tensor of dim batch x features
    # Then we sum along the the rows. i.e., for each observation in the
    # batch. So a tensor of length batch size.
    return(torch::torch_cat(log_prob, 2)$sum(-1))
  }
)


## CategoricalToOneHotLayer -------------------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a CategoricalToOneHotLayer
#'
#' @description
#' The CategoricalToOneHotLayer module/layer expands categorical features into one-hot vectors,
#' because multi-layer perceptrons are known to work better with this data representation.
#' It also replaces NaNs with zeros in order so that further layers may work correctly.
#'
#' @param one_hot_max_sizes A vector of integers where the i-th entry is the number of
#' one-hot encoding for the i-th feature.
#' I.e., a categorical feature with 5 levels will have a one_hot_max_size of 5.
#' A feature with a one_hot_max_size of either 0 or 1 will be treated as a continuous feature.
#' @param add_nans_map_for_columns Optional list which contains indices of columns which
#' is_nan masks are to be appended to the result tensor. This option is necessary for the full
#' encoder to distinguish whether value is to be reconstructed or not.
#'
#' @details
#' Note that the module works with mixed data represented as 2-dimensional inputs and it
#' works correctly with missing values in `groundtruth` as long as they are repsented by NaNs.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
CategoricalToOneHotLayer <- torch::nn_module(

  # @field classname Type of torch::nn_module
  classname = "CategoricalToOneHotLayer",
  initialize = function(one_hot_max_sizes, add_nans_map_for_columns = NULL) {
    # Here one_hot_max_sizes includes zeros at the end of the list
    # one_hot_max_sizes + [0] * len(one_hot_max_sizes)
    # So if we have that features have this many categories [1, 2, 3, 1],
    # then we get that one_hot_max_sizes = [1, 2, 3, 1, 0, 0, 0, 0]
    self$one_hot_max_sizes <- one_hot_max_sizes

    # Is always an empty column for the Masked Encoder network
    # while it is a list [0, 1, ..., length(one_hot_max_sizes)-1)
    # for the Full Encoder network.
    # So for the Full Encoder  network we apply the nan masks to each column/feature
    self$add_nans_map_for_columns <- add_nans_map_for_columns
  },
  forward = function(input) {
    # input = torch::torch_cat(c(batch, mask), -1)
    # Input is torch::torch_cat(c(batch, mask), -1), so a matrix of
    # dimension batch_size x 2*sum(one_hot_max_sizes)
    # At least for continuous data where one_hot_max_sizes
    # only consists of ones. Recall that ONE_HOT_MAX_SIZES
    # are padded with zeros at the end in this function.

    # Get the number of instances in the input batch.
    n <- input$shape[1]

    # variable to store the outcolumns.
    # that is the input columns / one hot encoding
    # + is nan.mask.
    out_cols <- NULL

    # We iterate over the features and get the number
    # of categories for each feature.
    # so i goes from 0 to 2*num_features-1
    # For i in [num_features, 2*num_features-1] will have size <= 1,
    # even for categorical features.
    i <- 1
    for (i in seq_along(self$one_hot_max_sizes)) {
      size <- self$one_hot_max_sizes[i]

      # Distinguish between continuous and categorical features
      if (size <= 1) {
        # If size <= 1, then the feature is continuous
        # just copy it and replace NaNs with zeros
        # OR, the last half of self.one_hot_max_sizes

        # Take the ith column of the input
        # NOTE THAT THIS IS NOT A DEEP COPY, so changing out_col changes input
        out_col <- input[, i:i] # maybe add '$clone()$detach()'?

        # check if any of the values are nan, i.e., missing
        nan_mask <- torch::torch_isnan(out_col)

        # set all the missing values to 0.
        # THIS CHANGES THE INPUT VARIABLE.
        out_col[nan_mask] <- 0
      } else {
        # Categorical feature

        # Get the categories for each instance for the ith feature
        # start to count at zero. So if we have 2 cat, then this
        # vector will contains zeros and ones.
        cat_idx <- input[, i:i] # $clone()$detach()

        # Check if any of the categories are nan / missing
        nan_mask <- torch::torch_isnan(cat_idx)

        # TODO:
        # THIS DOES NOT WORK BECAUSE, WHEN WE APPLY A MASK WE SET UNOBSERVED CATEGORICAL
        # FEATEURS TO CLASS 0. (AS WE MULITPLY WITH 0), BUT THEN NNF_ONE_HOT DOES NOT WORK
        # AS IT ONLY TAKES CLASSES 1,2,...,K.
        # # Done it using two approaches. One using torch, which is faster, so use it.
        # # Then we do the following:
        # {
        #   # Set the nan values to category one.
        #   # This will be overwritten below, but needed for nnf_one_hot to work
        #   # as it does not support observations without a class.
        #   cat_idx[nan_mask] = 1
        #
        #   # Generate the one hot encoding
        #   out_col = nnf_one_hot(cat_idx$to(dtype = torch::torch_long()),
        #                                    size)$to(dtype = torch::torch_bool())$squeeze()
        #
        #   # Overwrite the entries which we said belonged to category one, but was actually missing.
        #   out_col[torch::torch_repeat_interleave(nan_mask, as.integer(size), dim = -1)] = 0
        # }
        # or we can use the default R. THIS IS A BIT SLOWER. LOOK MORE INTO IT!

        # Set the nan values to 0
        cat_idx[nan_mask] <- 0

        # Create a matrix, where the jth row is the one-hot encoding of the ith feature of the jth instance.
        out_col <- matrix(0, nrow = n, ncol = size)
        out_col[cbind(seq(n), as.matrix(cat_idx))] <- 1
        out_col <- torch::torch_tensor(out_col, device = input$device)

      }

      # append this feature column to the result
      # out_col is n x size = batch_size x num_categories_for_this_feature
      out_cols <- torch::torch_cat(c(out_cols, out_col), dim = -1)

      # if necessary, append isnan mask of this feature to the result
      # which we always do for the proposal network.
      # This only happens for the first half of the i's,
      # so for i = 1, ..., num_features.
      if (i %in% self$add_nans_map_for_columns) {
        # so we add the columns of nan_mask
        out_cols <- torch::torch_cat(c(out_cols, nan_mask$to(dtype = torch::torch_float())), dim = -1)
      }
    }

    # ONLY FOR CONTINUOUS FEATURES.
    # out_cols now is a list of num_features tensors of shape n x size
    # = n x 1 for continuous variables. So we concatenate them
    # to get a matrix of dim n x 2*num_features (in cont case) for
    # prior net, but for proposal net, it is n x 3*num_features
    # They take the form  [batch1, is.nan1, batch2, is.nan2, …,
    # batch12, is.nan12, mask1, mask2, …, mask12]
    return(out_cols)
  }
)




# Mask Generators =====================================================================================================
## MCAR_mask_generator ------------------------------------------------------------------------------------------------

#' Missing Completely at Random (MCAR) Mask Generator
#'
#' @description
#' A mask generator which masks the entries in the input completely at random.
#'
#' @details
#' The mask generator mask each element in the `batch` (N x p) using a component-wise independent Bernoulli
#' distribution with probability `masking_ratio`. Default values for `masking_ratio` is 0.5, so all
#' masks are equally likely to be generated, including the empty and full masks.
#' The function returns a mask of the same shape as the input `batch`, and the `batch` can contain
#' missing values, indicated by the "NaN" token, which will always be masked.
#'
#' @param masking_ratio Numeric between 0 and 1. The probability for an entry in the generated mask to be 1 (masked).
#' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
#' If `TRUE`, then `batch` must be sampled using [shapr::paired_sampler()] which ensures that the `batch` contains
#' two instances for each original observation. That is, `batch` \eqn{= [X_1, X_1, X_2, X_2, X_3, X_3, ...]}, where
#' each entry \eqn{X_j} is a row of dimension \eqn{p} (i.e., the number of features).
#'
#' @section Shape:
#' - Input: \eqn{(N, p)} where N is the number of observations in the `batch` and \eqn{p} is the number of features.
#' - Output: \eqn{(N, p)}, same shape as the input
#'
#' @examples
#' # mask_gen <- MCAR_mask_generator(masking_ratio = 0.5, paired_sampling = FALSE)
#' # batch <- torch::torch_randn(c(5, 3))
#' # mask_gen(batch)
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
MCAR_mask_generator <- torch::nn_module(

  # @field name Type of mask generator
  name = "MCAR_mask_generator",

  # @description
  # Initialize a missing completely at random mask generator.
  # @param masking_ratio The probability for an entry in the generated mask to be 1 (masked).
  # @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
  # If TRUE, then batch must be sampled using `paired_sampler` which creates batches where
  # the first half and second half of the rows are duplicates of each other. That is,
  # batch = [row1, row1, row2, row2, row3, row3, ...].
  # @return A new `MCAR_mask_generator` object.
  initialize = function(masking_ratio = 0.5,
                        paired_sampling = FALSE) {
    self$masking_ratio <- masking_ratio
    self$paired_sampling <- paired_sampling
  },

  # @description
  # Generates a MCAR mask by calling self$MCAR_mask_generator_function function.
  # @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  # entries are missing. If any are missing, then the returned mask will ensure that
  # these missing entries are masked.
  forward = function(batch) {
    self$MCAR_mask_generator_function(batch,
      prob = self$masking_ratio,
      paired_sampling = self$paired_sampling
    )
  },

  # @description Missing Completely At Random Mask Generator: A mask generator where the masking
  # is determined by component-wise independent Bernoulli distribution.
  #
  # @details
  # Function that takes in a batch of observations and the probability
  # of masking each element based on a component-wise independent Bernoulli
  # distribution. Default value is 0.5, so all masks are equally likely to be trained.
  # Function returns the mask of same shape as batch.
  # Note that the batch can contain missing values, indicated by the "NaN" token.
  # The mask will always mask missing values.
  #
  # @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  # entries are missing. If any are missing, then the returned mask will ensure that
  # these missing entries are masked.
  # @param prob Numeric between 0 and 1. The probability that an entry will be masked.
  # @param seed Integer. Used to set the seed for the sampling process such that we
  # can reproduce the same masks.
  # @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
  # If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  # the first half and second half of the rows are duplicates of each other. That is,
  # batch = [row1, row1, row2, row2, row3, row3, ...].
  #
  # @examples
  # MCAR_mask_generator_function(torch::torch_rand(c(5, 3)))
  #
  # @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
  # observed feature value will be masked. '0' means that the entry is NOT masked,
  # i.e., the feature value will be observed/given/available.
  MCAR_mask_generator_function = function(batch,
                                          prob = 0.5,
                                          seed = NULL,
                                          paired_sampling = FALSE) {
    # If the user specify a seed for reproducibility
    if (!is.null(seed)) set.seed(seed)

    # Get the number of entries in the batch.
    size <- prod(batch$shape)

    # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
    if (paired_sampling) size <- size / 2

    # Check for missing values in the batch
    nan_mask <- batch$isnan()$to(torch::torch_float())

    # # Torch version, but marginally slower than r version when batch_size <= 128 and num_features <= 50
    # mask = torch::torch_bernoulli(torch::torch_full_like(batch, prob))

    # Create the Bernoulli mask where an element is masked (1) with probability 'prob'.
    mask <- torch::torch_tensor(
      matrix(
        sample(c(0, 1),
          size = size,
          replace = TRUE,
          prob = c(prob, 1 - prob)
        ),
        ncol = ncol(batch)
      ),
      dtype = torch::torch_float()
    )

    # If paired sampling, then concatenate the inverse mask.
    if (paired_sampling) {
      # Create the new order to ensure correct order [m1, !m1, m2, !m2, ...].
      new_order <- c(matrix(seq_len(nrow(batch)), nrow = 2, byrow = TRUE))

      # Concatenate the inverse mask and reorder.
      mask <- torch::torch_cat(c(mask, !mask), 1L)[new_order, ]
    }

    # Final mask all entries that is either missing or artificially masked
    # by the Bernoulli mask. A value of 1 means that the entry is masked.
    return(mask + nan_mask >= 1)
  }
)


## Specified_prob_mask_generator -------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a Specified_prob_mask_generator
#'
#' @description A mask generator which masks the entries based on specified probabilities.
#'
#' @details
#' A class that takes in the probabilities of having d masked observations.  I.e., for M dimensional data,
#' masking_probs is of length M+1, where the d'th entry is the probability of having d-1 masked values.
#'
#' A mask generator that first samples the number of entries 'd' to be masked in
#' the 'M'-dimensional observation 'x' in the batch based on the given M+1 probabilities. The
#' 'd' masked are uniformly sampled from the 'M' possible feature indices. The d'th entry
#' of the probability of having d-1 masked values.
#'
#' Note that MCAR_mask_generator with p = 0.5 is the same as using [shapr::Specified_prob_mask_generator()]
#' with `masking_ratio` = choose(M, 0:M), where M is the number of features. This function was initially
#' created to check if increasing the probability of having a masks with many masked features improved
#' vaeac's performance by focusing more on these situations during training.
#'
#' @param masking_probs An M+1 numerics containing the probabilities masking 'd' of the (0,...M) entries
#' for each observation.
#' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
#' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
#' the first half and second half of the rows are duplicates of each other. That is,
#' `batch = [row1, row1, row2, row2, row3, row3, ...]`.
#'
#' @examples
#' # probs <- c(1, 8, 6, 3, 2)
#' # mask_gen <- Specified_prob_mask_generator(probs)
#' # masks <- mask_gen(torch::torch_randn(c(10000, length(probs)) - 1))
#' # empirical_prob <- table(as.array(masks$sum(2)))
#' # empirical_prob / sum(empirical_prob)
#' # probs / sum(probs)
#'
#' @keywords internal
Specified_prob_mask_generator <- torch::nn_module(

  # @field name Type of mask generator
  name = "Specified_prob_mask_generator",

  # @description Initialize a specified_probability mask generator.
  initialize = function(masking_probs,
                        paired_sampling = FALSE) {
    self$masking_probs <- masking_probs / sum(masking_probs)
    self$paired_sampling <- paired_sampling
  },

  # @description Generates a specified probability mask by calling the
  # self$Specified_prob_mask_generator_function.
  # @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the entries are
  # missing. If any are missing, then the returned mask will ensure that these missing entries are masked.
  forward = function(batch) {
    self$Specified_prob_mask_generator_function(batch,
      masking_prob = self$masking_probs,
      paired_sampling = self$paired_sampling
    )
  },


  # @description Specified Probability Mask Generator:
  # A mask generator that first samples the number of entries 'd' to be masked in
  # the 'M'-dimensional observation 'x' in the batch based on the given M+1 probabilities. The
  # 'd' masked are uniformly sampled from the 'M' possible feature indices. The d'th entry
  # of the probability of having d-1 masked values.
  #
  # @details Note that MCAR_mask_generator with p = 0.5 is the same as using Specified_prob_mask_generator
  # with masking_ratio = choose(M, 0:M), where M is the number of features. This function was initially
  # created to check if increasing the probability of having a masks with many masked features improved
  # vaeac's performance by focusing more on these situations during training.
  #
  # @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  # entries are missing. If any are missing, then the returned mask will ensure that
  # these missing entries are masked.
  # @param masking_probs An M+1 numerics containing the probabilities masking 'd' (0,...M) entries for each observation.
  # @param seed Integer. Used to set the seed for the sampling process such that we
  # can reproduce the same masks.
  # @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  # If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  # the first half and second half of the rows are duplicates of each other. That is,
  # `batch = [row1, row1, row2, row2, row3, row3, ...]`.
  #
  # @examples Specified_prob_mask_generator_function(torch::torch_rand(c(5, 4)), masking_probs = c(2,7,5,3,3))
  #
  # @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
  # observed feature value will be masked. '0' means that the entry is NOT masked,
  # i.e., the feature value will be observed/given/available.
  Specified_prob_mask_generator_function = function(batch,
                                                           masking_probs,
                                                           seed = NULL,
                                                           paired_sampling = FALSE) {
    # # Check for valid input.
    # if (ncol(batch) != (length(masking_probs)-1)) {
    #   stop(sprintf("Number of probabilities should be one more than the number of features: %d != 1 + %d.\n",
    #        length(masking_probs), ncol(batch)))
    # }

    # If the user specify a seed for reproducibility
    if (!is.null(seed)) set.seed(seed)

    # Check if we are doing paired sampling.
    if (paired_sampling) {
      # Divide the size by two as we later concatenate with the inverse mask.
      size <- size / 2
    }

    # Check for missing values in the batch
    nan_mask <- batch$isnan()$to(torch::torch_float())

    # Get the number of features
    n_features <- ncol(batch)

    # Get the number of observations in the batch
    size <- nrow(batch)

    # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
    if (paired_sampling) size <- size / 2

    # Sample the number of masked features in each row.
    num_masked_each_row <- sample(
      x = seq(0, n_features),
      size = size,
      replace = TRUE,
      prob = masking_probs
    )

    # Crate the mask matrix
    mask <- torch::torch_zeros_like(batch)
    for (i in seq(size)) {
      if (num_masked_each_row[i] != 0) {
        mask[i, sample(n_features, size = num_masked_each_row[i], replace = FALSE)] <- 1
      }
    }

    # If paired sampling, then concatenate the inverse mask.
    if (paired_sampling) {
      # Create the new order to ensure correct order [m1, !m1, m2, !m2, ...].
      new_order <- c(matrix(seq_len(nrow(batch)), nrow = 2, byrow = TRUE))

      # Concatenate the inverse mask and reorder.
      mask <- torch::torch_cat(c(mask, !mask), 1L)[new_order, ]
    }

    # Final mask masks all entries that is either missing or artificially masked
    # by the generated mask. A value of 1 means that the entry is going to be masked.
    return(mask + nan_mask >= 1)
  }
)

## Specified_masks_mask_generator -------------------------------------------------------------------------------------
#' A [torch::nn_module] Representing a Specified_masks_mask_generator
#'
#' @description
#' A mask generator which masks the entries based on sampling provided 1D masks with corresponding probabilities.
#' Used for Shapley value estimation when only a subset of coalitions are used to compute the Shapley values.
#'
#'
#' @param masks Matrix/Tensor of possible/allowed 'masks' which we sample from.
#' @param masks_probs Array of 'probabilities' for each of the masks specified in 'masks'.
#' Note that they do not need to be between 0 and 1 (e.g. sampling frequency).
#' They are scaled, hence, they only need to be positive.
#' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
#' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
#' the first half and second half of the rows are duplicates of each other. That is,
#' `batch = [row1, row1, row2, row2, row3, row3, ...]`.
#' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
#' entries are missing. If any are missing, then the returned mask will ensure that
#' these missing entries are masked.
#'
#' @examples
#' # masks <- torch_tensor(matrix(c(0,0,1,0, 1,0,1,0, 1,1,1,1), nrow = 3, ncol = 4, byrow = TRUE))
#' # masks_probs <- c(3, 1, 6)
#' # mask_gen <- Specified_masks_mask_generator(masks = masks, masks_probs = masks_probs)
#' # empirical_prob <- table(as.array(mask_gen(torch::torch_randn(c(10000, ncol(masks))))$sum(-1)))
#' # empirical_prob / sum(empirical_prob)
#' # masks_probs / sum(masks_probs)
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
Specified_masks_mask_generator <- torch::nn_module(

  #' @field name Type of mask generator
  name = "Specified_masks_mask_generator",

  #' @description Initialize a specified masks mask generator.
  initialize = function(masks,
                        masks_probs,
                        paired_sampling = FALSE) {
    self$masks <- masks
    self$masks_probs <- masks_probs / sum(masks_probs)
    self$paired_sampling <- paired_sampling
  },

  # @description Generates a mask by calling self$Specified_masks_mask_generator_function function.
  # @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  # entries are missing. If any are missing, then the returned mask will ensure that
  # these missing entries are masked.
  forward = function(batch) {
    self$Specified_masks_mask_generator_function(batch,
      masks = self$masks,
      masks_probs = self$masks_probs,
      paired_sampling = self$paired_sampling
    )
  },

  # @description
  # Sampling Masks from the Provided Masks with the Given Probabilities
  #
  # @details
  # Function that takes in a 'batch' of observations and matrix of possible/allowed
  # 'masks' which we are going to sample from based on the provided probability in 'masks_probs'.
  # Function returns a mask of same shape as batch. Note that the batch can contain missing values,
  # indicated by the "NaN" token. The mask will always mask missing values.
  #
  # @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
  # entries are missing. If any are missing, then the returned mask will ensure that
  # these missing entries are masked.
  # @param masks Matrix/Tensor of possible/allowed 'masks' which we sample from.
  # @param masks_probs Array of 'probabilities' for each of the masks specified in 'masks'.
  # Note that they do not need to be between 0 and 1. They are scaled, hence, they only need to be positive.
  # @param seed Integer. Used to set the seed for the sampling process such that we
  # can reproduce the same masks.
  # @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
  # If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
  # the first half and second half of the rows are duplicates of each other. That is,
  # batch = [row1, row1, row2, row2, row3, row3, ...].
  #
  # @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
  # observed feature value will be masked. '0' means that the entry is NOT masked,
  # i.e., the feature value will be observed/given/available.
  Specified_masks_mask_generator_function = function(batch,
                                                     masks,
                                                     masks_probs,
                                                     seed = NULL,
                                                     paired_sampling = FALSE) {
    # Hashed out as we checking takes extra time
    # # Some check for valid inputs.
    # if (ncol(batch) != ncol(masks)) {
    #   stop(sprintf("The number of features in the 'batch' and 'masks' are incompatible: %d != %d.",
    #                ncol(batch), ncol(masks)))
    # }
    #
    # if (nrow(masks) != length(masks_probs)) {
    #   stop(sprintf("The number of masks in 'masks' and the number of probabilities
    #                 in 'masks_probs' are incompatible: %d != %d.",
    #                nrow(masks), length(masks_probs)))
    # }

    # Set seed if the user specifies a seed for reproducibility.
    if (!is.null(seed)) set.seed(seed)

    # Check for missing values in the batch
    nan_mask <- batch$isnan()$to(torch::torch_float())

    # Get the number of masks to choose from
    n_masks <- nrow(masks)

    # Get the number of observations in the batch
    size <- nrow(batch)

    # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
    if (paired_sampling) size <- size / 2

    # Sample 'n_observation' masks from the possible masks by first sampling the row indices
    # based on the given mask probabilities and then use these indices to extract the masks.
    mask_rows_indices <- sample.int(
      n = n_masks,
      size = size,
      replace = TRUE,
      prob = masks_probs
    )
    mask <- torch::torch_tensor(masks[mask_rows_indices, ], dtype = torch::torch_float())

    # If paired sampling, then concatenate the inverse mask.
    if (paired_sampling) {
      # Create the new order to ensure correct order [m1, !m1, m2, !m2, ...].
      new_order <- c(matrix(seq_len(nrow(batch)), nrow = 2, byrow = TRUE))

      # Concatenate the inverse mask and reorder.
      mask <- torch::torch_cat(c(mask, !mask), 1L)[new_order, ]
    }

    # Final mask masks all entries that is either missing or artificially masked
    # by the generated mask. A value of 1 means that the entry is going to be masked.
    return(mask + nan_mask >= 1)
  }
)
