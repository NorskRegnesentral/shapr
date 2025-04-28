# VAEAC Model ==========================================================================================================
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
#' @param one_hot_max_sizes A torch tensor of dimension `n_features` containing the one hot sizes of the `n_features`
#' features. That is, if the `i`th feature is a categorical feature with 5 levels, then `one_hot_max_sizes[i] = 5`.
#' While the size for continuous features can either be `0` or `1`.
#' @param width Integer. The number of neurons in each hidden layer in the neural networks
#' of the masked encoder, full encoder, and decoder.
#' @param depth Integer. The number of hidden layers in the neural networks of the
#' masked encoder, full encoder, and decoder.
#' @param latent_dim Integer. The number of dimensions in the latent space.
#' @param activation_function A [torch::nn_module()] representing an activation function such as, e.g.,
#' [torch::nn_relu()], [torch::nn_leaky_relu()], [torch::nn_selu()],
#' [torch::nn_sigmoid()].
#' @param skip_conn_layer Boolean. If we are to use skip connections in each layer, see [shapr::skip_connection()].
#' If `TRUE`, then we add the input to the outcome of each hidden layer, so the output becomes
#' \eqn{X + \operatorname{activation}(WX + b)}. I.e., the identity skip connection.
#' @param skip_conn_masked_enc_dec Boolean. If we are to apply concatenating skip
#' connections between the layers in the masked encoder and decoder. The first layer of the masked encoder will be
#' linked to the last layer of the decoder. The second layer of the masked encoder will be
#' linked to the second to last layer of the decoder, and so on.
#' @param batch_normalization Boolean. If we are to use batch normalization after the activation function.
#' Note that if `skip_conn_layer` is TRUE, then the normalization is
#' done after the adding from the skip connection. I.e, we batch normalize the whole quantity X + activation(WX + b).
#' @param paired_sampling Boolean. If we are doing paired sampling. I.e., if we are to include both coalition S
#' and \eqn{\bar{S}} when we sample coalitions during training for each batch.
#' @param mask_generator_name String specifying the type of mask generator to use. Need to be one of
#' 'mcar_mask_generator', 'specified_prob_mask_generator', and 'specified_masks_mask_generator'.
#' @param masking_ratio Scalar. The probability for an entry in the generated mask to be 1 (masked).
#' Not used if `mask_gen_coalitions` is given.
#' @param mask_gen_coalitions Matrix containing the different coalitions to learn.
#' Must be given if `mask_generator_name = 'specified_masks_mask_generator'`.
#' @param mask_gen_coalitions_prob Numerics containing the probabilities
#' for sampling each mask in `mask_gen_coalitions`.
#' Array containing the probabilities for sampling the coalitions in `mask_gen_coalitions`.
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
vaeac <- function(one_hot_max_sizes,
                  width = 32,
                  depth = 3,
                  latent_dim = 8,
                  activation_function = torch::nn_relu,
                  skip_conn_layer = FALSE,
                  skip_conn_masked_enc_dec = FALSE,
                  batch_normalization = FALSE,
                  paired_sampling = FALSE,
                  mask_generator_name = c(
                    "mcar_mask_generator",
                    "specified_prob_mask_generator",
                    "specified_masks_mask_generator"
                  ),
                  masking_ratio = 0.5,
                  mask_gen_coalitions = NULL,
                  mask_gen_coalitions_prob = NULL,
                  sigma_mu = 1e4,
                  sigma_sigma = 1e-4) {
  # Check that a valid mask_generator was provided.
  mask_generator_name <- match.arg(mask_generator_name)

  vaeac_tmp <- torch::nn_module(
    # Name of the torch::nn_module object
    classname = "vaeac",

    # Initializing a vaeac model
    initialize = function(one_hot_max_sizes,
                          width,
                          depth,
                          latent_dim,
                          activation_function,
                          skip_conn_layer,
                          skip_conn_masked_enc_dec,
                          batch_normalization,
                          paired_sampling,
                          mask_generator_name,
                          masking_ratio,
                          mask_gen_coalitions,
                          mask_gen_coalitions_prob,
                          sigma_mu,
                          sigma_sigma) {
      # Get the number of features
      n_features <- length(one_hot_max_sizes)

      # Extra strings to add to names of layers depending on if we use memory layers and/or batch normalization.
      # If FALSE, they are just an empty string and do not effect the names.
      name_extra_memory_layer <- ifelse(skip_conn_masked_enc_dec, "_and_memory", "")
      name_extra_batch_normalize <- ifelse(batch_normalization, "_and_batch_norm", "")

      # Set up an environment that the memory_layer objects will use as "memory", i.e., where they store the tensors.
      memory_layer_env <- new.env()

      # Save some of the initializing hyperparameters to the vaeac object. Others are saved later.
      self$one_hot_max_sizes <- one_hot_max_sizes
      self$depth <- depth
      self$width <- width
      self$latent_dim <- latent_dim
      self$activation_function <- activation_function
      self$skip_conn_layer <- skip_conn_layer
      self$skip_conn_masked_enc_dec <- skip_conn_masked_enc_dec
      self$batch_normalization <- batch_normalization
      self$sigma_mu <- sigma_mu
      self$sigma_sigma <- sigma_sigma
      self$paired_sampling <- paired_sampling

      # Save the how to compute the loss and how to sample from the vaeac model.
      self$reconstruction_log_prob <- gauss_cat_loss(one_hot_max_sizes)
      self$sampler_most_likely <- gauss_cat_sampler_most_likely(one_hot_max_sizes)
      self$sampler_random <- gauss_cat_sampler_random(one_hot_max_sizes)
      self$generative_parameters <- gauss_cat_parameters(one_hot_max_sizes)
      self$n_features <- n_features
      self$vlb_scale_factor <- 1 / n_features

      ##### Generate the mask generator
      if (mask_generator_name == "mcar_mask_generator") {
        # Create a mcar_mask_generator and attach it to the vaeac object. Note that masking_ratio is a singleton here.
        self$mask_generator <- mcar_mask_generator(
          masking_ratio = masking_ratio,
          paired_sampling = paired_sampling
        )

        # Attach the masking ratio to the vaeac object.
        self$masking_ratio <- masking_ratio
      } else if (mask_generator_name == "specified_prob_mask_generator") {
        # Create a specified_prob_mask_generator and attach it to the vaeac object.
        # Note that masking_ratio is an array here.
        self$mask_generator <- specified_prob_mask_generator(
          masking_probs = masking_ratio,
          paired_sampling = paired_sampling
        )

        # Attach the masking probabilities to the vaeac object.
        self$masking_probs <- masking_ratio
      } else if (mask_generator_name == "specified_masks_mask_generator") {
        # Small check that they have been provided.
        if (is.null(mask_gen_coalitions) | is.null(mask_gen_coalitions_prob)) {
          cli::cli_abort(paste0(
            "Both 'mask_gen_coalitions' and 'mask_gen_coalitions_prob' ",
            "must be provided when using 'specified_masks_mask_generator'."
          ))
        }

        # Create a specified_masks_mask_generator and attach it to the vaeac object.
        self$mask_generator <- specified_masks_mask_generator(
          masks = mask_gen_coalitions,
          masks_probs = mask_gen_coalitions_prob,
          paired_sampling = paired_sampling
        )

        # Save the possible masks and corresponding probabilities to the vaeac object.
        self$masks <- mask_gen_coalitions
        self$masks_probs <- mask_gen_coalitions_prob
      } else {
        # Print error to user.
        cli::cli_abort(paste0(
          "`mask_generator_name` must be one of 'mcar_mask_generator', 'specified_prob_mask_generator', or ",
          "'specified_masks_mask_generator', and not '", mask_generator_name, "'."
        ))
      }

      ##### Full Encoder
      full_encoder_network <- torch::nn_sequential()

      # Full Encoder: Input layer
      full_encoder_network$add_module(
        module = categorical_to_one_hot_layer(c(one_hot_max_sizes, rep(0, n_features)), seq(n_features)),
        name = "input_layer_cat_to_one_hot"
      )
      full_encoder_network$add_module(
        module = torch::nn_linear(
          in_features = sum(apply(rbind(one_hot_max_sizes, rep(1, n_features)), 2, max)) + n_features * 2,
          out_features = width
        ),
        name = "input_layer_linear"
      )
      full_encoder_network$add_module(
        module = activation_function(),
        name = "input_layer_layer_activation"
      )
      if (batch_normalization) {
        full_encoder_network$add_module(
          module = torch::nn_batch_norm1d(width),
          name = "input_layer_layer_batch_norm"
        )
      }

      # Full Encoder: Hidden layers
      for (i in seq(depth)) {
        if (skip_conn_layer) {
          # Add identity skip connection. Such that the input is added to the output of the linear layer
          # and activation function: output = X + activation(WX + b).
          full_encoder_network$add_module(
            module = skip_connection(
              torch::nn_linear(width, width),
              activation_function(),
              if (batch_normalization) torch::nn_batch_norm1d(width)
            ),
            name = paste0("hidden_layer_", i, "_skip_conn_with_linear_and_activation", name_extra_batch_normalize)
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
          if (batch_normalization) {
            full_encoder_network$add_module(
              module = torch::nn_batch_norm1d(width),
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
        module = categorical_to_one_hot_layer(c(one_hot_max_sizes, rep(0, n_features))),
        name = "input_layer_cat_to_one_hot"
      )
      if (skip_conn_masked_enc_dec) {
        masked_encoder_network$add_module(
          module = memory_layer(id = "#input", shared_env = memory_layer_env),
          name = "input_layer_memory"
        )
      }
      masked_encoder_network$add_module(
        module = torch::nn_linear(
          in_features = sum(apply(rbind(one_hot_max_sizes, rep(1, n_features)), 2, max)) + n_features,
          out_features = width
        ),
        name = "input_layer_linear"
      )
      masked_encoder_network$add_module(
        module = activation_function(),
        name = "input_layer_activation"
      )
      if (batch_normalization) {
        masked_encoder_network$add_module(
          module = torch::nn_batch_norm1d(width),
          name = "input_layer_batch_norm"
        )
      }

      # Masked Encoder: Hidden layers
      for (i in seq(depth)) {
        if (skip_conn_layer) {
          # Add identity skip connection. Such that the input is added to the output of the linear layer
          # and activation function: output = X + activation(WX + b).
          # Also check inside skip_connection if we are to use memory_layer. I.e., skip connection with
          # concatenation from masked encoder to decoder.
          masked_encoder_network$add_module(
            module = skip_connection(
              if (skip_conn_masked_enc_dec) memory_layer(id = paste0("#", i), shared_env = memory_layer_env),
              torch::nn_linear(width, width),
              activation_function()
            ),
            name = paste0("hidden_layer_", i, "_skip_conn_with_linear_and_activation", name_extra_memory_layer)
          )
          if (batch_normalization) {
            masked_encoder_network$add_module(
              module = torch::nn_batch_norm1d(width),
              name = paste0("hidden_layer_", i, "_batch_norm")
            )
          }
        } else {
          # Do not use skip connections and do not add the input to the output.
          if (skip_conn_masked_enc_dec) {
            masked_encoder_network$add_module(
              module = memory_layer(id = paste0("#", i), shared_env = memory_layer_env),
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
          if (batch_normalization) {
            masked_encoder_network$add_module(
              module = torch::nn_batch_norm1d(width),
              name = paste0("hidden_layer_", i, "_batch_norm")
            )
          }
        }
      }

      # Masked Encoder: Go to latent space
      if (skip_conn_masked_enc_dec) {
        masked_encoder_network$add_module(
          module = memory_layer(id = paste0("#", depth + 1), shared_env = memory_layer_env),
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
      if (batch_normalization) {
        decoder_network$add_module(
          module = torch::nn_batch_norm1d(width),
          name = "latent_space_layer_batch_norm"
        )
      }

      # Get the width of the hidden layers in the decoder. Needs to be multiplied with two if
      # we use skip connections between masked encoder and decoder as we concatenate the tensors.
      width_decoder <- ifelse(skip_conn_masked_enc_dec, 2 * width, width)

      # Same for the input dimension to the last layer in decoder that yields the distribution params.
      extra_params_skip_con_mask_enc <-
        ifelse(test = skip_conn_masked_enc_dec,
          yes = sum(apply(rbind(one_hot_max_sizes, rep(1, n_features)), 2, max)) + n_features,
          no = 0
        )

      # Will need an extra hidden layer if we use skip connection from masked encoder to decoder
      # as we send the full input layer of the masked encoder to the last layer in the decoder.
      depth_decoder <- ifelse(skip_conn_masked_enc_dec, depth + 1, depth)

      # Decoder: Hidden layers
      for (i in seq(depth_decoder)) {
        if (skip_conn_layer) {
          # Add identity skip connection. Such that the input is added to the output of the linear layer and activation
          # function: output = X + activation(WX + b). Also check inside skip_connection if we are to use memory_layer.
          # I.e., skip connection with concatenation from masked encoder to decoder. If TRUE, then the memory layers
          # extracts the corresponding input used in the masked encoder and concatenate them with the current input.
          # We add the memory layers in the opposite direction from how they were created. Thus, we get a classical
          # U-net with latent space at the bottom and a connection between the layers on the same height of the U-shape.
          decoder_network$add_module(
            module = torch::nn_sequential(
              skip_connection(
                if (skip_conn_masked_enc_dec) {
                  memory_layer(id = paste0("#", depth - i + 2), shared_env = memory_layer_env, output = TRUE)
                },
                torch::nn_linear(width_decoder, width),
                activation_function()
              )
            ),
            name = paste0("hidden_layer_", i, "_skip_conn_with_linear_and_activation", name_extra_memory_layer)
          )
          if (batch_normalization) {
            decoder_network$add_module(
              module = torch::nn_batch_norm1d(n_features = width),
              name = paste0("hidden_layer_", i, "_batch_norm")
            )
          }
        } else {
          # Do not use skip connections and do not add the input to the output.
          if (skip_conn_masked_enc_dec) {
            decoder_network$add_module(
              module = memory_layer(id = paste0("#", depth - i + 2), shared_env = memory_layer_env, output = TRUE),
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
          if (batch_normalization) {
            decoder_network$add_module(
              module = torch::nn_batch_norm1d(width),
              name = paste0("hidden_layer_", i, "_batch_norm")
            )
          }
        }
      }

      # Decoder: Go the parameter space of the generative distributions
      # Concatenate the input to the first layer of the masked encoder to the last layer of the decoder network.
      if (skip_conn_masked_enc_dec) {
        decoder_network$add_module(
          module = memory_layer(id = "#input", shared_env = memory_layer_env, output = TRUE),
          name = "output_layer_memory"
        )
      }
      # Linear layer to the parameters of the generative distributions Gaussian and Categorical.
      # Note that sum(apply(rbind(one_hot_max_sizes, rep(1, n_features)), 2, max)) is the number of
      # one hot variables to the masked encoder and n_features represents the binary variables if
      # the features was masked/missing or not when they entered the masked encoder.
      # The output dimension is 2 for the continuous features and K_i for categorical feature X_i,
      # where K_i is the number of classes the i'th categorical feature can take on.
      decoder_network$add_module(
        module = torch::nn_linear(
          in_features = width + extra_params_skip_con_mask_enc,
          out_features = sum(apply(rbind(one_hot_max_sizes, rep(2, n_features)), 2, max))
        ),
        name = "output_layer_linear"
      )

      # Save the networks to the vaeac object
      self$full_encoder_network <- full_encoder_network
      self$masked_encoder_network <- masked_encoder_network
      self$decoder_network <- decoder_network

      # Compute the number of trainable parameters in the different networks and save them
      n_para_full_encoder <- sum(sapply(full_encoder_network$parameters, function(p) prod(p$size())))
      n_para_masked_encoder <- sum(sapply(masked_encoder_network$parameters, function(p) prod(p$size())))
      n_para_decoder <- sum(sapply(decoder_network$parameters, function(p) prod(p$size())))
      n_para_total <- n_para_full_encoder + n_para_masked_encoder + n_para_decoder
      self$n_train_param <- rbind(n_para_total, n_para_full_encoder, n_para_masked_encoder, n_para_decoder)
    },

    # Forward functions are required in torch::nn_modules, but is it not needed in the way we have implemented vaeac.
    forward = function(...) {
      msg <- "No forward function implemented for vaeac!"
      cli::cli_warn(c("!" = msg))
      return("No forward function implemented for vaeac!")
    },

    # Apply Mask to Batch to Create Observed Batch
    #
    # description Clones the batch and applies the mask to set masked entries to 0 to create the observed batch.
    #
    # param batch Tensor of dimension batch_size x n_features containing a batch of observations.
    # param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
    make_observed = function(batch, mask) {
      observed <- batch$clone()$detach() # Clone and detach the batch from the graph (remove gradient element)
      observed[mask == 1] <- 0 # Apply the mask by masking every entry in batch where 'mask' is 1.
      return(observed) # Return the observed batch where masked entries are set to 0.
    },

    # Compute the Latent Distributions Inferred by the Encoders
    #
    # description Compute the parameters for the latent normal distributions inferred by the encoders.
    # If `only_masked_encoder = TRUE`, then we only compute the latent normal distributions inferred by the
    # masked encoder. This is used in the deployment phase when we do not have access to the full observation.
    #
    # param batch Tensor of dimension batch_size x n_features containing a batch of observations.
    # param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
    # param only_masked_encoder Boolean. If we are only to compute the latent distributions for the masked encoder.
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
        full_encoder <- vaeac_normal_parse_params(params = full_encoder_params, min_sigma = 1e-3)
      }

      # Column bind the batch and the mask to create the observed information sent to the masked encoder.
      observed_info <- torch::torch_cat(c(observed, mask), dim = -1)

      # Compute the latent normal dist parameters (mu, sigma) for the masked
      # encoder by sending the observed values and the mask to the masked encoder.
      masked_encoder_params <- self$masked_encoder_network(observed_info)

      # Create the latent normal distributions based on the parameters (mu, sigma) from the masked encoder
      masked_encoder <- vaeac_normal_parse_params(params = masked_encoder_params, min_sigma = 1e-3)

      # Return the full and masked encoders
      return(list(
        full_encoder = full_encoder,
        masked_encoder = masked_encoder
      ))
    },

    # Compute the Regularizes for the Latent Distribution Inferred by the Masked Encoder.
    #
    # description The masked encoder (prior) distribution regularization in the latent space.
    # This is used to compute the extended variational lower bound used to train vaeac, see
    # Section 3.3.1 in Olsen et al. (2022).
    # Though regularizing prevents the masked encoder distribution parameters from going to infinity,
    # the model usually doesn't diverge even without this regularization. It almost doesn't affect
    # learning process near zero with default regularization parameters which are recommended to be used.
    #
    # param masked_encoder The torch_Normal object returned when calling the masked encoder.
    masked_encoder_regularization = function(masked_encoder) {
      # Extract the number of observations. Same as batch_size.
      n_observations <- masked_encoder$mean$shape[1]

      # Extract the number of dimension in the latent space.
      n_latent_dimensions <- masked_encoder$mean$shape[2]

      # Extract means and ensure correct shape (batch_size x latent_dim).
      mu <- masked_encoder$mean$view(c(n_observations, n_latent_dimensions))

      # Extract the sigmas and ensure correct shape (batch_size x latent_dim).
      sigma <- masked_encoder$scale$view(c(n_observations, n_latent_dimensions))

      # Note that sum(-1) indicates that we sum together the columns.
      # mu_regularizer is then a tensor of length n_observations
      mu_regularizer <- -(mu^2)$sum(-1) / (2 * self$sigma_mu^2)

      # sigma_regularizer is then also a tensor of length n_observations.
      sigma_regularizer <- (sigma$log() - sigma)$sum(-1) * self$sigma_sigma

      # Add the regularization terms together and return them.
      return(mu_regularizer + sigma_regularizer)
    },

    # Compute the Variational Lower Bound for the Observations in the Batch
    #
    # description Compute differentiable lower bound for the given batch of objects and mask.
    # Used as the (negative) loss function for training the vaeac model.
    #
    # param batch Tensor of dimension batch_size x n_features containing a batch of observations.
    # param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
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

      # Send the latent samples through the decoder and get the batch_size x 2*n_features (in cont case)
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
      kl <- vaeac_kl_normal_normal(full_encoder, masked_encoder)$view(c(batch$shape[1], -1))$sum(-1)

      # Return the variational lower bound with the prior regularization. See Section 3.3.1 in Olsen et al. (2022)
      return(reconstruction_loss - kl + masked_encoder_regularization)
    },

    # Compute the Importance Sampling Estimator for the Observations in the Batch
    #
    # description Compute IWAE log likelihood estimate with K samples per object.
    #
    # details Technically, it is differentiable, but it is recommended to use it for
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
    # param batch Tensor of dimension batch_size x n_features containing a batch of observations.
    # param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
    # param K Integer. The number of samples generated to compute the IWAE for each observation in `batch`.
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

        # Send the latent samples through the decoder and get the batch_size x 2*n_features (in cont case)
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
    # description Generate the parameters of the generative distributions for samples from the batch.
    #
    # details The function makes K latent representation for each object from the batch, send these
    # latent representations through the decoder to obtain the parameters for the generative distributions.
    # I.e., means and variances for the normal distributions (continuous features) and probabilities
    # for the categorical distribution (categorical features).
    # The second axis is used to index samples for an object, i.e. if the batch shape is [n x D1 x D2], then
    # the result shape is [n x K x D1 x D2]. It is better to use it inside torch::with_no_grad in order to save
    # memory. With torch::with_no_grad the method doesn't require extra memory except the memory for the result.
    #
    # param batch Tensor of dimension batch_size x n_features containing a batch of observations.
    # param mask Tensor of zeros and ones indicating which entries in batch to mask. Same dimension as `batch`.
    # param K Integer. The number of imputations to be done for each observation in batch.
    generate_samples_params = function(batch, mask, K = 1) {
      # Compute the latent normal distributions obtained from only the masked encoder.
      encoders_list <- self$make_latent_distributions(batch = batch, mask = mask, only_masked_encoder = TRUE)

      # Only extract the masked encoder (torch_Normal object) as we are in the deployment phase.
      masked_encoder <- encoders_list$masked_encoder

      # Create a list to keep the sampled parameters.
      samples_params <- list()

      # Iterate over the number of imputations for each observation in the batch.
      for (i in seq(K)) {
        latent <- masked_encoder$rsample() # Generate latent representations by using the masked encoder
        sample_params <- self$decoder_network(latent) # Send the latent representations through the decoder
        samples_params <- append(samples_params, sample_params$unsqueeze(2)) # Store the inferred Gaussian distributions
      }

      # Concatenate the list to a 3d-tensor. 2nd dimensions is the imputations.
      return(torch::torch_cat(samples_params, 2))
    }
  )
  return(vaeac_tmp(
    one_hot_max_sizes = one_hot_max_sizes,
    width = width,
    depth = depth,
    latent_dim = latent_dim,
    activation_function = activation_function,
    skip_conn_layer = skip_conn_layer,
    skip_conn_masked_enc_dec = skip_conn_masked_enc_dec,
    batch_normalization = batch_normalization,
    paired_sampling = paired_sampling,
    mask_generator_name = mask_generator_name,
    masking_ratio = masking_ratio,
    mask_gen_coalitions = mask_gen_coalitions,
    mask_gen_coalitions_prob = mask_gen_coalitions_prob,
    sigma_mu = sigma_mu,
    sigma_sigma = sigma_sigma
  ))
}

# Dataset Utility Functions ============================================================================================
#' Compute Featurewise Means and Standard Deviations
#'
#' @description Returns the means and standard deviations for all continuous features in the data set.
#' Categorical features get \eqn{mean = 0} and \eqn{sd = 1} by default.
#'
#' @inheritParams vaeac
#' @param data A torch_tensor of dimension `n_observation` x `n_features` containing the data.
#'
#' @return List containing the means and the standard deviations of the different features.
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_compute_normalization <- function(data, one_hot_max_sizes) {
  # Create vectors of zeros that will store the means and sd for each feature.
  norm_vector_mean <- torch::torch_zeros(length(one_hot_max_sizes))
  norm_vector_std <- torch::torch_ones(length(one_hot_max_sizes))

  # Iterate over the features
  for (variable_j in seq_along(one_hot_max_sizes)) {
    size_j <- one_hot_max_sizes[variable_j] # Number of one hot encoded dummy features for the j'th variable
    if (size_j >= 2) next # Do nothing when the feature is categorical as we cannot normalize it
    variable_j_values <- data[, variable_j] # Get the values of the i'th features
    variable_j_values <- variable_j_values[variable_j_values$isnan()$logical_not()] # Only keep the non-missing values
    variable_j_values_mean <- variable_j_values$mean() # Compute the mean of the values
    variable_j_values_sd <- variable_j_values$std() # Compute the sd of the values
    norm_vector_mean[variable_j] <- variable_j_values_mean # Save the mean in the right place in norm_vector_mean
    norm_vector_std[variable_j] <- variable_j_values_sd # Save the sd in the right place in norm_vector_std
  }

  # return the vectors of means and standards deviations
  return(list(norm_vector_mean = norm_vector_mean, norm_vector_std = norm_vector_std))
}

#' Preprocess Data for the vaeac approach
#'
#' @description vaeac only supports numerical values. This function converts categorical features
#' to numerics with class labels 1,2,...,K, and keeps track of the map between the original and
#' new class labels. It also computes the one_hot_max_sizes.
#'
#' @param data matrix/data.frame/data.table containing the training data. Only the features and
#' not the response.
#' @param log_exp_cont_feat Boolean. If we are to log transform all continuous
#' features before sending the data to vaeac. vaeac creates unbounded values, so if the continuous
#' features are strictly positive, as for Burr and Abalone data, it can be advantageous to log-transform
#' the data to unbounded form before using vaeac. If TRUE, then `vaeac_postprocess_data` will
#' take the exp of the results to get back to strictly positive values.
#' @param norm_mean Torch tensor (optional). A 1D array containing the means of the columns of `x_torch`.
#' @param norm_std Torch tensor (optional). A 1D array containing the stds of the columns of `x_torch`.
#'
#' @return list containing data which can be used in vaeac, maps between original and new class
#' names for categorical features, one_hot_max_sizes, and list of information about the data.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_preprocess_data <- function(data, log_exp_cont_feat = FALSE,
                                  normalize = TRUE, norm_mean = NULL, norm_std = NULL) {
  # Ensure that data is data.table object
  data <- data.table::copy(data.table::as.data.table(data))

  # Create feature list which contains information about the features
  feature_list <- list()
  feature_list$labels <- colnames(data)
  feature_list$classes <- sapply(data, class)
  feature_list$factor_levels <- sapply(data, levels)

  # Create an return_list object to store information about the data
  return_list <- list()
  return_list$feature_list <- feature_list
  return_list$n_features <- ncol(return_list$x_train)

  # Compute the one_hot_max_sizes for the features
  one_hot_max_sizes <- unname(sapply(return_list$feature_list$factor_levels, length))
  one_hot_max_sizes[one_hot_max_sizes == 0] <- 1
  return_list$one_hot_max_sizes <- as.integer(one_hot_max_sizes)

  # Get the categorical and continuous features
  col_cat <- sapply(data, is.factor)
  col_cont <- sapply(data, is.numeric)
  cat_in_dataset <- sum(col_cat) > 0

  # Extract the names of the categorical and continuous features
  col_cat_names <- names(col_cat[col_cat])
  col_cont_names <- names(col_cont[col_cont])

  if (cat_in_dataset) {
    # We have one or several categorical features and need to ensure that these have levels 1,2,...,K for vaeac to work

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
    }

    # Convert the categorical features to numeric. Automatically gets class levels 1,2,...,K.
    data[, (col_cat_names) := lapply(.SD, as.numeric), .SDcols = col_cat_names]

    # Add the maps to the return_list object
    return_list$map_new_to_original_names <- map_new_to_original_names
    return_list$map_original_to_new_names <- map_original_to_new_names
  }

  # Check if we are to log transform all continuous features.
  if (log_exp_cont_feat) {
    if (any(data[, ..col_cont_names] <= 0, na.rm = TRUE)) { # Add na.rm as data can contain NaN values to be imputed
      cli::cli_abort("The continuous features cannot be log-transformed as they are not strictly positive.")
    }
    data[, (col_cont_names) := lapply(.SD, log), .SDcols = col_cont_names]
  }

  # Add the numerical data table to the return_list object, and some other variables.
  return_list$log_exp_cont_feat <- log_exp_cont_feat
  return_list$data_preprocessed <- as.matrix(data)
  return_list$col_cat <- col_cat
  return_list$col_cat_names <- col_cat_names
  return_list$col_cont <- col_cont
  return_list$col_cont_names <- col_cont_names
  return_list$cat_in_dataset <- cat_in_dataset

  # Check if we are to normalize the data. Then normalize it and add it to the return list.
  if (normalize) {
    data_norm_list <- vaeac_normalize_data(
      data_torch = torch::torch_tensor(return_list$data_preprocessed),
      norm_mean = norm_mean,
      norm_std = norm_std,
      one_hot_max_sizes = one_hot_max_sizes
    )
    return_list <- c(return_list, data_norm_list)
  }

  return(return_list)
}

#' Normalize mixed data for `vaeac`
#'
#' @description
#' Compute the mean and std for each continuous feature, while the categorical features will have mean 0 and std 1.
#'
#' @inheritParams vaeac_preprocess_data
#' @inheritParams vaeac
#'
#' @return A list containing the normalized version of `x_torch`, `norm_mean` and `norm_std`.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_normalize_data <- function(data_torch, one_hot_max_sizes, norm_mean = NULL, norm_std = NULL) {
  if (xor(!is.null(norm_mean), !is.null(norm_std))) cli::cli_abort("Both `norm_mean` and `norm_std` must be provided.")

  if (is.null(norm_mean) && is.null(norm_std)) {
    # Compute the mean and std for each continuous feature, while the categorical features will have mean 0 and std 1
    mean_and_sd <- vaeac_compute_normalization(data_torch, one_hot_max_sizes)
    norm_mean <- mean_and_sd$norm_vector_mean
    norm_std <- mean_and_sd$norm_vector_std

    # Make sure that the standard deviation is not too low, in that case clip it.
    norm_std <- norm_std$max(other = torch::torch_tensor(1e-9))
  }

  # Normalize the data to have mean 0 and std 1.
  data_normalized_torch <- (data_torch - norm_mean) / norm_std

  return(list(data_normalized_torch = data_normalized_torch, norm_mean = norm_mean, norm_std = norm_std))
}

#' Postprocess Data Generated by a vaeac Model
#'
#' @description vaeac generates numerical values. This function converts categorical features
#' to from numerics with class labels 1,2,...,K, to factors with the original and class labels.
#'
#' @param data data.table containing the data generated by a vaeac model
#' @param vaeac_model_state_list List. The returned list from the `vaeac_preprocess_data` function or
#' a loaded checkpoint list of a saved vaeac object.
#'
#' @return data.table with the generated data from a vaeac model where the categorical features
#' now have the original class names.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_postprocess_data <- function(data, vaeac_model_state_list) {
  # Go from vaeac type data back to data.table used in shapr
  if (!"data.table" %in% class(data)) data <- as.data.table(data)
  colnames(data) <- vaeac_model_state_list$feature_list$labels

  # Extract the column names for the categorical and continuous features, and the map from new to original name
  col_cat_names <- vaeac_model_state_list$col_cat_names
  col_cont_names <- vaeac_model_state_list$col_cont_names
  map_new_to_original_names <- vaeac_model_state_list$map_new_to_original_names

  # Convert all categorical features (if there are any) from numeric back to factors with the original class names
  if (length(col_cat_names) > 0) {
    lapply(col_cat_names, function(col_cat_name) {
      data[, (col_cat_name) := lapply(
        .SD,
        factor,
        labels = map_new_to_original_names[[col_cat_name]],
        levels = seq_along(map_new_to_original_names[[col_cat_name]])
      ),
      .SDcols = col_cat_name
      ]
    })
  }

  # Apply the exp transformation if we applied the log transformation in the pre-processing to the positive features
  if (vaeac_model_state_list$log_exp_cont_feat) data[, (col_cont_names) := lapply(.SD, exp), .SDcols = col_cont_names]

  # Return the postprocessed data table
  return(data)
}


#' Dataset used by the `vaeac` model
#'
#' @description
#' Convert a the data into a [torch::dataset()] which the vaeac model creates batches from.
#'
#' @details
#' This function creates a [torch::dataset()] object that represent a map from keys to data samples.
#' It is used by the [torch::dataloader()] to load data which should be used to extract the
#' batches for all epochs in the training phase of the neural network. Note that a dataset object
#' is an R6 instance, see \url{https://r6.r-lib.org/articles/Introduction.html}, which is classical
#' object-oriented programming, with self reference. I.e, [shapr::vaeac_dataset()] is a subclass
#' of type [torch::dataset()].
#'
#' @inheritParams vaeac
#' @param X A torch_tensor contain the data of shape N x p, where N and p are the number
#' of observations and features, respectively.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
vaeac_dataset <- function(X, one_hot_max_sizes) {
  vaeac_dataset_tmp <- torch::dataset(
    name = "vaeac_dataset", # field name The name of the `torch::dataset`.

    # description Create a new vaeac_dataset object.
    # param X A torch_tensor containing the data
    # param one_hot_max_sizes A torch tensor of dimension p containing the one hot sizes of the p features.
    # The sizes for the continuous features can either be '0' or '1'.
    initialize = function(X, one_hot_max_sizes) {
      # Save the number of observations and features in X, the one hot dummy feature sizes and the dataset
      self$N <- nrow(X)
      self$p <- ncol(X)
      self$one_hot_max_sizes <- one_hot_max_sizes
      self$X <- X
    },
    .getbatch = function(index) self$X[index, , drop = FALSE], # Get a batch of data based on the provided indices
    .length = function() nrow(self$X) # Get the number of observations in the dataset
  )
  return(vaeac_dataset_tmp(X = X, one_hot_max_sizes = one_hot_max_sizes))
}


#' Sampling Paired Observations
#'
#' @description
#' A sampler used to samples the batches where each instances is sampled twice
#'
#' @details A sampler object that allows for paired sampling by always including each observation from the
#' [shapr::vaeac_dataset()] twice. A [torch::sampler()] object can be used with [torch::dataloader()] when creating
#' batches from a torch dataset [torch::dataset()]. See \url{https://rdrr.io/cran/torch/src/R/utils-data-sampler.R} for
#' more information. This function does not use batch iterators, which might increase the speed.
#'
#' @param vaeac_dataset_object A [shapr::vaeac_dataset()] object containing the data.
#' @param shuffle Boolean. If `TRUE`, then the data is shuffled. If `FALSE`,
#' then the data is returned in chronological order.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
paired_sampler <- function(vaeac_dataset_object, shuffle = FALSE) {
  paired_sampler_tmp <- torch::sampler(
    classname = "paired_sampler", # field Name of the paired sampler object
    # description Initialize the paired_sampler object
    initialize = function(vaeac_dataset_object, shuffle = FALSE) {
      self$vaeac_dataset_object <- vaeac_dataset_object
      self$shuffle <- shuffle
    },
    # description Get the number of observations in the datasaet
    .length = function() length(self$vaeac_dataset_object) * 2, # Multiply by two do to get the actual number
    # description Function to iterate over the data
    .iter = function() {
      n <- length(self$vaeac_dataset_object) # Get the number of observations in the data
      indices <- if (self$shuffle) sample.int(n) else seq_len(n) # Check if randomly shuffle indices or increasing order
      return(coro::as_iterator(rep(indices, each = 2))) # Duplicate each index and return an iterator
    }
  )
  return(paired_sampler_tmp(vaeac_dataset_object = vaeac_dataset_object, shuffle = shuffle))
}


# Neural Network Utility Functions =====================================================================================
#' A [torch::nn_module()] Representing a Memory Layer
#'
#' @description The layer is used to make skip-connections inside a [torch::nn_sequential()] network
#' or between several [torch::nn_sequential()] networks without unnecessary code complication.
#'
#' @details If `output = FALSE`, this layer stores its input in the `shared_env` with the key `id` and then
#' passes the input to the next layer. I.e., when memory layer is used in the masked encoder. If `output = TRUE`, this
#' layer takes stored tensor from the storage. I.e., when memory layer is used in the decoder. If `add = TRUE`, it
#' returns sum of the stored vector and an `input`, otherwise it returns their concatenation. If the tensor with
#' specified `id` is not in storage when the layer with `output = TRUE` is called, it would cause an exception.
#'
#' @param id A unique id to use as a key in the storage list.
#' @param shared_env A shared environment for all instances of memory_layer where the inputs are stored.
#' @param output Boolean variable indicating if the memory layer is to store input in storage or extract from storage.
#' @param add Boolean variable indicating if the extracted value are to be added or concatenated to the input.
#' Only applicable when `output = TRUE`.
#' @param verbose Boolean variable indicating if we want to give printouts to the user.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
#'
memory_layer <- function(id, shared_env, output = FALSE, add = FALSE, verbose = FALSE) {
  memory_layer_tmp <- torch::nn_module(
    classname = "memory_layer", # field classname Name of the of torch::nn_module object.

    # description Create a new `memory_layer` object.
    # param id A unique id to use as a key in the storage list.
    # param shared_env A shared environment for all instances of memory_layer where the inputs are stored.
    # param output Boolean variable indicating if the memory layer is to store input in storage or extract from storage.
    # param add Boolean variable indicating if the extracted value are to be added or concatenated to the input.
    # Only applicable when `output = TRUE`.
    # param verbose Boolean variable indicating if we want to give printouts to the user.
    initialize = function(id, shared_env, output = FALSE, add = FALSE, verbose = FALSE) {
      self$id <- id
      self$shared_env <- shared_env
      self$output <- output
      self$add <- add
      self$verbose <- verbose
    },
    forward = function(input) {
      # Check if we are going to insert input into the storage or extract data from the storage.
      if (!self$output) {
        if (self$verbose) {
          msg <- paste0("Inserting data to memory layer `self$id = ", self$id, "`.")
          cli::cli_inform(c("i" = msg))
        }

        # Insert the input into the storage list which is in the shared environment of the memory_layer class.
        # Note that we do not check if self$id is unique.
        self$shared_env[[self$id]] <- input
        return(input) # Return/send the input to the next layer in the network.
      } else {
        # We are to extract data from the storage list.
        if (self$verbose) {
          msg <- paste0(
            "Extracting data to memory layer `self$id = ", self$id, "`. Using concatination = ", !self$add, "."
          )
          cli::cli_inform(c("i" = msg))
        }

        # Check that the memory layer has data is stored in it. If not, then thorw error.
        if (!self$id %in% names(self$shared_env)) {
          cli::cli_abort(paste0(
            "ValueError: Looking for memory layer `self$id = ", self$id, "`, but the only available ",
            "memory layers are: ", paste(names(self$shared_env), collapse = "`, `"), "`."
          ))
        }

        # Extract the stored data for the given memory layer and check if we are to concatenate or add the input
        stored <- self$shared_env[[self$id]]
        data <- if (self$add) input + stored else torch::torch_cat(c(input, stored), -1)

        # Return the data
        return(data)
      }
    }
  )

  return(memory_layer_tmp(id = id, shared_env = shared_env, output = output, add = add, verbose = verbose))
}

#' A [torch::nn_module()] Representing a skip connection
#'
#' @description Skip connection over the sequence of layers in the constructor. The module passes
#' input data sequentially through these layers and then adds original data to the result.
#'
#' @param ... network modules such as, e.g., [torch::nn_linear()], [torch::nn_relu()],
#' and [shapr::memory_layer()] objects. See [shapr::vaeac()] for more information.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
skip_connection <- function(...) {
  # Remove NULL objects from the arguments list
  args <- list(...)
  non_null_args <- args[!sapply(args, is.null)]

  skip_connection_tmp <- torch::nn_module(
    classname = "skip_connection", # field classname Name of the of torch::nn_module object
    # description Initialize a new skip_connection module
    initialize = function(...) self$inner_net <- torch::nn_sequential(...),
    # description What to do when a skip_connection module is called
    forward = function(input) {
      return(input + self$inner_net(input))
    }
  )

  return(do.call(skip_connection_tmp, non_null_args))
}


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
vaeac_extend_batch <- function(batch, dataloader, batch_size) {
  # Check if the batch contains too few observations and in that case add the missing number of obs from a new batch
  while (batch$shape[1] < batch_size) { # Use while in case a single extra batch is not enough to get to `batch_size`
    batch_extra <- dataloader$.iter()$.next()
    batch <- torch::torch_cat(c(batch, batch_extra[seq(min(nrow(batch_extra), batch_size - batch$shape[1])), ]), 1)
  }
  return(batch) # The returned batch is guaranteed to contain `batch_size` observations
}

#' Compute the Importance Sampling Estimator (Validation Error)
#'
#' @description Compute the Importance Sampling Estimator which the vaeac model
#' uses to evaluate its performance on the validation data.
#'
#' @details Compute mean IWAE log likelihood estimation of the validation set. IWAE is an abbreviation for Importance
#' Sampling Estimator \deqn{\log p_{\theta, \psi}(x|y) \approx \log {\frac{1}{S}\sum_{i=1}^S
#' p_\theta(x|z_i, y) p_\psi(z_i|y) \big/ q_\phi(z_i|x,y),}} where \eqn{z_i \sim q_\phi(z|x,y)}.
#' For more details, see \href{https://www.jmlr.org/papers/volume23/21-1413/21-1413.pdf}{Olsen et al. (2022)}.
#'
#' @param val_dataloader A torch dataloader which loads the validation data.
#' @param mask_generator A mask generator object that generates the masks.
#' @param batch_size Integer. The number of samples to include in each batch.
#' @param vaeac_model The vaeac model.
#' @param val_iwae_n_samples Number of samples to generate for computing the IWAE for each validation sample.
#'
#' @return The average iwae over all instances in the validation dataset.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_get_val_iwae <- function(val_dataloader,
                               mask_generator,
                               batch_size,
                               vaeac_model,
                               val_iwae_n_samples) {
  # Set variables to store the number of instances evaluated and avg_iwae
  cum_size <- 0
  avg_iwae <- 0

  # Iterate over all the batches in the validation set
  coro::loop(for (batch in val_dataloader) {
    init_size <- batch$shape[1] # Get the number of instances in the current batch

    # Extend the with observations from `val_dataloader` to ensure that batch contains `batch_size` observations
    batch <- vaeac_extend_batch(batch = batch, dataloader = val_dataloader, batch_size = batch_size)

    # Create the mask for the current batch. Mask consists of zeros (observed) and ones (missing or masked)
    mask <- mask_generator(batch = batch)

    # If the vaeac_model$parameters are located on a GPU, then we send batch and mask to the GPU too
    if (vaeac_model$parameters[[1]]$is_cuda) {
      batch <- batch$cuda()
      mask <- mask$cuda()
    }

    # Use torch::with_no_grad() since we are evaluation, and do not need the gradients to do backpropagation
    torch::with_no_grad({
      # Get the iwae for the first `init_size` observations in the batch. The other obs are just "padding".
      iwae <- vaeac_model$batch_iwae(batch, mask, val_iwae_n_samples)[1:init_size, drop = FALSE]

      # Update the average iwae over all batches (over all instances). This is called recursive/online updating of
      # the mean. Takes the old average * cum_size to get old sum of iwae and adds the sum of newly computed iwae.
      # Then divide the total iwae by the number of instances: cum_size + iwae.shape[0]
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
#' @param params Tensor of dimension `batch_size` x `2*n_featuers` containing the means and standard deviations
#' to be used in the normal distributions for of the `batch_size` observations.
#' @inheritParams gauss_cat_parameters
#'
#' @return A [torch::distr_normal()] distribution with the provided means and standard deviations.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_normal_parse_params <- function(params, min_sigma = 1e-4) {
  p <- params$shape[2] # Get the number of columns (2 times the number of features)
  mu <- params[, 1:(p %/% 2)] # Get the first halves which are the means
  sigma_params <- params[, (p %/% 2 + 1):p] # Get the second half which are transformed sigmas
  sigma <- torch::nnf_softplus(sigma_params) # apply softplus [ln(1 + exp(sigma_params))] to convert to positive values
  sigma <- sigma$clamp(min = min_sigma) # Make sure that the sigmas are larger than min_sigma
  distr <- torch::distr_normal(loc = mu, scale = sigma) # Create normal distributions based on the means and sds
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
#' @inheritParams gauss_cat_parameters
#' @param params Tensor of dimension `batch_size` x `K` containing the logits for each of the `K` classes and
#' `batch_size` observations.
#' @param max_prob For stability it might be desirable that the maximal probability is not too close to one.
#'
#' @return A [torch::distr_categorical] distributions with the provided probabilities for each class.
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_categorical_parse_params <- function(params, min_prob = 0, max_prob = 1) {
  params <- torch::nnf_softmax(params, dim = -1) # Use the softmax to convert from logits to probabilities
  params <- torch::torch_clamp(params, min = min_prob, max = max_prob) # Clamp probs between min and max allowed values
  params <- params / torch::torch_sum(params, dim = -1, keepdim = TRUE) # Ensure that probs sum to one
  distr <- torch::distr_categorical(probs = params) # Create categorical dist with prob for each level given by params
  return(distr)
}

#' Compute the KL Divergence Between Two Gaussian Distributions.
#'
#' @description Computes the KL divergence between univariate normal distributions using the analytical formula,
#' see \url{https://en.wikipedia.org/wiki/Kullback%E2%80%93Leibler_divergence#Multivariate_normal_distributions}.
#'
#' @param p A [torch::distr_normal()] object.
#' @param q A [torch::distr_normal()] object.
#'
#' @return The KL divergence between the two Gaussian distributions.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
vaeac_kl_normal_normal <- function(p, q) {
  var_ratio <- (p$scale / q$scale)$pow(2)
  t1 <- ((p$loc - q$loc) / q$scale)$pow(2)
  return(0.5 * (var_ratio + t1 - 1 - var_ratio$log()))
}


# Neural Network Modules ===============================================================================================
#' A [torch::nn_module()] Representing a `gauss_cat_sampler_most_likely`
#'
#' @description The `gauss_cat_sampler_most_likely` generates the most likely samples from the generative distribution
#' defined by the output of the vaeac. I.e., the layer will return the mean and most probable class for the Gaussian
#' (continuous features) and categorical (categorical features) distributions, respectively.
#'
#' @inheritParams vaeac
#' @inheritParams gauss_cat_parameters
#'
#' @return A `gauss_cat_sampler_most_likely` object.
#'
#' @keywords internal
#' @author Lars Henry Berge Olsen
gauss_cat_sampler_most_likely <- function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
  gauss_cat_sampler_most_lik_tmp <- torch::nn_module(
    classname = "gauss_cat_sampler_most_likely", # field classname Type of torch::nn_module

    # description Initialize a gauss_cat_sampler_most_likely which generates the most likely
    # sample from the generative distribution defined by the output of the neural network.
    initialize = function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
      self$one_hot_max_sizes <- one_hot_max_sizes
      self$min_sigma <- min_sigma
      self$min_prob <- min_prob
    },

    # param dist_params A matrix of form batch_size x (mu_1, sigma_1, ..., mu_p, sigma_p), when only considering
    # continuous features. For categorical features, we do NOT have mu and sigma for the decoder at the end of the
    # vaeac, but rather logits for the categorical distribution.
    # return A tensor containing the generated data.
    forward = function(distr_params) {
      cur_distr_col <- 1 # A counter to keep track of which column to extract from
      sample <- list() # List to store the samples sampled from the normal distr. with parameters from distr_params

      # Iterate over the features
      for (i in seq_along(self$one_hot_max_sizes)) {
        size <- self$one_hot_max_sizes[i] # Get the number of one hot dummy features to see if feature is cont or cat

        if (size <= 1) {
          # Continuous feature which are modeled using the Gaussian distribution
          params <- distr_params[, cur_distr_col:(cur_distr_col + 1), drop = FALSE] # Extract mean & sd, batch_size x 2
          cur_distr_col <- cur_distr_col + 2 # Update the pointer index by two (mean and sd)
          distr <- vaeac_normal_parse_params(params, self$min_sigma) # Create a Gaussian distribution based on params
          col_sample <- distr$mean # We sample the mean (most likely value)
        } else {
          # Categorical feature which are modeled using the categorical distribution
          # Extract the logits for each of the K-classes for the ith feature. The dimension is batch_size x size.
          params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
          cur_distr_col <- cur_distr_col + size # Update the pointer index by the number of categorical levels
          distr <- vaeac_categorical_parse_params(params, self$min_prob) # Create a categorical distr based on params
          col_sample <- torch::torch_argmax(distr$probs, -1)[, NULL]$to(dtype = torch::torch_float()) # Most lik class
        }
        sample <- append(sample, col_sample) # Add the vector of sampled values for the i-th feature to the sample list
      }
      return(torch::torch_cat(sample, -1)) # Create a 2D torch by column binding the vectors in the list
    }
  )

  return(
    gauss_cat_sampler_most_lik_tmp(one_hot_max_sizes = one_hot_max_sizes, min_sigma = min_sigma, min_prob = min_prob)
  )
}


#' A [torch::nn_module()] Representing a gauss_cat_sampler_random
#'
#' @description The `gauss_cat_sampler_random` generates random samples from the generative distribution defined by the
#' output of the vaeac. The random sample is generated by sampling from the inferred Gaussian and categorical
#' distributions for the continuous and categorical features, respectively.
#'
#' @inheritParams vaeac
#' @inheritParams gauss_cat_sampler_most_likely
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
gauss_cat_sampler_random <- function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
  gauss_cat_sampler_random_tmp <- torch::nn_module(
    classname = "gauss_cat_sampler_random", # field classname Type of torch::nn_module

    # description Initialize a gauss_cat_sampler_random which generates a sample from the
    # generative distribution defined by the output of the neural network by random sampling.
    # return A new `gauss_cat_sampler_random` object.
    initialize = function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
      self$one_hot_max_sizes <- one_hot_max_sizes
      self$min_sigma <- min_sigma
      self$min_prob <- min_prob
    },

    # param dist_params A matrix of form batch_size x (mu_1, sigma_1, ..., mu_p, sigma_p), when only considering
    # continuous features. For categorical features, we do NOT have mu and sigma for the decoder at the end of the
    # vaeac, but rather logits for the categorical distribution.
    # return A tensor containing the generated data.
    forward = function(distr_params) {
      cur_distr_col <- 1 # A counter to keep track of which column to extract from
      sample <- list() # List to store the samples sampled from the normal distr with parameters from distr_params

      # Iterate over the features
      for (i in seq_along(self$one_hot_max_sizes)) {
        size <- self$one_hot_max_sizes[i] # Get the number of one hot dummy features to see if feature is cont or cat

        if (size <= 1) {
          # Continuous feature which are modeled using the Gaussian distribution
          params <- distr_params[, cur_distr_col:(cur_distr_col + 1), drop = FALSE] # Extract mean & sd, batch_size x 2
          cur_distr_col <- cur_distr_col + 2 # Update the pointer index by two (mean and sd)
          distr <- vaeac_normal_parse_params(params, self$min_sigma) # Create a Gaussian distribution based on params
          col_sample <- distr$sample() # Sample from the inferred Gaussian distributions
        } else {
          # Categorical feature which are modeled using the categorical distribution
          # Extract the logits for each of the K-classes for the ith feature. The dimension is batch_size x size.
          params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
          cur_distr_col <- cur_distr_col + size # Update the pointer index by the number of categorical levels
          distr <- vaeac_categorical_parse_params(params, self$min_prob) # Create a categorical distr based on params
          col_sample <- distr$sample()$unsqueeze(-1)$to(dtype = torch::torch_float()) # Sample class using class prob
        }
        sample <- append(sample, col_sample) # Add the vector of sampled values for the i-th feature to the sample list
      }
      return(torch::torch_cat(sample, -1)) # Create a 2D torch by column binding the vectors in the list
    }
  )
  return(
    gauss_cat_sampler_random_tmp(one_hot_max_sizes = one_hot_max_sizes, min_sigma = min_sigma, min_prob = min_prob)
  )
}


#' A [torch::nn_module()] Representing a `gauss_cat_parameters`
#'
#' @description The `gauss_cat_parameters` module extracts the parameters from the inferred generative Gaussian and
#' categorical distributions for the continuous and categorical features, respectively.
#'
#' If `one_hot_max_sizes` is \eqn{[4, 1, 1, 2]}, then the inferred distribution parameters for one observation is the
#' vector \eqn{[p_{00}, p_{01}, p_{02}, p_{03}, \mu_1, \sigma_1, \mu_2, \sigma_2, p_{30}, p_{31}]}, where
#' \eqn{\operatorname{Softmax}([p_{00}, p_{01}, p_{02}, p_{03}])} and \eqn{\operatorname{Softmax}([p_{30}, p_{31}])}
#' are probabilities of the first and the fourth feature categories respectively in the model generative distribution,
#' and Gaussian(\eqn{\mu_1, \sigma_1^2}) and Gaussian(\eqn{\mu_2, \sigma_2^2}) are the model generative distributions
#' on the second and the third features.
#'
#' @inheritParams vaeac
#' @param min_sigma For stability it might be desirable that the minimal sigma is not too close to zero.
#' @param min_prob For stability it might be desirable that the minimal probability is not too close to zero.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
gauss_cat_parameters <- function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
  gauss_cat_parameters_tmp <- torch::nn_module(
    # field classname Type of torch::nn_module
    classname = "gauss_cat_parameters",

    # description Initialize a `gauss_cat_parameters` which extract the parameters from the generative distribution
    # defined by the output of the neural network.
    # return A new `gauss_cat_parameters` object.
    initialize = function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
      self$one_hot_max_sizes <- one_hot_max_sizes
      self$min_sigma <- min_sigma
      self$min_prob <- min_prob
    },

    # param dist_params A matrix of form batch_size x (mu_1, sigma_1, ..., mu_p, sigma_p), when only
    # considering continuous features. For categorical features, we do NOT have mu and sigma for the
    # decoder at the end of the vaeac, but rather logits for the categorical distribution.
    # return A tensor containing the final parameters of the generative distributions (after transformations).
    forward = function(distr_params) {
      cur_distr_col <- 1 # A counter to keep track of which column to extract from
      parameters <- list() # List to store the generative parameters from the normal and categorical distributions

      # Iterate over the features
      for (i in seq_along(self$one_hot_max_sizes)) {
        size <- self$one_hot_max_sizes[i] # Get the number of one hot dummy features to see if feature is cont or cat

        if (size <= 1) {
          # Continuous feature which are modeled using the Gaussian distribution.
          params <- distr_params[, cur_distr_col:(cur_distr_col + 1), drop = FALSE] # Extract mean & sd, batch_size x 2
          cur_distr_col <- cur_distr_col + 2 # Update the pointer index by two (mean and sd)
          distr <- vaeac_normal_parse_params(params, self$min_sigma) # Create a Gaussian distribution based on params
          current_parameters <- torch::torch_cat(c(distr$mean, distr$scale), -1) # Combine the current parameters
        } else {
          # Categorical feature which are modeled using the categorical distribution
          # Extract the logits for each of the K-classes for the ith feature. The dimension is batch_size x size.
          params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1)]
          cur_distr_col <- cur_distr_col + size # Update the pointer index by the number of categorical levels
          distr <- vaeac_categorical_parse_params(params, self$min_prob) # Create a categorical distr based on params
          current_parameters <- distr$probs # Extract the current probabilities for each classs
        }
        parameters <- append(parameters, current_parameters) # Add the i-th feature's parameters to the parameters list
      }
      return(torch::torch_cat(parameters, -1)) # Create a 2D torch_tensor by column binding the tensors in the list
    }
  )
  return(gauss_cat_parameters_tmp(one_hot_max_sizes = one_hot_max_sizes, min_sigma = min_sigma, min_prob = min_prob))
}


#' A [torch::nn_module()] Representing a `gauss_cat_loss`
#'
#' @description The `gauss_cat_loss module` layer computes the log probability of the `groundtruth` for each object
#' given the mask and the distribution parameters. That is, the log-likelihoods of the true/full training observations
#' based on the generative distributions parameters `distr_params` inferred by the masked versions of the observations.
#'
#' @details Note that the module works with mixed data represented as 2-dimensional inputs and it
#' works correctly with missing values in `groundtruth` as long as they are represented by NaNs.
#'
#' @inheritParams vaeac
#' @inheritParams gauss_cat_parameters
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
gauss_cat_loss <- function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
  gauss_cat_loss_tmp <- torch::nn_module(
    classname = "gauss_cat_loss", # field classname Type of torch::nn_module

    # description Initialize a `gauss_cat_loss`.
    # return A new `gauss_cat_loss` object.
    initialize = function(one_hot_max_sizes, min_sigma = 1e-4, min_prob = 1e-4) {
      self$one_hot_max_sizes <- one_hot_max_sizes
      self$min_sigma <- min_sigma
      self$min_prob <- min_prob
    },
    forward = function(groundtruth, distr_params, mask) {
      cur_distr_col <- 1 # A counter to keep track of which column to extract from
      log_prob <- list() # List to store the log probabilities

      # Iterate over the features
      for (i in seq_along(self$one_hot_max_sizes)) {
        size <- self$one_hot_max_sizes[i] # Get the number of one hot dummy features to see if feature is cont or cat
        groundtruth_col <- groundtruth[, i, drop = FALSE] # Get at the ith column of the truth
        mask_col <- mask[, i, drop = FALSE] # Get the ith column of the mask
        gt_col_nansafe <- groundtruth_col$clone()$detach() # Copy the ground truth column, can now alter this object
        nan_mask <- torch::torch_isnan(groundtruth_col) # Check if truth contains any missing values
        gt_col_nansafe[nan_mask] <- 0 # Set any missing values to 0

        # Mask_col masks both the nan/missing values and the artificially masked values. We want to compute the log prob
        # only over the artificially missing features, so we omit the true missing values. We remove the masking of the
        # missing values. So those ones in mask_col which are there due to missing values are now turned in to zeros.
        mask_col <- mask_col * (torch::torch_logical_not(nan_mask))$to(dtype = torch::torch_float())

        if (size <= 1) {
          # Continuous feature which are modeled using the Gaussian distribution
          params <- distr_params[, cur_distr_col:(cur_distr_col + 1), drop = FALSE] # Extract mean & sd, batch_size x 2
          cur_distr_col <- cur_distr_col + 2 # Update the pointer index by two (mean and sd)
          distr <- vaeac_normal_parse_params(params, self$min_sigma) # Create a Gaussian distribution based on params

          # Get the log-likelihood, but only of the masked values i.e., the ones hat are masked by the masking scheme
          # MCARGenerator. This one is batch_size x 1 and is the log-lik of observing the ground truth given the current
          # parameters, for only the artificially masked features.
          col_log_prob <- distr$log_prob(gt_col_nansafe) * mask_col
        } else {
          # Categorical feature which are modeled using the categorical distribution
          # Extract the probabilities for each of the K-classes for the ith feature. The dimension is batch_size x size.
          params <- distr_params[, cur_distr_col:(cur_distr_col + size - 1), drop = FALSE]
          cur_distr_col <- cur_distr_col + size # Update the pointer index by the number of categorical levels
          distr <- vaeac_categorical_parse_params(params, self$min_prob) # Create a categorical distr based on params
          col_log_prob <- distr$log_prob(gt_col_nansafe$squeeze())[, NULL] * mask_col # Get the log-likelihood
        }

        # Append the column of log probabilities for the i-th feature for those instances that are masked into log_prob.
        # log_prob is now a list of length n_features, where each element is a tensor batch_size x 1 containing the
        # log-lik of the parameters of the masked values.
        log_prob <- append(log_prob, col_log_prob)
      }

      # Concatenate the list into a tensor of dim batch x features. Then sum along the the rows.
      # That is, for each observation in the batch to get a tensor of length batch size.
      return(torch::torch_cat(log_prob, 2)$sum(-1))
    }
  )
  return(gauss_cat_loss_tmp(one_hot_max_sizes = one_hot_max_sizes, min_sigma = min_sigma, min_prob = min_prob))
}


#' A [torch::nn_module()] Representing a `categorical_to_one_hot_layer`
#'
#' @description
#' The `categorical_to_one_hot_layer` module/layer expands categorical features into one-hot vectors,
#' because multi-layer perceptrons are known to work better with this data representation.
#' It also replaces NaNs with zeros in order so that further layers may work correctly.
#'
#' @inheritParams vaeac
#' @param add_nans_map_for_columns Optional list which contains indices of columns which is_nan masks are to be appended
#' to the result tensor. This option is necessary for the full encoder to distinguish whether value is to be
#' reconstructed or not.
#'
#' @details Note that the module works with mixed data represented as 2-dimensional inputs and it
#' works correctly with missing values in `groundtruth` as long as they are represented by NaNs.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
categorical_to_one_hot_layer <- function(one_hot_max_sizes, add_nans_map_for_columns = NULL) {
  cat_to_one_hot_layer_tmp <- torch::nn_module(
    classname = "categorical_to_one_hot_layer", # field classname Type of torch::nn_module

    # description Initialize a `categorical_to_one_hot_layer`.
    # return A new `categorical_to_one_hot_layer` object.
    initialize = function(one_hot_max_sizes, add_nans_map_for_columns = NULL) {
      # Here one_hot_max_sizes includes zeros at the end of the list: one_hot_max_sizes + [0] * len(one_hot_max_sizes)
      # Thus, if features have this many categories [1, 2, 3, 1], then one_hot_max_sizes = [1, 2, 3, 1, 0, 0, 0, 0]
      self$one_hot_max_sizes <- one_hot_max_sizes

      # Always an empty column for the Masked Encoder network while it's a list [0, 1, ..., length(one_hot_max_sizes)-1)
      # for the Full Encoder network. So for the Full Encoder network we apply the nan masks to each column/feature
      self$add_nans_map_for_columns <- add_nans_map_for_columns
    },
    forward = function(input) {
      # Input is torch::torch_cat(c(batch, mask), -1), so a torch of dimension batch_size x 2*sum(one_hot_max_sizes)
      # for continuous data where one_hot_max_sizes only consists of ones. Recall that one_hot_max_sizes are padded with
      # zeros at the end in this function.
      n <- input$shape[1] # Get the number of instances in the input batch.
      out_cols <- NULL # variable to store the out columns, i.e., the input columns / one hot encoding + is nan.mask.

      # Iterate over the features. Note that i goes from 0 to 2*n_features-1.
      for (i in seq_along(self$one_hot_max_sizes)) {
        # Get the number of categories for each feature. For i in [n_features, 2*n_features-1], size <= 1, even for
        # categorical features.
        size <- self$one_hot_max_sizes[i]

        # Distinguish between continuous and categorical features
        if (size <= 1) {
          # Continuous feature. Copy it and replace NaNs with either zeros or the last half of self.one_hot_max_sizes
          # Take the ith column of the input (NOTE THAT THIS IS NOT A DEEP COPY, so changing out_col changes input)
          out_col <- input[, i:i]
          nan_mask <- torch::torch_isnan(out_col) # check if any of the values are nan, i.e., missing
          out_col[nan_mask] <- 0 # set all the missing values to 0. (This changes the input too)
        } else {
          # Categorical feature. Get the categories for each instance for the ith feature and start to count at zero.
          # So if we have 2 cat, then this vector will contains zeros and ones.
          cat_idx <- input[, i:i]
          nan_mask <- torch::torch_isnan(cat_idx) # Check if any of the categories are nan / missing
          cat_idx[nan_mask] <- 0 # Set the nan values to 0

          # Create a matrix, where the jth row is the one-hot encoding of the ith feature of the jth instance.
          out_col <- matrix(0, nrow = n, ncol = size)
          out_col[cbind(seq(n), as.matrix(cat_idx$cpu()))] <- 1
          out_col <- torch::torch_tensor(out_col, device = input$device)
        }

        # Append this feature column to the result. out_col is n x size = batch_size x n_categories_for_this_feature
        out_cols <- torch::torch_cat(c(out_cols, out_col), dim = -1)

        # If necessary, append isnan mask of this feature to the result which we always do for the proposal network.
        # This only happens for the first half of the i's, so for i = 1, ..., n_features.
        if (i %in% self$add_nans_map_for_columns) {
          # add the columns of nan_mask
          out_cols <- torch::torch_cat(c(out_cols, nan_mask$to(dtype = torch::torch_float())), dim = -1)
        }
      }

      # ONLY FOR CONTINUOUS FEATURES: out_cols now is a list of n_features tensors of shape n x size = n x 1 for
      # continuous variables. We concatenate them to a matrix of dim n x 2*n_features (in cont case) for prior net, but
      # for proposal net, it is n x 3*n_features, and they take the form
      # [batch1, is.nan1, batch2, is.nan2, ...,  batch12, is.nan12, mask1, mask2, ..., mask12]
      return(out_cols)
    }
  )

  return(
    cat_to_one_hot_layer_tmp(one_hot_max_sizes = one_hot_max_sizes, add_nans_map_for_columns = add_nans_map_for_columns)
  )
}


# Mask Generators ======================================================================================================
#' Missing Completely at Random (MCAR) Mask Generator
#'
#' @description A mask generator which masks the entries in the input completely at random.
#'
#' @details The mask generator mask each element in the `batch` (N x p) using a component-wise independent Bernoulli
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
#' @author Lars Henry Berge Olsen
#' @keywords internal
mcar_mask_generator <- function(masking_ratio = 0.5, paired_sampling = FALSE) {
  mcar_mask_gen_tmp <- torch::nn_module(
    name = "mcar_mask_generator", # field name Type of mask generator

    # description Initialize a missing completely at random mask generator.
    # param masking_ratio The probability for an entry in the generated mask to be 1 (masked).
    # param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
    # If TRUE, then batch must be sampled using `paired_sampler` which creates batches where
    # the first half and second half of the rows are duplicates of each other. That is,
    # batch = [row1, row1, row2, row2, row3, row3, ...].
    # return A new `mcar_mask_generator` object.
    initialize = function(masking_ratio = 0.5, paired_sampling = FALSE) {
      self$masking_ratio <- masking_ratio
      self$paired_sampling <- paired_sampling
    },

    # description Generates a MCAR mask by calling self$mcar_mask_generator_function function.
    # param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
    # entries are missing. If any are missing, then the returned mask will ensure that
    # these missing entries are masked.
    forward = function(batch) {
      self$mcar_mask_generator_function(batch, prob = self$masking_ratio, paired_sampling = self$paired_sampling)
    },

    # description Missing Completely At Random Mask Generator: A mask generator where the masking
    # is determined by component-wise independent Bernoulli distribution.
    #
    # details Function that takes in a batch of observations and the probability
    # of masking each element based on a component-wise independent Bernoulli
    # distribution. Default value is 0.5, so all masks are equally likely to be trained.
    # Function returns the mask of same shape as batch.
    # Note that the batch can contain missing values, indicated by the "NaN" token.
    # The mask will always mask missing values.
    #
    # param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
    # entries are missing. If any are missing, then the returned mask will ensure that
    # these missing entries are masked.
    # param prob Numeric between 0 and 1. The probability that an entry will be masked.
    # param seed Integer. Used to set the seed for the sampling process such that we
    # can reproduce the same masks.
    # param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
    # If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
    # the first half and second half of the rows are duplicates of each other. That is,
    # batch = [row1, row1, row2, row2, row3, row3, ...].
    #
    # examples
    # mcar_mask_generator_function(torch::torch_rand(c(5, 3)))
    #
    # return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
    # observed feature value will be masked. '0' means that the entry is NOT masked,
    # i.e., the feature value will be observed/given/available.
    mcar_mask_generator_function = function(batch, prob = 0.5, seed = NULL, paired_sampling = FALSE) {
      if (!is.null(seed)) set.seed(seed) # If the user specify a seed for reproducibility
      size <- prod(batch$shape) # Get the number of entries in the batch.
      nan_mask <- batch$isnan()$to(torch::torch_float()) # Check for missing values in the batch

      # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
      if (paired_sampling) size <- size / 2

      # # Torch version, but marginally slower than r version when batch_size <= 128 and n_features <= 50
      # mask = torch::torch_bernoulli(torch::torch_full_like(batch, prob))
      # Create the Bernoulli mask where an element is masked (1) with probability 'prob'.
      mask <- torch::torch_tensor(
        matrix(sample(c(0, 1), size = size, replace = TRUE, prob = c(prob, 1 - prob)), ncol = ncol(batch)),
        dtype = torch::torch_float()
      )

      # If paired sampling: concatenate the inverse mask and reorder to ensure correct order [m1, !m1, m2, !m2, ...].
      if (paired_sampling) {
        mask <- torch::torch_cat(c(mask, !mask), 1L)[c(matrix(seq_len(nrow(batch)), nrow = 2, byrow = TRUE)), ]
      }

      # Mask all missing or artificially masked entries by the Bernoulli mask. 1 means that the entry is masked.
      return(mask + nan_mask >= 1)
    }
  )
  return(mcar_mask_gen_tmp(masking_ratio = masking_ratio, paired_sampling = paired_sampling))
}


#' A [torch::nn_module()] Representing a specified_prob_mask_generator
#'
#' @description A mask generator which masks the entries based on specified probabilities.
#'
#' @details A class that takes in the probabilities of having d masked observations.  I.e., for M dimensional data,
#' masking_probs is of length M+1, where the d'th entry is the probability of having d-1 masked values.
#'
#' A mask generator that first samples the number of entries 'd' to be masked in the 'M'-dimensional observation 'x' in
#' the batch based on the given M+1 probabilities. The 'd' masked are uniformly sampled from the 'M' possible feature
#' indices. The d'th entry of the probability of having d-1 masked values.
#'
#' Note that mcar_mask_generator with p = 0.5 is the same as using [shapr::specified_prob_mask_generator()] with
#' `masking_ratio` = choose(M, 0:M), where M is the number of features. This function was initially created to check if
#' increasing the probability of having a masks with many masked features improved vaeac's performance by focusing more
#' on these situations during training.
#'
#' @param masking_probs An M+1 numerics containing the probabilities masking 'd' of the (0,...M) entries
#' for each observation.
#' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
#' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
#' the first half and second half of the rows are duplicates of each other. That is,
#' `batch = [row1, row1, row2, row2, row3, row3, ...]`.
#'
#' @keywords internal
specified_prob_mask_generator <- function(masking_probs, paired_sampling = FALSE) {
  specified_prob_mask_gen_tmp <- torch::nn_module(
    name = "specified_prob_mask_generator", # field name Type of mask generator

    # description Initialize a specified_probability mask generator.
    initialize = function(masking_probs, paired_sampling = FALSE) {
      self$masking_probs <- masking_probs / sum(masking_probs)
      self$paired_sampling <- paired_sampling
    },

    # description Generates a specified probability mask by calling the self$specified_prob_mask_generator_function.
    # param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the entries are
    # missing. If any are missing, then the returned mask will ensure that these missing entries are masked.
    forward = function(batch) {
      self$specified_prob_mask_generator_function(
        batch = batch,
        masking_prob = self$masking_probs,
        paired_sampling = self$paired_sampling
      )
    },

    # description Specified Probability Mask Generator: A mask generator that first samples the number of entries 'd' to
    # be masked in the 'M'-dimensional observation 'x' in the batch based on the given M+1 probabilities. The 'd' masks
    # are uniformly sampled from the 'M' possible feature indices. The d'th entry of the probability of having d-1
    # masked values.
    #
    # details Note that mcar_mask_generator with p = 0.5 is the same as using specified_prob_mask_generator
    # with masking_ratio = choose(M, 0:M), where M is the number of features. This function was initially
    # created to check if increasing the probability of having a masks with many masked features improved
    # vaeac's performance by focusing more on these situations during training.
    #
    # param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the entries are missing. If any
    # are missing, then the returned mask will ensure that these missing entries are masked.
    # param masking_probs An M+1 numerics containing the probabilities masking 'd' (0,...M) entries for each instance.
    # param seed Integer. Used to set the seed for the sampling process such that we can reproduce the same masks.
    # param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
    # If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
    # the first half and second half of the rows are duplicates of each other. That is,
    # `batch = [row1, row1, row2, row2, row3, row3, ...]`.
    #
    # examples specified_prob_mask_generator_function(torch::torch_rand(c(5, 4)), masking_probs = c(2,7,5,3,3))
    #
    # return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
    # observed feature value will be masked. '0' means that the entry is NOT masked,
    # i.e., the feature value will be observed/given/available.
    specified_prob_mask_generator_function = function(batch, masking_probs, seed = NULL, paired_sampling = FALSE) {
      if (!is.null(seed)) set.seed(seed) # If the user specify a seed for reproducibility
      n_features <- ncol(batch) # Get the number of features in the batch
      size <- nrow(batch) # Get the number of observations in the batch
      nan_mask <- batch$isnan()$to(torch::torch_float()) # Check for missing values in the batch

      # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
      if (paired_sampling) size <- size / 2

      # Sample the number of masked features in each row.
      n_masked_each_row <- sample(x = seq(0, n_features), size = size, replace = TRUE, prob = masking_probs)

      # Crate the mask matrix
      mask <- torch::torch_zeros_like(batch)
      for (i in seq(size)) {
        if (n_masked_each_row[i] != 0) mask[i, sample(n_features, size = n_masked_each_row[i], replace = FALSE)] <- 1
      }

      # If paired sampling: concatenate the inverse mask and reorder to ensure correct order [m1, !m1, m2, !m2, ...].
      if (paired_sampling) {
        mask <- torch::torch_cat(c(mask, !mask), 1L)[c(matrix(seq_len(nrow(batch)), nrow = 2, byrow = TRUE)), ]
      }

      # Mask all missing or artificially masked entries by the Bernoulli mask. 1 means that the entry is masked.
      return(mask + nan_mask >= 1)
    }
  )

  return(specified_prob_mask_gen_tmp(masking_probs = masking_probs, paired_sampling = paired_sampling))
}


#' A [torch::nn_module()] Representing a specified_masks_mask_generator
#'
#' @description
#' A mask generator which masks the entries based on sampling provided 1D masks with corresponding probabilities.
#' Used for Shapley value estimation when only a subset of coalitions are used to compute the Shapley values.
#'
#' @param masks Matrix/Tensor of possible/allowed 'masks' which we sample from.
#' @param masks_probs Array of 'probabilities' for each of the masks specified in 'masks'.
#' Note that they do not need to be between 0 and 1 (e.g. sampling frequency).
#' They are scaled, hence, they only need to be positive.
#' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \eqn{\bar{S}}.
#' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
#' the first half and second half of the rows are duplicates of each other. That is,
#' `batch = [row1, row1, row2, row2, row3, row3, ...]`.
#'
#' @author Lars Henry Berge Olsen
#' @keywords internal
specified_masks_mask_generator <- function(masks, masks_probs, paired_sampling = FALSE) {
  specified_masks_mask_gen_tmp <- torch::nn_module(
    name = "specified_masks_mask_generator", # field name Type of mask generator

    # description Initialize a specified masks mask generator.
    initialize = function(masks, masks_probs, paired_sampling = FALSE) {
      self$masks <- masks
      self$masks_probs <- masks_probs / sum(masks_probs)
      self$paired_sampling <- paired_sampling
    },

    # description Generates a mask by calling self$specified_masks_mask_generator_function function.
    # param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
    # entries are missing. If any are missing, then the returned mask will ensure that
    # these missing entries are masked.
    forward = function(batch) {
      self$specified_masks_mask_generator_function(
        batch = batch,
        masks = self$masks,
        masks_probs = self$masks_probs,
        paired_sampling = self$paired_sampling
      )
    },

    # description Sampling Masks from the Provided Masks with the Given Probabilities
    #
    # details Function that takes in a 'batch' of observations and matrix of possible/allowed
    # 'masks' which we are going to sample from based on the provided probability in 'masks_probs'.
    # Function returns a mask of same shape as batch. Note that the batch can contain missing values,
    # indicated by the "NaN" token. The mask will always mask missing values.
    #
    # param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
    # entries are missing. If any are missing, then the returned mask will ensure that
    # these missing entries are masked.
    # param masks Matrix/Tensor of possible/allowed 'masks' which we sample from.
    # param masks_probs Array of 'probabilities' for each of the masks specified in 'masks'.
    # Note that they do not need to be between 0 and 1. They are scaled, hence, they only need to be positive.
    # param seed Integer. Used to set the seed for the sampling process such that we
    # can reproduce the same masks.
    # param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
    # If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
    # the first half and second half of the rows are duplicates of each other. That is,
    # batch = [row1, row1, row2, row2, row3, row3, ...].
    #
    # return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
    # observed feature value will be masked. '0' means that the entry is NOT masked,
    # i.e., the feature value will be observed/given/available.
    specified_masks_mask_generator_function =
      function(batch, masks, masks_probs, seed = NULL, paired_sampling = FALSE) {
        if (!is.null(seed)) set.seed(seed) # Set seed if the user specifies a seed for reproducibility.
        nan_mask <- batch$isnan()$to(torch::torch_float()) # Check for missing values in the batch
        n_masks <- nrow(masks) # Get the number of masks to choose from
        size <- nrow(batch) # Get the number of observations in the batch

        # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
        if (paired_sampling) size <- size / 2

        # Sample 'n_observation' masks from the possible masks by first sampling the row indices
        # based on the given mask probabilities and then use these indices to extract the masks.
        mask_rows_indices <- sample.int(n = n_masks, size = size, replace = TRUE, prob = masks_probs)
        mask <- torch::torch_tensor(masks[mask_rows_indices, ], dtype = torch::torch_float())

        # If paired sampling: concatenate the inverse mask and reorder to ensure correct order [m1, !m1, m2, !m2, ...].
        if (paired_sampling) {
          mask <- torch::torch_cat(c(mask, !mask), 1L)[c(matrix(seq_len(nrow(batch)), nrow = 2, byrow = TRUE)), ]
        }

        # Mask all missing or artificially masked entries by the Bernoulli mask. 1 means that the entry is masked.
        return(mask + nan_mask >= 1)
      }
  )

  # Initate the specified_masks_mask_generator and return it
  return(specified_masks_mask_gen_tmp(masks = masks, masks_probs = masks_probs, paired_sampling = paired_sampling))
}
