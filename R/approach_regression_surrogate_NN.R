# File that contains the functions needed to create the regression NN
# proposed by Frye et al. I.e., masking out samples with -1.



# Libraries ----------------------------------------------------------------------------------------------------------------------------------



# Include the torch library#
library(torch)

# Include the tidyverse library
library(tidyverse)
library(dplyr)
library(ggplot2)

# Make progress bar
library(progress)


# Defining the Data Set Object ---------------------------------------------------------------------------------------------------------------

# Must create a data set object that represent a map from keys to data samples.
# It is used by the dataloader() to load data which should be used to extract
# the batches for all epochs in the training phase of the neural network.
# Note that a dataset object is an R6 instance (https://r6.r-lib.org/articles/Introduction.html)
# which is classical object-oriented programming, with self reference.
# That is, sample_data_for_shapley_regression is now a class of type 'dataset'.
# NOTE: THIS SETUP ONLY SUPPORTS REGRESSION, I.E., NUMERIC/CONTINUOUS Y.
NN_internal_sample_data_for_shapley_regression <- dataset(
  # A dataset must specify a 'name' for the object
  name = "shapley_regression_nn",

  # How to 'initialize' the object, i.e., put the data into the object.
  # Good idea to transform the results to torch_tensors here as the
  # neural network later on relies on the values being of the
  # tensor class.
  initialize = function(X, y) {
    self$X <- torch_tensor(as.matrix(X))
    self$y <- torch_tensor(as.numeric(y))
  },

  # TODO: change to getbatch
  # How to fetch a data sample for a given key/index.
  # Must not be a list, can be a combined matrix, but I decided to use list.
  .getitem = function(index) {
    X <- self$X[index, ]
    y <- self$y[index] # $to(torch_long())
    list(X = X, y = y)
  },

  # Return the size of the dataset
  .length = function() length(self$y)
)


# SkipConnection --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#' A torch::nn_module representing a Skip Connection
#'
#' @description Skip-connection over the sequence of layers in the constructor. The module passes
#' input data sequentially through these layers and then adds original data to the result.
#' @param ... network modules such as nn_linear, activation_function(), and memory layers. See \code{get_imputation_networks}.
SkipConnection <- nn_module(
  classname = "SkipConnection",
  initialize = function(...) self$inner_net <- nn_sequential(...),
  forward = function(input) {
    return(input + self$inner_net(input))
  }
)

# Mask Generators ----------------------------------------------------------------------------------------------------------------------------

#' Internal Mask Generator Frye Setup
#'
#' @details Function that takes in a batch of observations and generates a mask with
#' the same dimensions based on the masking approach described in Frye et al.
#' That is, the coalitions are sampled where the probability assigned to each
#' coalition S is the combinatorial factor |S|!(n − |S| − 1)!/n!.
#' This is a bit strange as we will then never sample S = M, and the sampling
#' produce an unsymmetric sampling procedure. Meaning that S = empty_set is
#' equally likely to happen as S only missing one value. Does not make
#' sense in the Shapr library where we set the predicted response in the
#' S = empty_set case to the mean of the training data. Note that the network
#' should aim for the same mean, as it is the one that minimizes the MSE.
#'
#' Note that the batch can contain missing values, indicated by the "NaN" token.
#' The mask will always mask missing values.
#'
#' @param batch Matrix. Only used to get the dimensions and to check if any of the
#' entries are missing. If any are missing, then the returned mask will ensure that
#' these missing entries are masked.
#' @param seed Integer. Used to set the seed for the sampling process such that we
#' can reproduce the same masks.
#'
#' @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
#' observed feature value will be masked. '0' means that the entry is NOT masked,
#' i.e., the feature value will be observed/given/available.
#' @export
#'
#' @examples
NN_internal_mask_generator <- function(batch, seed = NULL, paired_sampling = FALSE) {
  # If the user specify a seed for reproducibility
  if (!is.null(seed)) set.seed(seed)

  # Check for missing values in the batch
  nan_mask <- batch$isnan()$to(torch_float())

  # Extract the number of observations and features from the mask
  num_observations <- batch$size()[1] # Same as 'batch_size' outside this function
  num_features <- batch$size()[2] # Same as 'p' outside this function

  # Half it we are doing paired sampling
  if (paired_sampling) num_observations <- num_observations / 2

  # Possible sizes of the coalitions S
  S_size <- base::seq(0, num_features - 1)

  # Compute the sampling probabilities. Do it on the exp-log scale for numeric stability if num_features is high.
  sampling_prob_comb_S <- exp(lfactorial(S_size) + lfactorial(num_features - S_size - 1) - lfactorial(num_features))

  # Get the number of features to mask for each of the observation.
  # I.e., how many 1 entries should there be in each row in the mask.
  sampled_coalition_sizes <- sample(x = S_size, size = num_observations, replace = TRUE, prob = sampling_prob_comb_S)

  # Initialize the mask
  mask <- matrix(0, nrow = num_observations, ncol = num_features)

  # Iterate over the rows in the mask and set 'sampled_coalition_sizes[mask_row]' of
  # the entries in the 'mask_row' row of the mask to be 1. We uniformly sample
  # which features to mask. Meaning that all coalitions S and S' of equal size
  # are equally likely to be generated. Which is according to Frye's approach.
  for (mask_row in base::seq(num_observations)) {
    mask[mask_row, sample(base::seq(num_features), sampled_coalition_sizes[mask_row])] <- 1
  }
  mask <- torch_tensor(mask)

  # If paired sampling, then concatenate the inverse mask and reorder to ensure correct order [m1, !m1, m2, !m2, ...].
  if (paired_sampling) mask <- torch_cat(c(mask, !mask), 1L)[c(matrix(seq(nrow(batch)), nrow = 2, byrow = T)), ]

  # Final mask masks all entries that is either missing or artificially masked
  # by the Bernoulli mask. A value of 1 means that the entry is going to be masked.
  return(mask + nan_mask >= 1)
}


#' #' Sampling Masks
#' #'
#' #' @description Functions that samples masks from the provided masks.
#' #'
#' #' @details Function that takes in a 'batch' of observations and matrix of possible/allowed
#' #' 'masks' which we are going to sample from based on the provided probability in 'masks_prob'.
#' #' Function returns the mask of same shape as batch.
#' #' Note that the batch can contain missing values, indicated by the "NaN" token.
#' #' The mask will always mask missing values.
#' #' TODO: PAIRED SAMPLING SUCH THAT WE ALWAYS SAMPLE BOTH MASK 'S' AND MASK '1-S'.
#' #'
#' #' @param batch Matrix. Only used to get the dimensions and to check if any of the
#' #' entries are missing. If any are missing, then the returned mask will ensure that
#' #' these missing entries are masked.
#' #' @param masks Matrix of possible/allowed 'masks' which we sample from.
#' #' @param masks_prob Array of 'probabilities' for each of the masks specified in 'masks'.
#' #' Note that they do not need to be between 0 and 1. They are scaled, hence, they only need to be positive.
#' #' @param seed Integer. Used to set the seed for the sampling process such that we
#' #' can reproduce the same masks.
#' #'
#' #' @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
#' #' observed feature value will be masked. '0' means that the entry is NOT masked,
#' #' i.e., the feature value will be observed/given/available.
#' #' @export
#' #'
#' #' @examples
#' specified_mask_generator = function(batch, masks, masks_prob, seed = NULL) {
#'   # Some check for validity
#'   if (batch$shape[2] == masks$shape[2]) {
#'     stop("The number of features in the 'batch' and 'masks' are incompatible.")
#'   }
#'   if (masks$shape[1] == length(masks_prob)) {
#'     stop("The number of masks in 'masks' and the number of probabilities in 'masks_prob' are incompatible.")
#'   }
#'
#'   # If the user specify a seed for reproducibility
#'   if (!is.null(seed)) {
#'     set.seed(seed)
#'   }
#'
#'   # Check for missing values in the batch
#'   nan_mask = batch$isnan()$to(torch_float())
#'
#'   # Get the number of masks to choose from
#'   n_masks = masks$shape[1]
#'
#'   # Get the number of observations in the batch
#'   n_observations = batch$shape[1]
#'
#'   # Sample 'n_observation' masks from the possible masks by first sampling the
#'   # row indices based on the given mask probabilities and then
#'   # use these indices to extract the masks.
#'   mask_rows_indices = sample.int(n = n_masks,
#'                                  size = n_observations,
#'                                  replace = TRUE,
#'                                  prob = masks_prob)
#'   mask = torch_tensor(masks[mask_rows_indices, ], dtype = torch_float())
#'
#'   # Final mask masks all entries that is either missing or artificially masked
#'   # by the Bernoulli mask. A value of 1 means that the entry is going to be masked.
#'   return(mask + nan_mask >= 1)
#' }


#' Sampling Masks from the Provided Masks with the Given Probabilities
#'
#' @description Functions that samples masks from the provided masks.
#'
#' @details Function that takes in a 'batch' of observations and matrix of possible/allowed
#' 'masks' which we are going to sample from based on the provided probability in 'masks_probs'.
#' Function returns a mask of same shape as batch. Note that the batch can contain missing values,
#' indicated by the "NaN" token. The mask will always mask missing values.
#'
#' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
#' entries are missing. If any are missing, then the returned mask will ensure that
#' these missing entries are masked.
#' @param masks Matrix/Tensor of possible/allowed 'masks' which we sample from.
#' @param masks_probs Array of 'probabilities' for each of the masks specified in 'masks'.
#' Note that they do not need to be between 0 and 1. They are scaled, hence, they only need to be positive.
#' @param seed Integer. Used to set the seed for the sampling process such that we
#' can reproduce the same masks.
#' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
#' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
#' the first half and second half of the rows are duplicates of each other. That is,
#' batch = [row1, row1, row2, row2, row3, row3, ...].
#'
#' @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
#' observed feature value will be masked. '0' means that the entry is NOT masked,
#' i.e., the feature value will be observed/given/available.
#' @export
specified_mask_generator <- function(batch,
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
  #   stop(sprintf("The number of masks in 'masks' and the number of probabilities in 'masks_probs' are incompatible: %d != %d.",
  #                nrow(masks), length(masks_probs)))
  # }

  # Set seed if the user specifies a seed for reproducibility.
  if (!is.null(seed)) set.seed(seed)

  # Check for missing values in the batch
  nan_mask <- batch$isnan()$to(torch_float())

  # Get the number of masks to choose from
  n_masks <- nrow(masks)

  # Get the number of observations in the batch
  size <- nrow(batch)

  # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
  if (paired_sampling) size <- size / 2

  # Sample 'n_observation' masks from the possible masks by first sampling the row indices
  # based on the given mask probabilities and then use these indices to extract the masks.
  mask_rows_indices <- sample.int(n = n_masks, size = size, replace = TRUE, prob = masks_probs)
  mask <- torch_tensor(masks[mask_rows_indices, ], dtype = torch_float())

  # If paired sampling, then concatenate the inverse mask and reorder to ensure correct order [m1, !m1, m2, !m2, ...].
  if (paired_sampling) mask <- torch_cat(c(mask, !mask), 1L)[c(matrix(seq(nrow(batch)), nrow = 2, byrow = T)), ]

  # Final mask masks all entries that is either missing or artificially masked
  # by the generated mask. A value of 1 means that the entry is going to be masked.
  return(mask + nan_mask >= 1)
}








#' Missing Completely At Random Mask Generator
#'
#' @description A mask generator where the masking is determined by component-wise
#' independent Bernoulli distribution.
#'
#' @details Function that takes in a batch of observations and the probability
#' of masking each element based on a component-wise independent Bernoulli
#' distribution. Default value is 0.5, so all masks are equally likely to be trained.
#' Function returns the mask of same shape as batch.
#' Note that the batch can contain missing values, indicated by the "NaN" token.
#' The mask will always mask missing values.
#'
#' @param batch Matrix/Tensor. Only used to get the dimensions and to check if any of the
#' entries are missing. If any are missing, then the returned mask will ensure that
#' these missing entries are masked.
#' @param prob Numeric between 0 and 1. The probability that an entry will be masked.
#' @param seed Integer. Used to set the seed for the sampling process such that we
#' can reproduce the same masks.
#' @param paired_sampling Boolean. If we are doing paired sampling. So include both S and \bar{S}.
#' If TRUE, then batch must be sampled using 'paired_sampler' which creates batches where
#' the first half and second half of the rows are duplicates of each other. That is,
#' batch = [row1, row1, row2, row2, row3, row3, ...].
#'
#' @return A binary matrix of the same size as 'batch'. An entry of '1' indicates that the
#' observed feature value will be masked. '0' means that the entry is NOT masked,
#' i.e., the feature value will be observed/given/available.
#' @example  MCAR_mask_generator_NN(torch_rand(c(5, 3)))
MCAR_mask_generator_NN <- function(batch, prob = 0.5, seed = NULL, paired_sampling = TRUE) {
  if (!is.null(seed)) set.seed(seed) # Set seed for reproducibility
  size <- prod(batch$shape) # Get the number of entries in the batch. TODO: prod???

  # If doing paired sampling, divide size by two as we later concatenate with the inverse mask.
  if (paired_sampling) size <- size / 2

  # Check for missing values in the batch
  nan_mask <- batch$isnan()$to(torch_float())

  # # Torch version, but marginally slower than r version when batch_size <= 128 and num_features <= 50
  # mask = torch_bernoulli(torch_full_like(batch, prob))

  # Create the Bernoulli mask where an element is masked (1) with probability 'prob'.
  mask <- torch_tensor(matrix(sample(c(0, 1), size = size, replace = TRUE, prob = c(prob, 1 - prob)), ncol = ncol(batch)), dtype = torch_float())

  # If paired sampling, then concatenate the inverse mask and reorder to ensure correct order [m1, !m1, m2, !m2, ...].
  if (paired_sampling) mask <- torch_cat(c(mask, !mask), 1L)[c(matrix(seq(nrow(batch)), nrow = 2, byrow = T)), ]

  # Final mask all entries that is either missing or artificially masked
  # by the Bernoulli mask. A value of 1 means that the entry is masked.
  return(mask + nan_mask >= 1)
}




# Paired Sampler  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# A sampler object that allows for paired sampling by always including
# each observation twice. See more on https://rdrr.io/cran/torch/src/R/utils-data-sampler.R.
# Samplers can be used with dataloader() when creating batches from a torch dataset(), see `?dataloader` and `?sampler`.
# Have not used batch iterators that might increase the speed.
# Example how to use it combined with mask generators with paired sampling activated
# batch_size = 4
# if (batch_size %% 2 == 1) batch_size = batch_size - 1 # Make sure that batch size is even
# num_featuers = 3
# num_observations = 5
# shuffle = TRUE
# data = torch_tensor(matrix(rep(seq(num_observations), each = num_featuers), ncol = num_featuers, byrow = TRUE))
# data
# dataset = VAEAC_dataset(data, rep(1, num_featuers))
# dataload = dataloader(dataset,
#                       batch_size = batch_size,
#                       sampler = paired_sampler(dataset,
#                                                shuffle = shuffle))
# dataload$.length() # Number of batches, same as ceiling((2 * num_observation) / batch_size)
# mask_generator = MCAR_mask_generator_NN(paired = TRUE)
# coro::loop(for (batch in dataload) {
#   mask = mask_generator(batch)
#   obs = mask * batch
#   print(torch_cat(c(batch, mask, obs), -1))
# })
paired_sampler <- torch::sampler(
  classname = "paired_sampler",
  initialize = function(data_source, shuffle = FALSE) {
    self$data_source <- data_source
    self$shuffle <- shuffle
  },
  .length = function() length(self$data_source) * 2, # Multiply by two do to the sampling
  .iter = function() {
    n <- length(self$data_source) # Get the number of observations
    indices <- if (self$shuffle) sample.int(n) else seq_len(n) # Either shufle indices or increasing order
    return(coro::as_iterator(rep(indices, each = 2))) # Duplicate each index and return an iterator
  }
)




# Augment Data with Mask --------------------------------------------------


#' Augment Data by Adding Masks as Seperate Columns
#'
#' @description Function that
#'
#' @details This function is only called by the 'comb' versions of the regression methods.
#' This function masks each observation in X by all possible masks or the provided masks
#' and add the masks as additional columns in X.
#' The responses in y are repeated to match the augmented version of X.
#' S is a matrix of coalitions to consider and weights_S are the weights for each mask.
#' If S is not provided, then we use all 2^p-2 possible coalitions.
#' Examples of weights_S are the Shapley kernel weights of the sampling frequency of coalitions.
#' DO NOT INCLUDE THE EMPTY AND GRAND COALITION IN 'S' and 'weights_S'.
#' WE ONLY CONSIDER THE NON-EDGE CASES. DO NOT NEED THE MODEL TO
#' USE MODEL THOSE SITUATIONS (MAYBE KNOWING THE FULL SAMPLE CAN HELP, E.G., NN)
#'
#' @param X Matrix/data.frame/data.table containing the observed features values.
#' @param y Vector
#' @param S If 'NULL', then we consider all coalitions. Otherwise, a matrix of the
#' coalitions to be considered. Useful if we only want to consider a sampled set
#' of coalitions.
#' @param weights_S Must be 'NULL' if 'S' is 'NULL'. Otherwise, an array indicating the
#' weight/importance of the coalitions/masks in 'S'. I.e., the sampling frequencies.
#' @param masks_as_factor Boolean. If the masks are encoded as factors, otherwise,
#' they will be interpreted as numeric 0 and 1 values.
#' @param include_weights Boolean. If we are to include the 'weights_S' in the augmented data frame.
#' @param one_hot_encoding Boolean. If the categorical features are to be one-hot-encoded.
#' @param masking_value Which value we want to set for the masked features.
#' Must be a value which is not in the observed features. E.g., for positive
#' features Frye et al. use '-1' to indicate masked entries.
#'
#' @return An data.table containing the augmented data.
#' @export
#'
#' @examples
augment_data_by_masking_NN <- function(X,
                                       y = NULL,
                                       S = NULL,
                                       weights_S = NULL,
                                       masks_as_factor = TRUE,
                                       include_weights = TRUE,
                                       one_hot_encoding = FALSE,
                                       masking_value = 0) {
  # Convert to data.frame
  X <- as.data.table(X)

  # Get the number of observations.
  n <- nrow(X)

  # Get the number of features.
  p <- ncol(X)

  # If the features have no name, call them Var1, Var2, ...
  if (is.null(colnames(X))) colnames(X) <- c(paste("Var", seq(p), sep = ""))

  # Check that both S and weights_S are provided or that both are NULL.
  if (is.null(S) != is.null(weights_S)) {
    stop("Either the user needs to provide both the coalitions 'S' and weigths 'weight_S', or neither should be provided.")
  }

  # If the coalitions have not been provided, we consider all possible masks/coalitions.
  # Do not check that is.null(weights_S) is true, as it is given if is.null(S) true due to the check above.
  if (is.null(S)) {
    # return(augment_data_by_masking_NN(X = X, y = y, masks_as_factor = masks_as_factor))

    # Get the number of possible coalitions
    n_all_coalitions <- 2^p

    # Compute the coalitions and order them based on size of active elements
    # Remove the edge-cases as we do not need to train over model to handle
    # these cases as they are treated differently in the shapr library
    # S = expand.grid(lapply(1:p, function(j) 0:1))[2:(n_all_coalitions-1),]
    # S = S[order(rowSums(S)), ]
    # USE THE SAME METHOD AS IN SHAPR TO ENSURE THE RIGHT ORDER OF THE COALTIONS!
    S <- shapr:::feature_matrix_cpp(
      features = unlist(lapply(0:p, utils::combn, x = p, simplify = FALSE), recursive = FALSE),
      m = p
    )
    S <- S[2:(nrow(S) - 1), ]
    colnames(S) <- c(paste("Var", seq(p), sep = ""))
    rownames(S) <- 2:(n_all_coalitions - 1)
    # colnames(S) = c(paste("Var", seq(p), sep = ""))

    # We set the weights of the coalitions to be identical.
    weights_S <- rep(1, nrow(S))
  }

  # Likely not needed as the regression models do this automatically.
  # Standardize the weights
  # weights_S = weights_S / sum(weights_S)

  # Get the number of coalitions
  n_coalitions <- nrow(S)

  # Repeat the coalitions as many times as there are observations
  coalitions_repeated <- as.matrix(S[rep(seq_len(n_coalitions), each = n), ])
  coalitions_repeated <- S[rep(seq_len(n_coalitions), each = n), ]

  # Repeat the feature values as many times as there are active coalitions
  X_repeated <- as.matrix(X[rep(seq_len(nrow(X)), times = n_coalitions), ])
  X_repeated <- X[rep(seq_len(nrow(X)), times = n_coalitions), ]

  ## Divide X into the continuous and categorical features
  # Get the column indices for the continuous and categorical features
  cont_cols_idx <- seq(ncol(X))[sapply(X, class) == "numeric"]
  cat_cols_idx <- seq(ncol(X))[sapply(X, class) == "factor"]

  # Get the names of the continuous and categorical features
  cols <- colnames(X)
  cont_cols <- cols[cont_cols_idx]
  cat_cols <- cols[cat_cols_idx]

  # Check if there are any categorical features in the data frame
  if (sum(sapply(X, class) == "factor") >= 1) {
    ### At least one categorical feature
    # Extract the continuous and categorical features columns from X
    X_repeated_cont <- X_repeated[, ..cont_cols]
    X_repeated_cat <- X_repeated[, ..cat_cols]

    # Mask the continuous features
    coalitions_repeated_cont <- coalitions_repeated[, cont_cols_idx]
    # New version that supports arbitrary masking values
    # X_augmented_cont = X_repeated_cont * coalitions_repeated_cont
    X_augmented_cont <- X_repeated_cont
    X_augmented_cont[coalitions_repeated_cont == 0] <- masking_value

    # Mask the categorical features
    coalitions_repeated_cat <- coalitions_repeated[, cat_cols_idx]
    for (column in seq(ncol(X_repeated_cat))) {
      levels(X_repeated_cat[[column]]) <- c(levels(X_repeated_cat[[column]]), "level_masked")
      X_repeated_cat[[column]][coalitions_repeated_cat == 0] <- "level_masked"
    }

    # Combine the masked continuous and masked features.
    # Square brackets ensure that the features are in the same order as the were originally.
    X_augmented <- cbind(X_augmented_cont, X_repeated_cat)[, ..cols]
  } else {
    ### Only continuous features
    # Element-wise multiplication such that each observation is masked by every coalition
    # X_augmented = X_repeated * coalitions_repeated
    # New version that supports specified masking_value and not just zero.
    # Needed for Frye's approach as they do not necessarily mask with 0.
    X_augmented <- X_repeated
    X_augmented[coalitions_repeated == 0] <- masking_value
  }

  # Need to add indicator that feature values are masked by a binary matrix
  # They are now treated as numerical values. Convert to factors later if specified.
  masked_matrix <- 1 * (coalitions_repeated == 0)
  # colnames(masked_matrix) = c(paste("masked_Var", seq(p), sep = ""))
  colnames(masked_matrix) <- c(paste("masked_", colnames(X), sep = ""))
  # We do not want to include the masks for the categorical features
  # as we for them have added a level which corresponds to the feature
  # being masked. So this would add a new column with a 1-1 mapping, which
  # would produce a non-full rank design matrix which is a problem for e.g. lm.
  masked_matrix <- masked_matrix[, c(paste("masked_", cont_cols, sep = ""))]

  # Add the mask to the masked/augmented data
  X_augmented <- as.data.table(cbind(X_augmented, masked_matrix))

  # Convert the binary masks to factor if user has specified so
  if (masks_as_factor) {
    masked_columns <- names(X_augmented)[grep("masked_", names(X_augmented))]

    # https://cran.r-project.org/web/packages/data.table/vignettes/datatable-sd-usage.html
    X_augmented[, (masked_columns) := lapply(.SD, as.factor), .SDcols = masked_columns]
  }

  # If the weights of the coalitions were provided, we add them to the data.table.
  # MIGHT BE BEST TO ALWAYS INCLUDE THEM, SO WEIGHTS ARE 1 BY DEFAULT AND THEN
  # THE INTERNAL_COMB FUNCTIONS ALWAYS USE WEIGHTS.
  if (!is.null(weights_S) && include_weights) {
    # Add the weights of the coalition to the data.table
    X_augmented[, weight := rep(weights_S, each = n)]
  }

  # If given a response vector
  if (!is.null(y)) {
    # then augment the response y by repeating it and
    # adding it as a column in the augmented data
    X_augmented[, y := rep(y, times = n_coalitions)]
  }

  # If convert all categorical features to one-hot encoding
  if (one_hot_encoding) {
    X_augmented <- as.data.table(model.matrix(~ . - 1, data = X_augmented))
  }

  # Return the augmented data table.
  return(X_augmented)
}




# Full Connected Neural Network  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------


# 'nn_module' is the base class for all neural network modules, thus, our
# NN must be a subclass of this class. Also an R6 object with self reference.
# We want to create a fully connected neural network where the input of the
# network is 'input_width' dimensional. The network consists of 'hidden_depth'
# number of hidden neural layers with 'relu' as the activation function, and
# the width of the hidden layers are 'hidden_width'. The output is a one
# dimensional scalar as we are doing regression.
# nn_linear(in_features, out_features, bias = TRUE) is a linear module/layer that applies a linear transformation to the incoming data: y = xA^T + b.
# nn_relu is a relu module/'layer' that applies the rectified linear unit function element-wise: max(0, x).
# nn_sequential is a sequential container of modules, where the modules are called sequentially in the order they are added.
# nn_module_list holds submodules in a list and can be indexed like a regular R list.
# A class defining how to initialize a fully connected neural network.
# FCNN = Fully Connected Neural Network
FCNN <- nn_module(
  # Set the name
  name = "Fully Connected Neural Network",

  # Define the initializer
  initialize = function(input_width, # The number of features in the data set
                        hidden_layer_widths = c(32, 32, 32), # A array with the number of neurons per layer. If you add more elements to the list you create a deeper network.
                        output_width = 1, # Since we are using the NN to do single regression
                        activation_function = nn_relu(), # Which activation function we want to use. Must be on the 'nn_' form and not 'nnf_' form as we need to add it to a nn_sequential. See "https://cran.r-project.org/web/packages/torch/torch.pdf"
                        use_skip_connections = TRUE,
                        use_batch_normalization = TRUE) {
    # Could make this more elegant by including input, hidden and output widths in a common array and then just iterate over them.
    # But would then need to check for last layer such that we do not add activation function at the end / or we could remove it


    # Extra strings to add to names of layers depending on if we use memory layers and/or batch normalization.
    # If FALSE, they are just an empty string and do not effect the names.
    name_extra_batch_normalize <- ifelse(use_batch_normalization, "_and_batch_norm", "")

    # Initialize an empty nn_sequential module. I call it 'network' but can be called whatever
    self$network <- nn_sequential()

    ### Initialize the zeroth layer.
    # if (use_skip_connections) {
    #   # Add identity skip connection. Such that the input is added to the output of the linear layer and activation function: output = X + activation(WX + b).
    #   self$network$add_module(
    #     module = SkipConnection(
    #       nn_linear(input_width, hidden_layer_widths[1]),
    #       activation_function,
    #       if (use_batch_normalization) nn_batch_norm1d(num_features = hidden_layer_widths[1])),
    #     name = paste0("input_layer_skip_connection_with_linear_and_activation", name_extra_batch_normalize)
    #   )
    # } else {
    # Linear layer from the input to the width of the first hidden layer
    self$network$add_module(module = nn_linear(input_width, hidden_layer_widths[1]), name = "input_layer")

    # Add the activation function
    self$network$add_module(module = activation_function, name = "input_activation_function")

    if (use_batch_normalization) {
      # Apply batch normalization
      self$network$add_module(
        module = nn_batch_norm1d(num_features = hidden_layer_widths[1]),
        name = "input_batch_normalization"
      )
    }
    # }

    ### Hidden layers
    # Check if there are more than one hidden layers
    if (length(hidden_layer_widths) > 1) {
      # Iterate over the hidden layer widths and add the hidden layers to the nn_sequential module
      for (depth in base::seq(length(hidden_layer_widths) - 1)) {
        # Subtract one as the last hidden layer is connected to the output_width
        # Note that depth will update its value outside this loop too!

        if (use_skip_connections) {
          # Add identity skip connection. Such that the input is added to the output of the linear layer and activation function: output = X + activation(WX + b).
          self$network$add_module(
            module = SkipConnection(
              nn_linear(hidden_layer_widths[depth], hidden_layer_widths[depth + 1]),
              activation_function,
              if (use_batch_normalization) nn_batch_norm1d(num_features = hidden_layer_widths[depth + 1])
            ),
            name = paste0("hidden_layer_", depth, "_skip_connection_with_linear_and_activation", name_extra_batch_normalize)
          )
        } else {
          # Do not use skip connections and do not add the input to the output.
          # Add a linear layer between the hidden layers with the correct widths
          self$network$add_module(
            module = nn_linear(hidden_layer_widths[depth], hidden_layer_widths[depth + 1]),
            name = paste0("hidden_layer_", depth)
          )

          # Send the output of the linear layer through the activation function.
          self$network$add_module(module = activation_function, name = paste0("hidden_activation_function_", depth))

          if (use_batch_normalization) {
            # Apply batch normalization
            self$network$add_module(
              module = nn_batch_norm1d(num_features = hidden_layer_widths[depth + 1]),
              name = paste0("hidden_batch_normalization_", depth)
            )
          }
        }
      }
    }

    ### Last layer / the output layer
    # Linear layer from the input to the width of the first hidden layer
    self$network$add_module(
      module = nn_linear(hidden_layer_widths[length(hidden_layer_widths)], output_width),
      name = "output_layer"
    )

    # Do not use any activation functions in the output layer
  },

  # How the forward pass of the network is computed.
  forward = function(input) {
    return(self$network(input))
  }
)





#' Scale a data set
#'
#' Compute the normalization parameters (i. e. mean to subtract and std
#' to divide by) for each feature of the dataset.
#' For categorical features mean is zero and std is one.
#' i-th feature is denoted to be categorical if one_hot_max_sizes[i] >= 2.
#' Returns two vectors: means and stds.
#'
#' @param data A torch tensor of dimension n x p.
#' @param one_hot_max_sizes The one-hot max sizes of the columns of data.
#'
#' @return A list containing the means and standard deviations of the numerical columns
#' @export
#'
#' @examples
compute_normalization_NN <- function(data, one_hot_max_sizes) {
  # Create vectors of zeros that will store the means and std for each feature.
  norm_vector_mean <- torch_zeros(length(one_hot_max_sizes))
  norm_vector_std <- torch_ones(length(one_hot_max_sizes))

  # Iterate over the differ
  for (i in seq(one_hot_max_sizes)) {
    size <- one_hot_max_sizes[i] # Get the current size/number of categories.
    if (size >= 2) next # Do not do anything if the feature is categorical
    v <- data[, i] # Get the values of the i'th features
    v <- v[torch_logical_not(torch_isnan(v))] # Only keep the non-missing values
    norm_vector_mean[i] <- mean(v) # Compute the mean of the values and save it in the right place of the vector
    norm_vector_std[i] <- std(v) # Compute the std of the values and save it in the right place of the vector
  }

  # return the vectors of means and standards deviations
  return(norm_vector_mean = norm_vector_mean, norm_vector_std = norm_vector_std)
}



# Internal function for the main function ------------------------------------------------------


#' Internal function for 'NN()'
#'
#' Internal function that fits a NN based on the given hidden_layer_depth,
#' hidden_layer_width and learning_rate. The best model is saved while the
#' training/validation error is returned. Found it best to make it into a separate
#' functions as it was nested inside a triple for loop in the main function 'NN()'.
#'
#' @param hidden_layer_depth the number of hidden layers in the neural network.
#' @param hidden_layer_width the number of neurons in each hidden layer (same in all layers).
#' @param learning_rate the learning rate in the adam optimizer.
#' @param masking_scheme if we are to use the mask Frye seems to use in his paper or a MCAR mask.
#' @param masking_value which value to give to the features that are masked. Frye use -1 and states that it should be a value not in the training data.
#' @param activation_function which activation function to use in the network. Frye does not specify it.
#' @param optimizer which optimizer to use. DEFAULT to adam as Frye specifies that it is the one they used.
#' @param n_epochs the number of times we iterate over the whole training set in the training phase of the neural network.
#' @param dl_train a data loader which loads the training data.
#' @param dl_validation a data loader which loads the validataion data.
#' @param device if we are using CPU or CUDA.
#' @param verbose if the function should give printouts to the user.
#' @param verbose_plot if we are to plot the training and validation error.
#' @param concatenate_mask
#' @param paired_sampling
#' @param use_skip_connections
#' @param use_batch_normalization
#' @param model_save_name
#'
#' @return saves the neural network and returns list containing the training/validation error.
#' @export
#'
#' @examples
NN_internal <- function(hidden_layer_depth,
                        hidden_layer_width,
                        learning_rate,
                        masking_scheme,
                        masking_value,
                        concatenate_mask,
                        paired_sampling,
                        activation_function,
                        use_skip_connections,
                        use_batch_normalization,
                        optimizer,
                        n_epochs,
                        n_networks_to_initiate,
                        n_initiation_epochs,
                        n_epochs_early_stopping,
                        dl_train,
                        dl_validation,
                        device,
                        model_save_name,
                        verbose,
                        verbose_plot,
                        explainer,
                        save_NN_every_100th_epoch) {
  if (n_initiation_epochs > 99) {
    n_initiation_epochs <- 99
    warning(sprintf(
      "The argument 'n_initiation_epochs' must be striclty less than 100. Changed it from %d to 99.",
      n_initiation_epochs
    ))
  }

  if (n_networks_to_initiate == 1) n_initiation_epochs <- 0

  # We are not doing early stopping
  if (is.null(n_epochs_early_stopping)) n_epochs_early_stopping <- n_epochs

  ### Compute some necessary variables.
  # Extract the number of features.
  n_features <- ncol(dl_train$dataset$X)
  n_train_internal <- nrow(dl_train$dataset$X)
  n_validation_internal <- nrow(dl_validation$dataset$X)

  # Get the masking_scheme function.
  masking_function <- if (masking_scheme == "Frye") NN_internal_mask_generator else MCAR_mask_generator_NN
  if (masking) { # TODO::: WE DO NOT WANT FRYE!
    if (masking_scheme == "Frye") {
      masking_function <- NN_internal_mask_generator
    } else if (masking_scheme == "MCAR") {
      masking_function <- MCAR_mask_generator_NN
    } else {
      stop(sprintf("The function does not support masking scheme '%s', it only supports 'Frye' and 'MCAR'."))
    }
  }

  # Variable to keep track of best epoch on the validation data
  best_epoch <- 0
  best_loss <- Inf # Since we are doing regression we set an arbitrary large best loss.

  # Several networks
  if (n_networks_to_initiate > 1) {
    ### Initialize several neural networks
    for (current_initialization_idx in seq(n_networks_to_initiate)) {
      # Variable to keep track of best epoch on the validation data
      current_best_epoch <- 0
      current_best_loss <- Inf # Since we are doing regression we set an arbitrary large best loss.

      ### Initialing the network
      # Now, we can set up the fully connected neural network and send it to our device (cpu or gpu)
      current_NN_model <- FCNN(
        input_width = ifelse(concatenate_mask, 2 * n_features, n_features),
        hidden_layer_widths = base::rep(hidden_layer_width, times = hidden_layer_depth),
        output_width = 1,
        activation_function = activation_function,
        use_skip_connections = use_skip_connections,
        use_batch_normalization = use_batch_normalization
      )$to(device = device)
      # # Get a quick overview of how many parameters our network has
      # NN_model
      # # More specific details
      # NN_model$network
      # NN_model$modules


      ### Initialing the optimizer
      # To update the parameters of the network we need to setup an optimizer.
      # We use the adam optimizer with default parameters, see '?optim_adam'.
      # NOTE: If you need to move a model to GPU via ⁠$cuda()⁠, please do
      # so before constructing optimizers for it. Parameters of a model
      # after ⁠$cuda()⁠ will be different objects from those before the call.
      if (optimizer == "adam") {
        current_NN_model_optimizer <- optim_adam(
          params = current_NN_model$parameters,
          lr = learning_rate,
          betas = c(0.9, 0.999),
          eps = 1e-08,
          weight_decay = 0,
          amsgrad = FALSE
        )
      } else {
        stop("The implementation currenclty only supportes the default version of the ADAM optimizer.")
      }

      ### The Training Loop
      # Tell the network that we are training and that gradients should be computed
      current_NN_model$train()
      if (current_NN_model$training != TRUE) stop("The network is not in its training mode. Will not keep track of gradients.")

      # Array to store the error of the model on the training and validation data after each epoch
      current_training_error <- rep(NA, n_epochs)
      current_validation_error <- rep(NA, n_epochs)

      # Create a progress bar. TODO: Maybe include width, depth, latent_dim, lr, if doing hyperparemeter tuning.
      current_pb <- progress_bar$new(
        format = "(:spin) [:bar] :percent [NN#:idx | Time: :elapsedfull | ETR: :eta | Epoch: :epoch | Train: :train_error | Val: :val_error | BEpo: :best_epoch | BT: :best_train_error | BVal: :best_val_error]",
        total = n_initiation_epochs, # * train_dataloader$.length(),
        complete = "=", # Completion bar character
        incomplete = "-", # Incomplete bar character
        current = ">", # Current bar character
        clear = !verbose, # If TRUE, clears the bar when finish
        width = 150
      ) # Width of the progress bar

      # Start the training loop
      epoch <- 1
      for (epoch in base::seq(n_initiation_epochs)) {
        # Array to keep track of the training errors
        training_error_batch <- c()

        # Set the network to its the training mode
        current_NN_model$train()

        # Iterate over the training batches
        # Note that `enumerate` construct is deprecated in favor of the `coro::loop`, see 'https://github.com/mlverse/torch/issues/558'.
        # batch = dl_train$.iter()$.next()
        coro::loop(for (batch in dl_train) {
          # Generate a missing completely at random mask.
          # Mask with entry equal to 1 means that that entry should be masked.
          # Get the mask. Either the one that it seems that Frye uses or a MCAR mask.
          mask_now <- masking_function(batch = batch$X, paired_sampling = paired_sampling)

          # Apply the mask to the batch, such that we are left with the observed
          # values while the masked values are set to zero.
          # observed_values = batch$X * (mask_now == 0)
          # observed_values
          # batch$X[mask_now == 1] = 0
          # batch$X
          # In Frye et al's approach, we set the missing values to the provided value
          batch$X[mask_now == 1] <- masking_value

          # If we are to do Olsen approach by concatenating the mask instead.
          if (concatenate_mask) {
            batch$X <- torch_cat(c(batch$X, mask_now), -1)
          }

          # This is not used in Frye et al.
          # # Add the mask to the observed values as additional features.
          # # So the dimension is now batch_size x (2*p).
          # observed_values_with_mask = torch_cat(c(observed_values, mask_now), dim = 2)
          # observed_values_with_mask

          # Send the training batch through the network and compute the predictions/output
          # output = model(observed_values_with_mask)
          current_NN_model_output <- current_NN_model(batch$X)

          # # Compute the predicted response
          # output = model(batch$X)

          # Compute the mse loss. Use the functional 'nnf_' as I do not need to create a module.
          # Squeeze the torch to make it go from dimension batch_size x 1 to be a tensor/vector of length batch_size.
          current_NN_model_loss <- nnf_mse_loss(current_NN_model_output$squeeze(), batch$y)

          # Set all previous gradients to zero.
          current_NN_model_optimizer$zero_grad()

          # Backpropagate/pass the loss backward through the neural network.
          current_NN_model_loss$backward()

          # Take one step of the optimizer.
          current_NN_model_optimizer$step()

          # Keep track of the loss
          training_error_batch <- c(training_error_batch, current_NN_model_loss$item() * nrow(batch$X))
        })

        # Note that the training error is based on several versions of the network.
        # That is, we update the parameters after each batch. So we will get a
        # slightly different value when calling
        # nnf_mse_loss(NN_model(torch_tensor(training$x)), torch_tensor(training$y)))
        # Do not call that to save computational time for larger networks
        current_training_error[epoch] <- sum(training_error_batch) / n_train_internal

        # Evaluate the model on the validation data
        current_NN_model$eval()

        # Array to keep track of the validation errors
        validation_error_batch <- c()

        # Iterate over the validation data
        # batch_validation = dl_validation$.iter()$.next()
        coro::loop(for (batch_validation in dl_validation) {
          # n_levels = unname(sapply(explainer$feature_list$factor_levels, length))
          # n_levels[n_levels == 0] = 1
          # n_levels

          n_levels <- explainer$x_train_attribute

          # explainer$active_coalitions
          # explainer$n_coalitions
          if (explainer$n_coalitions > 512) {
            # We sample a subset of the coalitions
            sampled_indices <- sample(nrow(explainer$active_coalitions), 512)
            explainer$active_coalitions_sampled <- explainer$active_coalitions[sampled_indices, ]
            explainer$active_coalitions_weights_sampled <- explainer$active_coalitions_weights[sampled_indices]
          } else {
            # We use all coalitions
            explainer$active_coalitions_sampled <- explainer$active_coalitions
            explainer$active_coalitions_weights_sampled <- explainer$active_coalitions_weights
          }
          explainer$active_coalitions_extended <- t(sapply(seq(nrow(explainer$active_coalitions_sampled)), function(current_row_idx) rep(explainer$active_coalitions_sampled[current_row_idx, ], times = n_levels)))

          # Mask the validation data by all possible masks/coalitions.
          # TODO: THIS FUNCTION COMPUTES A LOT OF REDUNDANT STUFF. UPDATE.
          batch_validation_augmented <-
            augment_data_by_masking_NN(
              X = as.matrix(batch_validation$X),
              y = as.matrix(batch_validation$y),
              S = explainer$active_coalitions_extended,
              weights_S = explainer$active_coalitions_weights_sampled,
              masking_value = masking_value,
              include_weights = FALSE,
              one_hot_encoding = FALSE,
              masks_as_factor = FALSE
            )

          # Extract the augmented features and response
          cols_var <- if (concatenate_mask) seq(2 * n_features) else seq(n_features)
          batch_validation_augmented_x <- torch_tensor(as.matrix(batch_validation_augmented[, ..cols_var]))
          batch_validation_augmented_y <- torch_tensor(as.matrix(batch_validation_augmented[[ncol(batch_validation_augmented)]]))

          # Compute the output of the NN
          output_validation <- current_NN_model(batch_validation_augmented_x)

          # Compute the squared validation error for the batch
          validation_error_batch <- c(validation_error_batch, nnf_mse_loss(output_validation, batch_validation_augmented_y)$item() * nrow(output_validation))
        })

        # Compute the mean squared validation error averaged over all batches/observations
        # validation_error[epoch] = sum(validation_error_batch)/(n_validation_internal * (2^explainer$n_features - 2))
        current_validation_error[epoch] <- sum(validation_error_batch) / (n_validation_internal * nrow(explainer$active_coalitions_sampled))

        # Check if this is the best epoch/model thus far. If yes, then we save it.
        if (current_validation_error[epoch] < best_loss) {
          torch_save(current_NN_model, paste(model_save_name, "_best_epoch.pt", sep = ""))
          best_loss <- current_validation_error[epoch]
          best_epoch <- epoch

          # Store which of the initializations that performed best
          best_initialization_idx <- current_initialization_idx

          # Store the networks and other stuff related to the best network initialization.
          NN_model <- current_NN_model
          NN_model_optimizer <- current_NN_model_optimizer
          training_error <- current_training_error
          validation_error <- current_validation_error
        }

        # Check if this is the best epoch/model for this network initiazation
        if (current_validation_error[epoch] < current_best_loss) {
          current_best_loss <- current_validation_error[epoch]
          current_best_epoch <- epoch
        }

        # Update the progress bar
        current_pb$tick(tokens = list(
          idx = current_initialization_idx,
          epoch = epoch,
          train_error = round(current_training_error[epoch], 3),
          val_error = round(current_validation_error[epoch], 3),
          best_epoch = current_best_epoch,
          best_train_error = round(current_training_error[current_best_epoch], 3),
          best_val_error = round(current_validation_error[current_best_epoch], 3)
        ))
      } # End learning epoch
    }
    cat(sprintf("Best initialization was number %d. Continue training that network.\n", best_initialization_idx))
  } else {
    # only 1 network

    ### Initialing the network
    # Now, we can set up the fully connected neural network and send it to our device (cpu or gpu)
    NN_model <- FCNN(
      input_width = ifelse(concatenate_mask, 2 * n_features, n_features),
      hidden_layer_widths = base::rep(hidden_layer_width, times = hidden_layer_depth),
      output_width = 1,
      activation_function = activation_function,
      use_skip_connections = use_skip_connections,
      use_batch_normalization = use_batch_normalization
    )$to(device = device)
    # # Get a quick overview of how many parameters our network has
    # NN_model
    # # More specific details
    # NN_model$network
    # NN_model$modules


    ### Initialing the optimizer
    # To update the parameters of the network we need to setup an optimizer.
    # We use the adam optimizer with default parameters, see '?optim_adam'.
    # NOTE: If you need to move a model to GPU via ⁠$cuda()⁠, please do
    # so before constructing optimizers for it. Parameters of a model
    # after ⁠$cuda()⁠ will be different objects from those before the call.
    if (optimizer == "adam") {
      NN_model_optimizer <- optim_adam(
        params = NN_model$parameters,
        lr = learning_rate,
        betas = c(0.9, 0.999),
        eps = 1e-08,
        weight_decay = 0,
        amsgrad = FALSE
      )
    } else {
      stop("The implementation currenclty only supportes the default version of the ADAM optimizer.")
    }
    # Array to store the error of the model on the training and validation data after each epoch
    training_error <- rep(NA, n_epochs)
    validation_error <- rep(NA, n_epochs)

    # Variable to keep track of best epoch on the validation data
    best_epoch <- 0
    best_loss <- Inf # Since we are doing regression we set an arbitrary large best loss.

    # Get the masking_scheme function.
    if (masking_scheme == "Frye") {
      masking_function <- NN_internal_mask_generator
    } else if (masking_scheme == "MCAR") {
      masking_function <- MCAR_mask_generator_NN
    } else {
      stop(sprintf("The function does not support masking scheme '%s', it only supports 'Frye' and 'MCAR'."))
    }
  }


  ### The Training Loop
  # Tell the network that we are training and that gradients should be computed
  NN_model$train()
  if (NN_model$training != TRUE) stop("The network is not in its training mode. Will not keep track of gradients.")

  # List to store the NN every 100th epoch
  if (save_NN_every_100th_epoch) list_save_names_every_100th_epoch <- list()

  # Create a progress bar. TODO: Maybe include width, depth, latent_dim, lr, if doing hyperparemeter tuning.
  pb <- progress_bar$new(
    format = "(:spin) [:bar] :percent [Time: :elapsedfull | ETR: :eta | Epoch: :epoch | Train: :train_error | Val: :val_error | BEpo: :best_epoch | BT: :best_train_error | BVal: :best_val_error]",
    total = n_epochs - n_initiation_epochs, # * train_dataloader$.length(),
    complete = "=", # Completion bar character
    incomplete = "-", # Incomplete bar character
    current = ">", # Current bar character
    clear = !verbose, # If TRUE, clears the bar when finish
    width = 150
  ) # Width of the progress bar

  # Start the training loop
  epoch <- n_initiation_epochs + 1
  for (epoch in base::seq(n_initiation_epochs + 1, n_epochs)) {
    # Array to keep track of the training errors
    training_error_batch <- c()

    # Set the network to its the training mode
    NN_model$train()

    # Iterate over the training batches
    # Note that `enumerate` construct is deprecated in favor of the `coro::loop`, see 'https://github.com/mlverse/torch/issues/558'.
    # batch = dl_train$.iter()$.next()
    coro::loop(for (batch in dl_train) {
      # Generate a missing completely at random mask.
      # Mask with entry equal to 1 means that that entry should be masked.
      # Get the mask. Either the one that it seems that Frye uses or a MCAR mask.
      mask_now <- masking_function(batch = batch$X, paired_sampling = paired_sampling)

      # Apply the mask to the batch, such that we are left with the observed
      # values while the masked values are set to zero.
      # observed_values = batch$X * (mask_now == 0)
      # observed_values
      # batch$X[mask_now == 1] = 0
      # batch$X
      # In Frye et al's approach, we set the missing values to the provided value
      batch$X[mask_now == 1] <- masking_value

      # If we are to do Olsen approach by concatenating the mask instead.
      if (concatenate_mask) {
        batch$X <- torch_cat(c(batch$X, mask_now), -1)
      }

      # This is not used in Frye et al.
      # # Add the mask to the observed values as additional features.
      # # So the dimension is now batch_size x (2*p).
      # observed_values_with_mask = torch_cat(c(observed_values, mask_now), dim = 2)
      # observed_values_with_mask

      # Send the training batch through the network and compute the predictions/output
      # output = model(observed_values_with_mask)
      NN_model_output <- NN_model(batch$X)

      # # Compute the predicted response
      # output = model(batch$X)

      # Compute the mse loss. Use the functional 'nnf_' as I do not need to create a module.
      # Squeeze the torch to make it go from dimension batch_size x 1 to be a tensor/vector of length batch_size.
      NN_model_loss <- nnf_mse_loss(NN_model_output$squeeze(), batch$y)

      # Set all previous gradients to zero.
      NN_model_optimizer$zero_grad()

      # Backpropagate/pass the loss backward through the neural network.
      NN_model_loss$backward()

      # Take one step of the optimizer.
      NN_model_optimizer$step()

      # Keep track of the loss
      training_error_batch <- c(training_error_batch, NN_model_loss$item() * nrow(batch$X))
    })

    # Note that the training error is based on several versions of the network.
    # That is, we update the parameters after each batch. So we will get a
    # slightly different value when calling
    # nnf_mse_loss(NN_model(torch_tensor(training$x)), torch_tensor(training$y)))
    # Do not call that to save computational time for larger networks
    training_error[epoch] <- sum(training_error_batch) / n_train_internal

    # Evaluate the model on the validation data
    NN_model$eval()

    # Array to keep track of the validation errors
    validation_error_batch <- c()

    # Iterate over the validation data
    # batch_validation = dl_validation$.iter()$.next()
    coro::loop(for (batch_validation in dl_validation) {
      # n_levels = unname(sapply(explainer$feature_list$factor_levels, length))
      # n_levels[n_levels == 0] = 1
      # n_levels

      n_levels <- explainer$x_train_attribute

      # explainer$active_coalitions
      # explainer$n_coalitions
      if (explainer$n_coalitions > 512) {
        # We sample a subset of the coalitions
        sampled_indices <- sample(nrow(explainer$active_coalitions), 512)
        explainer$active_coalitions_sampled <- explainer$active_coalitions[sampled_indices, ]
        explainer$active_coalitions_weights_sampled <- explainer$active_coalitions_weights[sampled_indices]
      } else {
        # We use all coalitions
        explainer$active_coalitions_sampled <- explainer$active_coalitions
        explainer$active_coalitions_weights_sampled <- explainer$active_coalitions_weights
      }
      explainer$active_coalitions_extended <- t(sapply(seq(nrow(explainer$active_coalitions_sampled)), function(current_row_idx) rep(explainer$active_coalitions_sampled[current_row_idx, ], times = n_levels)))

      # Mask the validation data by all possible masks/coalitions.
      # TODO: THIS FUNCTION COMPUTES A LOT OF REDUNDANT STUFF. UPDATE.
      batch_validation_augmented <-
        augment_data_by_masking_NN(
          X = as.matrix(batch_validation$X),
          y = as.matrix(batch_validation$y),
          S = explainer$active_coalitions_extended,
          weights_S = explainer$active_coalitions_weights_sampled,
          masking_value = masking_value,
          include_weights = FALSE,
          one_hot_encoding = FALSE,
          masks_as_factor = FALSE
        )

      # Extract the augmented features and response
      cols_var <- if (concatenate_mask) seq(2 * n_features) else seq(n_features)
      batch_validation_augmented_x <- torch_tensor(as.matrix(batch_validation_augmented[, ..cols_var]))
      batch_validation_augmented_y <- torch_tensor(as.matrix(batch_validation_augmented[[ncol(batch_validation_augmented)]]))

      # Compute the output of the NN
      output_validation <- NN_model(batch_validation_augmented_x)

      # Compute the squared validation error for the batch
      validation_error_batch <- c(validation_error_batch, nnf_mse_loss(output_validation, batch_validation_augmented_y)$item() * nrow(output_validation))
    })

    # Compute the mean squared validation error averaged over all batches/observations
    # validation_error[epoch] = sum(validation_error_batch)/(n_validation_internal * (2^explainer$n_features - 2))
    validation_error[epoch] <- sum(validation_error_batch) / (n_validation_internal * nrow(explainer$active_coalitions_sampled))

    # Check if this is the best epoch/model thus far. If yes, then we save it.
    if (validation_error[epoch] < best_loss) {
      torch_save(NN_model, paste(model_save_name, "_best_epoch.pt", sep = ""))
      best_loss <- validation_error[epoch]
      best_epoch <- epoch
    }

    if (save_NN_every_100th_epoch & (epoch %% 100 == 0)) {
      torch_save(NN_model, paste(model_save_name, "_epoch_", epoch, ".pt", sep = ""))
      list_save_names_every_100th_epoch[paste("epoch_", epoch, sep = "")] <- paste(model_save_name, "_epoch_", epoch, ".pt", sep = "")
    }

    # # Printout to the user
    # if (verbose) {
    #   # if (validation_error[epoch] == best_loss) {
    #   #   cat(sprintf("Loss at epoch %3d: training = %7.4f, validation = %7.4f (new best validation).\n", epoch, training_error[epoch], validation_error[epoch]))
    #   # } else if (epoch %% (n_epochs/10) == 0) {
    #   #   cat(sprintf("Loss at epoch %3d: training = %7.4f, validation = %7.4f.\n", epoch, training_error[epoch], validation_error[epoch]))
    #   # }
    #   if (epoch %% (n_epochs/10) == 0) {
    #     cat(sprintf("Epoch %3d. Best epoch so far is %d with training error %5.3f and validation error %5.3f.\n",
    #                 epoch, best_epoch, training_error[best_epoch], validation_error[best_epoch]))
    #   }
    # } # End printout

    # Update the progress bar
    pb$tick(tokens = list(
      epoch = epoch,
      train_error = round(training_error[epoch], 3),
      val_error = round(validation_error[epoch], 3),
      best_epoch = best_epoch,
      best_train_error = round(training_error[best_epoch], 3),
      best_val_error = round(validation_error[best_epoch], 3)
    ))

    # Early stopping
    if (epoch - best_epoch > n_epochs_early_stopping) {
      pb$terminate()
      cat(sprintf("Early stopping at epoch %d. No validation imporvment has been made in %d epochs.\n", epoch, n_epochs_early_stopping))

      # Remove NA entries from arrays as they have not been added due to early stopping
      training_error <- training_error[!is.na(training_error)]
      validation_error <- validation_error[!is.na(validation_error)]
      early_stopping_applied <- TRUE
      break
    }
  } # End learning epoch

  # Save the last model to disk
  torch_save(NN_model, paste(model_save_name, "_last_epoch.pt", sep = ""))

  # Create a list of values to return
  return_list <- list(
    training_error = training_error,
    validation_error = validation_error,
    best_epoch = best_epoch,
    last_epoch = epoch,
    early_stopping_applied = epoch != n_epochs,
    best_loss = best_loss,
    save_path_best_model = paste(model_save_name, "_best_epoch.pt", sep = ""),
    save_path_last_model = paste(model_save_name, "_last_epoch.pt", sep = ""),
    learning_rate = learning_rate,
    hidden_layer_depth = hidden_layer_depth,
    hidden_layer_width = hidden_layer_width,
    learning_rate = learning_rate,
    masking_scheme = masking_scheme,
    paired_sampling = paired_sampling,
    concatenate_mask = concatenate_mask,
    use_skip_connections = use_skip_connections,
    use_batch_normalization = use_batch_normalization
  )

  if (save_NN_every_100th_epoch) {
    return_list$list_save_names_every_100th_epoch <- list_save_names_every_100th_epoch
  }

  # If we are to plot the
  if (verbose_plot) {
    library(tidyr)
    library(ggplot2)

    return_list$plot_data <- tibble(training_error, validation_error) %>%
      pivot_longer(c(1, 2), names_to = "type", values_to = "error") %>%
      dplyr::arrange(type) %>%
      dplyr::mutate(epoch = rep(seq(n_epochs), times = 2), masking_scheme = masking_scheme, hld = hidden_layer_depth, hlw = hidden_layer_width, lr = learning_rate)

    figure <-
      return_list$plot_data %>%
      ggplot(aes(x = epoch, y = error, color = type)) +
      geom_line() +
      geom_point() +
      # scale_color_brewer(palette="Dark2") +
      # theme_minimal() +
      # theme(legend.position="top") +
      scale_color_brewer(palette = "Paired") +
      labs(title = sprintf(
        "hidden layer depth = %d, hidden layer width = %d, learning rate = %g",
        hidden_layer_depth, hidden_layer_width, learning_rate
      ))
    plot(figure)
  }

  # Return the list
  return(return_list)
}


# Main function -----------------------------------------------------------

# training_split_ratio = 0.75
# training_split_seed = NULL
# training_split_indices = NULL
# hidden_layer_depths = 2
# hidden_layer_widths = c(128, 256, 512)
# learning_rates = c(10^-3, 10^-4)
# n_epochs = 10^4
# optimizer = "adam"
# batch_size = 256
# batch_size_validation = 64
# masking_value = -10
# use_cuda = TRUE
# test = NULL
# verbose = TRUE

#' Neural network à la Frye et al.
#'
#' Function that computes Shapley values using a single for all coalitions.
#'
#' @param training a list containing the feature values as 'training$x' and 'training$y' are the corresponding
#'  estimated responses of the prediction model to explain. Should be a matrix. Not tested with data.frame or table.
#'  The features must be numeric. Categorical data must be one-hot encoded before sent to this function.
#' @param training_split_ratio the proportion of training data we are going to use as training data,
#'  while the remaining is used as evaluation data.
#' @param training_split_seed if we are going to set some specified seed before splitting the training data.
#' @param training_split_indices if the user want to manually specify the observations used to train the model.
#'  Overwrites the two parameters above.
#' @param hidden_layer_depths the number of hidden layers in the neural network. Iterate over the provided depths and finds the best.
#' @param hidden_layer_widths the number of neurons in each hidden layer (same in all layers). Iterate over the provided widths and finds the best.
#' @param learning_rates the learning rate in the adam optimizer. Iterate over the provided learning rates and finds the best.
#' @param n_epochs the number of times we iterate over the whole training set in the training phase of the neural network.
#' @param optimizer which optimizer to use. DEFAULT to adam as Frye specifies that it is the one they used.
#' @param batch_size the batch size used during the training phase.
#' @param batch_size_validation the batch size used during the validation phase. Should be less as each observation is masked in 2^{num_features} ways.
#' @param masking_schemes if we are to use the mask Frye seems to use in his paper or a MCAR mask.
#' @param masking_value which value to give to the features that are masked. Frye use -1 and states that it should be a value not in the training data.
#' @param activation_function which activation function to use in the network. Frye does not specify it.
#' @param use_cuda if we are to run the code on the CPU or CUDA/GPU, if available.
#' @param test same as the training data. Used to evaluate the fitted NN. NOT IMPLEMENTED YET!
#' @param save_folder which folder to save the fitted neural networks.
#' @param verbose if the function should give printouts to the user.
#' @param verbose_plot if we are to plot the training and validation error.
#' @param verbose_plot_points if we are to plot points along the training and validation error curves.
#' @param first_epoch_to_show if we are to plot, which epoch is the first to show. Using 1 might result in large range on y-axis.
#' @param only_plot_epochs_divisible_by_this_value if we are to skip some of the of the epochs in the plot.
#' @param extra_save_name If we are to add any post text to the filenames.
#' @param paired_sampling
#' @param concatenate_mask
#' @param use_skip_connections
#' @param use_batch_normalization
#'
#' @return the best NN configuration based on the given hyperparameters and using the masking technique of Frye et al.
#' @export
#'
#' @examples
NN <- function(training,
               training_split_ratio = 0.75,
               training_split_seed = NULL,
               training_split_indices = NULL,
               hidden_layer_depths = 2,
               hidden_layer_widths = c(128, 256, 512),
               learning_rates = c(0.01, 0.001, 0.0001), # c(10^-3, 10^-4),
               n_epochs = 10^4,
               n_networks_to_initiate = 10,
               n_initiation_epochs = 20,
               n_epochs_early_stopping = 150,
               optimizer = "adam",
               batch_size = 256,
               batch_size_validation = 64,
               masking_schemes = c("Frye", "MCAR"),
               masking_value = -10,
               concatenate_mask = FALSE,
               paired_sampling = TRUE,
               activation_function = nn_relu,
               use_skip_connections = TRUE,
               use_batch_normalization = TRUE,
               col_means = NULL,
               col_standard_deviations = NULL,
               use_cuda = TRUE,
               test = NULL,
               save_folder = "/Users/larsolsen/PhD/Paper2/Simulations/NN_models/",
               verbose = FALSE,
               verbose_plot = TRUE,
               verbose_plot_points = FALSE,
               first_epoch_to_show = 5,
               only_plot_epochs_divisible_by_this_value = 1,
               extra_save_name = "",
               save_NN_every_100th_epoch = TRUE,
               explainer = NULL) {
  # TODO. ADD SUPPORT FOR CATEGORICAL VARIABLES THORUGH ONE-HOT ENCODING.
  #       ADD SUPPORT FOR SCALING/NORMALIZING OF THE CONTINUOUS VARIABLES.
  # Paired sampling
  # Specified mask generator in high dim
  #

  if (concatenate_mask) {
    # We are attaching the mask to the batch and zero out the masked values in the batch.
    masking_value <- 0
  }

  # Check for valid masking scheme
  masking_schemes <- match.arg(masking_schemes)
  # if ("Frye" %in% masking_schemes & paired_sampling == TRUE) {
  #   stop(sprintf("Masking scheme 'Frye' does not support paired sampling. Paired sampling is only applicable for masking scheme 'MCAR'.\n"))
  # }

  ### Some test for valid parameter input
  if (training_split_ratio <= 0 | 1 <= training_split_ratio) stop("Training error is outside (0,1). We recommend 'training_split_ratio = 0.75'.")

  # Check how close the masking_value is any value in the training data
  closest_observation <- which.min(as.matrix(abs(training$x - masking_value))) %% nrow(training$x)
  closest_observation_feature <- which.min(abs(training$x[closest_observation, ] - masking_value))
  if (!concatenate_mask) {
    warning(sprintf(
      "Closest value to 'masking_value = %g' is %g in training observation %d feature %d. (Only applicable for masking scheme 'Frye'.",
      masking_value, training$x[closest_observation, closest_observation_feature], closest_observation, closest_observation_feature
    ))
  }

  ### Device set-up: CPU or GPU
  # First, we check whether a compatible GPU is available for computation, and
  # if the user wants the code to run on cuda/GPU, then we set the device to cuda.
  # Note, default is CPU.
  if (torch::cuda_is_available() & use_cuda) {
    device <- torch_device("cuda")
  } else {
    if (use_cuda) warning("cuda is not available. Fall back on CPU instead of GPU.")
    device <- torch_device("CPU")
  }

  ### Extract some relevant parameters
  # Number of training observations
  n_train <- nrow(training$x)
  # n_features = ncol(training$x)

  ### Split the training data into a training set and validation set
  # Check if the training/validation split indices of the training data is provided
  if (is.null(training_split_indices)) {
    ## We sample the indices of the training data

    # Set the seed if given to us by the user
    if (!is.null(training_split_seed)) set.seed(training_split_seed)

    # Get the training indices
    training_indices <- sample(n_train, round(n_train * training_split_ratio))
  }

  # Split the data into the training and validation sets
  train_x <- training$x[training_indices, ]
  train_y <- training$y[training_indices, ]
  validation_x <- training$x[-training_indices, ]
  validation_y <- training$y[-training_indices, ]

  # NOTE: we do not want out network to estimate the true response y, but rather
  # estimate the predicted value of the prediction function f which we want to explain.
  # Hence, as specified in the description of the model, the 'train_y' and 'validation_y'
  # must NOT be the actual response, but rather the estimated responses based on f.
  # At least in the Shapley value situation. Outside our domain, it might be
  # reasonable to use the true/actual response in other applications.

  ### Create the Data Set object
  ds_train <- NN_internal_sample_data_for_shapley_regression(train_x, train_y)
  ds_validation <- NN_internal_sample_data_for_shapley_regression(validation_x, validation_y)
  # # Look at the number of observations in the data set object
  # ds_train$.length()
  # # Can look at the first 10 observations, in the same order as inputted.
  # ds_train$.getitem(1:10)


  # Create the Data Loader object which can iterate over the data in the Data Set object
  # See more parameters here '?dataloader', but these are the most important.
  if (paired_sampling) {
    # Use paired sampling
    dl_train <- dataloader(ds_train,
      batch_size = batch_size,
      sampler = paired_sampler(ds_train, shuffle = TRUE)
    )
    # dl_validation = dataloader(ds_validation,
    #                            batch_size = batch_size_validation,
    #                            sampler = paired_sampler(ds_validation, shuffle = FALSE))
    dl_validation <- dataloader(ds_validation, batch_size = batch_size_validation, shuffle = FALSE)
  } else {
    # Usual approach
    dl_train <- dataloader(ds_train, batch_size = batch_size, shuffle = TRUE)
    dl_validation <- dataloader(ds_validation, batch_size = batch_size_validation, shuffle = FALSE)
  }

  # Variables to store the best model and the corresponding best validation loss obtained
  best_model <- NULL
  best_loss <- Inf

  # List to store all the models
  NN_results_list <- list()

  # # Try out different hyper parameters
  # hidden_layer_depth = hidden_layer_depths[1]
  # hidden_layer_width = hidden_layer_widths[3]
  # learning_rate = learning_rates[1]
  # masking_scheme = "Frye"

  # Small printout to the user
  if (verbose) {
    # cat(sprintf("\n"))
  }

  # print(masking_schemes)
  # print(hidden_layer_depths)
  # print(hidden_layer_widths)
  # print(learning_rates)


  # Iterate over the given masking schemes
  for (masking_scheme in masking_schemes) {
    # Update the list storing all the models
    tmp_str_1 <- paste0("mask_", masking_scheme)
    NN_results_list[[tmp_str_1]] <- list()

    # Iterate over the given hidden layer depths
    for (hidden_layer_depth in hidden_layer_depths) {
      # Update the list storing all the models
      tmp_str_2 <- paste0("depth_", hidden_layer_depth)
      NN_results_list[[tmp_str_1]][[tmp_str_2]] <- list()

      # Iterate over the given hidden layer widths
      for (hidden_layer_width in hidden_layer_widths) {
        # Update the list storing all the models
        tmp_str_3 <- paste0("width_", hidden_layer_width)
        NN_results_list[[tmp_str_1]][[tmp_str_2]][[tmp_str_3]] <- list()

        # Iterate over the given learning rates
        for (learning_rate in learning_rates) {
          # Update the list storing all the models
          tmp_str_4 <- sprintf("lr_%g", learning_rate)

          # Small printout to the user
          if (verbose) {
            cat(sprintf(
              "Best setup so far: masking = %s, concat = %s, paired = %s, depth = %d, width = %d, lr = %g, and val_loss = %.3g.\n",
              best_model$masking_scheme, best_model$concatenate_mask, best_model$paired_sampling, best_model$hidden_layer_depth, best_model$hidden_layer_width, best_model$learning_rate, best_model$best_loss
            ))
            cat(sprintf(
              "Currently working on: masking = %s, concat = %s, paired = %s, depth = %d, width = %d, and lr = %g.\n",
              masking_scheme, concatenate_mask, paired_sampling, hidden_layer_depth, hidden_layer_width, learning_rate
            ))
          }

          # Create the save name based on the hidden layer depth, width and the learning rate
          model_save_name <- sprintf(
            "Shap_reg_NN_mask_%s_cat_%s_paired_%s_depth_%d_width_%d_lr_%g",
            masking_scheme, concatenate_mask, paired_sampling, hidden_layer_depth, hidden_layer_width, learning_rate
          )
          if (extra_save_name != "" && substr(extra_save_name, 1, 1) != "_") extra_save_name <- paste("_", extra_save_name, sep = "")
          model_save_name <- paste(save_folder, model_save_name, extra_save_name, sep = "")

          # Fit the neural network
          NN_results_time <- system.time(
            {
              NN_results <- NN_internal(
                hidden_layer_depth = hidden_layer_depth,
                hidden_layer_width = hidden_layer_width,
                learning_rate = learning_rate,
                masking_scheme = masking_scheme,
                masking_value = masking_value,
                concatenate_mask = concatenate_mask,
                paired_sampling = paired_sampling,
                activation_function = activation_function,
                use_skip_connections = use_skip_connections,
                use_batch_normalization = use_batch_normalization,
                optimizer = optimizer,
                n_epochs = n_epochs,
                n_networks_to_initiate = n_networks_to_initiate,
                n_initiation_epochs = n_initiation_epochs,
                n_epochs_early_stopping = n_epochs_early_stopping,
                dl_train = dl_train,
                dl_validation = dl_validation,
                device = device,
                model_save_name = model_save_name,
                verbose = verbose,
                verbose_plot = verbose_plot,
                save_NN_every_100th_epoch = save_NN_every_100th_epoch,
                explainer = explainer
              )
            },
            gcFirst = FALSE
          )
          NN_results$time <- NN_results_time

          # Check if this setup of hyper-parameters are best
          if (NN_results$best_loss < best_loss) {
            best_model <- NN_results
            best_loss <- NN_results$best_loss
          }

          # plot
          if (verbose) {
            NN_results$plot_data %>%
              ggplot(aes(x = epoch, y = error, color = type)) +
              geom_line() +
              # geom_point() +
              # scale_color_brewer(palette="Dark2") +
              # theme_minimal() +
              # theme(legend.position="top") +
              scale_color_brewer(palette = "Paired") +
              labs(title = sprintf(
                "hidden layer depth = %d, hidden layer width = %d, learning rate = %g",
                hidden_layer_depth, hidden_layer_width, learning_rate
              ))

            # Save the model to the list
            NN_results_list[[tmp_str_1]][[tmp_str_2]][[tmp_str_3]][[tmp_str_4]] <- NN_results

            # NN_results$plot_data %>%
            #   dplyr::filter(type == "validation_error") %>%
            #   dplyr::mutate(min_val_error = cummin(error))
          }
        } # End learning_rate
      } # End hidden_layer_width
    } # End hidden_layer_depth
  } # End type of masking



  ### Make a compressed matrix containing the results
  NN_results_compressed <- data.frame(
    masking_scheme = character(),
    hidden_layer_depth = integer(),
    hidden_layer_width = integer(),
    learning_rate = numeric(),
    best_epoch = integer(),
    best_loss = numeric(),
    save_path_best_model = character()
  )


  for (masking_scheme in masking_schemes) { # Iterate over the given masking schemes
    tmp_str_1 <- paste0("mask_", masking_scheme)
    for (hidden_layer_depth in hidden_layer_depths) { # Iterate over the given hidden layer depths
      tmp_str_2 <- paste0("depth_", hidden_layer_depth)
      for (hidden_layer_width in hidden_layer_widths) { # Iterate over the given hidden layer widths
        tmp_str_3 <- paste0("width_", hidden_layer_width)
        for (learning_rate in learning_rates) { # Iterate over the given learning rates
          tmp_str_4 <- sprintf("lr_%g", learning_rate)
          res_now <- NN_results_list[[tmp_str_1]][[tmp_str_2]][[tmp_str_3]][[tmp_str_4]]

          NN_results_compressed[nrow(NN_results_compressed) + 1, ] <-
            list(
              res_now$masking_scheme,
              res_now$hidden_layer_depth,
              res_now$hidden_layer_width,
              res_now$learning_rate,
              res_now$best_epoch,
              res_now$best_loss,
              res_now$save_path_best_model
            )
        }
      }
    }
  }
  NN_results_compressed_best <- NN_results_compressed[which.min(NN_results_compressed$best_loss), ]

  # Looked at some speeds
  # library(microbenchmark)
  # microbenchmark(
  #   {NN_results_compressed %>%
  #       filter(best_loss == min(best_loss))},
  #   {NN_results_compressed %>%
  #       filter(row_number() == which.min(best_loss))},
  #   {NN_results_compressed %>%
  #       slice(which.min(best_loss))},
  #   {NN_results_compressed %>%
  #       slice_min(best_loss)},
  #   {NN_results_compressed[which.min(NN_results_compressed$best_loss),]})


  # Create a return list
  return_list <- list(
    best_mask = NN_results_compressed_best$masking_scheme,
    best_hidden_layer_depth = NN_results_compressed_best$hidden_layer_depth,
    best_hidden_layer_width = NN_results_compressed_best$hidden_layer_width,
    best_learning_rate = NN_results_compressed_best$learning_rate,
    best_epoch = NN_results_compressed_best$best_epoch,
    best_loss = NN_results_compressed_best$best_loss,
    best_model = NN_results_compressed_best$save_path_best_model,
    best_model_device = device,
    NN_results_list = NN_results_list,
    NN_results_compressed = NN_results_compressed,
    param = list(
      training_split_ratio = training_split_ratio,
      training_split_seed = training_split_seed,
      training_split_indices = training_split_indices,
      hidden_layer_depths = hidden_layer_depths,
      hidden_layer_widths = hidden_layer_widths,
      learning_rates = learning_rates,
      n_epochs = n_epochs,
      n_networks_to_initiate = n_networks_to_initiate,
      n_initiation_epochs = n_initiation_epochs,
      n_epochs_early_stopping = n_epochs_early_stopping,
      optimizer = optimizer,
      batch_size = batch_size,
      batch_size_validation = batch_size_validation,
      masking_schemes = masking_schemes,
      masking_value = masking_value,
      col_means = col_means,
      col_standard_deviations = col_standard_deviations,
      paired_sampling = paired_sampling,
      activation_function = activation_function,
      use_skip_connections = use_skip_connections,
      use_batch_normalization = use_batch_normalization,
      use_cuda = use_cuda,
      save_NN_every_100th_epoch = save_NN_every_100th_epoch,
      test = test,
      save_folder = save_folder
    )
  )


  # Plot the training and validation error if requested by the user
  if (verbose_plot) {
    # Unlist the result list to only extract the plot data and combine it to a single tibble
    tmp <- unlist(unlist(unlist(unlist(NN_results_list, recursive = FALSE), recursive = FALSE), recursive = FALSE), recursive = FALSE)
    plot_data <- tmp[grepl("plot_data", names(tmp))]
    plot_data_comb <- dplyr::bind_rows(plot_data)
    plot_data_comb

    # Convert lr to a factor to be able to use 'interaction()' in ggplot.
    plot_data_comb$lr <- as.factor(plot_data_comb$lr)

    # New facet label names for masking variable
    mask_labs <- c("Frye masking", "MCAR masking")
    names(mask_labs) <- c("Frye", "MCAR")

    # New facet label names for hidden_layer_width variable
    hlw_labs <- c("Width = 128", "Width = 256", "Width = 512") # paste("Width =", hlw_labs)
    names(hlw_labs) <- c("128", "256", "512")

    # Filter the tibble and plot the results
    figure <-
      plot_data_comb %>%
      filter((epoch > first_epoch_to_show) & (epoch %% only_plot_epochs_divisible_by_this_value == 0)) %>%
      ggplot(aes(x = epoch, y = error, col = interaction(type, lr), shape = type, group = interaction(type, lr))) +
      geom_line() +
      facet_grid(rows = vars(hlw), cols = vars(masking_scheme), labeller = labeller(hlw = hlw_labs, masking_scheme = mask_labs[seq(length(masking_schemes))]), scales = "free_x") +
      scale_color_brewer(palette = "Paired") +
      labs(x = "Epochs", y = "Training/validation mean squared error")

    # if we are to add points to the curves
    if (verbose_plot_points) {
      figure <- figure + geom_point()
    }

    # Plot the figure
    plot(figure)

    # Add the data for the figure to the results list.
    return_list$plot_data_comb <- plot_data_comb
    return_list$figure <- figure
  }

  # Return the results
  return(return_list)
}



# NN_first_time_calling = NN(training, n_epochs = 25)
#
#
# NN_first_time_calling$figure +
#   xlim(10, 100) +
#   ylim(2, 15)
#
#
# NN_first_time_calling
#
# NN_model = torch::torch_load(NN_first_time_calling$best_model, device = NN_first_time_calling$param$device)





# Explain Functions used in rshapr --------------------------------------------------------------------------------------------------------------




#' Title
#'
#' Internal function that should be called by explain_regression().
#' This function computes the Shapley values for the test observations
#' using the NN method. Fit a single model to all coalitions.
#' See 'NN()' for explanation of the other parameters.
#'
#' @param explainer  An explainer object to use for explaining the observations. See ?shapr and explain_regression().
#' @param training_split_ratio
#' @param training_split_seed
#' @param training_split_indices
#' @param hidden_layer_depths
#' @param hidden_layer_widths
#' @param learning_rates
#' @param n_epochs
#' @param optimizer
#' @param batch_size
#' @param batch_size_validation
#' @param masking_schemes
#' @param masking_value
#' @param activation_function
#' @param use_cuda
#' @param test
#' @param save_folder
#' @param verbose
#' @param verbose_plot
#' @param verbose_plot_points
#' @param first_epoch_to_show
#' @param only_plot_epochs_divisible_by_this_value
#' @param ...
#' @param normalize_features
#' @param paired_sampling
#' @param extra_save_name
#' @param concatenate_mask
#' @param use_skip_connections
#' @param use_batch_normalization
#'
#' @return
#' @export
#'
#' @examples
explain_regression_internal_combined.NN <- function(explainer,
                                                    normalize_features = TRUE, # Not properly tested yet
                                                    scale_response = FALSE,
                                                    training_split_ratio = 0.75,
                                                    training_split_seed = NULL,
                                                    training_split_indices = NULL,
                                                    hidden_layer_depths = 2,
                                                    hidden_layer_widths = c(128, 256, 512),
                                                    learning_rates = c(0.01, 0.001, 0.0001), # c(0.01, 0.001, 0.0001)
                                                    n_epochs = 10^4,
                                                    n_networks_to_initiate = 10,
                                                    n_initiation_epochs = 20,
                                                    n_epochs_early_stopping = 150,
                                                    optimizer = "adam",
                                                    batch_size = 256,
                                                    batch_size_validation = 64,
                                                    masking_schemes = c("Frye", "MCAR"),
                                                    masking_value = -10,
                                                    concatenate_mask = TRUE,
                                                    paired_sampling = TRUE,
                                                    use_skip_connections = TRUE,
                                                    use_batch_normalization = TRUE,
                                                    activation_function = nn_relu(),
                                                    use_cuda = TRUE,
                                                    test = NULL,
                                                    save_folder = "/Users/larsolsen/PhD/Paper2/Simulations/NN_models/",
                                                    extra_save_name = "",
                                                    save_NN_every_100th_epoch = TRUE,
                                                    verbose = TRUE,
                                                    verbose_plot = TRUE,
                                                    verbose_plot_points = FALSE,
                                                    first_epoch_to_show = 5,
                                                    only_plot_epochs_divisible_by_this_value = 10,
                                                    ...) {
  # TODO: ADD SUPPORT FOR CATEGORICAL DATA

  # Skal ikke være her. Den må være der vi deler dataene.
  # Bruk treningsdataene til å finne mean og std, og så bruk disse verdiene på
  # kolonnene i validation and test data.
  if (normalize_features) {
    cat(sprintf("Normalized the features in the NN approach.\n"))

    # MAYBE I DONT NEED TO THINK ABOUT CATEGORICAL DATA HERE.
    # BECAUSE, IF WE HAVE CATEGORICAL DATA, THEN IT SHOULD HAVE BEEN
    # ONE-HOT-ENCODED IN THE 'explain_regression' FUNCTION.
    # MEANING THAT WE SHOULD NEVER SEE A CATEGORICAL VARIABLE IN THIS FUNCTION!

    # Get the numerical columns
    # one_hot_max_sizes = as.integer(unname(sapply(explainer$feature_list$factor_levels, length)))
    # one_hot_max_sizes[one_hot_max_sizes == 0] = 1
    #
    # cont_cols = one_hot_max_sizes == 1

    if (any(explainer$feature_list$classes == "factor")) {
      # New support for cat
      cont_cols <- colnames(explainer$x_train)
      col_names <- cont_cols
    } else {
      # Old before support for cat data
      cont_cols <- explainer$feature_list$classes == "numeric"
      col_names <- names(cont_cols)
    }

    # Extract the training set
    x_train <- explainer$x_train

    # compute the column means and standard deviations needed to scale the data
    col_means <- apply(x_train[, ..cont_cols], 2, mean)
    col_standard_deviations <- apply(x_train[, ..cont_cols], 2, sd)

    # Subtract the means and divide by the standard deviations
    # aux1 = sweep(explainer$x_train[,..cont_cols], 2,  col_means)
    aux1 <- data.table(mapply(`-`, x_train[, ..cont_cols], col_means))
    aux2 <- data.table(mapply(`/`, aux1, col_standard_deviations))
    # apply(aux2, 2, mean)
    # apply(aux2, 2, sd)

    # Combine the training data into the same order as before
    x_train <- as.matrix(cbind(aux2, x_train[, !..cont_cols])[, ..col_names])

    # Do the same to the test data
    x_test <- explainer$x_test
    aux1 <- data.table(mapply(`-`, x_test[, ..cont_cols], col_means))
    aux2 <- data.table(mapply(`/`, aux1, col_standard_deviations))
    x_test <- as.matrix(cbind(aux2, x_test[, !..cont_cols])[, ..col_names])
  } else {
    # Must be updated if we include categorical variables
    x_train <- as.matrix(explainer$x_train)
    x_test <- as.matrix(explainer$x_test)
  }

  # Scale response to be between -1 and 1.
  if (scale_response) {
    cat(sprintf("Scaled the responses in the NN approach.\n"))
    explainer$largest_absolute_value <- max(abs(explainer$y_train_hat))
    # hist(explainer$y_train_hat / explainer$largest_absolute_value, breaks = 100)
  } else {
    explainer$largest_absolute_value <- 1
  }

  # Create the list containing the training features and the response.
  # Note that the response is the prediction of the predictive model f,
  # which we are to explain.
  # AND NOT THE ACTUAL RESPONSE THE MODEL WAS TRAINDED ON.
  training_list <- list(x = x_train, y = as.matrix(explainer$y_train_hat / explainer$largest_absolute_value)) # as.matrix WILL NOT WORK FOR CATEGORICAL DATA.

  explainer$time_surrogate_training_model <- system.time(
    {
      # First we need to fit the NN.
      NN_return_list <- NN(
        training = training_list,
        training_split_ratio = training_split_ratio,
        training_split_seed = training_split_seed,
        training_split_indices = training_split_indices,
        hidden_layer_depths = hidden_layer_depths,
        hidden_layer_widths = hidden_layer_widths,
        learning_rates = learning_rates,
        n_epochs = n_epochs,
        n_networks_to_initiate = n_networks_to_initiate,
        n_initiation_epochs = n_initiation_epochs,
        n_epochs_early_stopping = n_epochs_early_stopping,
        optimizer = optimizer,
        batch_size = batch_size,
        batch_size_validation = batch_size_validation,
        masking_schemes = masking_schemes,
        masking_value = masking_value,
        concatenate_mask = concatenate_mask,
        use_skip_connections = use_skip_connections,
        use_batch_normalization = use_batch_normalization,
        paired_sampling = paired_sampling,
        activation_function = activation_function,
        use_cuda = use_cuda,
        col_means = col_means,
        col_standard_deviations = col_standard_deviations,
        test = test,
        save_folder = save_folder,
        save_NN_every_100th_epoch = save_NN_every_100th_epoch,
        verbose = verbose,
        verbose_plot = verbose_plot,
        verbose_plot_points = verbose_plot_points,
        first_epoch_to_show = first_epoch_to_show,
        only_plot_epochs_divisible_by_this_value = only_plot_epochs_divisible_by_this_value,
        extra_save_name = extra_save_name,
        explainer = explainer
      )
    },
    gcFirst = FALSE
  )

  # Add the values to the return list
  NN_return_list[["normalize_features"]] <- normalize_features
  NN_return_list[["scale_response"]] <- scale_response
  NN_return_list[["largest_absolute_value"]] <- explainer$largest_absolute_value

  # load the best model
  NN_model <- torch_load(NN_return_list$best_model, device = NN_return_list$best_model_device)

  # Extend the active coalitions for categorical one hot encoded data
  # So if we have three features. cont cont cat(2). Then we add two 1
  # for when cat is known and two 0 when it is unknown.
  # n_levels = unname(sapply(explainer$feature_list$factor_levels, length))
  # n_levels[n_levels == 0] = 1
  n_levels <- explainer$x_train_attribute
  explainer$active_coalitions_extended <- t(sapply(seq(nrow(explainer$active_coalitions)), function(current_row_idx) rep(explainer$active_coalitions[current_row_idx, ], times = n_levels)))

  if (nrow(x_train) <= 5000 & ncol(x_train) < 10) {
    # Cannot handle to big datasets due to memmory

    # Augment the training data
    explainer$time_surrogate_make_augmented_train_data <- system.time(
      {
        data_train_augmented <- augment_data_by_masking_NN(
          X = x_train,
          y = explainer$y_train_hat, # Not scaled version on purpose
          S = explainer$active_coalitions_extended,
          weights_S = explainer$active_coalitions_weights,
          include_weights = FALSE,
          masks_as_factor = FALSE,
          one_hot_encoding = explainer$one_hot_encoding,
          masking_value = masking_value
        )
      },
      gcFirst = FALSE
    )

    # Convert to torch tensor
    n_features_with_one_hot <- sum(n_levels)
    cols_var <- seq(if (concatenate_mask) 2 * n_features_with_one_hot else n_features_with_one_hot)
    train_augmented_x <- torch_tensor(as.matrix(data_train_augmented[, ..cols_var]))
    train_augmented_y <- torch_tensor(as.matrix(data_train_augmented[[ncol(data_train_augmented)]]))

    # Compute the predicted output of the training data and the mse loss
    NN_model$eval()
    output_train <- NN_model(train_augmented_x) * explainer$largest_absolute_value # Multiply with largest value to get back to original scale
    explainer$train_mse_loss <- nnf_mse_loss(output_train, train_augmented_y)$item()

    # Compute the train root mean squared error.
    explainer$rmse_train <- sqrt(rowMeans(matrix((as.matrix(output_train) - data_train_augmented$y)^2,
      nrow = explainer$n_coalitions - 2, ncol = explainer$n_train, byrow = TRUE
    )))
  } else {
    # Not enough memory. Skip computing the training MSE
    n_features_with_one_hot <- sum(n_levels)
    cols_var <- seq(if (concatenate_mask) 2 * n_features_with_one_hot else n_features_with_one_hot)
    NN_model$eval()
    explainer$train_mse_loss <- NULL
    explainer$rmse_train <- NULL
  }

  print("Augmenting the test data...")
  explainer$time_surrogate_make_augmented_test_data <- system.time(
    {
      # Augment the test data
      data_test_augmented <- augment_data_by_masking_NN(
        X = x_test,
        y = explainer$v_contribution_function_matrix[explainer$n_coalitions, ], # original scale on purpose
        S = explainer$active_coalitions_extended,
        weights_S = explainer$active_coalitions_weights,
        include_weights = FALSE,
        masks_as_factor = FALSE,
        one_hot_encoding = explainer$one_hot_encoding,
        masking_value = masking_value
      )
      test_augmented_x <- torch_tensor(as.matrix(data_test_augmented[, ..cols_var]))
      test_augmented_y <- torch_tensor(as.matrix(data_test_augmented[[ncol(data_test_augmented)]]))
    },
    gcFirst = FALSE
  )

  print("Predicting the test data...")
  explainer$time_surrogate_predicting_cont_funcs <- system.time(
    {
      # Compute the predicted output of the test data
      output_test <- NN_model(test_augmented_x) * explainer$largest_absolute_value # Multiply with largest value to get back to original scale

      # Compute the predicted conditional means and insert them into the contribution function
      explainer$v_contribution_function_matrix[2:(explainer$n_coalitions - 1), ] <-
        matrix(as.matrix(output_test), nrow = nrow(explainer$S) - 2, ncol = explainer$n, byrow = TRUE)
    },
    gcFirst = FALSE
  )

  # Compute the predicted output of the test mse loss
  explainer$test_mse_loss <- nnf_mse_loss(output_test, test_augmented_y)$item()

  print("Returning the results...")

  # Compute Shapley values and return a list of same stuff as shapr:::explain and time
  return_list <- c(
    explain_regression_compute_Shapley_internal(explainer),
    list(NN_return_list = NN_return_list),
    list(
      time_surrogate_predicting_empty_coalition_cont_func = explainer$time_surrogate_predicting_empty_coalition_cont_func,
      time_surrogate_predicting_full_coalition_cont_func = explainer$time_surrogate_predicting_full_coalition_cont_func,
      time_surrogate_make_augmented_train_data = explainer$time_surrogate_make_augmented_train_data,
      time_surrogate_make_augmented_test_data = explainer$time_surrogate_make_augmented_test_data,
      time_surrogate_training_model = explainer$time_surrogate_training_model,
      time_surrogate_predicting_cont_funcs = explainer$time_surrogate_predicting_cont_funcs
    )
  )
  # Set the class of the return list. This is needed such that we can
  # e.g. call the generic plot() function, which will then call the
  # shapr:::plot.shapr() function.
  attr(return_list, "class") <- c("shapr", "list")
  return(return_list)

  # # Compute Shapley values and return a list of same stuff as shapr:::explain.
  # return(c(list(NN_return_list = NN_return_list), explain_regression_compute_Shapley_internal(explainer)))
}


explain_regression_internal.NN <- function(...) {
  warning("The 'NN' approach is a combined approach. This function do and return nothing.")
}




# Explain several NN checkpoints --------------------------------------------------------------

explain_regression <- function(x,
                               explainer,
                               approach,
                               prediction_zero,
                               combined = FALSE,
                               return_explainer = FALSE,
                               return_model = FALSE,
                               masks_as_factor = TRUE,
                               one_hot_encoding = FALSE,
                               verbose = FALSE,
                               ...) {
  #' Title
  #'
  #' Internal function that should be called by explain_regression().
  #' This function computes the Shapley values for the test observations
  #' using the NN method. Fit a single model to all coalitions.
  #' See 'NN()' for explanation of the other parameters.
  #'
  #' @param explainer  An explainer object to use for explaining the observations. See ?shapr and explain_regression().
  #' @param training_split_ratio
  #' @param training_split_seed
  #' @param training_split_indices
  #' @param hidden_layer_depths
  #' @param hidden_layer_widths
  #' @param learning_rates
  #' @param n_epochs
  #' @param optimizer
  #' @param batch_size
  #' @param batch_size_validation
  #' @param masking_schemes
  #' @param masking_value
  #' @param activation_function
  #' @param use_cuda
  #' @param test
  #' @param save_folder
  #' @param verbose
  #' @param verbose_plot
  #' @param verbose_plot_points
  #' @param first_epoch_to_show
  #' @param only_plot_epochs_divisible_by_this_value
  #' @param ...
  #' @param normalize_features
  #' @param paired_sampling
  #' @param extra_save_name
  #' @param concatenate_mask
  #' @param use_skip_connections
  #' @param use_batch_normalization
  #'
  #' @return
  #' @export
  #'
  #' @examples
  explain_regression_internal_combined.NN_without_training <- function( # x,
                                                                       explainer,
                                                                       earlier_explainer,
                                                                       # model_save_path,
                                                                       # prediction_zero,
                                                                       # verbose = FALSE,
                                                                       # normalize_features = TRUE, # Not properly tested yet
                                                                       # scale_response = FALSE,
                                                                       # training_split_ratio = 0.75,
                                                                       # training_split_seed = NULL,
                                                                       # training_split_indices = NULL,
                                                                       # hidden_layer_depths = 2,
                                                                       # hidden_layer_widths = c(128, 256, 512),
                                                                       # learning_rates = c(0.01, 0.001, 0.0001), #c(0.01, 0.001, 0.0001)
                                                                       # n_epochs = 10^4,
                                                                       # optimizer = "adam",
                                                                       # batch_size = 256,
                                                                       # batch_size_validation = 64,
                                                                       # masking_schemes = c("Frye", "MCAR"),
                                                                       # masking_value = -10,
                                                                       # concatenate_mask = TRUE,
                                                                       # paired_sampling = TRUE,
                                                                       # use_skip_connections = TRUE,
                                                                       # use_batch_normalization = TRUE,
                                                                       # activation_function = nn_relu(),
                                                                       # use_cuda = TRUE,
                                                                       # test = NULL,
                                                                       # save_folder = "/Users/larsolsen/PhD/Paper2/Simulations/NN_models/",
                                                                       # extra_save_name = "",
                                                                       # save_NN_every_100th_epoch = TRUE,
                                                                       # verbose = TRUE,
                                                                       # verbose_plot = TRUE,
                                                                       # verbose_plot_points = FALSE,
                                                                       # first_epoch_to_show = 5,
                                                                       # only_plot_epochs_divisible_by_this_value = 10,
                                                                       ...) {
    x <- earlier_explainer$x_test

    verbose <- TRUE


    normalize_features <- earlier_explainer$NN_return_list$normalize_features
    scale_response <- earlier_explainer$NN_return_list$scale_response

    # If we are to one-hot encode the data
    explainer$one_hot_encoding <- TRUE
    # If the masks are going to be treated as factors
    explainer$masks_as_factor <- FALSE

    # If we are to print out how far the individual approaches have come
    explainer$verbose <- verbose

    explainer$return_explainer <- TRUE

    # Get the predicted response of the training data for the model we want to explain.
    # These values will be used as the response when we train the regression models
    # which aims to predict the conditional contribution functions for each coalition.
    explainer$y_train_hat <- as.matrix(shapr::predict_model(explainer$model, explainer$x_train))
    colnames(explainer$y_train_hat) <- "y_train_hat"

    # Check x for correct column names, convert it to data.table and add it to explainer object.
    explainer$x_test <- shapr:::preprocess_data(x, explainer$feature_list)$x_dt

    # Get the number of features and number of test observations
    explainer$p <- ncol(x)
    explainer$n <- nrow(x)
    explainer$n_train <- nrow(explainer$x_train)

    # Get the number of coalitions including the empty and grand coalition
    # Used when we sample coalitions, when n_coalitions is not equal to 2^p.
    explainer$n_coalitions <- nrow(explainer$S)

    # Create a matrix to store the estimated v(s) for all S and all test observations.
    explainer$v_contribution_function_matrix <- matrix(NA, nrow = explainer$n_coalitions, ncol = explainer$n)

    # Variables to store the times related to the surrogate approaches
    explainer$time_surrogate_make_augmented_train_data <- NULL
    explainer$time_surrogate_make_augmented_test_data <- NULL
    explainer$time_surrogate_training_model <- NULL
    explainer$time_surrogate_predicting_cont_funcs <- NULL
    explainer$time_surrogate_predicting_empty_coalition_cont_func <- NULL
    explainer$time_surrogate_predicting_full_coalition_cont_func <- NULL

    # Variable to store the time it took to do the matrix multiplication.
    explainer$time_Shapley_value_matrix_multiplication <- NULL

    # First row corresponds to coalition S being the empty set.
    # I.e., all features are unknown and we use the provided prediction_zero.
    explainer$time_surrogate_predicting_empty_coalition_cont_func <- system.time(
      {
        explainer$v_contribution_function_matrix[1, ] <- earlier_explainer$dt$none[1]
      },
      gcFirst = FALSE
    )

    # Last row corresponds to coalition S being the set of all features.
    # I.e., we know all the features and can just call the original model.
    explainer$time_surrogate_predicting_full_coalition_cont_func <- system.time(
      {
        explainer$v_contribution_function_matrix[explainer$n_coalitions, ] <- shapr:::predict_model(explainer$model, newdata = explainer$x_test)
      },
      gcFirst = FALSE
    )

    # Vector to store the train root mean squared error.
    explainer$rmse_train <- rep(NA, explainer$n_coalitions)

    # Vector to store the test root mean squared error.
    explainer$rmse_test <- rep(NA, explainer$n_coalitions)

    explainer$return_model <- FALSE

    # Save if we treat the coalitions as combined or individually.
    explainer$combined <- TRUE

    # Extract relevant information needed augment the training and test data with the masks
    explainer$active_coalitions <- explainer$S[2:(nrow(explainer$S) - 1), ]
    explainer$active_coalitions_weights <- explainer$X$shapley_weight[2:(length(explainer$X$shapley_weight) - 1)]

    # CURRENTLY I WANT THE COMBINED MODEL TO LEARN ALL COALITIONS EQUALLY.
    # CHANGE THIS LATER!
    if (explainer$exact) {
      explainer$active_coalitions_weights <- rep(1, length(explainer$active_coalitions_weights))
    }

    # Added 30 jan 2023
    if (explainer$one_hot_encoding) {
      tmp_train <- model.matrix(~ . - 1, explainer$x_train)
      tmp_test <- model.matrix(~ . - 1, explainer$x_test)
      explainer$x_train_attribute_assign <- table(attr(tmp_train, "assign"))
      explainer$x_test_attribute_assign <- table(attr(tmp_test, "assign"))
      explainer$x_train <- as.data.table(tmp_train)
      explainer$x_test <- as.data.table(tmp_test)
    }


    # Skal ikke være her. Den må være der vi deler dataene.
    # Bruk treningsdataene til å finne mean og std, og så bruk disse verdiene på
    # kolonnene i validation and test data.
    if (normalize_features) {
      cat(sprintf("Normalized the features in the NN approach. NOT THE RESPONSE.\n"))

      # MAYBE I DONT NEED TO THINK ABOUT CATEGORICAL DATA HERE.
      # BECAUSE, IF WE HAVE CATEGORICAL DATA, THEN IT SHOULD HAVE BEEN
      # ONE-HOT-ENCODED IN THE 'explain_regression' FUNCTION.
      # MEANING THAT WE SHOULD NEVER SEE A CATEGORICAL VARIABLE IN THIS FUNCTION!

      # Get the numerical columns
      # one_hot_max_sizes = as.integer(unname(sapply(explainer$feature_list$factor_levels, length)))
      # one_hot_max_sizes[one_hot_max_sizes == 0] = 1
      #
      # cont_cols = one_hot_max_sizes == 1

      if (any(explainer$feature_list$classes == "factor")) {
        # New support for cat
        cont_cols <- colnames(explainer$x_train)
        col_names <- cont_cols
      } else {
        # Old before support for cat data
        cont_cols <- explainer$feature_list$classes == "numeric"
        col_names <- names(cont_cols)
      }

      # Extract the training set
      x_train <- explainer$x_train

      # compute the column means and standard deviations needed to scale the data
      col_means <- apply(x_train[, ..cont_cols], 2, mean)
      col_standard_deviations <- apply(x_train[, ..cont_cols], 2, sd)

      # Subtract the means and divide by the standard deviations
      # aux1 = sweep(explainer$x_train[,..cont_cols], 2,  col_means)
      aux1 <- data.table(mapply(`-`, x_train[, ..cont_cols], col_means))
      aux2 <- data.table(mapply(`/`, aux1, col_standard_deviations))
      # apply(aux2, 2, mean)
      # apply(aux2, 2, sd)

      # Combine the training data into the same order as before
      x_train <- as.matrix(cbind(aux2, x_train[, !..cont_cols])[, ..col_names])

      # Do the same to the test data
      x_test <- explainer$x_test
      aux1 <- data.table(mapply(`-`, x_test[, ..cont_cols], col_means))
      aux2 <- data.table(mapply(`/`, aux1, col_standard_deviations))
      x_test <- as.matrix(cbind(aux2, x_test[, !..cont_cols])[, ..col_names])
    } else {
      # Must be updated if we include categorical variables
      x_train <- as.matrix(explainer$x_train)
      x_test <- as.matrix(explainer$x_test)
    }

    # Create the list containing the training features and the response.
    # Note that the response is the prediction of the predictive model f,
    # which we are to explain.
    # AND NOT THE ACTUAL RESPONSE THE MODEL WAS TRAINDED ON.
    training_list <- list(x = x_train, y = as.matrix(explainer$y_train_hat)) # as.matrix WILL NOT WORK FOR CATEGORICAL DATA.

    print("Augmenting the test data...")
    explainer$time_surrogate_make_augmented_test_data <- system.time(
      {
        # Augment the test data
        data_test_augmented <- augment_data_by_masking_NN(
          X = x_test,
          y = explainer$v_contribution_function_matrix[explainer$n_coalitions, ],
          S = explainer$active_coalitions_extended,
          weights_S = explainer$active_coalitions_weights,
          include_weights = FALSE,
          masks_as_factor = FALSE,
          one_hot_encoding = explainer$one_hot_encoding,
          masking_value = earlier_explainer$NN_return_list$param$masking_value
        )
        test_augmented_x <- torch_tensor(as.matrix(data_test_augmented[, ..cols_var]))
        test_augmented_y <- torch_tensor(as.matrix(data_test_augmented[[ncol(data_test_augmented)]]))
      },
      gcFirst = FALSE
    )


    NN_results_list <- list()

    NN_save_paths <- earlier_explainer$NN_return_list$NN_results_list[[1]][[1]][[1]][[1]]$list_save_names_every_100th_epoch


    NN_save_path_idx <- 1
    for (NN_save_path_idx in seq(length(NN_save_paths))) {
      NN_save_path_current <- NN_save_paths[[NN_save_path_idx]]

      NN_results_list[[NN_save_path_current]] <- explainer

      # load the best model
      NN_model <- torch_load(NN_save_path_current)

      print("Predicting the test data...")
      # Compute the predicted output of the test data
      output_test <- NN_model(test_augmented_x)

      # Compute the predicted conditional means and insert them into the contribution function
      explainer$v_contribution_function_matrix[2:(explainer$n_coalitions - 1), ] <-
        matrix(as.matrix(output_test), nrow = nrow(explainer$S) - 2, ncol = explainer$n, byrow = TRUE)

      # Compute the predicted output of the test mse loss
      explainer$test_mse_loss <- nnf_mse_loss(output_test, test_augmented_y)$item()

      print("Returning the results...")
      # Compute Shapley values and return a list of same stuff as shapr:::explain and time
      NN_results_list[[NN_save_path_idx]] <- c(explain_regression_compute_Shapley_internal(explainer))

      print(NN_results_list[[NN_save_path_idx]]$mse_frye)
    }

    # Set the class of the return list. This is needed such that we can
    # e.g. call the generic plot() function, which will then call the
    # shapr:::plot.shapr() function.
    # attr(return_list, "class") = c("shapr", "list")
    return(NN_results_list)
    # # Compute Shapley values and return a list of same stuff as shapr:::explain.
    # return(c(list(NN_return_list = NN_return_list), explain_regression_compute_Shapley_internal(explainer)))
  }
}
