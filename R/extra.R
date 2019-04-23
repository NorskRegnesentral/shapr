#' Helper function to sample a combination of training and testing rows, which does not risk
#' getting the same observation twice. Need to improve this help file.
#'
#' @inheritParams global_arguments
#' @param separate Logical indicating whether the train and test data should be sampled separately
#' or in a joint sampling space. If they are sampled separately (which typically would be used when
#' optimizing more than one distribution at once) we sample with replacement if more samples than
#' training data. Not optimal, but for now fine if careful when using more samples than the number
#' training observations while at the same time doing optimization over every test observation.
#'
#' @return Numeric
#'
#' @export
#'
#' @author Martin Jullum
sample_combinations <- function(n_train, n_test, nosamp, separate = F) {
  if (separate) {
    # With separate sampling, we do sampling with replacement if nosamp is larger than n_train
    sampinds_train <- 1:n_train
    sampinds_test <- 1:n_test
    if (nosamp < n_train) {
      # Not optimal in general, but works for the current purpose. test data is always sampled,
      # while only reducing if the training data goes above nosamp.
      samp_train <- sample(
        x = sampinds_train,
        size = nosamp,
        replace = F
      )
      samp_test <- sample(
        x = sampinds_test,
        size = nosamp,
        replace = nosamp > length(sampinds_test)
      )
    } else {
      samp_train <- sample(
        x = sampinds_train,
        size = nosamp,
        replace = T
      )
      samp_test <- sample(
        x = sampinds_test,
        size = nosamp,
        replace = T
      )
    }
  } else {
    sampinds <- 1:(n_train * n_test)
    if (nosamp < max(sampinds)) {
      input_samp <- sample(
        x = sampinds,
        size = nosamp,
        replace = F
      )
    } else {
      input_samp <- sampinds
    }

    samp_train <- (input_samp - 1) %% n_train + 1
    samp_test <- (input_samp - 1) %/% n_train + 1
  }

  ret <- data.frame(samp_train = samp_train, samp_test = samp_test)
  return(ret)
}
