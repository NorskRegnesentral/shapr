#' Helper function to sample a combination of training and testing rows, which does not risk
#' getting the same observation twice. Need to improve this help file.
#'
#' @inheritParams global_arguments
#'
#' @param nTrain Positive integer. Number of training observations to sample from.
#'
#' @param nTest Positive integer. Number of test observations to sample from.
#'
#' @param nosamp Positive integer. Number of samples.
#'
#' @param separate Logical indicating whether the train and test data should be sampled separately
#' or in a joint sampling space. If they are sampled separately (which typically would be used when
#' optimizing more than one distribution at once) we sample with replacement if more samples than
#' training data. Not optimal, but for now fine if careful when using more samples than the number
#' training observations while at the same time doing optimization over every test observation.
#'
#' @return Data.frame. Contains \code{nosamp} rows of re-sampled train and test observations.
#'
#' @export
#'
#' @author Martin Jullum
sample_combinations <- function(nTrain, nTest, nosamp, separate = F) {

  if (separate) {

    # Sample training data
    samp_train <- sample(
      x = nTrain,
      size = nosamp,
      replace = ifelse(nosamp < nTrain, FALSE, TRUE)
    )

    # Sample test data
    samp_test <- sample(
      x = nTest,
      size = nosamp,
      replace = ifelse(nosamp < nTrain, nosamp > nTest, TRUE)
    )
  } else {

    n <- nTrain * nTest
    if (nosamp < n) {
      input_samp <- sample(
        x = n,
        size = nosamp,
        replace = FALSE
      )
    } else {
      input_samp <- seq(n)
    }

    samp_train <- (input_samp - 1) %% nTrain + 1
    samp_test <- (input_samp - 1) %/% nTrain + 1
  }
  ret <- data.frame(samp_train = samp_train, samp_test = samp_test)

  return(ret)
}
