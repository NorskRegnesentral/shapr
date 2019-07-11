#' Helper function to sample a combination of training and testing rows, which does not risk
#' getting the same observation twice. Need to improve this help file.
#'
#' @inheritParams global_arguments
#'
#' @param ntrain Positive integer. Number of training observations to sample from.
#'
#' @param ntest Positive integer. Number of test observations to sample from.
#'
#' @param nsamples Positive integer. Number of samples.
#'
#' @param joint_sampling Logical. Indicates whether train- and test data should be sampled
#' separately or in a joint sampling space. If they are sampled separately (which typically
#' would be used when optimizing more than one distribution at once) we sample with replacement
#' if \code{nsamples > ntrain}. Note that this solution is not optimal. Be careful if you're
#' doing optimization over every test observation when \code{nsamples > ntrain}.
#'
#'
#' @return Data.frame. Contains \code{nsamples} rows of re-sampled train and test observations.
#'
#' @export
#'
#' @author Martin Jullum
sample_combinations <- function(ntrain, ntest, nsamples, joint_sampling = TRUE) {

  if (!joint_sampling) {

    # Sample training data
    samp_train <- sample(
      x = ntrain,
      size = nsamples,
      replace = ifelse(nsamples < ntrain, FALSE, TRUE)
    )

    # Sample test data
    samp_test <- sample(
      x = ntest,
      size = nsamples,
      replace = ifelse(nsamples < ntrain, nsamples > ntest, TRUE)
    )
  } else {

    n <- ntrain * ntest
    if (nsamples < n) {
      input_samp <- sample(
        x = n,
        size = nsamples,
        replace = FALSE
      )
    } else {
      input_samp <- seq(n)
    }

    samp_train <- (input_samp - 1) %% ntrain + 1
    samp_test <- (input_samp - 1) %/% ntrain + 1
  }
  ret <- data.frame(samp_train = samp_train, samp_test = samp_test)

  return(ret)
}
