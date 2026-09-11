# Helper function to sample a combination of training and testing rows, which does not risk getting the same observation twice. Need to improve this help file.

Helper function to sample a combination of training and testing rows,
which does not risk getting the same observation twice. Need to improve
this help file.

## Usage

``` r
sample_combinations(ntrain, ntest, nsamples, joint_sampling = TRUE)
```

## Arguments

- ntrain:

  Positive integer. Number of training observations to sample from.

- ntest:

  Positive integer. Number of test observations to sample from.

- nsamples:

  Positive integer. Number of samples.

- joint_sampling:

  Logical. Indicates whether train- and test data should be sampled
  separately or in a joint sampling space. If they are sampled
  separately (which typically would be used when optimizing more than
  one distribution at once) we sample with replacement if
  `nsamples > ntrain`. Note that this solution is not optimal. Be
  careful if you're doing optimization over every test observation when
  `nsamples > ntrain`.

## Value

data.frame

## Author

Martin Jullum
