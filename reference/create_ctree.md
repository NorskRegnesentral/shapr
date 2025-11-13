# Build all the conditional inference trees

Build all the conditional inference trees

## Usage

``` r
create_ctree(
  given_ind,
  x_train,
  mincriterion,
  minsplit,
  minbucket,
  use_partykit = "on_error"
)
```

## Arguments

- given_ind:

  Integer vector. Indicates which features are conditioned on.

- x_train:

  Data.table with training data.

- use_partykit:

  String. In some semi-rare cases
  [`party::ctree()`](https://rdrr.io/pkg/party/man/ctree.html) runs into
  an error related to the LINPACK used by R. To get around this problem,
  one may fall back to using the newer (but slower)
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html)
  function, which is a reimplementation of the same method. Setting this
  parameter to `"on_error"` (default) falls back to
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html), if
  [`party::ctree()`](https://rdrr.io/pkg/party/man/ctree.html) fails.
  Other options are `"never"`, which always uses
  [`party::ctree()`](https://rdrr.io/pkg/party/man/ctree.html), and
  `"always"`, which always uses
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html). A
  warning message is created whenever
  [`partykit::ctree()`](https://rdrr.io/pkg/partykit/man/ctree.html) is
  used.

## Value

List with conditional inference tree and the variables conditioned/not
conditioned on.

## Details

See the documentation of the
[`setup_approach.ctree()`](https://norskregnesentral.github.io/shapr/reference/setup_approach.md)
function for undocumented parameters.

## Author

Annabelle Redelmeier, Martin Jullum
