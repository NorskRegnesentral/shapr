# Round numbers to the specified number of decimal places

This function rounds numbers to the specified number of decimal places
using a manual method that avoids the typical rounding issues in R which
may vary across different OS.

## Usage

``` r
round_manual(x, digits = 0L)
```

## Arguments

- x:

  Numeric vector. The numbers to round.

- digits:

  Integer. The number of digits to round to. Defaults 0.

## Value

Numeric vector. The rounded numbers.
