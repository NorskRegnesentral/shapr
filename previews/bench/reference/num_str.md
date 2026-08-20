# Convert a character to a numeric class

To be used in cli calls like
`cli::cli_text("{.val {shapr:::num_str('12.10')}}")` to format character
strings that typically represent a number as if it were numeric. May
also be used with strings not representing a number.

## Usage

``` r
num_str(x)
```

## Arguments

- x:

  Character. A single character that represents a number, or a vector of
  characters.

## Value

A numeric class object with the value of the string.
