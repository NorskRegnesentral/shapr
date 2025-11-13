# Calculate weight matrix

Calculate weight matrix

## Usage

``` r
weight_matrix_cpp(coalitions, m, n, w)
```

## Arguments

- coalitions:

  List. Each of the elements equals an integer vector representing a
  valid combination of features/feature groups.

- m:

  Integer. Number of features/feature groups.

- n:

  Integer. Number of combinations.

- w:

  Numeric vector Should have length `n`. `w[i]` equals the Shapley
  weight of feature/feature group combination `i`, represented by
  `coalitions[[i]]`.

## Value

Matrix of dimension n x m + 1

## Author

Nikolai Sellereite, Martin Jullum
