# Set up user provided groups for explanation in a forecast model.

Set up user provided groups for explanation in a forecast model.

## Usage

``` r
group_forecast_setup(group, horizon_features)
```

## Arguments

- group:

  The list of groups to be explained.

- horizon_features:

  A list of features per horizon, to split appropriate groups over.

## Value

A list containing

- group The list group with entries that differ per horizon split
  accordingly.

- horizon_group A list of which groups are applicable per horizon.
