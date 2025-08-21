# output_lm_numeric_independence

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_independence_MSEv_Shapley_weights

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Month    Day
              <int> <num>   <num>   <num>  <num>    <num>  <num>
      1:          1 42.44 -13.252  15.541 12.826 -5.77179  3.259
      2:          2 42.44   2.758  -3.325 -7.992 -7.12800  1.808
      3:          3 42.44   6.805 -22.126  3.730 -0.09235 -5.885

# output_lm_numeric_empirical_n_coalitions

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 20 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month    Day
              <int> <num>   <num>   <num>  <num>   <num>  <num>
      1:          1 42.44 -13.143  16.584 13.624 -6.3475  1.884
      2:          2 42.44   3.044  -4.511 -8.918 -6.1276  2.632
      3:          3 42.44   5.599 -23.352  4.228 -0.8872 -3.156

# output_lm_numeric_empirical_n_coal_unpaired_on_N

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 20 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44 -14.780  14.745 13.447 -3.975  3.164
      2:          2 42.44   4.254  -3.284 -7.536 -9.798  2.485
      3:          3 42.44   4.144 -20.306  2.532  3.106 -7.043

# output_lm_numeric_empirical_independence

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Warning:
      ! Using empirical.type = 'independence' for approach = 'empirical' is deprecated.
        Please use approach = 'independence' instead.
    Message
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
      i Setting `empirical.eta` to 1 because `empirical.type = 'independence'`.
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical_AICc_each

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 8 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month      Day
              <int> <num>   <num>   <num>  <num>   <num>    <num>
      1:          1 42.44  -9.778   1.983 16.038   1.983  2.37679
      2:          2 42.44   6.833  -5.547 -9.636  -5.547  0.01837
      3:          3 42.44   6.895 -11.847  3.643 -11.847 -4.41122

# output_lm_numeric_empirical_AICc_full

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 8 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month      Day
              <int> <num>   <num>   <num>  <num>   <num>    <num>
      1:          1 42.44  -9.778   1.983 16.038   1.983  2.37679
      2:          2 42.44   6.833  -5.547 -9.636  -5.547  0.01837
      3:          3 42.44   6.895 -11.847  3.643 -11.847 -4.41122

# output_lm_numeric_gaussian

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp  Month     Day
              <int> <num>   <num>   <num>    <num>  <num>   <num>
      1:          1 42.44  -8.645   7.842  14.4120  0.535 -1.5427
      2:          2 42.44   4.751  -4.814 -11.6985 -1.132 -0.9848
      3:          3 42.44   7.339 -25.590   0.2717 -0.562  0.9729

# output_lm_numeric_copula

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: copula
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month    Day
              <int> <num>   <num>   <num>   <num>   <num>  <num>
      1:          1 42.44  -6.512   7.341  14.357 -0.5201 -2.064
      2:          2 42.44   3.983  -4.656 -10.001 -1.8813 -1.324
      3:          3 42.44   6.076 -25.219   1.754 -1.3488  1.169

# output_lm_numeric_ctree

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month    Day
              <int> <num>   <num>   <num>  <num>   <num>  <num>
      1:          1 42.44  -9.198   9.679 16.925 -1.3310 -3.473
      2:          2 42.44   5.283  -6.046 -8.095 -2.7998 -2.222
      3:          3 42.44   6.984 -20.837 -4.762 -0.1545  1.201

# output_lm_numeric_vaeac

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: vaeac
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 10
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind  Temp  Month    Day
              <int> <num>   <num>  <num> <num>  <num>  <num>
      1:          1  42.4   -4.94   7.49 17.47 -4.355 -3.069
      2:          2  42.4    1.82  -5.19 -8.94  0.071 -1.638
      3:          3  42.4    4.53 -20.29  3.17 -4.285 -0.698

# output_lm_categorical_ctree

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is therefore set to `2^n_features = 16`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -5.719              15.22         -6.220      -3.791
      2:          2 42.44       -5.687             -17.48         22.095     -13.755
      3:          3 42.44        6.839             -21.90          1.997      -5.301

# output_lm_categorical_vaeac

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is therefore set to `2^n_features = 16`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: vaeac
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 10
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1  42.4        -1.97               12.6          -4.72       -6.38
      2:          2  42.4        -2.41              -14.4          14.43      -12.47
      3:          3  42.4         2.76              -14.2           3.22      -10.10

# output_lm_categorical_categorical

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is therefore set to `2^n_features = 16`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: categorical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -5.448              11.31        -11.445       5.078
      2:          2 42.44       -7.493             -12.27         19.672     -14.744
      3:          3 42.44       13.656             -19.73          4.369     -16.659

# output_lm_categorical_independence

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is therefore set to `2^n_features = 16`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -5.252              13.95         -7.041      -2.167
      2:          2 42.44       -5.252             -15.61         20.086     -14.050
      3:          3 42.44        4.833             -15.61          0.596      -8.178

# output_lm_ts_timeseries

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 16`, and is therefore set to `2^n_groups = 16`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: timeseries
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 4
      * Feature groups: S1: {"X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
      "X10"}; S2: {"X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19",
      "X20"}; S3: {"X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28", "X29",
      "X30"}; S4: {"X31", "X32", "X33", "X34", "X35", "X36", "X37", "X38", "X39",
      "X40"}
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none      S1     S2       S3      S4
              <int> <num>   <num>  <num>    <num>   <num>
      1:          1 4.895 -0.5261 0.7831 -0.21023 -0.3885
      2:          2 4.895 -0.6310 1.6288 -0.04498 -2.9298

# output_lm_numeric_comb1

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian, empirical, ctree, and independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month     Day
              <int> <num>   <num>   <num>  <num>   <num>   <num>
      1:          1 42.44  -8.987   9.070 15.511 -2.5647 -0.4281
      2:          2 42.44   2.916  -4.516 -7.845 -4.1649 -0.2686
      3:          3 42.44   6.968 -22.988 -1.717  0.6776 -0.5086

# output_lm_numeric_comb2

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree, copula, independence, and copula
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp  Month     Day
              <int> <num>   <num>   <num>   <num>  <num>   <num>
      1:          1 42.44  -9.394   9.435 17.0084 -1.700 -2.7465
      2:          2 42.44   5.227  -5.209 -8.5226 -2.968 -2.4068
      3:          3 42.44   6.186 -22.904 -0.3273 -1.132  0.6081

# output_lm_numeric_comb3

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence, empirical, gaussian, and empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month      Day
              <int> <num>   <num>   <num>  <num>  <num>    <num>
      1:          1 42.44  -6.887  10.715 12.199 -3.670  0.24393
      2:          2 42.44   2.603  -2.648 -8.464 -5.405  0.03414
      3:          3 42.44   5.868 -22.184  3.401 -2.955 -1.69888

# output_lm_mixed_independence

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2:          2 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3:          3 42.44   3.857 -17.469 -1.466  1.099        3.379

# output_lm_mixed_ctree

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp     Day Month_factor
              <int> <num>   <num>   <num>  <num>   <num>        <num>
      1:          1 42.44  -9.150  12.057 13.162 -0.8269       -4.658
      2:          2 42.44   4.425  -6.006 -6.260 -0.3910       -7.151
      3:          3 42.44   6.941 -21.427 -7.518  1.3987       10.006

# output_lm_mixed_vaeac

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: vaeac
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 10
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind    Temp    Day Month_factor
              <int> <num>   <num>  <num>   <num>  <num>        <num>
      1:          1  42.4   -5.05   6.86 15.7301 -0.208        -6.75
      2:          2  42.4    2.60  -4.64 -2.2641 -3.129        -7.95
      3:          3  42.4    5.14 -17.88 -0.0137  0.585         1.57

# output_lm_mixed_comb

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree, independence, ctree, and independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -7.677  10.757 16.247 -1.446       -7.297
      2:          2 42.44   5.049  -5.028 -6.965 -1.265       -7.174
      3:          3 42.44   5.895 -20.744 -4.468  0.775        7.943

# output_custom_lm_numeric_independence_1

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_custom_xgboost_mixed_dummy_ctree

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i You passed a model to `shapr::explain()` which is not natively supported, and did not supply a `get_model_specs` function to `shapr::explain()`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <xgb.Booster>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind   Temp     Day Month_factor
              <int> <num>   <num>  <num>  <num>   <num>        <num>
      1:          1 42.44  -5.639  13.31  20.93 -0.4716       -0.425
      2:          2 42.44   5.709 -13.30 -16.52  1.4006       -2.738
      3:          3 42.44   6.319 -14.07 -19.77  1.0831        5.870

# output_lm_numeric_interaction

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 4`, and is therefore set to `2^n_features = 4`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 2
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 4 of 4 coalitions. 
    Output
         explain_id  none Solar.R    Wind
              <int> <num>   <num>   <num>
      1:          1 42.44 -13.818  10.579
      2:          2 42.44   4.642  -6.287
      3:          3 42.44   4.452 -34.602

# output_lm_numeric_ctree_parallelized

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month    Day
              <int> <num>   <num>   <num>  <num>   <num>  <num>
      1:          1 42.44  -9.198   9.679 16.925 -1.3310 -3.473
      2:          2 42.44   5.283  -6.046 -8.095 -2.7998 -2.222
      3:          3 42.44   6.984 -20.837 -4.762 -0.1545  1.201

# output_lm_numeric_empirical_progress

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Month    Day
              <int> <num>   <num>   <num>  <num>    <num>  <num>
      1:          1 42.44 -13.252  15.541 12.826 -5.77179  3.259
      2:          2 42.44   2.758  -3.325 -7.992 -7.12800  1.808
      3:          3 42.44   6.805 -22.126  3.730 -0.09235 -5.885

