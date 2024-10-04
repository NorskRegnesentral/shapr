# output_lm_numeric_independence

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: empirical
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      * Model class: <lm>
      * Approach: empirical
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 20 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month    Day
              <int> <num>   <num>   <num>  <num>   <num>  <num>
      1:          1 42.44 -14.030  18.711  9.718 -6.1533  4.356
      2:          2 42.44   3.015  -3.442 -7.095 -7.8174  1.459
      3:          3 42.44   8.566 -24.310  3.208  0.6956 -5.728

# output_lm_numeric_empirical_independence

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Warning in `setup_approach.empirical()`:
      Using empirical.type = 'independence' for approach = 'empirical' is deprecated.
      Please use approach = 'independence' instead.
    Message
      * Model class: <lm>
      * Approach: empirical
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical_AICc_each

    Code
      (out <- code)
    Message
      * Model class: <lm>
      * Approach: empirical
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 8 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month      Day
              <int> <num>   <num>   <num>   <num>   <num>    <num>
      1:          1 42.44  -9.778   9.084  5.4596  5.4596  2.37679
      2:          2 42.44   6.833  -4.912 -7.9095 -7.9095  0.01837
      3:          3 42.44   6.895 -21.308  0.6281  0.6281 -4.41122

# output_lm_numeric_empirical_AICc_full

    Code
      (out <- code)
    Message
      * Model class: <lm>
      * Approach: empirical
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 8 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month      Day
              <int> <num>   <num>   <num>   <num>   <num>    <num>
      1:          1 42.44  -9.778   9.084  5.4596  5.4596  2.37679
      2:          2 42.44   6.833  -4.912 -7.9095 -7.9095  0.01837
      3:          3 42.44   6.895 -21.308  0.6281  0.6281 -4.41122

# output_lm_numeric_gaussian

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: copula
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: ctree
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: vaeac
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Month     Day
              <int> <num>   <num>   <num>  <num>    <num>   <num>
      1:          1 42.44  -4.941   7.495 17.471 -4.35451 -3.0686
      2:          2 42.44   1.824  -5.193 -8.943  0.07104 -1.6383
      3:          3 42.44   4.530 -20.285  3.170 -4.28496 -0.6978

# output_lm_categorical_ctree

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: ctree
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: vaeac
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -1.966              12.55         -4.716       -6.38
      2:          2 42.44       -2.405             -14.39         14.433      -12.47
      3:          3 42.44        2.755             -14.24          3.222      -10.10

# output_lm_categorical_categorical

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: categorical
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       13.656             -19.73          4.369     -16.659
      2:          2 42.44       -5.448              11.31        -11.445       5.078
      3:          3 42.44       -7.493             -12.27         19.672     -14.744

# output_lm_categorical_independence

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 16, 
      and is therefore set to 2^n_groups = 16.
      
      * Model class: <lm>
      * Approach: timeseries
      * Adaptive estimation: FALSE
      * Number of group-wise Shapley values: 4
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian, empirical, ctree, and independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: ctree, copula, independence, and copula
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence, empirical, gaussian, and empirical
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: ctree
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: vaeac
      * Adaptive estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp     Day Month_factor
              <int> <num>   <num>   <num>    <num>   <num>        <num>
      1:          1 42.44  -5.050   6.861 15.73013 -0.2083       -6.749
      2:          2 42.44   2.600  -4.636 -2.26409 -3.1294       -7.954
      3:          3 42.44   5.139 -17.878 -0.01372  0.5855        1.567

# output_lm_mixed_comb

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: ctree, independence, ctree, and independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Adaptive estimation: FALSE
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

# output_custom_lm_numeric_independence_2

    Code
      (out <- code)
    Message
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <whatever>
      * Approach: independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <xgb.Booster>
      * Approach: ctree
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 4, 
      and is therefore set to 2^n_features = 4.
      
      * Model class: <lm>
      * Approach: independence
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: ctree
      * Adaptive estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: empirical
      * Adaptive estimation: FALSE
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

# output_lm_numeric_independence_keep_samp_for_vS

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Adaptive estimation: FALSE
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

