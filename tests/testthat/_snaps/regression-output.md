# output_lm_numeric_lm_separate_iterative

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 24 of 32 coalitions, 6 new. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.942   6.609  15.1578  1.1758 -1.3984
      2:          2 42.44   4.766  -5.909 -11.2770 -0.5816 -0.8785
      3:          3 42.44   7.456 -25.853   0.4259 -0.3951  0.7973

# output_lm_numeric_lm_separate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.577   7.845  14.4756  0.6251 -1.7664
      2:          2 42.44   4.818  -4.811 -11.6350 -1.0423 -1.2086
      3:          3 42.44   7.406 -25.587   0.3353 -0.4718  0.7491

# output_lm_numeric_lm_separate_n_comb

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 10 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44 -0.4428 -0.4428  14.4046  0.6728 -1.5898
      2:          2 42.44 -0.2195 -0.2195 -11.2945 -1.0234 -1.1226
      3:          3 42.44 -8.9576 -8.9576   0.2316 -0.4745  0.5898

# output_lm_categorical_lm_separate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is therefore set to `2^n_features = 16`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -9.806              18.60        -11.788       2.489
      2:          2 42.44       -7.256             -18.88         24.751     -13.445
      3:          3 42.44       15.594             -26.01          5.887     -13.834

# output_lm_mixed_lm_separate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp     Day Month_factor
              <int> <num>   <num>   <num>  <num>   <num>        <num>
      1:          1 42.44  -8.782   8.165 20.389 -1.2383       -7.950
      2:          2 42.44   4.623  -3.551 -6.199 -0.9110       -9.345
      3:          3 42.44   8.029 -25.200 -4.821  0.4172       10.975

# output_lm_mixed_splines_separate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -8.083   7.102 18.732  1.483       -8.651
      2:          2 42.44   6.147  -4.314 -6.445 -2.136       -8.635
      3:          3 42.44   7.536 -22.504 -5.081 -2.170       11.619

# output_lm_mixed_decision_tree_cv_separate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind    Temp     Day Month_factor
              <int> <num>   <num>  <num>   <num>   <num>        <num>
      1:          1 42.44  -7.742 12.044   9.676  2.0107       -5.405
      2:          2 42.44   2.688 -4.973  -6.982  1.5650       -7.681
      3:          3 42.44   6.018 -8.864 -17.781 -0.9106       10.937

# output_lm_mixed_decision_tree_cv_separate_parallel

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind    Temp     Day Month_factor
              <int> <num>   <num>  <num>   <num>   <num>        <num>
      1:          1 42.44  -7.742 12.044   9.676  2.0107       -5.405
      2:          2 42.44   2.688 -4.973  -6.982  1.5650       -7.681
      3:          3 42.44   6.018 -8.864 -17.781 -0.9106       10.937

# output_lm_mixed_xgboost_separate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none  Solar.R    Wind    Temp    Day Month_factor
              <int> <num>    <num>   <num>   <num>  <num>        <num>
      1:          1 42.44 -14.1953   9.381  20.535  2.480       -7.617
      2:          2 42.44   7.6719  -1.624 -14.085  3.126      -10.472
      3:          3 42.44  -0.4555 -13.801  -3.414 -5.871       12.941

# output_lm_numeric_lm_surrogate_iterative

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
    Condition
      Warning:
      ! Iterative estimation of Shapley values are not supported for approach = regression_surrogate. Setting iterative = FALSE.
    Message
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month     Day
              <int> <num>   <num>   <num>  <num>   <num>   <num>
      1:          1 42.44  -9.273   9.578 16.536 -1.2690 -2.9707
      2:          2 42.44   2.623  -5.766 -6.717 -1.4694 -2.5496
      3:          3 42.44   6.801 -24.090 -1.295  0.1202  0.8953

# output_lm_numeric_lm_surrogate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month     Day
              <int> <num>   <num>   <num>  <num>   <num>   <num>
      1:          1 42.44  -9.273   9.578 16.536 -1.2690 -2.9707
      2:          2 42.44   2.623  -5.766 -6.717 -1.4694 -2.5496
      3:          3 42.44   6.801 -24.090 -1.295  0.1202  0.8953

# output_lm_numeric_lm_surrogate_n_comb

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 12 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp  Month    Day
              <int> <num>   <num>   <num>   <num>  <num>  <num>
      1:          1 42.44  -4.934  10.750 15.7538 -3.969 -4.998
      2:          2 42.44   4.092  -4.283 -6.9973 -3.279 -3.412
      3:          3 42.44   4.660 -24.040 -0.4972  0.211  2.098

# output_lm_numeric_lm_surrogate_reg_surr_n_comb

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 12 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month    Day
              <int> <num>   <num>   <num>   <num>   <num>  <num>
      1:          1 42.44  -4.492  10.394 16.1816 -4.1450 -5.337
      2:          2 42.44   4.895  -4.735 -6.6494 -3.5473 -3.843
      3:          3 42.44   5.513 -24.619 -0.1201 -0.1766  1.835

# output_lm_categorical_lm_surrogate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 16`, and is therefore set to `2^n_features = 16`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -7.137              16.29         -9.895      0.2304
      2:          2 42.44       -6.018             -16.28         23.091    -15.6258
      3:          3 42.44       10.042             -18.58          2.415    -12.2431

# output_lm_mixed_lm_surrogate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp     Day Month_factor
              <int> <num>   <num>   <num>  <num>   <num>        <num>
      1:          1 42.44  -7.427  10.831 16.477 -0.6280       -8.669
      2:          2 42.44   3.916  -4.232 -4.849 -0.8776       -9.341
      3:          3 42.44   5.629 -24.012 -2.274 -0.4774       10.534

# output_lm_mixed_decision_tree_cv_surrogate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind   Temp    Day Month_factor
              <int> <num>   <num>  <num>  <num>  <num>        <num>
      1:          1 42.44  -4.219 -4.219 27.460 -4.219       -4.219
      2:          2 42.44  -3.077 -3.077 -3.077 -3.077       -3.077
      3:          3 42.44  -6.716 -6.716 -6.716 -6.716       16.262

# output_lm_mixed_xgboost_surrogate

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_surrogate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind   Temp     Day Month_factor
              <int> <num>   <num>  <num>  <num>   <num>        <num>
      1:          1 42.44 -15.551  11.85 14.524  4.3014       -4.540
      2:          2 42.44   5.513  -4.57 -9.402  2.1304       -9.055
      3:          3 42.44   2.492 -17.29 -2.532 -0.6971        7.422

