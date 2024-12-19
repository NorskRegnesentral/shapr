# output_lm_numeric_lm_separate_iterative

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
    Output
         explain_id  none Solar.R    Wind      Temp   Month     Day
              <int> <num>   <num>   <num>     <num>   <num>   <num>
      1:          1 42.44  -8.474   8.357  14.07721  0.6924 -2.0512
      2:          2 42.44   4.861  -4.451 -12.09391 -0.7750 -1.4210
      3:          3 42.44   7.312 -25.448   0.05524 -0.2448  0.7567

# output_lm_numeric_lm_separate

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 10 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44 -0.4556 -0.4556  14.4174  0.6856 -1.5898
      2:          2 42.44 -0.2281 -0.2281 -11.2859 -1.0148 -1.1226
      3:          3 42.44 -8.9589 -8.9589   0.2329 -0.4732  0.5898

# output_lm_categorical_lm_separate

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp    Day Month_factor
              <int> <num>   <num>   <num>   <num>  <num>        <num>
      1:          1 42.44 -13.991  14.352  16.490   1.82       -8.088
      2:          2 42.44   8.183  -1.463 -16.499   3.63       -9.233
      3:          3 42.44   3.364 -14.946   0.401 -11.32       11.905

# output_lm_numeric_lm_surrogate_iterative

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Condition
      Warning in `check_and_set_iterative()`:
      Iterative estimation of Shapley values are not supported for approach = regression_surrogate. Setting iterative = FALSE.
    Message
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 12 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month    Day
              <int> <num>   <num>   <num>   <num>   <num>  <num>
      1:          1 42.44  -4.908  10.750 15.7404 -3.9824 -4.998
      2:          2 42.44   4.111  -4.283 -7.0067 -3.2881 -3.412
      3:          3 42.44   4.649 -24.040 -0.4918  0.2164  2.098

# output_lm_numeric_lm_surrogate_reg_surr_n_comb

    Code
      (out <- code)
    Message
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 12 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month    Day
              <int> <num>   <num>   <num>   <num>   <num>  <num>
      1:          1 42.44  -4.460  10.394 16.1653 -4.1613 -5.337
      2:          2 42.44   4.921  -4.735 -6.6623 -3.5602 -3.843
      3:          3 42.44   5.508 -24.619 -0.1181 -0.1746  1.835

# output_lm_categorical_lm_surrogate

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: regression_surrogate
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44 -11.165   8.002  20.61  2.030       -8.896
      2:          2 42.44   4.143  -1.515 -11.23  2.025       -8.806
      3:          3 42.44   6.515 -18.268  -4.06 -3.992        9.204

