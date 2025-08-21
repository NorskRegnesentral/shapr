# output_asymmetric_conditional

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44 -24.516  29.347 11.557 -0.626 -3.161
      2:          2 42.44  -7.632   8.053 -7.467 -4.634 -2.200
      3:          3 42.44  -3.458 -18.240  4.321 -1.347  1.156

# output_asym_cond_reg

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Non-iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44 -11.337  15.032 14.506 -2.656 -2.943
      2:          2 42.44   5.546  -6.262 -4.518 -6.664 -1.982
      3:          3 42.44   9.720 -32.555  7.270 -3.377  1.374

# output_asym_cond_reg_iterative

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 8 coalitions, 5 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 8 coalitions, 3 new. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44 -11.354  15.015 14.544 -2.658 -2.945
      2:          2 42.44   5.553  -6.255 -4.527 -6.666 -1.984
      3:          3 42.44   9.720 -32.556  7.270 -3.377  1.374

# output_symmetric_conditional

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month     Day
              <int> <num>   <num>   <num>   <num>   <num>   <num>
      1:          1 42.44 -11.395   7.610  15.278  1.3845 -0.2755
      2:          2 42.44   2.001  -5.047 -10.833 -0.2829  0.2824
      3:          3 42.44   4.589 -25.823   1.138  0.2876  2.2401

# output_symmetric_marginal_independence

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind, Temp, Month, Day}
      * Components with confounding: {Solar.R, Wind, Temp, Month, Day}
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp   Month    Day
              <int> <num>   <num>   <num>   <num>   <num>  <num>
      1:          1 42.44  -2.644   6.870 16.5974 -0.5859 -7.636
      2:          2 42.44  -1.315  -3.251 -6.6438 -5.9780  3.308
      3:          3 42.44  -1.114 -10.549 -0.8839 -7.0244  2.004

# output_symmetric_marginal_gaussian

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind, Temp, Month, Day}
      * Components with confounding: {Solar.R, Wind, Temp, Month, Day}
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp   Month    Day
              <int> <num>   <num>   <num>  <num>   <num>  <num>
      1:          1 42.44 -8.1241   6.631 15.251 -2.3173  1.161
      2:          2 42.44  0.8798  -2.652 -6.971 -1.2012 -3.935
      3:          3 42.44  3.3391 -14.550 -3.145 -0.4127 -2.800

# output_asym_caus_conf_TRUE

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp  Month    Day
              <int> <num>   <num>   <num>   <num>  <num>  <num>
      1:          1 42.44 -12.804  11.755 17.3723 -0.499 -3.222
      2:          2 42.44   1.471  -2.609 -5.9820 -4.592 -2.168
      3:          3 42.44  14.736 -31.711 -0.3884 -1.430  1.225

# output_asym_caus_conf_FALSE

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: No component with confounding
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none  Solar.R     Wind   Temp  Month    Day
              <int> <num>    <num>    <num>  <num>  <num>  <num>
      1:          1 42.44 -15.4362  17.9420 13.883 -0.626 -3.161
      2:          2 42.44  -0.8741  -0.4898 -5.682 -4.634 -2.200
      3:          3 42.44   7.2517 -30.3922  5.763 -1.347  1.156

# output_asym_caus_conf_mix

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp  Month    Day
              <int> <num>   <num>   <num>   <num>  <num>  <num>
      1:          1 42.44 -12.804  11.755 17.4378 -0.626 -3.161
      2:          2 42.44   1.471  -2.609 -5.9087 -4.634 -2.200
      3:          3 42.44  14.736 -31.711 -0.4028 -1.347  1.156

# output_asym_caus_conf_mix_n_coal

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}
      
      -- Main computation started --
      
      i Using 6 of 6 coalitions. 
    Output
         explain_id  none Solar.R    Wind     Temp  Month    Day
              <int> <num>   <num>   <num>    <num>  <num>  <num>
      1:          1 42.44   1.488   8.454   6.0255 -1.004 -2.362
      2:          2 42.44   1.115   4.578 -13.1691 -4.993 -1.411
      3:          3 42.44  19.410 -37.477   0.3474 -1.911  2.062

# output_asym_caus_conf_mix_empirical

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month     Day
              <int> <num>   <num>   <num>  <num>  <num>   <num>
      1:          1 42.44  -9.609   9.859 17.410 -4.136 -0.9212
      2:          2 42.44  14.220 -17.195 -7.333 -1.904 -1.6682
      3:          3 42.44   0.661 -20.737  7.258 -5.048  0.2978

# output_asym_caus_conf_mix_ctree

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R   Wind   Temp  Month     Day
              <int> <num>   <num>  <num>  <num>  <num>   <num>
      1:          1 42.44 -17.734  20.45 19.217 -5.820 -3.5086
      2:          2 42.44  19.188 -15.28 -9.429 -8.159 -0.1952
      3:          3 42.44   5.409 -29.78  8.986 -1.464 -0.7140

# output_sym_caus_conf_TRUE

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month     Day
              <int> <num>   <num>   <num>  <num>  <num>   <num>
      1:          1 42.44 -10.586   9.603 14.085 -2.429  1.9293
      2:          2 42.44   1.626  -3.712 -2.724 -7.310 -1.7595
      3:          3 42.44   9.581 -25.344  1.892 -4.089  0.3918

# output_sym_caus_conf_FALSE

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: No component with confounding
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month     Day
              <int> <num>   <num>   <num>  <num>  <num>   <num>
      1:          1 42.44  -8.606  10.387 13.963 -4.010  0.8685
      2:          2 42.44   3.506  -6.071 -4.521 -6.183 -0.6104
      3:          3 42.44   2.371 -26.300  2.790  1.600  1.9707

# output_sym_caus_conf_mix

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month     Day
              <int> <num>   <num>   <num>  <num>  <num>   <num>
      1:          1 42.44  -10.60   9.600 14.068 -2.464  1.9983
      2:          2 42.44    1.62  -3.719 -2.722 -7.284 -1.7747
      3:          3 42.44    9.58 -25.345  1.893 -4.005  0.3084

# output_sym_caus_conf_TRUE_group

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 8`, and is therefore set to `2^n_groups = 8`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of group-wise Shapley values: 3
      * Feature groups: A: {"Solar.R", "Wind"}; B: {"Temp"}; C: {"Month", "Day"}
      * Number of observations to explain: 3
      * Causal ordering: {A, B}, {C}
      * Components with confounding: {A, B}, {C}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none      A       B      C
              <int> <num>  <num>   <num>  <num>
      1:          1 42.44 11.547  16.725 -15.67
      2:          2 42.44  7.269 -10.685 -10.46
      3:          3 42.44 -5.058   1.578 -14.09

# output_sym_caus_conf_mix_group

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 8`, and is therefore set to `2^n_groups = 8`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of group-wise Shapley values: 3
      * Feature groups: A: {"Solar.R"}; B: {"Wind", "Temp"}; C: {"Month", "Day"}
      * Number of observations to explain: 3
      * Causal ordering: {A}, {B}, {C}
      * Components with confounding: {A}, {B}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none       A       B       C
              <int> <num>   <num>   <num>   <num>
      1:          1 42.44 -13.728  31.822  -5.493
      2:          2 42.44   3.126  -6.343 -10.662
      3:          3 42.44   5.310 -17.036  -5.842

# output_sym_caus_conf_mix_group_iterative

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.23 [needs 0.02]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 8 coalitions after 2 iterations, due to:
      All (8) coalitions used!
      Maximum number of coalitions (8) reached!
    Output
         explain_id  none       A      B       C
              <int> <num>   <num>  <num>   <num>
      1:          1 42.44 -6.7937  35.93 -16.534
      2:          2 42.44  0.8771 -11.25  -3.507
      3:          3 42.44  9.0029 -18.97  -7.599

# output_mixed_sym_caus_conf_TRUE

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      * Components with confounding: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R   Wind   Temp     Day Month_factor
              <int> <num>   <num>  <num>  <num>   <num>        <num>
      1:          1 42.44  -1.065  18.16  8.030 -0.1478      -14.394
      2:          2 42.44   4.729 -11.40 -7.837  1.6971       -2.570
      3:          3 42.44   3.010 -23.62  3.218  4.8728        1.922

# output_mixed_sym_caus_conf_TRUE_iterative

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
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      * Components with confounding: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      
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
      
      -- Iteration 6 -----------------------------------------------------------------
      i Using 28 of 32 coalitions, 4 new. 
      
      -- Iteration 7 -----------------------------------------------------------------
      i Using 30 of 32 coalitions, 2 new. 
      
      -- Iteration 8 -----------------------------------------------------------------
      i Using 32 of 32 coalitions, 2 new. 
    Output
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -1.185   9.662  9.097 -2.617       -4.372
      2:          2 42.44   4.587 -10.237 -5.900 -2.779       -1.054
      3:          3 42.44   5.672 -18.778 -2.893 -2.084        7.483

# output_mixed_asym_caus_conf_mixed

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      * Components with confounding: {Solar.R, Wind}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none  Solar.R    Wind     Temp    Day Month_factor
              <int> <num>    <num>   <num>    <num>  <num>        <num>
      1:          1 42.44  -2.8521  17.231  5.46662 -6.018       -3.243
      2:          2 42.44   0.6492  -4.826 -0.02641 -5.053       -6.127
      3:          3 42.44 -10.7232 -14.690  8.32742  1.080        5.406

# output_mixed_asym_caus_conf_mixed_2

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: ctree
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      * Components with confounding: {Temp}, {Day, Month_factor}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp     Day Month_factor
              <int> <num>   <num>   <num>   <num>   <num>        <num>
      1:          1 42.44   1.656  17.903  0.2668 -3.7786       -5.463
      2:          2 42.44  -2.941  -6.389  4.8876 -4.4941       -6.446
      3:          3 42.44   4.715 -34.627 13.1031  0.4327        5.776

# output_mixed_asym_cond_reg

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than the number of coalitions respecting the causal ordering (8), and is therefore set to 8.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Regression
      * Approach: regression_separate
      * Procedure: Iterative
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 8 coalitions, 5 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 8 coalitions, 3 new. 
    Output
         explain_id  none Solar.R    Wind   Temp     Day Month_factor
              <int> <num>   <num>   <num>  <num>   <num>        <num>
      1:          1 42.44 -11.302  15.083 14.284 -2.2810       -5.200
      2:          2 42.44   5.496  -6.311 -4.642 -1.6403       -8.286
      3:          3 42.44   9.635 -32.764  7.453  0.8939        4.183

# output_categorical_asym_causal_mixed_cat

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 2
      * Causal ordering: {Solar.R_factor, Wind_factor}, {Ozone_sub30_factor},
      {Month_factor}
      * Components with confounding: {Solar.R_factor, Wind_factor}
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44      -10.128              15.35         -10.26       4.526
      2:          2 42.44       -4.316             -10.80          21.06     -20.769

# output_cat_asym_causal_mixed_cat_ad

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
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R_factor, Wind_factor}, {Ozone_sub30_factor},
      {Month_factor}
      * Components with confounding: {Solar.R_factor, Wind_factor}
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 16 coalitions, 6 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 16 coalitions, 2 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 10 of 16 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 12 of 16 coalitions, 2 new. 
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 14 of 16 coalitions, 2 new. 
      
      -- Iteration 6 -----------------------------------------------------------------
      i Using 16 of 16 coalitions, 2 new. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -9.089              15.45         -9.112       2.237
      2:          2 42.44       -3.126             -10.25         15.463     -16.914
      3:          3 42.44       19.458             -21.09          3.881     -20.614

# output_categorical_asym_causal_mixed_ctree

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
      * Number of Monte Carlo integration samples: 5
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R_factor, Wind_factor}, {Ozone_sub30_factor},
      {Month_factor}
      * Components with confounding: {Solar.R_factor, Wind_factor}
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44       -7.113              11.37         -6.100       1.336
      2:          2 42.44       -2.421             -21.49         23.445     -14.366
      3:          3 42.44       11.296             -16.94          2.581     -15.297

