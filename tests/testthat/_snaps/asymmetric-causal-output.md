# output_asymmetric_conditional

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 8 coalitions, 5 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 8 coalitions, 3 new. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44 -11.446  14.923 14.754 -2.671 -2.958
      2:          2 42.44   5.593  -6.215 -4.578 -6.680 -1.999
      3:          3 42.44   9.719 -32.557  7.272 -3.376  1.375

# output_symmetric_conditional

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}
      
      -- Main computation started --
      
      i Using 6 of 6 coalitions. 
    Output
         explain_id  none Solar.R     Wind     Temp  Month     Day
              <int> <num>   <num>    <num>    <num>  <num>   <num>
      1:          1 42.44  -25.73  21.4308  21.4308 -1.140 -3.3927
      2:          2 42.44   -7.98   0.8414   0.8414 -5.149 -2.4328
      3:          3 42.44    8.03 -12.3436 -12.3436 -1.848  0.9372

# output_asym_caus_conf_mix_empirical

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: empirical
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: ctree
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: No component with confounding
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R    Wind    Temp  Month     Day
              <int> <num>   <num>   <num>   <num>  <num>   <num>
      1:          1 42.44  -7.978  10.871 12.1981 -2.188 -0.3003
      2:          2 42.44   3.637  -6.474 -9.6711 -1.850  0.4779
      3:          3 42.44   1.926 -27.039  0.7298  1.404  5.4112

# output_sym_caus_conf_mix

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 8, 
      and is therefore set to 2^n_groups = 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of group-wise Shapley values: 3
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 8, 
      and is therefore set to 2^n_groups = 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of group-wise Shapley values: 3
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 8, 
      and is therefore set to 2^n_groups = 8.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 6 coalitions:
      Convergence tolerance reached!
    Output
         explain_id  none      A      B       C
              <int> <num>  <num>  <num>   <num>
      1:          1 42.44 -5.082  32.51 -14.823
      2:          2 42.44 10.059 -29.61   5.675
      3:          3 42.44 11.355 -23.68  -5.247

# output_mixed_sym_caus_conf_TRUE

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: ctree
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: ctree
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      * Components with confounding: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      
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
      
      -- Iteration 6 -----------------------------------------------------------------
      i Using 26 of 32 coalitions, 4 new. 
      
      -- Iteration 7 -----------------------------------------------------------------
      i Using 28 of 32 coalitions, 2 new. 
      
      -- Iteration 8 -----------------------------------------------------------------
      i Using 30 of 32 coalitions, 2 new. 
    Output
         explain_id  none Solar.R   Wind    Temp     Day Month_factor
              <int> <num>   <num>  <num>   <num>   <num>        <num>
      1:          1 42.44  -2.421  11.79  8.6450 -4.5647       -2.870
      2:          2 42.44   3.042  -6.71 -4.2354 -4.0375       -3.442
      3:          3 42.44   2.905 -18.44 -0.2186  0.2627        4.892

# output_mixed_asym_caus_conf_mixed

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: ctree
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: ctree
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or number of coalitions respecting the causal
      ordering 8, and is therefore set to 8.
      
      * Model class: <lm>
      * Approach: regression_separate
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Day, Month_factor}
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 8 coalitions, 5 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 8 coalitions, 3 new. 
    Output
         explain_id  none Solar.R    Wind   Temp     Day Month_factor
              <int> <num>   <num>   <num>  <num>   <num>        <num>
      1:          1 42.44 -11.397  14.988 14.418 -2.2531       -5.172
      2:          2 42.44   5.535  -6.273 -4.731 -1.6342       -8.280
      3:          3 42.44   9.633 -32.766  7.516  0.8637        4.153

# output_categorical_asym_causal_mixed_cat

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: categorical
      * Iterative estimation: FALSE
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
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: categorical
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 4
      * Number of observations to explain: 3
      * Causal ordering: {Solar.R_factor, Wind_factor}, {Ozone_sub30_factor},
      {Month_factor}
      * Components with confounding: {Solar.R_factor, Wind_factor}
      
      -- iterative computation started --
      
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
    Output
         explain_id  none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
              <int> <num>        <num>              <num>          <num>       <num>
      1:          1 42.44      -0.8228              14.26         -10.99      -2.961
      2:          2 42.44       3.2689             -13.45          14.43     -19.089
      3:          3 42.44       8.4092             -19.04          11.81     -19.535

# output_categorical_asym_causal_mixed_ctree

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 16, 
      and is therefore set to 2^n_features = 16.
      
      * Model class: <lm>
      * Approach: ctree
      * Iterative estimation: FALSE
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

