# output_sage_independence_lm

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
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R  Wind  Temp  Month     Day
              <int> <num>   <num> <num> <num>  <num>   <num>
      1:          1 -1119   55.44 240.1 426.5 -23.91 -0.7152

# output_sage_gaussian_xgboost

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i Feature classes extracted from the model contain `NA`.
        Assuming feature classes from the data are correct.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <xgboost>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R  Wind  Temp Month   Day
              <int> <num>   <num> <num> <num> <num> <num>
      1:          1 -1119   110.2 478.4 497.9 1.356 26.78

# output_sage_empirical_lm

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
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 10 of 32 coalitions. 
    Output
         explain_id  none Solar.R  Wind  Temp Month   Day
              <int> <num>   <num> <num> <num> <num> <num>
      1:          1 -1119   123.1 123.1 263.6 90.75 96.98

# output_sage_copula_lm_iter

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
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 108
      
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
         explain_id  none Solar.R  Wind  Temp Month   Day
              <int> <num>   <num> <num> <num> <num> <num>
      1:          1 -1119   64.84 257.2 362.6 10.57 2.339

# output_sage_groups_lm_copula

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: copula
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 2
      * Feature groups: A: {"Temp", "Month", "Day"}; B: {"Wind", "Solar.R"}
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 4 of 4 coalitions. 
    Output
         explain_id  none     A     B
              <int> <num> <num> <num>
      1:          1 -1119 385.9 311.6

# output_sage_loss_mae

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
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id   none Solar.R  Wind  Temp  Month     Day
              <int>  <num>   <num> <num> <num>  <num>   <num>
      1:          1 -26.87  0.9472 3.318 7.258 0.2682 0.05485

# output_sage_loss_mape

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
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id   none Solar.R  Wind  Temp Month     Day
              <int>  <num>   <num> <num> <num> <num>   <num>
      1:          1 -153.4   19.29 11.15 53.72 2.367 -0.9627

# output_sage_xgboost_binary

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i Feature classes extracted from the model contain `NA`.
        Assuming feature classes from the data are correct.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <xgboost>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id    none Solar.R    Wind   Temp     Month    Day
              <int>   <num>   <num>   <num>  <num>     <num>  <num>
      1:          1 -0.6916   0.108 0.09312 0.3831 -0.003701 0.1019

# output_sage_causal_lm_gaussian

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
      * Number of observations to explain: 108
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id  none Solar.R  Wind  Temp  Month    Day
              <int> <num>   <num> <num> <num>  <num>  <num>
      1:          1 -1119   77.13 316.3 395.4 -43.55 -47.74

# output_sage_asymmetric_conditional_lm_gaussian

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
      * Number of observations to explain: 108
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R  Wind  Temp Month   Day
              <int> <num>   <num> <num> <num> <num> <num>
      1:          1 -1119  -22.11 423.6 273.5 18.65 3.925

# output_sage_asymmetric_causal_lm_gaussian

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
      * Number of observations to explain: 108
      * Number of asymmetric coalitions: 8
      * Causal ordering: {Solar.R, Wind}, {Temp}, {Month, Day}
      * Components with confounding: {Solar.R, Wind}, {Temp}, {Month, Day}
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_id  none Solar.R  Wind  Temp Month   Day
              <int> <num>   <num> <num> <num> <num> <num>
      1:          1 -1119     156 356.5 162.4 18.68 3.923

