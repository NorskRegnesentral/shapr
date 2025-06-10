# output_sage_independence_lm

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: independence
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id    none Solar.R  Wind  Temp  Month     Day
             <lgcl>   <num>   <num> <num> <num>  <num>   <num>
      1:         NA 2.2e-05   55.44 240.1 426.5 -23.91 -0.7152

# output_sage_gaussian_xgboost

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i Feature classes extracted from the model contains `NA`.
        Assuming feature classes from the data are correct.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <xgb.Booster>
      * Approach: gaussian
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 111
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id    none Solar.R  Wind  Temp Month   Day
             <lgcl>   <num>   <num> <num> <num> <num> <num>
      1:         NA 3.8e-05   156.5 422.2 495.2 3.053  16.3

# output_sage_empirical_lm_iter

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: empirical
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 108
      
      -- iterative computation started --
      
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
         explain_id    none Solar.R  Wind  Temp Month   Day
             <lgcl>   <num>   <num> <num> <num> <num> <num>
      1:         NA 0.00012   106.4 194.9   205 99.23 91.95

# output_sage_copula_lm

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * Approach: copula
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 108
      
      -- Main computation started --
      
      i Using 32 of 32 coalitions. 
    Output
         explain_id    none Solar.R  Wind  Temp Month   Day
             <lgcl>   <num>   <num> <num> <num> <num> <num>
      1:         NA 4.6e-05   60.81 258.3 366.5 9.615 2.231

