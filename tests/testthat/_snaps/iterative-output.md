# output_lm_numeric_independence_reach_exact

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.31 [needs 0.02]
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none      Solar.R         Wind       Temp        Month
              <int> <char>       <char>       <char>     <char>       <char>
      1:          1 42 (0) -0.58 (2.09) -0.58 (2.09) 17.5 (6.0) -0.58 (2.09)
      2:          2 42 (0) -2.19 (0.35) -2.19 (0.35) -5.3 (1.0) -2.19 (0.35)
      3:          3 42 (0) -5.78 (0.74) -5.78 (0.74) -1.5 (1.4) -5.78 (0.74)
                 Day
              <char>
      1: -3.1 (0.81)
      2: -2.0 (0.05)
      3:  1.3 (2.23)
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.25 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind       Temp       Month         Day
              <int> <char>     <char>      <char>     <char>      <char>      <char>
      1:          1 42 (0) -4.4 (4.2)   1.3 (1.0) 17.5 (5.5)   1.3 (1.0) -3.1 (1.61)
      2:          2 42 (0)  2.4 (1.7)  -4.5 (1.1) -5.3 (1.8)  -4.5 (1.1) -2.0 (0.83)
      3:          3 42 (0)  3.8 (3.8) -10.6 (2.4) -1.5 (1.3) -10.6 (2.4)  1.3 (1.19)
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.16 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind        Temp       Month         Day
              <int> <char>     <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0) -4.5 (2.2)   8.3 (3.8) 17.5 (1.70) -5.6 (3.39) -3.1 (0.35)
      2:          2 42 (0)  2.2 (1.2)  -3.3 (1.1) -5.2 (0.42) -5.6 (0.82) -2.0 (0.30)
      3:          3 42 (0)  3.7 (3.6) -18.6 (4.8) -1.4 (1.60) -2.5 (3.75)  1.3 (0.83)
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.042 [needs 0.02]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 40% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.02)   8.3 (0.98) 17.5 (0.03) -5.6 (0.98)
      2:          2 42 (0)  2.2 (0.02)  -3.3 (0.16) -5.2 (0.03) -5.6 (0.16)
      3:          3 42 (0)  3.7 (0.02) -18.6 (1.13) -1.4 (0.03) -2.5 (1.13)
                 Day
              <char>
      1: -3.1 (0.02)
      2: -2.0 (0.02)
      3:  1.3 (0.02)
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 24 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 24 coalitions after 5 iterations, due to:
      Standard deviation convergence threshold (0.02) reached: 0.001!
      
      Final estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.01)   8.3 (0.02) 17.5 (0.01) -5.6 (0.02)
      2:          2 42 (0)  2.2 (0.01)  -3.3 (0.02) -5.2 (0.01) -5.6 (0.02)
      3:          3 42 (0)  3.7 (0.01) -18.6 (0.02) -1.4 (0.01) -2.5 (0.02)
                 Day
              <char>
      1: -3.1 (0.02)
      2: -2.0 (0.02)
      3:  1.3 (0.02)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.536   8.304 17.505 -5.593 -3.078
      2:          2 42.44   2.251  -3.311 -5.244 -5.593 -1.982
      3:          3 42.44   3.709 -18.576 -1.453 -2.553  1.304

# output_lm_numeric_independence_converges_tol

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.1]
      Estimated remaining coalitions: 12
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R        Wind        Temp       Month
              <int> <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0)  1.9 (1.47)  1.9 (1.47) 17.5 (3.41) -5.6 (2.84)
      2:          2 42 (0) -0.5 (0.53) -0.5 (0.53) -5.3 (0.67) -5.6 (0.79)
      3:          3 42 (0) -7.4 (1.34) -7.4 (1.34) -1.5 (0.84) -2.6 (0.75)
                 Day
              <char>
      1: -3.1 (1.56)
      2: -2.0 (0.47)
      3:  1.3 (2.68)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind        Temp      Month         Day
              <int> <char>     <char>      <char>      <char>     <char>      <char>
      1:          1 42 (0) -4.4 (3.3)   8.3 (3.5) 17.5 (2.27) -5.6 (1.4) -3.1 (1.25)
      2:          2 42 (0)  2.4 (1.5)  -3.3 (1.3) -5.3 (0.70) -5.6 (0.4) -2.0 (0.85)
      3:          3 42 (0)  3.8 (4.8) -18.6 (6.1) -1.5 (0.86) -2.6 (1.3)  1.3 (2.16)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.11 [needs 0.1]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 30% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R        Wind        Temp       Month
              <int> <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0) -4.5 (1.22)   8.3 (1.5) 17.5 (1.49) -5.6 (0.02)
      2:          2 42 (0)  2.3 (0.85)  -3.4 (0.3) -5.2 (0.42) -5.6 (0.02)
      3:          3 42 (0)  3.7 (0.93) -18.6 (2.8) -1.4 (0.30) -2.5 (0.02)
                 Day
              <char>
      1: -3.1 (1.76)
      2: -2.0 (0.81)
      3:  1.3 (2.51)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 18 coalitions after 4 iterations, due to:
      Standard deviation convergence threshold (0.1) reached: 0.0016!
      
      Final estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.04)   8.3 (0.02) 17.5 (0.01) -5.6 (0.01)
      2:          2 42 (0)  2.3 (0.04)  -3.3 (0.02) -5.2 (0.01) -5.6 (0.01)
      3:          3 42 (0)  3.7 (0.04) -18.6 (0.02) -1.4 (0.01) -2.5 (0.01)
                 Day
              <char>
      1: -3.1 (0.02)
      2: -2.0 (0.02)
      3:  1.3 (0.02)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.529   8.270 17.513 -5.586 -3.066
      2:          2 42.44   2.258  -3.345 -5.236 -5.586 -1.971
      3:          3 42.44   3.716 -18.610 -1.445 -2.546  1.316

# output_lm_numeric_independence_converges_maxit

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.001]
      Estimated remaining coalitions: 22
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R        Wind        Temp       Month
              <int> <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0)  1.9 (1.47)  1.9 (1.47) 17.5 (3.41) -5.6 (2.84)
      2:          2 42 (0) -0.5 (0.53) -0.5 (0.53) -5.3 (0.67) -5.6 (0.79)
      3:          3 42 (0) -7.4 (1.34) -7.4 (1.34) -1.5 (0.84) -2.6 (0.75)
                 Day
              <char>
      1: -3.1 (1.56)
      2: -2.0 (0.47)
      3:  1.3 (2.68)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.001]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind        Temp      Month         Day
              <int> <char>     <char>      <char>      <char>     <char>      <char>
      1:          1 42 (0) -4.4 (3.3)   8.3 (3.5) 17.5 (2.27) -5.6 (1.4) -3.1 (1.25)
      2:          2 42 (0)  2.4 (1.5)  -3.3 (1.3) -5.3 (0.70) -5.6 (0.4) -2.0 (0.85)
      3:          3 42 (0)  3.8 (4.8) -18.6 (6.1) -1.5 (0.86) -2.6 (1.3)  1.3 (2.16)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.15 [needs 0.001]
      Estimated remaining coalitions: 18
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind        Temp       Month         Day
              <int> <char>     <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0) -4.4 (2.2)   8.3 (2.3) 17.5 (0.02) -5.6 (0.02) -3.1 (0.40)
      2:          2 42 (0)  2.4 (1.2)  -3.3 (1.0) -5.3 (0.02) -5.6 (0.02) -2.0 (0.69)
      3:          3 42 (0)  3.8 (3.9) -18.6 (4.1) -1.5 (0.02) -2.6 (0.02)  1.3 (0.71)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.067 [needs 0.001]
      Estimated remaining coalitions: 16
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R        Wind        Temp       Month
              <int> <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.92)   8.3 (0.9) 17.5 (0.02) -5.6 (0.02)
      2:          2 42 (0)  2.2 (0.52)  -3.3 (0.4) -5.2 (0.02) -5.6 (0.02)
      3:          3 42 (0)  3.7 (1.57) -18.6 (1.6) -1.5 (0.02) -2.6 (0.02)
                 Day
              <char>
      1: -3.1 (0.13)
      2: -2.0 (0.37)
      3:  1.3 (0.22)
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.0015 [needs 0.001]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.03)   8.3 (0.03) 17.5 (0.02) -5.6 (0.02)
      2:          2 42 (0)  2.3 (0.03)  -3.3 (0.03) -5.2 (0.02) -5.6 (0.02)
      3:          3 42 (0)  3.7 (0.03) -18.6 (0.03) -1.4 (0.02) -2.5 (0.02)
                 Day
              <char>
      1: -3.1 (0.03)
      2: -2.0 (0.03)
      3:  1.3 (0.03)
      
      -- Iteration 6 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 20 coalitions:
      Current convergence measure: 0.025 [needs 0.001]
      Estimated remaining coalitions: 12
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.05)   8.3 (0.56) 17.5 (0.02) -5.6 (0.03)
      2:          2 42 (0)  2.3 (0.05)  -3.4 (0.08) -5.2 (0.02) -5.6 (0.03)
      3:          3 42 (0)  3.7 (0.05) -18.6 (1.00) -1.4 (0.02) -2.5 (0.03)
                 Day
              <char>
      1: -3.1 (0.57)
      2: -2.0 (0.08)
      3:  1.3 (0.99)
      
      -- Iteration 7 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 22 coalitions:
      Current convergence measure: 0.0057 [needs 0.001]
      Estimated remaining coalitions: 10
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.08)   8.3 (0.03) 17.5 (0.02) -5.6 (0.02)
      2:          2 42 (0)  2.3 (0.21)  -3.4 (0.03) -5.2 (0.02) -5.6 (0.02)
      3:          3 42 (0)  3.7 (0.12) -18.6 (0.03) -1.4 (0.02) -2.5 (0.02)
                 Day
              <char>
      1: -3.1 (0.08)
      2: -2.0 (0.22)
      3:  1.3 (0.13)
      
      -- Iteration 8 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 24 coalitions after 8 iterations, due to:
      Maxium number of iterations (8) reached!
      
      Final estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.03)   8.3 (0.03) 17.5 (0.01) -5.6 (0.01)
      2:          2 42 (0)  2.3 (0.03)  -3.4 (0.03) -5.2 (0.01) -5.6 (0.01)
      3:          3 42 (0)  3.7 (0.03) -18.6 (0.03) -1.4 (0.01) -2.5 (0.01)
                 Day
              <char>
      1: -3.1 (0.02)
      2: -2.0 (0.02)
      3:  1.3 (0.02)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.529   8.267 17.519 -5.584 -3.071
      2:          2 42.44   2.258  -3.348 -5.231 -5.584 -1.975
      3:          3 42.44   3.716 -18.613 -1.439 -2.544  1.311

# output_lm_numeric_indep_conv_max_n_coalitions

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.31 [needs 0.02]
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none      Solar.R         Wind       Temp        Month
              <int> <char>       <char>       <char>     <char>       <char>
      1:          1 42 (0) -0.58 (2.09) -0.58 (2.09) 17.5 (6.0) -0.58 (2.09)
      2:          2 42 (0) -2.19 (0.35) -2.19 (0.35) -5.3 (1.0) -2.19 (0.35)
      3:          3 42 (0) -5.78 (0.74) -5.78 (0.74) -1.5 (1.4) -5.78 (0.74)
                 Day
              <char>
      1: -3.1 (0.81)
      2: -2.0 (0.05)
      3:  1.3 (2.23)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.25 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind       Temp       Month         Day
              <int> <char>     <char>      <char>     <char>      <char>      <char>
      1:          1 42 (0) -4.4 (4.2)   1.3 (1.0) 17.5 (5.5)   1.3 (1.0) -3.1 (1.61)
      2:          2 42 (0)  2.4 (1.7)  -4.5 (1.1) -5.3 (1.8)  -4.5 (1.1) -2.0 (0.83)
      3:          3 42 (0)  3.8 (3.8) -10.6 (2.4) -1.5 (1.3) -10.6 (2.4)  1.3 (1.19)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.16 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind        Temp       Month         Day
              <int> <char>     <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0) -4.5 (2.2)   8.3 (3.8) 17.5 (1.70) -5.6 (3.39) -3.1 (0.35)
      2:          2 42 (0)  2.2 (1.2)  -3.3 (1.1) -5.2 (0.42) -5.6 (0.82) -2.0 (0.30)
      3:          3 42 (0)  3.7 (3.6) -18.6 (4.8) -1.4 (1.60) -2.5 (3.75)  1.3 (0.83)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.042 [needs 0.02]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 40% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.02)   8.3 (0.98) 17.5 (0.03) -5.6 (0.98)
      2:          2 42 (0)  2.2 (0.02)  -3.3 (0.16) -5.2 (0.03) -5.6 (0.16)
      3:          3 42 (0)  3.7 (0.02) -18.6 (1.13) -1.4 (0.03) -2.5 (1.13)
                 Day
              <char>
      1: -3.1 (0.02)
      2: -2.0 (0.02)
      3:  1.3 (0.02)
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 20 coalitions after 5 iterations, due to:
      Standard deviation convergence threshold (0.02) reached: 0.0016!
      Maxium number of coalitions (20) reached!
      
      Final estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.03)   8.3 (0.03) 17.5 (0.04) -5.6 (0.03)
      2:          2 42 (0)  2.2 (0.03)  -3.4 (0.03) -5.2 (0.04) -5.6 (0.03)
      3:          3 42 (0)  3.7 (0.03) -18.6 (0.03) -1.4 (0.04) -2.5 (0.03)
                 Day
              <char>
      1: -3.1 (0.02)
      2: -2.0 (0.02)
      3:  1.3 (0.02)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.548   8.258 17.525 -5.557 -3.076
      2:          2 42.44   2.239  -3.356 -5.224 -5.557 -1.981
      3:          3 42.44   3.697 -18.621 -1.433 -2.517  1.306

# output_lm_numeric_gaussian_group_converges_tol

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.23 [needs 0.1]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none            A            B            C
              <int> <char>       <char>       <char>       <char>
      1:          1 42 (0)   1.04 (2.7)  12.95 (3.3) -1.38 (3.20)
      2:          2 42 (0)   0.87 (3.3) -13.83 (3.5) -0.92 (2.88)
      3:          3 42 (0) -18.10 (4.0)  -0.26 (4.1)  0.80 (0.24)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 8 coalitions after 2 iterations, due to:
      All (8) coalitions used!
      Maxium number of coalitions (8) reached!
      
      Final estimated Shapley values
         explain_id   none      A      B      C
              <int> <char> <char> <char> <char>
      1:          1     42   0.63   13.8  -1.79
      2:          2     42   0.55  -13.2  -1.24
      3:          3     42 -18.13   -0.2   0.76
    Output
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   0.6309  13.7619 -1.7908
      2:          2 42.44   0.5461 -13.1883 -1.2372
      3:          3 42.44 -18.1325  -0.2007  0.7649

# output_lm_numeric_independence_converges_tol_paired

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.1]
      Estimated remaining coalitions: 12
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R        Wind        Temp       Month
              <int> <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0)  1.9 (1.47)  1.9 (1.47) 17.5 (3.41) -5.6 (2.84)
      2:          2 42 (0) -0.5 (0.53) -0.5 (0.53) -5.3 (0.67) -5.6 (0.79)
      3:          3 42 (0) -7.4 (1.34) -7.4 (1.34) -1.5 (0.84) -2.6 (0.75)
                 Day
              <char>
      1: -3.1 (1.56)
      2: -2.0 (0.47)
      3:  1.3 (2.68)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind        Temp      Month         Day
              <int> <char>     <char>      <char>      <char>     <char>      <char>
      1:          1 42 (0) -4.4 (3.3)   8.3 (3.5) 17.5 (2.27) -5.6 (1.4) -3.1 (1.25)
      2:          2 42 (0)  2.4 (1.5)  -3.3 (1.3) -5.3 (0.70) -5.6 (0.4) -2.0 (0.85)
      3:          3 42 (0)  3.8 (4.8) -18.6 (6.1) -1.5 (0.86) -2.6 (1.3)  1.3 (2.16)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.11 [needs 0.1]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 30% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R        Wind        Temp       Month
              <int> <char>      <char>      <char>      <char>      <char>
      1:          1 42 (0) -4.5 (1.22)   8.3 (1.5) 17.5 (1.49) -5.6 (0.02)
      2:          2 42 (0)  2.3 (0.85)  -3.4 (0.3) -5.2 (0.42) -5.6 (0.02)
      3:          3 42 (0)  3.7 (0.93) -18.6 (2.8) -1.4 (0.30) -2.5 (0.02)
                 Day
              <char>
      1: -3.1 (1.76)
      2: -2.0 (0.81)
      3:  1.3 (2.51)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 18 coalitions after 4 iterations, due to:
      Standard deviation convergence threshold (0.1) reached: 0.0016!
      
      Final estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind        Temp       Month
              <int> <char>      <char>       <char>      <char>      <char>
      1:          1 42 (0) -4.5 (0.04)   8.3 (0.02) 17.5 (0.01) -5.6 (0.01)
      2:          2 42 (0)  2.3 (0.04)  -3.3 (0.02) -5.2 (0.01) -5.6 (0.01)
      3:          3 42 (0)  3.7 (0.04) -18.6 (0.02) -1.4 (0.01) -2.5 (0.01)
                 Day
              <char>
      1: -3.1 (0.02)
      2: -2.0 (0.02)
      3:  1.3 (0.02)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.529   8.270 17.513 -5.586 -3.066
      2:          2 42.44   2.258  -3.345 -5.236 -5.586 -1.971
      3:          3 42.44   3.716 -18.610 -1.445 -2.546  1.316

# output_verbose_1

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
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
      1:          1 42.44  -9.152   6.096  15.2603  1.6224 -1.2246
      2:          2 42.44   4.556  -6.421 -11.1745 -0.1350 -0.7046
      3:          3 42.44   7.246 -26.365   0.5284  0.0516  0.9712

# output_verbose_1_3

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.15 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.047 [needs 0.02]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 40% of that (6 coalitions) in the next iteration.
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 24 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 24 coalitions after 5 iterations, due to:
      Standard deviation convergence threshold (0.02) reached: 0.017!
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -9.152   6.096  15.2603  1.6224 -1.2246
      2:          2 42.44   4.556  -6.421 -11.1745 -0.1350 -0.7046
      3:          3 42.44   7.246 -26.365   0.5284  0.0516  0.9712

# output_verbose_1_3_4

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none      Solar.R         Wind         Temp        Month
              <int> <char>       <char>       <char>       <char>       <char>
      1:          1 42 (0) -0.48 (1.77) -0.48 (1.77)  15.31 (5.2) -0.48 (1.77)
      2:          2 42 (0) -0.79 (1.09) -0.79 (1.09) -10.71 (3.3) -0.79 (1.09)
      3:          3 42 (0) -6.25 (0.86) -6.25 (0.86)   0.28 (2.2) -6.25 (0.86)
                  Day
               <char>
      1: -1.27 (0.25)
      2: -0.80 (0.00)
      3:  0.91 (2.27)
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind         Temp       Month
              <int> <char>     <char>      <char>       <char>      <char>
      1:          1 42 (0) -8.8 (4.3)   3.7 (2.0)  15.31 (5.8)   3.7 (2.0)
      2:          2 42 (0)  4.5 (2.7)  -3.4 (1.2) -10.71 (3.6)  -3.4 (1.2)
      3:          3 42 (0)  8.2 (5.6) -13.5 (3.6)   0.28 (1.8) -13.5 (3.6)
                 Day
              <char>
      1: -1.27 (1.7)
      2: -0.80 (1.1)
      3:  0.91 (1.8)
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.15 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind          Temp       Month
              <int> <char>     <char>      <char>        <char>      <char>
      1:          1 42 (0) -8.7 (2.7)   5.3 (2.5)  15.30 (1.65)  2.01 (1.1)
      2:          2 42 (0)  4.7 (2.1)  -7.0 (2.3) -10.79 (0.95)  0.08 (1.7)
      3:          3 42 (0)  7.3 (5.3) -26.1 (7.4)   0.55 (2.46) -0.47 (6.0)
                  Day
               <char>
      1: -1.28 (0.40)
      2: -0.88 (0.21)
      3:  1.18 (0.99)
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.047 [needs 0.02]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 40% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind         Temp        Month
              <int> <char>      <char>       <char>       <char>       <char>
      1:          1 42 (0) -9.1 (0.16)   6.3 (0.60)  15.1 (0.19)  1.30 (0.56)
      2:          2 42 (0)  4.6 (0.16)  -6.1 (0.75) -11.3 (0.31) -0.47 (0.71)
      3:          3 42 (0)  7.3 (0.17) -26.2 (1.86)   0.5 (0.22) -0.16 (1.86)
                  Day
               <char>
      1: -1.01 (0.22)
      2: -0.63 (0.21)
      3:  1.02 (0.13)
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 24 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 24 coalitions after 5 iterations, due to:
      Standard deviation convergence threshold (0.02) reached: 0.017!
      
      Final estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind          Temp        Month
              <int> <char>      <char>       <char>        <char>       <char>
      1:          1 42 (0) -9.2 (0.14)   6.1 (0.35)  15.26 (0.16)  1.62 (0.41)
      2:          2 42 (0)  4.6 (0.13)  -6.4 (0.41) -11.17 (0.25) -0.13 (0.37)
      3:          3 42 (0)  7.2 (0.11) -26.4 (0.18)   0.53 (0.10)  0.05 (0.19)
                  Day
               <char>
      1: -1.22 (0.18)
      2: -0.70 (0.22)
      3:  0.97 (0.10)
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -9.152   6.096  15.2603  1.6224 -1.2246
      2:          2 42.44   4.556  -6.421 -11.1745 -0.1350 -0.7046
      3:          3 42.44   7.246 -26.365   0.5284  0.0516  0.9712

# output_verbose_1_3_4_5

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: gaussian
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none      Solar.R         Wind         Temp        Month
              <int> <char>       <char>       <char>       <char>       <char>
      1:          1 42 (0) -0.48 (1.77) -0.48 (1.77)  15.31 (5.2) -0.48 (1.77)
      2:          2 42 (0) -0.79 (1.09) -0.79 (1.09) -10.71 (3.3) -0.79 (1.09)
      3:          3 42 (0) -6.25 (0.86) -6.25 (0.86)   0.28 (2.2) -6.25 (0.86)
                  Day
               <char>
      1: -1.27 (0.25)
      2: -0.80 (0.00)
      3:  0.91 (2.27)
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind         Temp       Month
              <int> <char>     <char>      <char>       <char>      <char>
      1:          1 42 (0) -8.8 (4.3)   3.7 (2.0)  15.31 (5.8)   3.7 (2.0)
      2:          2 42 (0)  4.5 (2.7)  -3.4 (1.2) -10.71 (3.6)  -3.4 (1.2)
      3:          3 42 (0)  8.2 (5.6) -13.5 (3.6)   0.28 (1.8) -13.5 (3.6)
                 Day
              <char>
      1: -1.27 (1.7)
      2: -0.80 (1.1)
      3:  0.91 (1.8)
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.15 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none    Solar.R        Wind          Temp       Month
              <int> <char>     <char>      <char>        <char>      <char>
      1:          1 42 (0) -8.7 (2.7)   5.3 (2.5)  15.30 (1.65)  2.01 (1.1)
      2:          2 42 (0)  4.7 (2.1)  -7.0 (2.3) -10.79 (0.95)  0.08 (1.7)
      3:          3 42 (0)  7.3 (5.3) -26.1 (7.4)   0.55 (2.46) -0.47 (6.0)
                  Day
               <char>
      1: -1.28 (0.40)
      2: -0.88 (0.21)
      3:  1.18 (0.99)
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.047 [needs 0.02]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 40% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind         Temp        Month
              <int> <char>      <char>       <char>       <char>       <char>
      1:          1 42 (0) -9.1 (0.16)   6.3 (0.60)  15.1 (0.19)  1.30 (0.56)
      2:          2 42 (0)  4.6 (0.16)  -6.1 (0.75) -11.3 (0.31) -0.47 (0.71)
      3:          3 42 (0)  7.3 (0.17) -26.2 (1.86)   0.5 (0.22) -0.16 (1.86)
                  Day
               <char>
      1: -1.01 (0.22)
      2: -0.63 (0.21)
      3:  1.02 (0.13)
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 24 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 24 coalitions after 5 iterations, due to:
      Standard deviation convergence threshold (0.02) reached: 0.017!
      
      Final estimated Shapley values (sd)
         explain_id   none     Solar.R         Wind          Temp        Month
              <int> <char>      <char>       <char>        <char>       <char>
      1:          1 42 (0) -9.2 (0.14)   6.1 (0.35)  15.26 (0.16)  1.62 (0.41)
      2:          2 42 (0)  4.6 (0.13)  -6.4 (0.41) -11.17 (0.25) -0.13 (0.37)
      3:          3 42 (0)  7.2 (0.11) -26.4 (0.18)   0.53 (0.10)  0.05 (0.19)
                  Day
               <char>
      1: -1.22 (0.18)
      2: -0.70 (0.22)
      3:  0.97 (0.10)
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -9.152   6.096  15.2603  1.6224 -1.2246
      2:          2 42.44   4.556  -6.421 -11.1745 -0.1350 -0.7046
      3:          3 42.44   7.246 -26.365   0.5284  0.0516  0.9712

