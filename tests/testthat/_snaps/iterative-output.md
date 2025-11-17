# output_lm_numeric_independence_reach_exact

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
         explain_id      none      Solar.R         Wind         Temp        Month
              <int>    <char>       <char>       <char>       <char>       <char>
      1:          1 42.44 (0) -0.58 (2.09) -0.58 (2.09) 17.46 (5.97) -0.58 (2.09)
      2:          2 42.44 (0) -2.19 (0.35) -2.19 (0.35) -5.29 (1.02) -2.19 (0.35)
      3:          3 42.44 (0) -5.78 (0.74) -5.78 (0.74) -1.49 (1.42) -5.78 (0.74)
                  Day
               <char>
      1: -3.12 (0.81)
      2: -2.03 (0.05)
      3:  1.26 (2.23)
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.25 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp         Month
              <int>    <char>       <char>        <char>       <char>        <char>
      1:          1 42.44 (0) -4.41 (4.18)   1.34 (1.01) 17.46 (5.50)   1.34 (1.01)
      2:          2 42.44 (0)  2.38 (1.66)  -4.47 (1.09) -5.29 (1.76)  -4.47 (1.09)
      3:          3 42.44 (0)  3.83 (3.85) -10.58 (2.44) -1.49 (1.30) -10.58 (2.44)
                  Day
               <char>
      1: -3.12 (1.61)
      2: -2.03 (0.83)
      3:  1.26 (1.19)
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.16 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.55 (2.22)   8.31 (3.75) 17.51 (1.70) -5.59 (3.39)
      2:          2 42.44 (0)  2.24 (1.19)  -3.30 (1.10) -5.24 (0.42) -5.59 (0.82)
      3:          3 42.44 (0)  3.70 (3.60) -18.57 (4.75) -1.45 (1.60) -2.55 (3.75)
                  Day
               <char>
      1: -3.08 (0.35)
      2: -1.98 (0.30)
      3:  1.30 (0.83)
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.042 [needs 0.02]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 40% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.55 (0.02)   8.29 (0.98) 17.51 (0.03) -5.57 (0.98)
      2:          2 42.44 (0)  2.24 (0.02)  -3.32 (0.16) -5.24 (0.03) -5.57 (0.16)
      3:          3 42.44 (0)  3.70 (0.02) -18.59 (1.13) -1.44 (0.03) -2.53 (1.13)
                  Day
               <char>
      1: -3.09 (0.02)
      2: -1.99 (0.02)
      3:  1.30 (0.02)
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 24 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 24 coalitions after 5 iterations, due to:
      Standard deviation convergence threshold (0.02) reached: 0.001!
      
      Final estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.54 (0.01)   8.30 (0.02) 17.50 (0.01) -5.59 (0.02)
      2:          2 42.44 (0)  2.25 (0.01)  -3.31 (0.02) -5.24 (0.01) -5.59 (0.02)
      3:          3 42.44 (0)  3.71 (0.01) -18.58 (0.02) -1.45 (0.01) -2.55 (0.02)
                  Day
               <char>
      1: -3.08 (0.02)
      2: -1.98 (0.02)
      3:  1.30 (0.02)
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
         explain_id      none      Solar.R         Wind         Temp        Month
              <int>    <char>       <char>       <char>       <char>       <char>
      1:          1 42.44 (0)  1.92 (1.47)  1.92 (1.47) 17.49 (3.41) -5.61 (2.84)
      2:          2 42.44 (0) -0.50 (0.53) -0.50 (0.53) -5.26 (0.67) -5.61 (0.79)
      3:          3 42.44 (0) -7.40 (1.34) -7.40 (1.34) -1.47 (0.84) -2.57 (0.75)
                  Day
               <char>
      1: -3.12 (1.56)
      2: -2.03 (0.47)
      3:  1.26 (2.68)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.44 (3.27)   8.28 (3.54) 17.49 (2.27) -5.61 (1.40)
      2:          2 42.44 (0)  2.35 (1.47)  -3.34 (1.31) -5.26 (0.70) -5.61 (0.40)
      3:          3 42.44 (0)  3.81 (4.83) -18.60 (6.10) -1.47 (0.86) -2.57 (1.27)
                  Day
               <char>
      1: -3.12 (1.25)
      2: -2.03 (0.85)
      3:  1.26 (2.16)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.11 [needs 0.1]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 30% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (1.22)   8.27 (1.54) 17.52 (1.49) -5.59 (0.02)
      2:          2 42.44 (0)  2.26 (0.85)  -3.35 (0.30) -5.23 (0.42) -5.59 (0.02)
      3:          3 42.44 (0)  3.71 (0.93) -18.61 (2.75) -1.44 (0.30) -2.55 (0.02)
                  Day
               <char>
      1: -3.06 (1.76)
      2: -1.97 (0.81)
      3:  1.32 (2.51)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 18 coalitions after 4 iterations, due to:
      Standard deviation convergence threshold (0.1) reached: 0.0016!
      
      Final estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (0.04)   8.27 (0.02) 17.51 (0.01) -5.59 (0.01)
      2:          2 42.44 (0)  2.26 (0.04)  -3.34 (0.02) -5.24 (0.01) -5.59 (0.01)
      3:          3 42.44 (0)  3.72 (0.04) -18.61 (0.02) -1.44 (0.01) -2.55 (0.01)
                  Day
               <char>
      1: -3.07 (0.02)
      2: -1.97 (0.02)
      3:  1.32 (0.02)
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
         explain_id      none      Solar.R         Wind         Temp        Month
              <int>    <char>       <char>       <char>       <char>       <char>
      1:          1 42.44 (0)  1.92 (1.47)  1.92 (1.47) 17.49 (3.41) -5.61 (2.84)
      2:          2 42.44 (0) -0.50 (0.53) -0.50 (0.53) -5.26 (0.67) -5.61 (0.79)
      3:          3 42.44 (0) -7.40 (1.34) -7.40 (1.34) -1.47 (0.84) -2.57 (0.75)
                  Day
               <char>
      1: -3.12 (1.56)
      2: -2.03 (0.47)
      3:  1.26 (2.68)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.001]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.44 (3.27)   8.28 (3.54) 17.49 (2.27) -5.61 (1.40)
      2:          2 42.44 (0)  2.35 (1.47)  -3.34 (1.31) -5.26 (0.70) -5.61 (0.40)
      3:          3 42.44 (0)  3.81 (4.83) -18.60 (6.10) -1.47 (0.86) -2.57 (1.27)
                  Day
               <char>
      1: -3.12 (1.25)
      2: -2.03 (0.85)
      3:  1.26 (2.16)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.15 [needs 0.001]
      Estimated remaining coalitions: 18
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.44 (2.22)   8.28 (2.33) 17.49 (0.02) -5.61 (0.02)
      2:          2 42.44 (0)  2.35 (1.16)  -3.34 (1.01) -5.26 (0.02) -5.61 (0.02)
      3:          3 42.44 (0)  3.81 (3.92) -18.60 (4.10) -1.47 (0.02) -2.57 (0.02)
                  Day
               <char>
      1: -3.12 (0.40)
      2: -2.03 (0.69)
      3:  1.26 (0.71)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.067 [needs 0.001]
      Estimated remaining coalitions: 16
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.54 (0.92)   8.31 (0.90) 17.50 (0.02) -5.60 (0.02)
      2:          2 42.44 (0)  2.25 (0.52)  -3.31 (0.40) -5.25 (0.02) -5.60 (0.02)
      3:          3 42.44 (0)  3.71 (1.57) -18.57 (1.57) -1.46 (0.02) -2.56 (0.02)
                  Day
               <char>
      1: -3.07 (0.13)
      2: -1.97 (0.37)
      3:  1.31 (0.22)
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.0015 [needs 0.001]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (0.03)   8.30 (0.03) 17.51 (0.02) -5.59 (0.02)
      2:          2 42.44 (0)  2.26 (0.03)  -3.32 (0.03) -5.24 (0.02) -5.59 (0.02)
      3:          3 42.44 (0)  3.72 (0.03) -18.58 (0.03) -1.45 (0.02) -2.55 (0.02)
                  Day
               <char>
      1: -3.08 (0.03)
      2: -1.98 (0.03)
      3:  1.30 (0.03)
      
      -- Iteration 6 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 20 coalitions:
      Current convergence measure: 0.025 [needs 0.001]
      Estimated remaining coalitions: 12
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (0.05)   8.26 (0.56) 17.51 (0.02) -5.58 (0.03)
      2:          2 42.44 (0)  2.26 (0.05)  -3.35 (0.08) -5.23 (0.02) -5.58 (0.03)
      3:          3 42.44 (0)  3.72 (0.05) -18.62 (1.00) -1.44 (0.02) -2.54 (0.03)
                  Day
               <char>
      1: -3.06 (0.57)
      2: -1.97 (0.08)
      3:  1.32 (0.99)
      
      -- Iteration 7 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 22 coalitions:
      Current convergence measure: 0.0057 [needs 0.001]
      Estimated remaining coalitions: 10
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (0.08)   8.26 (0.03) 17.52 (0.02) -5.58 (0.02)
      2:          2 42.44 (0)  2.26 (0.21)  -3.35 (0.03) -5.23 (0.02) -5.58 (0.02)
      3:          3 42.44 (0)  3.71 (0.12) -18.61 (0.03) -1.44 (0.02) -2.54 (0.02)
                  Day
               <char>
      1: -3.07 (0.08)
      2: -1.97 (0.22)
      3:  1.31 (0.13)
      
      -- Iteration 8 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 24 coalitions after 8 iterations, due to:
      Maximum number of iterations (8) reached!
      
      Final estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (0.03)   8.27 (0.03) 17.52 (0.01) -5.58 (0.01)
      2:          2 42.44 (0)  2.26 (0.03)  -3.35 (0.03) -5.23 (0.01) -5.58 (0.01)
      3:          3 42.44 (0)  3.72 (0.03) -18.61 (0.03) -1.44 (0.01) -2.54 (0.01)
                  Day
               <char>
      1: -3.07 (0.02)
      2: -1.98 (0.02)
      3:  1.31 (0.02)
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
         explain_id      none      Solar.R         Wind         Temp        Month
              <int>    <char>       <char>       <char>       <char>       <char>
      1:          1 42.44 (0) -0.58 (2.09) -0.58 (2.09) 17.46 (5.97) -0.58 (2.09)
      2:          2 42.44 (0) -2.19 (0.35) -2.19 (0.35) -5.29 (1.02) -2.19 (0.35)
      3:          3 42.44 (0) -5.78 (0.74) -5.78 (0.74) -1.49 (1.42) -5.78 (0.74)
                  Day
               <char>
      1: -3.12 (0.81)
      2: -2.03 (0.05)
      3:  1.26 (2.23)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.25 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp         Month
              <int>    <char>       <char>        <char>       <char>        <char>
      1:          1 42.44 (0) -4.41 (4.18)   1.34 (1.01) 17.46 (5.50)   1.34 (1.01)
      2:          2 42.44 (0)  2.38 (1.66)  -4.47 (1.09) -5.29 (1.76)  -4.47 (1.09)
      3:          3 42.44 (0)  3.83 (3.85) -10.58 (2.44) -1.49 (1.30) -10.58 (2.44)
                  Day
               <char>
      1: -3.12 (1.61)
      2: -2.03 (0.83)
      3:  1.26 (1.19)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.16 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.55 (2.22)   8.31 (3.75) 17.51 (1.70) -5.59 (3.39)
      2:          2 42.44 (0)  2.24 (1.19)  -3.30 (1.10) -5.24 (0.42) -5.59 (0.82)
      3:          3 42.44 (0)  3.70 (3.60) -18.57 (4.75) -1.45 (1.60) -2.55 (3.75)
                  Day
               <char>
      1: -3.08 (0.35)
      2: -1.98 (0.30)
      3:  1.30 (0.83)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.042 [needs 0.02]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 40% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.55 (0.02)   8.29 (0.98) 17.51 (0.03) -5.57 (0.98)
      2:          2 42.44 (0)  2.24 (0.02)  -3.32 (0.16) -5.24 (0.03) -5.57 (0.16)
      3:          3 42.44 (0)  3.70 (0.02) -18.59 (1.13) -1.44 (0.03) -2.53 (1.13)
                  Day
               <char>
      1: -3.09 (0.02)
      2: -1.99 (0.02)
      3:  1.30 (0.02)
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 20 coalitions after 5 iterations, due to:
      Standard deviation convergence threshold (0.02) reached: 0.0016!
      Maximum number of coalitions (20) reached!
      
      Final estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.55 (0.03)   8.26 (0.03) 17.53 (0.04) -5.56 (0.03)
      2:          2 42.44 (0)  2.24 (0.03)  -3.36 (0.03) -5.22 (0.04) -5.56 (0.03)
      3:          3 42.44 (0)  3.70 (0.03) -18.62 (0.03) -1.43 (0.04) -2.52 (0.03)
                  Day
               <char>
      1: -3.08 (0.02)
      2: -1.98 (0.02)
      3:  1.31 (0.02)
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
         explain_id      none             A             B            C
              <int>    <char>        <char>        <char>       <char>
      1:          1 42.44 (0)   1.04 (2.66)  12.95 (3.32) -1.38 (3.20)
      2:          2 42.44 (0)   0.87 (3.28) -13.83 (3.47) -0.92 (2.88)
      3:          3 42.44 (0) -18.10 (3.98)  -0.26 (4.07)  0.80 (0.24)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 8 coalitions after 2 iterations, due to:
      All (8) coalitions used!
      Maximum number of coalitions (8) reached!
      
      Final estimated Shapley values
         explain_id   none      A      B      C
              <int> <char> <char> <char> <char>
      1:          1  42.44   0.63  13.76  -1.79
      2:          2  42.44   0.55 -13.19  -1.24
      3:          3  42.44 -18.13  -0.20   0.76
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
         explain_id      none      Solar.R         Wind         Temp        Month
              <int>    <char>       <char>       <char>       <char>       <char>
      1:          1 42.44 (0)  1.92 (1.47)  1.92 (1.47) 17.49 (3.41) -5.61 (2.84)
      2:          2 42.44 (0) -0.50 (0.53) -0.50 (0.53) -5.26 (0.67) -5.61 (0.79)
      3:          3 42.44 (0) -7.40 (1.34) -7.40 (1.34) -1.47 (0.84) -2.57 (0.75)
                  Day
               <char>
      1: -3.12 (1.56)
      2: -2.03 (0.47)
      3:  1.26 (2.68)
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.44 (3.27)   8.28 (3.54) 17.49 (2.27) -5.61 (1.40)
      2:          2 42.44 (0)  2.35 (1.47)  -3.34 (1.31) -5.26 (0.70) -5.61 (0.40)
      3:          3 42.44 (0)  3.81 (4.83) -18.60 (6.10) -1.47 (0.86) -2.57 (1.27)
                  Day
               <char>
      1: -3.12 (1.25)
      2: -2.03 (0.85)
      3:  1.26 (2.16)
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.11 [needs 0.1]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 30% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (1.22)   8.27 (1.54) 17.52 (1.49) -5.59 (0.02)
      2:          2 42.44 (0)  2.26 (0.85)  -3.35 (0.30) -5.23 (0.42) -5.59 (0.02)
      3:          3 42.44 (0)  3.71 (0.93) -18.61 (2.75) -1.44 (0.30) -2.55 (0.02)
                  Day
               <char>
      1: -3.06 (1.76)
      2: -1.97 (0.81)
      3:  1.32 (2.51)
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 18 coalitions after 4 iterations, due to:
      Standard deviation convergence threshold (0.1) reached: 0.0016!
      
      Final estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind         Temp        Month
              <int>    <char>       <char>        <char>       <char>       <char>
      1:          1 42.44 (0) -4.53 (0.04)   8.27 (0.02) 17.51 (0.01) -5.59 (0.01)
      2:          2 42.44 (0)  2.26 (0.04)  -3.34 (0.02) -5.24 (0.01) -5.59 (0.01)
      3:          3 42.44 (0)  3.72 (0.04) -18.61 (0.02) -1.44 (0.01) -2.55 (0.01)
                  Day
               <char>
      1: -3.07 (0.02)
      2: -1.97 (0.02)
      3:  1.32 (0.02)
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
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
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
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
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
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
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
         explain_id      none      Solar.R         Wind          Temp        Month
              <int>    <char>       <char>       <char>        <char>       <char>
      1:          1 42.44 (0) -0.48 (1.77) -0.48 (1.77)  15.31 (5.22) -0.48 (1.77)
      2:          2 42.44 (0) -0.79 (1.09) -0.79 (1.09) -10.71 (3.28) -0.79 (1.09)
      3:          3 42.44 (0) -6.25 (0.86) -6.25 (0.86)   0.28 (2.16) -6.25 (0.86)
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
         explain_id      none      Solar.R          Wind          Temp         Month
              <int>    <char>       <char>        <char>        <char>        <char>
      1:          1 42.44 (0) -8.75 (4.26)   3.66 (1.98)  15.31 (5.78)   3.66 (1.98)
      2:          2 42.44 (0)  4.47 (2.66)  -3.42 (1.25) -10.71 (3.63)  -3.42 (1.25)
      3:          3 42.44 (0)  8.16 (5.59) -13.46 (3.59)   0.28 (1.85) -13.46 (3.59)
                  Day
               <char>
      1: -1.27 (1.67)
      2: -0.80 (1.10)
      3:  0.91 (1.85)
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.15 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind          Temp        Month
              <int>    <char>       <char>        <char>        <char>       <char>
      1:          1 42.44 (0) -8.74 (2.74)   5.30 (2.46)  15.30 (1.65)  2.01 (1.13)
      2:          2 42.44 (0)  4.73 (2.08)  -7.02 (2.31) -10.79 (0.95)  0.08 (1.70)
      3:          3 42.44 (0)  7.27 (5.34) -26.09 (7.43)   0.55 (2.46) -0.47 (5.99)
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
         explain_id      none      Solar.R          Wind          Temp        Month
              <int>    <char>       <char>        <char>        <char>       <char>
      1:          1 42.44 (0) -9.07 (0.16)   6.31 (0.60)  15.07 (0.19)  1.30 (0.56)
      2:          2 42.44 (0)  4.60 (0.16)  -6.12 (0.75) -11.27 (0.31) -0.47 (0.71)
      3:          3 42.44 (0)  7.30 (0.17) -26.22 (1.86)   0.50 (0.22) -0.16 (1.86)
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
         explain_id      none      Solar.R          Wind          Temp        Month
              <int>    <char>       <char>        <char>        <char>       <char>
      1:          1 42.44 (0) -9.15 (0.14)   6.10 (0.35)  15.26 (0.16)  1.62 (0.41)
      2:          2 42.44 (0)  4.56 (0.13)  -6.42 (0.41) -11.17 (0.25) -0.13 (0.37)
      3:          3 42.44 (0)  7.25 (0.11) -26.37 (0.18)   0.53 (0.10)  0.05 (0.19)
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
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 32`, and is therefore set to `2^n_features = 32`.
      
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
         explain_id      none      Solar.R         Wind          Temp        Month
              <int>    <char>       <char>       <char>        <char>       <char>
      1:          1 42.44 (0) -0.48 (1.77) -0.48 (1.77)  15.31 (5.22) -0.48 (1.77)
      2:          2 42.44 (0) -0.79 (1.09) -0.79 (1.09) -10.71 (3.28) -0.79 (1.09)
      3:          3 42.44 (0) -6.25 (0.86) -6.25 (0.86)   0.28 (2.16) -6.25 (0.86)
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
         explain_id      none      Solar.R          Wind          Temp         Month
              <int>    <char>       <char>        <char>        <char>        <char>
      1:          1 42.44 (0) -8.75 (4.26)   3.66 (1.98)  15.31 (5.78)   3.66 (1.98)
      2:          2 42.44 (0)  4.47 (2.66)  -3.42 (1.25) -10.71 (3.63)  -3.42 (1.25)
      3:          3 42.44 (0)  8.16 (5.59) -13.46 (3.59)   0.28 (1.85) -13.46 (3.59)
                  Day
               <char>
      1: -1.27 (1.67)
      2: -0.80 (1.10)
      3:  0.91 (1.85)
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.15 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none      Solar.R          Wind          Temp        Month
              <int>    <char>       <char>        <char>        <char>       <char>
      1:          1 42.44 (0) -8.74 (2.74)   5.30 (2.46)  15.30 (1.65)  2.01 (1.13)
      2:          2 42.44 (0)  4.73 (2.08)  -7.02 (2.31) -10.79 (0.95)  0.08 (1.70)
      3:          3 42.44 (0)  7.27 (5.34) -26.09 (7.43)   0.55 (2.46) -0.47 (5.99)
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
         explain_id      none      Solar.R          Wind          Temp        Month
              <int>    <char>       <char>        <char>        <char>       <char>
      1:          1 42.44 (0) -9.07 (0.16)   6.31 (0.60)  15.07 (0.19)  1.30 (0.56)
      2:          2 42.44 (0)  4.60 (0.16)  -6.12 (0.75) -11.27 (0.31) -0.47 (0.71)
      3:          3 42.44 (0)  7.30 (0.17) -26.22 (1.86)   0.50 (0.22) -0.16 (1.86)
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
         explain_id      none      Solar.R          Wind          Temp        Month
              <int>    <char>       <char>        <char>        <char>       <char>
      1:          1 42.44 (0) -9.15 (0.14)   6.10 (0.35)  15.26 (0.16)  1.62 (0.41)
      2:          2 42.44 (0)  4.56 (0.13)  -6.42 (0.41) -11.17 (0.25) -0.13 (0.37)
      3:          3 42.44 (0)  7.25 (0.11) -26.37 (0.18)   0.53 (0.10)  0.05 (0.19)
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

