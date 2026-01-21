# output_semi_determ_iterative_reach_exact

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
      Standard deviation convergence threshold (0.02) reached: 0.00021!
      
      Final estimated Shapley values (sd)
         explain_id      none   Solar.R       Wind      Temp     Month       Day
              <int>    <char>    <char>     <char>    <char>    <char>    <char>
      1:          1 42.44 (0) -4.54 (0)   8.27 (0) 17.53 (0) -5.58 (0) -3.07 (0)
      2:          2 42.44 (0)  2.25 (0)  -3.34 (0) -5.22 (0) -5.58 (0) -1.97 (0)
      3:          3 42.44 (0)  3.70 (0) -18.61 (0) -1.43 (0) -2.54 (0)  1.31 (0)
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.541   8.271 17.526 -5.584 -3.069
      2:          2 42.44   2.246  -3.344 -5.224 -5.584 -1.974
      3:          3 42.44   3.704 -18.609 -1.432 -2.544  1.313

# output_semi_determ_group_converges_tol

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
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 3
      * Feature groups: A: {"Solar.R", "Wind"}; B: {"Temp", "Month"}; C: {"Day"}
      * Number of observations to explain: 3
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 8 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.23 [needs 1e-05]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id      none             A             B            C
              <int>    <char>        <char>        <char>       <char>
      1:          1 42.44 (0)   1.04 (2.66)  12.95 (3.32) -1.38 (3.20)
      2:          2 42.44 (0)   0.87 (3.28) -13.83 (3.47) -0.92 (2.88)
      3:          3 42.44 (0) -18.10 (3.98)  -0.26 (4.07)  0.80 (0.24)
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 8 coalitions, 2 new. 
      
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

# output_semi_determ_ts_timeseries

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain()` -------------------------------------------------
      
      -- Explanation overview --
      
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: timeseries
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 4
      * Feature groups: S1: {"X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
      "X10"}; S2: {"X11", "X12", "X13", "X14", "X15", "X16", "X17", "X18", "X19",
      "X20"}; S3: {"X21", "X22", "X23", "X24", "X25", "X26", "X27", "X28", "X29",
      "X30"}; S4: {"X31", "X32", "X33", "X34", "X35", "X36", "X37", "X38", "X39",
      "X40"}
      * Number of observations to explain: 2
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 16 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.29 [needs 0.02]
      Estimated remaining coalitions: 10
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id    none           S1          S2           S3           S4
              <int>  <char>       <char>      <char>       <char>       <char>
      1:          1 4.9 (0) -1.38 (0.46) 1.82 (0.96) -1.38 (0.46)  0.59 (0.57)
      2:          2 4.9 (0) -1.45 (0.66) 2.80 (1.28) -1.45 (0.66) -1.88 (0.12)
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 16 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.21 [needs 0.02]
      Estimated remaining coalitions: 8
      (Conservatively) adding about 20% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id    none           S1          S2          S3           S4
              <int>  <char>       <char>      <char>      <char>       <char>
      1:          1 4.9 (0) -3.60 (0.99) 1.82 (1.12) 0.84 (1.02)  0.59 (0.87)
      2:          2 4.9 (0) -3.94 (1.23) 2.80 (1.48) 1.04 (1.20) -1.88 (0.48)
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 10 of 16 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.3 [needs 0.02]
      Estimated remaining coalitions: 6
      (Conservatively) adding about 30% of that (2 coalitions) in the next iteration.
      
      Current estimated Shapley values (sd)
         explain_id    none           S1          S2          S3           S4
              <int>  <char>       <char>      <char>      <char>       <char>
      1:          1 4.9 (0) -2.20 (1.01) 1.12 (1.00) 0.15 (0.66)  0.59 (0.10)
      2:          2 4.9 (0) -2.28 (1.20) 1.97 (1.21) 0.21 (0.79) -1.88 (0.03)
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 12 of 16 coalitions, 2 new. 
      
      -- Convergence info 
      v Iterative Shapley value estimation stopped at 12 coalitions after 4 iterations, due to:
      Maximum number of coalitions (12) reached!
      
      Final estimated Shapley values (sd)
         explain_id    none           S1          S2          S3           S4
              <int>  <char>       <char>      <char>      <char>       <char>
      1:          1 4.9 (0) -1.59 (0.77) 0.81 (0.79) 0.29 (0.40)  0.14 (0.48)
      2:          2 4.9 (0) -1.46 (0.93) 1.56 (0.97) 0.40 (0.46) -2.48 (0.59)
    Output
         explain_id  none     S1     S2     S3      S4
              <int> <num>  <num>  <num>  <num>   <num>
      1:          1 4.895 -1.586 0.8147 0.2924  0.1376
      2:          2 4.895 -1.462 1.5615 0.3995 -2.4758

