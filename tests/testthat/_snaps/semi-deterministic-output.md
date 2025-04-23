# output_semi_determ_sampling_iterative_reach_exact

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.31 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00) -0.580 (2.09) -0.580 (2.09) 17.463 (5.97) -0.580 (2.09)
      2: 42.444 (0.00) -2.189 (0.35) -2.189 (0.35) -5.286 (1.02) -2.189 (0.35)
      3: 42.444 (0.00) -5.778 (0.74) -5.778 (0.74) -1.495 (1.42) -5.778 (0.74)
                   Day
                <char>
      1: -3.121 (0.81)
      2: -2.025 (0.05)
      3:  1.261 (2.23)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.411 (4.98)   8.305 (2.80) 17.463 (5.66) -5.635 (2.09)
      2: 42.444 (0.00)  2.376 (1.96)  -3.309 (0.48) -5.286 (1.70) -5.635 (1.86)
      3: 42.444 (0.00)  3.834 (2.10) -18.574 (4.81) -1.495 (1.17) -2.595 (1.56)
                   Day
                <char>
      1: -3.121 (2.66)
      2: -2.025 (0.30)
      3:  1.261 (4.61)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.16 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.549 (0.93)   8.360 (2.80) 17.505 (0.03) -5.594 (0.99)
      2: 42.444 (0.00)  2.238 (1.26)  -3.254 (0.51) -5.244 (0.03) -5.594 (1.24)
      3: 42.444 (0.00)  3.696 (1.80) -18.519 (4.74) -1.453 (0.03) -2.554 (1.53)
                   Day
                <char>
      1: -3.121 (2.66)
      2: -2.025 (0.29)
      3:  1.261 (4.60)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.532 (0.06)   8.262 (0.04) 17.522 (0.03) -5.582 (0.03)
      2: 42.444 (0.00)  2.255 (0.06)  -3.352 (0.04) -5.227 (0.03) -5.582 (0.03)
      3: 42.444 (0.00)  3.713 (0.06) -18.617 (0.04) -1.436 (0.03) -2.542 (0.03)
                   Day
                <char>
      1: -3.068 (0.03)
      2: -1.972 (0.03)
      3:  1.314 (0.03)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.532   8.262 17.522 -5.582 -3.068
      2:          2 42.44   2.255  -3.352 -5.227 -5.582 -1.972
      3:          3 42.44   3.713 -18.617 -1.436 -2.542  1.314

# output_semi_determ_sampling_group_converges_tol

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 8, 
      and is therefore set to 2^n_groups = 8.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: TRUE
      * Number of group-wise Shapley values: 3
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 8 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 6 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none              A              B             C
                <char>         <char>         <char>        <char>
      1: 42.444 (0.00)   1.037 (2.66)  12.949 (3.32) -1.385 (3.20)
      2: 42.444 (0.00)   0.866 (3.28) -13.828 (3.47) -0.917 (2.88)
      3: 42.444 (0.00) -18.102 (3.98)  -0.261 (4.07)  0.795 (0.24)
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   1.0372  12.9494 -1.3845
      2:          2 42.44   0.8661 -13.8283 -0.9173
      3:          3 42.44 -18.1023  -0.2611  0.7951

# output_semi_determ_sampling_ts_timeseries

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      * Model class: <lm>
      * Approach: timeseries
      * Iterative estimation: TRUE
      * Number of group-wise Shapley values: 4
      * Number of observations to explain: 2
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 16 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.29 [needs 0.02]
      Estimated remaining coalitions: 8
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                 none            S1           S2            S3            S4
               <char>        <char>       <char>        <char>        <char>
      1: 4.895 (0.00) -1.375 (0.46) 1.819 (0.96) -1.375 (0.46)  0.590 (0.57)
      2: 4.895 (0.00) -1.450 (0.66) 2.800 (1.28) -1.450 (0.66) -1.877 (0.12)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 16 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.21 [needs 0.02]
      Estimated remaining coalitions: 6
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                 none            S1           S2           S3            S4
               <char>        <char>       <char>       <char>        <char>
      1: 4.895 (0.00) -3.595 (0.99) 1.819 (1.12) 0.845 (1.02)  0.590 (0.87)
      2: 4.895 (0.00) -3.940 (1.23) 2.800 (1.48) 1.039 (1.20) -1.877 (0.48)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 10 of 16 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.3 [needs 0.02]
      Estimated remaining coalitions: 4
      (Concervatively) adding 20% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                 none            S1           S2           S3            S4
               <char>        <char>       <char>       <char>        <char>
      1: 4.895 (0.00) -2.198 (1.01) 1.120 (1.00) 0.146 (0.66)  0.590 (0.10)
      2: 4.895 (0.00) -2.275 (1.20) 1.968 (1.21) 0.207 (0.79) -1.877 (0.03)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 12 of 16 coalitions, 2 new. 
      
      -- Convergence info 
      v Converged after 12 coalitions:
      Maximum number of coalitions reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                 none            S1           S2           S3            S4
               <char>        <char>       <char>       <char>        <char>
      1: 4.895 (0.00) -1.586 (0.77) 0.815 (0.79) 0.292 (0.40)  0.138 (0.48)
      2: 4.895 (0.00) -1.462 (0.93) 1.561 (0.97) 0.399 (0.46) -2.476 (0.59)
         explain_id  none     S1     S2     S3      S4
              <int> <num>  <num>  <num>  <num>   <num>
      1:          1 4.895 -1.586 0.8147 0.2924  0.1376
      2:          2 4.895 -1.462 1.5615 0.3995 -2.4758

