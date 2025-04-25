# output_semi_determ_iterative_reach_exact

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
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
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
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.25 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp          Month
                <char>        <char>         <char>        <char>         <char>
      1: 42.444 (0.00) -4.411 (4.18)   1.335 (1.01) 17.463 (5.50)   1.335 (1.01)
      2: 42.444 (0.00)  2.376 (1.66)  -4.472 (1.09) -5.286 (1.76)  -4.472 (1.09)
      3: 42.444 (0.00)  3.834 (3.85) -10.585 (2.44) -1.495 (1.30) -10.585 (2.44)
                   Day
                <char>
      1: -3.121 (1.61)
      2: -2.025 (0.83)
      3:  1.261 (1.19)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.16 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.549 (2.22)   8.311 (3.75) 17.506 (1.70) -5.587 (3.39)
      2: 42.444 (0.00)  2.238 (1.19)  -3.303 (1.10) -5.244 (0.42) -5.587 (0.82)
      3: 42.444 (0.00)  3.696 (3.60) -18.568 (4.75) -1.452 (1.60) -2.547 (3.75)
                   Day
                <char>
      1: -3.078 (0.35)
      2: -1.983 (0.30)
      3:  1.304 (0.83)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.042 [needs 0.02]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 40% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.546 (0.02)   8.294 (0.98) 17.513 (0.03) -5.573 (0.98)
      2: 42.444 (0.00)  2.241 (0.02)  -3.321 (0.16) -5.236 (0.03) -5.573 (0.16)
      3: 42.444 (0.00)  3.699 (0.02) -18.586 (1.13) -1.445 (0.03) -2.533 (1.13)
                   Day
                <char>
      1: -3.085 (0.02)
      2: -1.990 (0.02)
      3:  1.297 (0.02)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 20 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.538 (0.00)   8.268 (0.00) 17.531 (0.00) -5.587 (0.00)
      2: 42.444 (0.00)  2.249 (0.00)  -3.347 (0.00) -5.218 (0.00) -5.587 (0.00)
      3: 42.444 (0.00)  3.707 (0.00) -18.611 (0.00) -1.427 (0.00) -2.547 (0.00)
                   Day
                <char>
      1: -3.072 (0.00)
      2: -1.977 (0.00)
      3:  1.310 (0.00)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.538   8.268 17.531 -5.587 -3.072
      2:          2 42.44   2.249  -3.347 -5.218 -5.587 -1.977
      3:          3 42.44   3.707 -18.611 -1.427 -2.547  1.310

# output_semi_determ_group_converges_tol

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
      i Not converged after 6 coalitions:
      Current convergence measure: 0.23 [needs 0.00001]
      Estimated remaining coalitions: 2
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none              A              B             C
                <char>         <char>         <char>        <char>
      1: 42.444 (0.00)   1.037 (2.66)  12.949 (3.32) -1.385 (3.20)
      2: 42.444 (0.00)   0.866 (3.28) -13.828 (3.47) -0.917 (2.88)
      3: 42.444 (0.00) -18.102 (3.98)  -0.261 (4.07)  0.795 (0.24)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 8 coalitions, 2 new. 
      
      -- Convergence info 
      v Converged after 8 coalitions:
      All (8) coalitions used.
      Maximum number of coalitions reached!
      
      -- Final estimated Shapley values 
    Output
           none       A       B      C
         <char>  <char>  <char> <char>
      1: 42.444   0.631  13.762 -1.791
      2: 42.444   0.546 -13.188 -1.237
      3: 42.444 -18.133  -0.201  0.765
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
      Estimated remaining coalitions: 10
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
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
      Estimated remaining coalitions: 8
      (Conservatively) adding about 20% of that (2 coalitions) in the next iteration.
      
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
      Estimated remaining coalitions: 6
      (Conservatively) adding about 30% of that (2 coalitions) in the next iteration.
      
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

