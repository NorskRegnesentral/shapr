# output_lm_numeric_independence_reach_exact

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
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
      v Converged after 20 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.548 (0.03)   8.258 (0.03) 17.525 (0.04) -5.557 (0.03)
      2: 42.444 (0.00)  2.239 (0.03)  -3.356 (0.03) -5.224 (0.04) -5.557 (0.03)
      3: 42.444 (0.00)  3.697 (0.03) -18.621 (0.03) -1.433 (0.04) -2.517 (0.03)
                   Day
                <char>
      1: -3.076 (0.02)
      2: -1.981 (0.02)
      3:  1.306 (0.02)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.548   8.258 17.525 -5.557 -3.076
      2:          2 42.44   2.239  -3.356 -5.224 -5.557 -1.981
      3:          3 42.44   3.697 -18.621 -1.433 -2.517  1.306

# output_lm_numeric_independence_converges_tol

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.1]
      Estimated remaining coalitions: 10
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.919 (1.47)  1.919 (1.47) 17.492 (3.41) -5.607 (2.84)
      2: 42.444 (0.00) -0.495 (0.53) -0.495 (0.53) -5.257 (0.67) -5.607 (0.79)
      3: 42.444 (0.00) -7.399 (1.34) -7.399 (1.34) -1.466 (0.84) -2.567 (0.75)
                   Day
                <char>
      1: -3.121 (1.56)
      2: -2.025 (0.47)
      3:  1.261 (2.68)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 20% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.439 (3.27)   8.277 (3.54) 17.491 (2.27) -5.607 (1.40)
      2: 42.444 (0.00)  2.348 (1.47)  -3.337 (1.31) -5.258 (0.70) -5.607 (0.40)
      3: 42.444 (0.00)  3.806 (4.83) -18.602 (6.10) -1.467 (0.86) -2.567 (1.27)
                   Day
                <char>
      1: -3.121 (1.25)
      2: -2.025 (0.85)
      3:  1.261 (2.16)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 14 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.437 (2.22)   8.279 (2.33) 17.490 (0.02) -5.609 (0.02)
      2: 42.444 (0.00)  2.350 (1.16)  -3.335 (1.01) -5.260 (0.02) -5.609 (0.02)
      3: 42.444 (0.00)  3.808 (3.92) -18.600 (4.10) -1.468 (0.02) -2.569 (0.02)
                   Day
                <char>
      1: -3.121 (0.40)
      2: -2.025 (0.69)
      3:  1.261 (0.71)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.437   8.279 17.490 -5.609 -3.121
      2:          2 42.44   2.350  -3.335 -5.260 -5.609 -2.025
      3:          3 42.44   3.808 -18.600 -1.468 -2.569  1.261

# output_lm_numeric_independence_converges_maxit

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.001]
      Estimated remaining coalitions: 22
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.919 (1.47)  1.919 (1.47) 17.492 (3.41) -5.607 (2.84)
      2: 42.444 (0.00) -0.495 (0.53) -0.495 (0.53) -5.257 (0.67) -5.607 (0.79)
      3: 42.444 (0.00) -7.399 (1.34) -7.399 (1.34) -1.466 (0.84) -2.567 (0.75)
                   Day
                <char>
      1: -3.121 (1.56)
      2: -2.025 (0.47)
      3:  1.261 (2.68)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.001]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.439 (3.27)   8.277 (3.54) 17.491 (2.27) -5.607 (1.40)
      2: 42.444 (0.00)  2.348 (1.47)  -3.337 (1.31) -5.258 (0.70) -5.607 (0.40)
      3: 42.444 (0.00)  3.806 (4.83) -18.602 (6.10) -1.467 (0.86) -2.567 (1.27)
                   Day
                <char>
      1: -3.121 (1.25)
      2: -2.025 (0.85)
      3:  1.261 (2.16)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.15 [needs 0.001]
      Estimated remaining coalitions: 18
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.437 (2.22)   8.279 (2.33) 17.490 (0.02) -5.609 (0.02)
      2: 42.444 (0.00)  2.350 (1.16)  -3.335 (1.01) -5.260 (0.02) -5.609 (0.02)
      3: 42.444 (0.00)  3.808 (3.92) -18.600 (4.10) -1.468 (0.02) -2.569 (0.02)
                   Day
                <char>
      1: -3.121 (0.40)
      2: -2.025 (0.69)
      3:  1.261 (0.71)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.067 [needs 0.001]
      Estimated remaining coalitions: 16
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.539 (0.92)   8.307 (0.90) 17.500 (0.02) -5.598 (0.02)
      2: 42.444 (0.00)  2.248 (0.52)  -3.307 (0.40) -5.249 (0.02) -5.598 (0.02)
      3: 42.444 (0.00)  3.706 (1.57) -18.572 (1.57) -1.458 (0.02) -2.558 (0.02)
                   Day
                <char>
      1: -3.068 (0.13)
      2: -1.973 (0.37)
      3:  1.314 (0.22)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.0015 [needs 0.001]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.529 (0.03)   8.295 (0.03) 17.507 (0.02) -5.592 (0.02)
      2: 42.444 (0.00)  2.258 (0.03)  -3.320 (0.03) -5.243 (0.02) -5.592 (0.02)
      3: 42.444 (0.00)  3.716 (0.03) -18.584 (0.03) -1.451 (0.02) -2.552 (0.02)
                   Day
                <char>
      1: -3.079 (0.03)
      2: -1.983 (0.03)
      3:  1.303 (0.03)
    Message
      
      -- Iteration 6 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 20 coalitions:
      Current convergence measure: 0.025 [needs 0.001]
      Estimated remaining coalitions: 12
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.527 (0.05)   8.262 (0.56) 17.515 (0.02) -5.584 (0.03)
      2: 42.444 (0.00)  2.260 (0.05)  -3.353 (0.08) -5.235 (0.02) -5.584 (0.03)
      3: 42.444 (0.00)  3.718 (0.05) -18.618 (1.00) -1.443 (0.02) -2.544 (0.03)
                   Day
                <char>
      1: -3.064 (0.57)
      2: -1.969 (0.08)
      3:  1.318 (0.99)
    Message
      
      -- Iteration 7 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 22 coalitions:
      Current convergence measure: 0.0057 [needs 0.001]
      Estimated remaining coalitions: 10
      (Conservatively) adding about 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.531 (0.08)   8.265 (0.03) 17.518 (0.02) -5.581 (0.02)
      2: 42.444 (0.00)  2.256 (0.21)  -3.350 (0.03) -5.232 (0.02) -5.581 (0.02)
      3: 42.444 (0.00)  3.714 (0.12) -18.615 (0.03) -1.440 (0.02) -2.541 (0.02)
                   Day
                <char>
      1: -3.068 (0.08)
      2: -1.972 (0.22)
      3:  1.314 (0.13)
    Message
      
      -- Iteration 8 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 24 coalitions:
      Maximum number of iterations reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.529 (0.03)   8.267 (0.03) 17.519 (0.01) -5.584 (0.01)
      2: 42.444 (0.00)  2.258 (0.03)  -3.348 (0.03) -5.231 (0.01) -5.584 (0.01)
      3: 42.444 (0.00)  3.716 (0.03) -18.613 (0.03) -1.439 (0.01) -2.544 (0.01)
                   Day
                <char>
      1: -3.071 (0.02)
      2: -1.975 (0.02)
      3:  1.311 (0.02)
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
      
      -- Iteration 1 -----------------------------------------------------------------
      
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
      
      -- Convergence info 
      v Converged after 20 coalitions:
      Convergence tolerance reached!
      Maximum number of coalitions reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.548 (0.03)   8.258 (0.03) 17.525 (0.04) -5.557 (0.03)
      2: 42.444 (0.00)  2.239 (0.03)  -3.356 (0.03) -5.224 (0.04) -5.557 (0.03)
      3: 42.444 (0.00)  3.697 (0.03) -18.621 (0.03) -1.433 (0.04) -2.517 (0.03)
                   Day
                <char>
      1: -3.076 (0.02)
      2: -1.981 (0.02)
      3:  1.306 (0.02)
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
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.23 [needs 0.1]
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

# output_lm_numeric_independence_converges_tol_paired

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.1]
      Estimated remaining coalitions: 10
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.919 (1.47)  1.919 (1.47) 17.492 (3.41) -5.607 (2.84)
      2: 42.444 (0.00) -0.495 (0.53) -0.495 (0.53) -5.257 (0.67) -5.607 (0.79)
      3: 42.444 (0.00) -7.399 (1.34) -7.399 (1.34) -1.466 (0.84) -2.567 (0.75)
                   Day
                <char>
      1: -3.121 (1.56)
      2: -2.025 (0.47)
      3:  1.261 (2.68)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 14
      (Conservatively) adding about 20% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.439 (3.27)   8.277 (3.54) 17.491 (2.27) -5.607 (1.40)
      2: 42.444 (0.00)  2.348 (1.47)  -3.337 (1.31) -5.258 (0.70) -5.607 (0.40)
      3: 42.444 (0.00)  3.806 (4.83) -18.602 (6.10) -1.467 (0.86) -2.567 (1.27)
                   Day
                <char>
      1: -3.121 (1.25)
      2: -2.025 (0.85)
      3:  1.261 (2.16)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 14 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.437 (2.22)   8.279 (2.33) 17.490 (0.02) -5.609 (0.02)
      2: 42.444 (0.00)  2.350 (1.16)  -3.335 (1.01) -5.260 (0.02) -5.609 (0.02)
      3: 42.444 (0.00)  3.808 (3.92) -18.600 (4.10) -1.468 (0.02) -2.569 (0.02)
                   Day
                <char>
      1: -3.121 (0.40)
      2: -2.025 (0.69)
      3:  1.261 (0.71)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.437   8.279 17.490 -5.609 -3.121
      2:          2 42.44   2.350  -3.335 -5.260 -5.609 -2.025
      3:          3 42.44   3.808 -18.600 -1.468 -2.569  1.261

# output_verbose_1

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
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
      i Using 22 of 32 coalitions, 4 new. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.821   7.635  14.4860  0.5242 -1.2222
      2:          2 42.44   4.679  -4.875 -11.6951 -1.0583 -0.9300
      3:          3 42.44   7.323 -25.679   0.3017 -0.4086  0.8949

# output_verbose_1_3

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
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
      Estimated remaining coalitions: 8
      (Conservatively) adding about 40% of that (4 coalitions) in the next iteration.
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.821   7.635  14.4860  0.5242 -1.2222
      2:          2 42.44   4.679  -4.875 -11.6951 -1.0583 -0.9300
      3:          3 42.44   7.323 -25.679   0.3017 -0.4086  0.8949

# output_verbose_1_3_4

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind           Temp         Month
                <char>        <char>        <char>         <char>        <char>
      1: 42.444 (0.00) -0.478 (1.77) -0.478 (1.77)  15.306 (5.22) -0.478 (1.77)
      2: 42.444 (0.00) -0.790 (1.09) -0.790 (1.09) -10.706 (3.28) -0.790 (1.09)
      3: 42.444 (0.00) -6.251 (0.86) -6.251 (0.86)   0.277 (2.16) -6.251 (0.86)
                   Day
                <char>
      1: -1.271 (0.25)
      2: -0.804 (0.00)
      3:  0.909 (2.27)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp          Month
                <char>        <char>         <char>         <char>         <char>
      1: 42.444 (0.00) -8.754 (4.26)   3.660 (1.98)  15.306 (5.78)   3.660 (1.98)
      2: 42.444 (0.00)  4.467 (2.66)  -3.418 (1.25) -10.706 (3.63)  -3.418 (1.25)
      3: 42.444 (0.00)  8.163 (5.59) -13.458 (3.59)   0.277 (1.85) -13.458 (3.59)
                   Day
                <char>
      1: -1.271 (1.67)
      2: -0.804 (1.10)
      3:  0.909 (1.85)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.15 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.736 (2.74)   5.304 (2.46)  15.301 (1.65)  2.010 (1.13)
      2: 42.444 (0.00)  4.731 (2.08)  -7.016 (2.31) -10.786 (0.95)  0.077 (1.70)
      3: 42.444 (0.00)  7.265 (5.34) -26.095 (7.43)   0.551 (2.46) -0.473 (5.99)
                   Day
                <char>
      1: -1.276 (0.40)
      2: -0.884 (0.21)
      3:  1.183 (0.99)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.047 [needs 0.02]
      Estimated remaining coalitions: 8
      (Conservatively) adding about 40% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -9.069 (0.16)   6.311 (0.60)  15.071 (0.19)  1.304 (0.56)
      2: 42.444 (0.00)  4.602 (0.16)  -6.119 (0.75) -11.269 (0.31) -0.466 (0.71)
      3: 42.444 (0.00)  7.295 (0.17) -26.220 (1.86)   0.501 (0.22) -0.162 (1.86)
                   Day
                <char>
      1: -1.015 (0.22)
      2: -0.628 (0.21)
      3:  1.017 (0.13)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.821 (0.32)   7.635 (0.75)  14.486 (0.32)  0.524 (0.72)
      2: 42.444 (0.00)  4.679 (0.32)  -4.875 (0.87) -11.695 (0.34) -1.058 (0.79)
      3: 42.444 (0.00)  7.323 (0.17) -25.679 (0.37)   0.302 (0.15) -0.409 (0.30)
                   Day
                <char>
      1: -1.222 (0.41)
      2: -0.930 (0.41)
      3:  0.895 (0.15)
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.821   7.635  14.4860  0.5242 -1.2222
      2:          2 42.44   4.679  -4.875 -11.6951 -1.0583 -0.9300
      3:          3 42.44   7.323 -25.679   0.3017 -0.4086  0.8949

# output_verbose_1_3_4_5

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      i max_n_coalitions is NULL or larger than or 2^n_features = 32, and is therefore set to 2^n_features = 32.
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 26
      (Conservatively) adding about 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind           Temp         Month
                <char>        <char>        <char>         <char>        <char>
      1: 42.444 (0.00) -0.478 (1.77) -0.478 (1.77)  15.306 (5.22) -0.478 (1.77)
      2: 42.444 (0.00) -0.790 (1.09) -0.790 (1.09) -10.706 (3.28) -0.790 (1.09)
      3: 42.444 (0.00) -6.251 (0.86) -6.251 (0.86)   0.277 (2.16) -6.251 (0.86)
                   Day
                <char>
      1: -1.271 (0.25)
      2: -0.804 (0.00)
      3:  0.909 (2.27)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 8 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 8 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 24
      (Conservatively) adding about 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp          Month
                <char>        <char>         <char>         <char>         <char>
      1: 42.444 (0.00) -8.754 (4.26)   3.660 (1.98)  15.306 (5.78)   3.660 (1.98)
      2: 42.444 (0.00)  4.467 (2.66)  -3.418 (1.25) -10.706 (3.63)  -3.418 (1.25)
      3: 42.444 (0.00)  8.163 (5.59) -13.458 (3.59)   0.277 (1.85) -13.458 (3.59)
                   Day
                <char>
      1: -1.271 (1.67)
      2: -0.804 (1.10)
      3:  0.909 (1.85)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.15 [needs 0.02]
      Estimated remaining coalitions: 20
      (Conservatively) adding about 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.736 (2.74)   5.304 (2.46)  15.301 (1.65)  2.010 (1.13)
      2: 42.444 (0.00)  4.731 (2.08)  -7.016 (2.31) -10.786 (0.95)  0.077 (1.70)
      3: 42.444 (0.00)  7.265 (5.34) -26.095 (7.43)   0.551 (2.46) -0.473 (5.99)
                   Day
                <char>
      1: -1.276 (0.40)
      2: -0.884 (0.21)
      3:  1.183 (0.99)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 18 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.047 [needs 0.02]
      Estimated remaining coalitions: 8
      (Conservatively) adding about 40% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -9.069 (0.16)   6.311 (0.60)  15.071 (0.19)  1.304 (0.56)
      2: 42.444 (0.00)  4.602 (0.16)  -6.119 (0.75) -11.269 (0.31) -0.466 (0.71)
      3: 42.444 (0.00)  7.295 (0.17) -26.220 (1.86)   0.501 (0.22) -0.162 (1.86)
                   Day
                <char>
      1: -1.015 (0.22)
      2: -0.628 (0.21)
      3:  1.017 (0.13)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.821 (0.32)   7.635 (0.75)  14.486 (0.32)  0.524 (0.72)
      2: 42.444 (0.00)  4.679 (0.32)  -4.875 (0.87) -11.695 (0.34) -1.058 (0.79)
      3: 42.444 (0.00)  7.323 (0.17) -25.679 (0.37)   0.302 (0.15) -0.409 (0.30)
                   Day
                <char>
      1: -1.222 (0.41)
      2: -0.930 (0.41)
      3:  0.895 (0.15)
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.821   7.635  14.4860  0.5242 -1.2222
      2:          2 42.44   4.679  -4.875 -11.6951 -1.0583 -0.9300
      3:          3 42.44   7.323 -25.679   0.3017 -0.4086  0.8949

