# output_lm_numeric_independence_reach_exact

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
      1: 42.444 (0.00) -4.550 (0.93)   8.362 (2.80) 17.505 (0.03) -5.594 (0.99)
      2: 42.444 (0.00)  2.237 (1.26)  -3.253 (0.51) -5.245 (0.03) -5.594 (1.24)
      3: 42.444 (0.00)  3.695 (1.80) -18.518 (4.74) -1.453 (0.03) -2.554 (1.53)
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
      1: 42.444 (0.00) -4.532 (0.05)   8.262 (0.04) 17.522 (0.03) -5.582 (0.03)
      2: 42.444 (0.00)  2.255 (0.05)  -3.352 (0.04) -5.227 (0.03) -5.582 (0.03)
      3: 42.444 (0.00)  3.713 (0.05) -18.617 (0.04) -1.436 (0.03) -2.542 (0.03)
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

# output_lm_numeric_independence_converges_tol

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.919 (1.47)  1.919 (1.47) 17.492 (3.41) -5.607 (2.84)
      2: 42.444 (0.00) -0.495 (0.53) -0.495 (0.53) -5.258 (0.67) -5.607 (0.79)
      3: 42.444 (0.00) -7.398 (1.34) -7.398 (1.34) -1.466 (0.84) -2.567 (0.75)
                   Day
                <char>
      1: -3.121 (1.56)
      2: -2.025 (0.47)
      3:  1.261 (2.69)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 12
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.438 (3.27)   8.278 (3.54) 17.491 (2.27) -5.608 (1.40)
      2: 42.444 (0.00)  2.349 (1.47)  -3.337 (1.31) -5.259 (0.70) -5.608 (0.40)
      3: 42.444 (0.00)  3.807 (4.83) -18.602 (6.10) -1.467 (0.86) -2.568 (1.27)
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
      1: 42.444 (0.00) -4.437 (2.22)   8.279 (2.33) 17.489 (0.02) -5.609 (0.02)
      2: 42.444 (0.00)  2.350 (1.16)  -3.335 (1.01) -5.260 (0.02) -5.609 (0.02)
      3: 42.444 (0.00)  3.808 (3.93) -18.600 (4.10) -1.469 (0.02) -2.570 (0.02)
                   Day
                <char>
      1: -3.121 (0.40)
      2: -2.025 (0.69)
      3:  1.261 (0.71)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.437   8.279 17.489 -5.609 -3.121
      2:          2 42.44   2.350  -3.335 -5.260 -5.609 -2.025
      3:          3 42.44   3.808 -18.600 -1.469 -2.570  1.261

# output_lm_numeric_independence_converges_maxit

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.001]
      Estimated remaining coalitions: 20
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.919 (1.47)  1.919 (1.47) 17.492 (3.41) -5.607 (2.84)
      2: 42.444 (0.00) -0.495 (0.53) -0.495 (0.53) -5.258 (0.67) -5.607 (0.79)
      3: 42.444 (0.00) -7.398 (1.34) -7.398 (1.34) -1.466 (0.84) -2.567 (0.75)
                   Day
                <char>
      1: -3.121 (1.56)
      2: -2.025 (0.47)
      3:  1.261 (2.69)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.001]
      Estimated remaining coalitions: 18
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.438 (3.27)   8.278 (3.54) 17.491 (2.27) -5.608 (1.40)
      2: 42.444 (0.00)  2.349 (1.47)  -3.337 (1.31) -5.259 (0.70) -5.608 (0.40)
      3: 42.444 (0.00)  3.807 (4.83) -18.602 (6.10) -1.467 (0.86) -2.568 (1.27)
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
      Estimated remaining coalitions: 16
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.437 (2.22)   8.279 (2.33) 17.489 (0.02) -5.609 (0.02)
      2: 42.444 (0.00)  2.350 (1.16)  -3.335 (1.01) -5.260 (0.02) -5.609 (0.02)
      3: 42.444 (0.00)  3.808 (3.93) -18.600 (4.10) -1.469 (0.02) -2.570 (0.02)
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
      Estimated remaining coalitions: 14
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.539 (0.91)   8.308 (0.90) 17.500 (0.02) -5.599 (0.02)
      2: 42.444 (0.00)  2.248 (0.53)  -3.306 (0.40) -5.249 (0.02) -5.599 (0.02)
      3: 42.444 (0.00)  3.706 (1.57) -18.571 (1.57) -1.458 (0.02) -2.559 (0.02)
                   Day
                <char>
      1: -3.069 (0.13)
      2: -1.973 (0.37)
      3:  1.313 (0.21)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.0014 [needs 0.001]
      Estimated remaining coalitions: 12
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.529 (0.03)   8.296 (0.03) 17.507 (0.02) -5.592 (0.02)
      2: 42.444 (0.00)  2.258 (0.03)  -3.319 (0.03) -5.243 (0.02) -5.592 (0.02)
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
      Estimated remaining coalitions: 10
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.527 (0.04)   8.262 (0.56) 17.515 (0.01) -5.584 (0.03)
      2: 42.444 (0.00)  2.260 (0.04)  -3.353 (0.08) -5.234 (0.01) -5.584 (0.03)
      3: 42.444 (0.00)  3.718 (0.04) -18.618 (1.00) -1.443 (0.01) -2.544 (0.03)
                   Day
                <char>
      1: -3.064 (0.57)
      2: -1.969 (0.07)
      3:  1.318 (0.99)
    Message
      
      -- Iteration 7 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 22 coalitions:
      Current convergence measure: 0.0056 [needs 0.001]
      Estimated remaining coalitions: 8
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.531 (0.08)   8.265 (0.02) 17.518 (0.01) -5.581 (0.02)
      2: 42.444 (0.00)  2.256 (0.21)  -3.350 (0.02) -5.232 (0.01) -5.581 (0.02)
      3: 42.444 (0.00)  3.714 (0.12) -18.615 (0.02) -1.440 (0.01) -2.541 (0.02)
                   Day
                <char>
      1: -3.068 (0.08)
      2: -1.972 (0.21)
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
      1: 42.444 (0.00) -4.529 (0.02)   8.267 (0.02) 17.519 (0.01) -5.584 (0.01)
      2: 42.444 (0.00)  2.258 (0.02)  -3.348 (0.02) -5.231 (0.01) -5.584 (0.01)
      3: 42.444 (0.00)  3.716 (0.02) -18.613 (0.02) -1.439 (0.01) -2.544 (0.01)
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
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.16 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.550 (0.93)   8.362 (2.80) 17.505 (0.03) -5.594 (0.99)
      2: 42.444 (0.00)  2.237 (1.26)  -3.253 (0.51) -5.245 (0.03) -5.594 (1.24)
      3: 42.444 (0.00)  3.695 (1.80) -18.518 (4.74) -1.453 (0.03) -2.554 (1.53)
                   Day
                <char>
      1: -3.121 (2.66)
      2: -2.025 (0.29)
      3:  1.261 (4.60)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.532 (0.05)   8.262 (0.04) 17.522 (0.03) -5.582 (0.03)
      2: 42.444 (0.00)  2.255 (0.05)  -3.352 (0.04) -5.227 (0.03) -5.582 (0.03)
      3: 42.444 (0.00)  3.713 (0.05) -18.617 (0.04) -1.436 (0.03) -2.542 (0.03)
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

# output_lm_numeric_gaussian_group_converges_tol

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 8, 
      and is therefore set to 2^n_groups = 8.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
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

# output_lm_numeric_independence_converges_tol_paired

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.15 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.919 (1.47)  1.919 (1.47) 17.492 (3.41) -5.607 (2.84)
      2: 42.444 (0.00) -0.495 (0.53) -0.495 (0.53) -5.258 (0.67) -5.607 (0.79)
      3: 42.444 (0.00) -7.398 (1.34) -7.398 (1.34) -1.466 (0.84) -2.567 (0.75)
                   Day
                <char>
      1: -3.121 (1.56)
      2: -2.025 (0.47)
      3:  1.261 (2.69)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 12
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.438 (3.27)   8.278 (3.54) 17.491 (2.27) -5.608 (1.40)
      2: 42.444 (0.00)  2.349 (1.47)  -3.337 (1.31) -5.259 (0.70) -5.608 (0.40)
      3: 42.444 (0.00)  3.807 (4.83) -18.602 (6.10) -1.467 (0.86) -2.568 (1.27)
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
      1: 42.444 (0.00) -4.437 (2.22)   8.279 (2.33) 17.489 (0.02) -5.609 (0.02)
      2: 42.444 (0.00)  2.350 (1.16)  -3.335 (1.01) -5.260 (0.02) -5.609 (0.02)
      3: 42.444 (0.00)  3.808 (3.93) -18.600 (4.10) -1.469 (0.02) -2.570 (0.02)
                   Day
                <char>
      1: -3.121 (0.40)
      2: -2.025 (0.69)
      3:  1.261 (0.71)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.437   8.279 17.489 -5.609 -3.121
      2:          2 42.44   2.350  -3.335 -5.260 -5.609 -2.025
      3:          3 42.44   3.808 -18.600 -1.469 -2.570  1.261

# output_verbose_1

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.648   7.809  14.6337  0.5440 -1.7362
      2:          2 42.44   4.789  -4.388 -11.4817 -1.5130 -1.2857
      3:          3 42.44   7.623 -25.456   0.1543 -0.7341  0.8447

# output_verbose_1_3

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.11 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.648   7.809  14.6337  0.5440 -1.7362
      2:          2 42.44   4.789  -4.388 -11.4817 -1.5130 -1.2857
      3:          3 42.44   7.623 -25.456   0.1543 -0.7341  0.8447

# output_verbose_1_3_4

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
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
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none        Solar.R           Wind           Temp         Month
                <char>         <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -10.824 (5.39)   7.703 (2.44)  15.306 (6.19)  1.688 (2.89)
      2: 42.444 (0.00)   2.311 (2.71)  -4.360 (0.95) -10.706 (3.22) -0.321 (0.98)
      3: 42.444 (0.00)   6.980 (2.82) -25.418 (6.48)   0.277 (1.48) -0.316 (1.84)
                   Day
                <char>
      1: -1.271 (2.09)
      2: -0.804 (0.83)
      3:  0.909 (6.12)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.11 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.629 (1.94)   6.814 (2.33)  14.653 (0.43)  1.035 (2.00)
      2: 42.444 (0.00)  4.819 (1.76)  -5.376 (1.28) -11.452 (0.49) -1.067 (1.54)
      3: 42.444 (0.00)  7.570 (2.65) -25.658 (6.42)   0.101 (0.11) -0.491 (2.24)
                   Day
                <char>
      1: -1.271 (2.00)
      2: -0.804 (0.95)
      3:  0.909 (6.14)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.648 (0.63)   7.809 (0.71)  14.634 (0.31)  0.544 (0.46)
      2: 42.444 (0.00)  4.789 (0.71)  -4.388 (0.69) -11.482 (0.36) -1.513 (0.48)
      3: 42.444 (0.00)  7.623 (0.17) -25.456 (0.30)   0.154 (0.08) -0.734 (0.18)
                   Day
                <char>
      1: -1.736 (0.41)
      2: -1.286 (0.39)
      3:  0.845 (0.18)
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.648   7.809  14.6337  0.5440 -1.7362
      2:          2 42.44   4.789  -4.388 -11.4817 -1.5130 -1.2857
      3:          3 42.44   7.623 -25.456   0.1543 -0.7341  0.8447

# output_verbose_1_3_4_5

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
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
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
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
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.24 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none        Solar.R           Wind           Temp         Month
                <char>         <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -10.824 (5.39)   7.703 (2.44)  15.306 (6.19)  1.688 (2.89)
      2: 42.444 (0.00)   2.311 (2.71)  -4.360 (0.95) -10.706 (3.22) -0.321 (0.98)
      3: 42.444 (0.00)   6.980 (2.82) -25.418 (6.48)   0.277 (1.48) -0.316 (1.84)
                   Day
                <char>
      1: -1.271 (2.09)
      2: -0.804 (0.83)
      3:  0.909 (6.12)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.11 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.629 (1.94)   6.814 (2.33)  14.653 (0.43)  1.035 (2.00)
      2: 42.444 (0.00)  4.819 (1.76)  -5.376 (1.28) -11.452 (0.49) -1.067 (1.54)
      3: 42.444 (0.00)  7.570 (2.65) -25.658 (6.42)   0.101 (0.11) -0.491 (2.24)
                   Day
                <char>
      1: -1.271 (2.00)
      2: -0.804 (0.95)
      3:  0.909 (6.14)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.648 (0.63)   7.809 (0.71)  14.634 (0.31)  0.544 (0.46)
      2: 42.444 (0.00)  4.789 (0.71)  -4.388 (0.69) -11.482 (0.36) -1.513 (0.48)
      3: 42.444 (0.00)  7.623 (0.17) -25.456 (0.30)   0.154 (0.08) -0.734 (0.18)
                   Day
                <char>
      1: -1.736 (0.41)
      2: -1.286 (0.39)
      3:  0.845 (0.18)
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.648   7.809  14.6337  0.5440 -1.7362
      2:          2 42.44   4.789  -4.388 -11.4817 -1.5130 -1.2857
      3:          3 42.44   7.623 -25.456   0.1543 -0.7341  0.8447

