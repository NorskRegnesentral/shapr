# output_lm_numeric_independence_reach_exact

    Code
      (out <- code)
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
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  0.258 (2.11)  0.258 (2.11) 17.463 (5.46) -5.635 (1.92)
      2: 42.444 (0.00) -0.986 (0.56) -0.986 (0.56) -5.286 (1.36) -5.635 (1.52)
      3: 42.444 (0.00) -4.493 (0.32) -4.493 (0.32) -1.495 (0.95) -2.595 (0.62)
                   Day
                <char>
      1:  0.258 (2.11)
      2: -0.986 (0.56)
      3: -4.493 (0.32)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.17 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.857 (0.83)  1.857 (0.83) 17.492 (3.90) -5.607 (3.49)
      2: 42.444 (0.00) -0.557 (0.26) -0.557 (0.26) -5.258 (0.34) -5.607 (0.53)
      3: 42.444 (0.00) -7.460 (1.45) -7.460 (1.45) -1.466 (0.34) -2.567 (0.48)
                   Day
                <char>
      1: -2.997 (2.42)
      2: -1.901 (0.75)
      3:  1.385 (2.80)
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
      1: 42.444 (0.00) -4.563 (2.99)   8.277 (3.53) 17.491 (1.46) -5.607 (1.31)
      2: 42.444 (0.00)  2.224 (1.28)  -3.337 (1.26) -5.258 (0.36) -5.607 (0.58)
      3: 42.444 (0.00)  3.682 (4.67) -18.602 (5.44) -1.467 (1.99) -2.567 (1.37)
                   Day
                <char>
      1: -2.996 (1.09)
      2: -1.900 (0.94)
      3:  1.386 (2.02)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.13 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.465 (2.26)   8.231 (2.27) 17.517 (1.66) -5.598 (1.66)
      2: 42.444 (0.00)  2.322 (1.05)  -3.384 (1.00) -5.233 (0.21) -5.598 (0.28)
      3: 42.444 (0.00)  3.780 (3.93) -18.649 (3.95) -1.441 (0.83) -2.558 (0.85)
                   Day
                <char>
      1: -3.083 (0.19)
      2: -1.987 (0.47)
      3:  1.299 (0.32)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.536 (0.03)   8.262 (0.02) 17.518 (0.01) -5.583 (0.02)
      2: 42.444 (0.00)  2.251 (0.03)  -3.352 (0.02) -5.231 (0.01) -5.583 (0.02)
      3: 42.444 (0.00)  3.709 (0.03) -18.617 (0.02) -1.440 (0.01) -2.543 (0.02)
                   Day
                <char>
      1: -3.059 (0.02)
      2: -1.964 (0.02)
      3:  1.323 (0.02)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.536   8.262 17.518 -5.583 -3.059
      2:          2 42.44   2.251  -3.352 -5.231 -5.583 -1.964
      3:          3 42.44   3.709 -18.617 -1.440 -2.543  1.323

# output_lm_numeric_independence_converges_tol

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.2 [needs 0.1]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.591 (2.15)   8.215 (3.41) 17.463 (5.45) -5.545 (3.11)
      2: 42.444 (0.00)  2.196 (1.42)  -3.399 (0.52) -5.286 (1.12) -5.545 (0.97)
      3: 42.444 (0.00)  3.654 (1.31) -18.664 (4.36) -1.495 (0.87) -2.505 (3.86)
                   Day
                <char>
      1: -2.940 (4.51)
      2: -1.845 (1.50)
      3:  1.442 (1.98)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 16
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.591 (0.36)   8.215 (3.28) 17.489 (4.07) -5.513 (4.06)
      2: 42.444 (0.00)  2.196 (0.99)  -3.399 (0.50) -5.260 (0.33) -5.513 (0.72)
      3: 42.444 (0.00)  3.654 (0.57) -18.664 (4.06) -1.468 (0.68) -2.473 (3.97)
                   Day
                <char>
      1: -2.998 (2.09)
      2: -1.903 (1.26)
      3:  1.384 (1.44)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.14 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 20% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp        Month
                <char>        <char>         <char>        <char>       <char>
      1: 42.444 (0.00) -4.564 (0.58)   8.243 (2.28) 17.489 (3.99) -5.54 (3.40)
      2: 42.444 (0.00)  2.223 (0.60)  -3.372 (0.37) -5.261 (0.41) -5.54 (0.71)
      3: 42.444 (0.00)  3.681 (0.89) -18.637 (2.71) -1.469 (0.48) -2.50 (2.92)
                   Day
                <char>
      1: -3.026 (2.62)
      2: -1.930 (1.07)
      3:  1.356 (1.27)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.568 (0.81)   8.252 (0.02) 17.498 (3.24) -5.554 (2.15)
      2: 42.444 (0.00)  2.219 (0.80)  -3.363 (0.02) -5.252 (0.47) -5.554 (0.62)
      3: 42.444 (0.00)  3.676 (0.49) -18.628 (0.02) -1.460 (0.37) -2.514 (0.64)
                   Day
                <char>
      1: -3.025 (2.03)
      2: -1.930 (0.89)
      3:  1.357 (0.72)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.568   8.252 17.498 -5.554 -3.025
      2:          2 42.44   2.219  -3.363 -5.252 -5.554 -1.930
      3:          3 42.44   3.676 -18.628 -1.460 -2.514  1.357

# output_lm_numeric_independence_converges_maxit

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.2 [needs 0.001]
      Estimated remaining coalitions: 20
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.591 (2.15)   8.215 (3.41) 17.463 (5.45) -5.545 (3.11)
      2: 42.444 (0.00)  2.196 (1.42)  -3.399 (0.52) -5.286 (1.12) -5.545 (0.97)
      3: 42.444 (0.00)  3.654 (1.31) -18.664 (4.36) -1.495 (0.87) -2.505 (3.86)
                   Day
                <char>
      1: -2.940 (4.51)
      2: -1.845 (1.50)
      3:  1.442 (1.98)
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
      1: 42.444 (0.00) -4.591 (0.36)   8.215 (3.28) 17.489 (4.07) -5.513 (4.06)
      2: 42.444 (0.00)  2.196 (0.99)  -3.399 (0.50) -5.260 (0.33) -5.513 (0.72)
      3: 42.444 (0.00)  3.654 (0.57) -18.664 (4.06) -1.468 (0.68) -2.473 (3.97)
                   Day
                <char>
      1: -2.998 (2.09)
      2: -1.903 (1.26)
      3:  1.384 (1.44)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.14 [needs 0.001]
      Estimated remaining coalitions: 16
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp        Month
                <char>        <char>         <char>        <char>       <char>
      1: 42.444 (0.00) -4.564 (0.58)   8.243 (2.28) 17.489 (3.99) -5.54 (3.40)
      2: 42.444 (0.00)  2.223 (0.60)  -3.372 (0.37) -5.261 (0.41) -5.54 (0.71)
      3: 42.444 (0.00)  3.681 (0.89) -18.637 (2.71) -1.469 (0.48) -2.50 (2.92)
                   Day
                <char>
      1: -3.026 (2.62)
      2: -1.930 (1.07)
      3:  1.356 (1.27)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.11 [needs 0.001]
      Estimated remaining coalitions: 14
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.568 (0.81)   8.252 (0.02) 17.498 (3.24) -5.554 (2.15)
      2: 42.444 (0.00)  2.219 (0.80)  -3.363 (0.02) -5.252 (0.47) -5.554 (0.62)
      3: 42.444 (0.00)  3.676 (0.49) -18.628 (0.02) -1.460 (0.37) -2.514 (0.64)
                   Day
                <char>
      1: -3.025 (2.03)
      2: -1.930 (0.89)
      3:  1.357 (0.72)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.065 [needs 0.001]
      Estimated remaining coalitions: 12
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.576 (0.24)   8.268 (0.02) 17.501 (1.44) -5.598 (0.23)
      2: 42.444 (0.00)  2.211 (0.62)  -3.347 (0.02) -5.249 (0.24) -5.598 (0.32)
      3: 42.444 (0.00)  3.669 (0.35) -18.612 (0.02) -1.457 (0.20) -2.558 (0.34)
                   Day
                <char>
      1: -2.992 (1.50)
      2: -1.897 (0.76)
      3:  1.390 (0.54)
    Message
      
      -- Iteration 6 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 20 coalitions:
      Current convergence measure: 0.009 [needs 0.001]
      Estimated remaining coalitions: 10
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.559 (0.02)   8.270 (0.02) 17.495 (0.02) -5.586 (0.13)
      2: 42.444 (0.00)  2.228 (0.02)  -3.345 (0.02) -5.254 (0.02) -5.586 (0.19)
      3: 42.444 (0.00)  3.686 (0.02) -18.610 (0.02) -1.463 (0.02) -2.546 (0.20)
                   Day
                <char>
      1: -3.017 (0.13)
      2: -1.922 (0.19)
      3:  1.365 (0.20)
    Message
      
      -- Iteration 7 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 22 coalitions:
      Current convergence measure: 0.039 [needs 0.001]
      Estimated remaining coalitions: 8
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.548 (0.02)   8.271 (0.02) 17.506 (1.03) -5.593 (0.18)
      2: 42.444 (0.00)  2.239 (0.02)  -3.343 (0.02) -5.243 (0.17) -5.593 (0.26)
      3: 42.444 (0.00)  3.697 (0.02) -18.608 (0.02) -1.452 (0.14) -2.553 (0.28)
                   Day
                <char>
      1: -3.034 (1.05)
      2: -1.939 (0.30)
      3:  1.348 (0.31)
    Message
      
      -- Iteration 8 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 24 coalitions:
      Convergence tolerance reached!
      Maximum number of iterations reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.540 (0.01)   8.261 (0.01) 17.514 (0.02) -5.584 (0.02)
      2: 42.444 (0.00)  2.247 (0.01)  -3.353 (0.01) -5.235 (0.02) -5.584 (0.02)
      3: 42.444 (0.00)  3.705 (0.01) -18.618 (0.01) -1.443 (0.02) -2.544 (0.02)
                   Day
                <char>
      1: -3.049 (0.02)
      2: -1.954 (0.02)
      3:  1.333 (0.02)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.540   8.261 17.514 -5.584 -3.049
      2:          2 42.44   2.247  -3.353 -5.235 -5.584 -1.954
      3:          3 42.44   3.705 -18.618 -1.443 -2.544  1.333

# output_lm_numeric_indep_conv_max_n_coalitions

    Code
      (out <- code)
    Message
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  0.258 (2.11)  0.258 (2.11) 17.463 (5.46) -5.635 (1.92)
      2: 42.444 (0.00) -0.986 (0.56) -0.986 (0.56) -5.286 (1.36) -5.635 (1.52)
      3: 42.444 (0.00) -4.493 (0.32) -4.493 (0.32) -1.495 (0.95) -2.595 (0.62)
                   Day
                <char>
      1:  0.258 (2.11)
      2: -0.986 (0.56)
      3: -4.493 (0.32)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.17 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind          Temp         Month
                <char>        <char>        <char>        <char>        <char>
      1: 42.444 (0.00)  1.857 (0.83)  1.857 (0.83) 17.492 (3.90) -5.607 (3.49)
      2: 42.444 (0.00) -0.557 (0.26) -0.557 (0.26) -5.258 (0.34) -5.607 (0.53)
      3: 42.444 (0.00) -7.460 (1.45) -7.460 (1.45) -1.466 (0.34) -2.567 (0.48)
                   Day
                <char>
      1: -2.997 (2.42)
      2: -1.901 (0.75)
      3:  1.385 (2.80)
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
      1: 42.444 (0.00) -4.563 (2.99)   8.277 (3.53) 17.491 (1.46) -5.607 (1.31)
      2: 42.444 (0.00)  2.224 (1.28)  -3.337 (1.26) -5.258 (0.36) -5.607 (0.58)
      3: 42.444 (0.00)  3.682 (4.67) -18.602 (5.44) -1.467 (1.99) -2.567 (1.37)
                   Day
                <char>
      1: -2.996 (1.09)
      2: -1.900 (0.94)
      3:  1.386 (2.02)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.13 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.465 (2.26)   8.231 (2.27) 17.517 (1.66) -5.598 (1.66)
      2: 42.444 (0.00)  2.322 (1.05)  -3.384 (1.00) -5.233 (0.21) -5.598 (0.28)
      3: 42.444 (0.00)  3.780 (3.93) -18.649 (3.95) -1.441 (0.83) -2.558 (0.85)
                   Day
                <char>
      1: -3.083 (0.19)
      2: -1.987 (0.47)
      3:  1.299 (0.32)
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
      1: 42.444 (0.00) -4.532 (0.04)   8.260 (0.04) 17.515 (1.98) -5.579 (1.98)
      2: 42.444 (0.00)  2.255 (0.04)  -3.354 (0.04) -5.234 (0.05) -5.579 (0.05)
      3: 42.444 (0.00)  3.713 (0.04) -18.619 (0.04) -1.443 (0.10) -2.539 (0.11)
                   Day
                <char>
      1: -3.062 (0.04)
      2: -1.966 (0.04)
      3:  1.320 (0.04)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.532   8.260 17.515 -5.579 -3.062
      2:          2 42.44   2.255  -3.354 -5.234 -5.579 -1.966
      3:          3 42.44   3.713 -18.619 -1.443 -2.539  1.320

# output_lm_numeric_gaussian_group_converges_tol

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
      
      -- Final estimated Shapley values (sd) 
    Output
                  none              A              B             C
                <char>         <char>         <char>        <char>
      1: 42.444 (0.00)   0.772 (2.77)  13.337 (3.46) -1.507 (3.48)
      2: 42.444 (0.00)   0.601 (3.09) -13.440 (3.27) -1.040 (2.91)
      3: 42.444 (0.00) -18.368 (4.08)   0.127 (4.13)  0.673 (0.13)
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   0.7716  13.3373 -1.5069
      2:          2 42.44   0.6006 -13.4404 -1.0396
      3:          3 42.44 -18.3678   0.1268  0.6728

# output_lm_numeric_independence_converges_tol_paired

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.2 [needs 0.1]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.591 (2.15)   8.215 (3.41) 17.463 (5.45) -5.545 (3.11)
      2: 42.444 (0.00)  2.196 (1.42)  -3.399 (0.52) -5.286 (1.12) -5.545 (0.97)
      3: 42.444 (0.00)  3.654 (1.31) -18.664 (4.36) -1.495 (0.87) -2.505 (3.86)
                   Day
                <char>
      1: -2.940 (4.51)
      2: -1.845 (1.50)
      3:  1.442 (1.98)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.1]
      Estimated remaining coalitions: 16
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.591 (0.36)   8.215 (3.28) 17.489 (4.07) -5.513 (4.06)
      2: 42.444 (0.00)  2.196 (0.99)  -3.399 (0.50) -5.260 (0.33) -5.513 (0.72)
      3: 42.444 (0.00)  3.654 (0.57) -18.664 (4.06) -1.468 (0.68) -2.473 (3.97)
                   Day
                <char>
      1: -2.998 (2.09)
      2: -1.903 (1.26)
      3:  1.384 (1.44)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.14 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 20% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp        Month
                <char>        <char>         <char>        <char>       <char>
      1: 42.444 (0.00) -4.564 (0.58)   8.243 (2.28) 17.489 (3.99) -5.54 (3.40)
      2: 42.444 (0.00)  2.223 (0.60)  -3.372 (0.37) -5.261 (0.41) -5.54 (0.71)
      3: 42.444 (0.00)  3.681 (0.89) -18.637 (2.71) -1.469 (0.48) -2.50 (2.92)
                   Day
                <char>
      1: -3.026 (2.62)
      2: -1.930 (1.07)
      3:  1.356 (1.27)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind          Temp         Month
                <char>        <char>         <char>        <char>        <char>
      1: 42.444 (0.00) -4.568 (0.81)   8.252 (0.02) 17.498 (3.24) -5.554 (2.15)
      2: 42.444 (0.00)  2.219 (0.80)  -3.363 (0.02) -5.252 (0.47) -5.554 (0.62)
      3: 42.444 (0.00)  3.676 (0.49) -18.628 (0.02) -1.460 (0.37) -2.514 (0.64)
                   Day
                <char>
      1: -3.025 (2.03)
      2: -1.930 (0.89)
      3:  1.357 (0.72)
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.568   8.252 17.498 -5.554 -3.025
      2:          2 42.44   2.219  -3.363 -5.252 -5.554 -1.930
      3:          3 42.44   3.676 -18.628 -1.460 -2.514  1.357

# output_lm_numeric_independence_saving_and_cont_est

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.529   8.230 17.526 -5.611 -3.014
      2:          2 42.44   2.258  -3.385 -5.223 -5.611 -1.918
      3:          3 42.44   3.716 -18.650 -1.432 -2.571  1.368

---

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.529   8.230 17.526 -5.611 -3.014
      2:          2 42.44   2.258  -3.385 -5.223 -5.611 -1.918
      3:          3 42.44   3.716 -18.650 -1.432 -2.571  1.368

# output_verbose_1

    Code
      (out <- code)
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
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.248   8.187  13.8548  0.7408 -1.9327
      2:          2 42.44   5.087  -4.622 -12.3163 -0.7265 -1.3025
      3:          3 42.44   7.538 -25.618  -0.1671 -0.1964  0.8752

# output_verbose_1_3

    Code
      (out <- code)
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
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.17 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.13 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
    Output
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.248   8.187  13.8548  0.7408 -1.9327
      2:          2 42.44   5.087  -4.622 -12.3163 -0.7265 -1.3025
      3:          3 42.44   7.538 -25.618  -0.1671 -0.1964  0.8752

# output_verbose_1_3_4

    Code
      (out <- code)
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
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind           Temp         Month
                <char>        <char>        <char>         <char>        <char>
      1: 42.444 (0.00) -1.428 (1.68) -1.428 (1.68)  15.197 (5.27)  1.688 (1.02)
      2: 42.444 (0.00) -0.914 (1.07) -0.914 (1.07) -10.815 (3.14) -0.321 (0.19)
      3: 42.444 (0.00) -5.807 (0.72) -5.807 (0.72)   0.168 (1.90) -0.316 (1.79)
                   Day
                <char>
      1: -1.428 (1.68)
      2: -0.914 (1.07)
      3: -5.807 (0.72)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.17 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind           Temp         Month
                <char>        <char>        <char>         <char>        <char>
      1: 42.444 (0.00)  0.473 (0.63)  0.473 (0.63)  14.309 (2.93)  0.800 (2.22)
      2: 42.444 (0.00)  0.388 (0.45)  0.388 (0.45) -11.395 (1.78) -0.901 (1.80)
      3: 42.444 (0.00) -9.097 (1.65) -9.097 (1.65)   0.124 (0.12) -0.359 (0.44)
                   Day
                <char>
      1: -3.452 (2.02)
      2: -2.359 (1.50)
      3:  0.860 (3.21)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -6.031 (3.02)   6.978 (3.60)  14.320 (1.42)  0.811 (1.30)
      2: 42.444 (0.00)  7.649 (3.27)  -6.874 (3.29) -11.387 (1.02) -0.893 (0.90)
      3: 42.444 (0.00)  8.367 (7.18) -26.561 (8.35)   0.125 (3.16) -0.359 (1.96)
                   Day
                <char>
      1: -3.476 (1.35)
      2: -2.374 (2.01)
      3:  0.859 (3.04)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.13 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -7.899 (2.85)   8.043 (2.83)  13.822 (1.35)  0.731 (1.36)
      2: 42.444 (0.00)  5.106 (2.17)  -4.753 (2.08) -12.177 (1.21) -0.706 (1.12)
      3: 42.444 (0.00)  7.736 (5.91) -25.781 (5.92)  -0.111 (1.25) -0.194 (1.25)
                   Day
                <char>
      1: -2.096 (0.93)
      2: -1.350 (0.83)
      3:  0.781 (0.72)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.248 (0.47)   8.187 (0.38)  13.855 (0.35)  0.741 (0.52)
      2: 42.444 (0.00)  5.087 (0.49)  -4.622 (0.58) -12.316 (0.42) -0.727 (0.41)
      3: 42.444 (0.00)  7.538 (0.17) -25.618 (0.23)  -0.167 (0.15) -0.196 (0.11)
                   Day
                <char>
      1: -1.933 (0.30)
      2: -1.302 (0.38)
      3:  0.875 (0.17)
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.248   8.187  13.8548  0.7408 -1.9327
      2:          2 42.44   5.087  -4.622 -12.3163 -0.7265 -1.3025
      3:          3 42.44   7.538 -25.618  -0.1671 -0.1964  0.8752

# output_verbose_1_3_4_5

    Code
      (out <- code)
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
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind           Temp         Month
                <char>        <char>        <char>         <char>        <char>
      1: 42.444 (0.00) -1.428 (1.68) -1.428 (1.68)  15.197 (5.27)  1.688 (1.02)
      2: 42.444 (0.00) -0.914 (1.07) -0.914 (1.07) -10.815 (3.14) -0.321 (0.19)
      3: 42.444 (0.00) -5.807 (0.72) -5.807 (0.72)   0.168 (1.90) -0.316 (1.79)
                   Day
                <char>
      1: -1.428 (1.68)
      2: -0.914 (1.07)
      3: -5.807 (0.72)
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.17 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R          Wind           Temp         Month
                <char>        <char>        <char>         <char>        <char>
      1: 42.444 (0.00)  0.473 (0.63)  0.473 (0.63)  14.309 (2.93)  0.800 (2.22)
      2: 42.444 (0.00)  0.388 (0.45)  0.388 (0.45) -11.395 (1.78) -0.901 (1.80)
      3: 42.444 (0.00) -9.097 (1.65) -9.097 (1.65)   0.124 (0.12) -0.359 (0.44)
                   Day
                <char>
      1: -3.452 (2.02)
      2: -2.359 (1.50)
      3:  0.860 (3.21)
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.18 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -6.031 (3.02)   6.978 (3.60)  14.320 (1.42)  0.811 (1.30)
      2: 42.444 (0.00)  7.649 (3.27)  -6.874 (3.29) -11.387 (1.02) -0.893 (0.90)
      3: 42.444 (0.00)  8.367 (7.18) -26.561 (8.35)   0.125 (3.16) -0.359 (1.96)
                   Day
                <char>
      1: -3.476 (1.35)
      2: -2.374 (2.01)
      3:  0.859 (3.04)
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.13 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -7.899 (2.85)   8.043 (2.83)  13.822 (1.35)  0.731 (1.36)
      2: 42.444 (0.00)  5.106 (2.17)  -4.753 (2.08) -12.177 (1.21) -0.706 (1.12)
      3: 42.444 (0.00)  7.736 (5.91) -25.781 (5.92)  -0.111 (1.25) -0.194 (1.25)
                   Day
                <char>
      1: -2.096 (0.93)
      2: -1.350 (0.83)
      3:  0.781 (0.72)
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                  none       Solar.R           Wind           Temp         Month
                <char>        <char>         <char>         <char>        <char>
      1: 42.444 (0.00) -8.248 (0.47)   8.187 (0.38)  13.855 (0.35)  0.741 (0.52)
      2: 42.444 (0.00)  5.087 (0.49)  -4.622 (0.58) -12.316 (0.42) -0.727 (0.41)
      3: 42.444 (0.00)  7.538 (0.17) -25.618 (0.23)  -0.167 (0.15) -0.196 (0.11)
                   Day
                <char>
      1: -1.933 (0.30)
      2: -1.302 (0.38)
      3:  0.875 (0.17)
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.248   8.187  13.8548  0.7408 -1.9327
      2:          2 42.44   5.087  -4.622 -12.3163 -0.7265 -1.3025
      3:          3 42.44   7.538 -25.618  -0.1671 -0.1964  0.8752

# output_lm_numeric_independence_keep_samp_for_vS

    Code
      (out <- code)
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
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
    Output
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.536   8.262 17.518 -5.583 -3.059
      2:          2 42.44   2.251  -3.352 -5.231 -5.583 -1.964
      3:          3 42.44   3.709 -18.617 -1.440 -2.543  1.323

