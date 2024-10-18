# output_lm_numeric_independence_reach_exact

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: independence
<<<<<<< HEAD
      * iterative estimation: TRUE
=======
      * Iterative estimation: TRUE
>>>>>>> origin/shapr-1.0.0
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.31 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)   0.258 (2.14)   0.258 (2.14)  17.463 (5.62)  -5.635 (1.84) 
      2: 42.444 (0.00)  -0.986 (0.56)  -0.986 (0.56)  -5.286 (1.40)  -5.635 (1.45) 
      3: 42.444 (0.00)  -4.493 (0.33)  -4.493 (0.33)  -1.495 (0.98)  -2.595 (0.59) 
                    Day
                 <char>
      1:  0.258 (2.14) 
      2: -0.986 (0.56) 
      3: -4.493 (0.33) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.18 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.411 (3.37)    8.305 (3.82)  17.463 (3.50)  -5.635 (0.19) 
      2: 42.444 (0.00)   2.376 (1.47)   -3.309 (1.07)  -5.286 (1.24)  -5.635 (1.02) 
      3: 42.444 (0.00)   3.834 (3.22)  -18.574 (5.10)  -1.495 (2.37)  -2.595 (0.83) 
                    Day
                 <char>
      1: -3.121 (3.24) 
      2: -2.025 (1.13) 
      3:  1.261 (4.44) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.079 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.467 (0.21)    8.284 (0.98)  17.485 (0.01)  -5.635 (0.12) 
      2: 42.444 (0.00)   2.320 (0.75)   -3.331 (0.11)  -5.264 (0.01)  -5.635 (0.39) 
      3: 42.444 (0.00)   3.778 (0.47)  -18.596 (1.70)  -1.473 (0.01)  -2.595 (0.34) 
                    Day
                 <char>
      1: -3.065 (1.02) 
      2: -1.969 (0.67) 
      3:  1.317 (1.77) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.541 (0.05)    8.330 (0.80)  17.491 (0.02)  -5.585 (0.02) 
      2: 42.444 (0.00)   2.246 (0.05)   -3.285 (0.10)  -5.258 (0.02)  -5.585 (0.02) 
      3: 42.444 (0.00)   3.704 (0.05)  -18.549 (1.40)  -1.467 (0.02)  -2.545 (0.02) 
                    Day
                 <char>
      1: -3.093 (0.80) 
      2: -1.997 (0.10) 
      3:  1.289 (1.40) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.541   8.330 17.491 -5.585 -3.093
      2:          2 42.44   2.246  -3.285 -5.258 -5.585 -1.997
      3:          3 42.44   3.704 -18.549 -1.467 -2.545  1.289

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
      Current convergence measure: 0.19 [needs 0.1]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.591 (2.23)    8.215 (3.14)  17.463 (5.65)  -5.545 (3.30) 
      2: 42.444 (0.00)   2.196 (1.45)   -3.399 (0.45)  -5.286 (1.14)  -5.545 (1.04) 
      3: 42.444 (0.00)   3.654 (0.94)  -18.664 (4.32)  -1.495 (1.14)  -2.505 (3.75) 
                    Day
                 <char>
      1: -2.940 (4.17) 
      2: -1.845 (1.51) 
      3:  1.442 (2.14) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.14 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.591 (0.76)    8.215 (2.20)  17.463 (4.64)  -5.545 (2.14) 
      2: 42.444 (0.00)   2.196 (0.98)   -3.399 (0.47)  -5.286 (0.76)  -5.545 (0.98) 
      3: 42.444 (0.00)   3.654 (1.12)  -18.664 (3.06)  -1.495 (0.82)  -2.505 (2.55) 
                    Day
                 <char>
      1: -2.940 (4.54) 
      2: -1.845 (1.11) 
      3:  1.442 (1.96) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.14 [needs 0.1]
      Estimated remaining coalitions: 10
      (Concervatively) adding 20% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.570 (0.87)    8.236 (1.92)  17.463 (4.97)  -5.593 (1.32) 
      2: 42.444 (0.00)   2.217 (0.66)   -3.378 (0.33)  -5.286 (0.86)  -5.593 (0.26) 
      3: 42.444 (0.00)   3.675 (0.52)  -18.643 (3.19)  -1.495 (0.72)  -2.553 (1.19) 
                    Day
                 <char>
      1: -2.934 (4.68) 
      2: -1.839 (1.06) 
      3:  1.448 (3.00) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.538 (1.90)    8.268 (0.56)  17.523 (3.29)  -5.589 (0.04) 
      2: 42.444 (0.00)   2.249 (0.66)   -3.347 (0.09)  -5.227 (0.77)  -5.589 (0.04) 
      3: 42.444 (0.00)   3.707 (0.45)  -18.611 (1.01)  -1.435 (0.58)  -2.549 (0.04) 
                    Day
                 <char>
      1: -3.061 (2.86) 
      2: -1.966 (0.50) 
      3:  1.321 (1.06) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.538   8.268 17.523 -5.589 -3.061
      2:          2 42.44   2.249  -3.347 -5.227 -5.589 -1.966
      3:          3 42.44   3.707 -18.611 -1.435 -2.549  1.321

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
      Current convergence measure: 0.19 [needs 0.001]
      Estimated remaining coalitions: 20
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.591 (2.23)    8.215 (3.14)  17.463 (5.65)  -5.545 (3.30) 
      2: 42.444 (0.00)   2.196 (1.45)   -3.399 (0.45)  -5.286 (1.14)  -5.545 (1.04) 
      3: 42.444 (0.00)   3.654 (0.94)  -18.664 (4.32)  -1.495 (1.14)  -2.505 (3.75) 
                    Day
                 <char>
      1: -2.940 (4.17) 
      2: -1.845 (1.51) 
      3:  1.442 (2.14) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.14 [needs 0.001]
      Estimated remaining coalitions: 18
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.591 (0.76)    8.215 (2.20)  17.463 (4.64)  -5.545 (2.14) 
      2: 42.444 (0.00)   2.196 (0.98)   -3.399 (0.47)  -5.286 (0.76)  -5.545 (0.98) 
      3: 42.444 (0.00)   3.654 (1.12)  -18.664 (3.06)  -1.495 (0.82)  -2.505 (2.55) 
                    Day
                 <char>
      1: -2.940 (4.54) 
      2: -1.845 (1.11) 
      3:  1.442 (1.96) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.14 [needs 0.001]
      Estimated remaining coalitions: 16
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.570 (0.87)    8.236 (1.92)  17.463 (4.97)  -5.593 (1.32) 
      2: 42.444 (0.00)   2.217 (0.66)   -3.378 (0.33)  -5.286 (0.86)  -5.593 (0.26) 
      3: 42.444 (0.00)   3.675 (0.52)  -18.643 (3.19)  -1.495 (0.72)  -2.553 (1.19) 
                    Day
                 <char>
      1: -2.934 (4.68) 
      2: -1.839 (1.06) 
      3:  1.448 (3.00) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.099 [needs 0.001]
      Estimated remaining coalitions: 14
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.538 (1.90)    8.268 (0.56)  17.523 (3.29)  -5.589 (0.04) 
      2: 42.444 (0.00)   2.249 (0.66)   -3.347 (0.09)  -5.227 (0.77)  -5.589 (0.04) 
      3: 42.444 (0.00)   3.707 (0.45)  -18.611 (1.01)  -1.435 (0.58)  -2.549 (0.04) 
                    Day
                 <char>
      1: -3.061 (2.86) 
      2: -1.966 (0.50) 
      3:  1.321 (1.06) 
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.06 [needs 0.001]
      Estimated remaining coalitions: 12
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.536 (1.11)    8.270 (0.03)  17.519 (2.34)  -5.592 (1.16) 
      2: 42.444 (0.00)   2.251 (0.47)   -3.344 (0.03)  -5.231 (0.47)  -5.592 (0.03) 
      3: 42.444 (0.00)   3.709 (0.30)  -18.609 (0.03)  -1.439 (0.36)  -2.552 (0.06) 
                    Day
                 <char>
      1: -3.059 (1.77) 
      2: -1.964 (0.42) 
      3:  1.323 (0.30) 
    Message
      
      -- Iteration 6 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 20 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.534 (0.01)    8.272 (0.01)  17.520 (0.01)  -5.592 (0.01) 
      2: 42.444 (0.00)   2.253 (0.01)   -3.342 (0.01)  -5.229 (0.01)  -5.592 (0.01) 
      3: 42.444 (0.00)   3.711 (0.01)  -18.607 (0.01)  -1.438 (0.01)  -2.553 (0.01) 
                    Day
                 <char>
      1: -3.064 (0.01) 
      2: -1.968 (0.01) 
      3:  1.318 (0.01) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.534   8.272 17.520 -5.592 -3.064
      2:          2 42.44   2.253  -3.342 -5.229 -5.592 -1.968
      3:          3 42.44   3.711 -18.607 -1.438 -2.553  1.318

# output_lm_numeric_indep_conv_max_n_coalitions

    Code
      (out <- code)
    Message
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.31 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)   0.258 (2.14)   0.258 (2.14)  17.463 (5.62)  -5.635 (1.84) 
      2: 42.444 (0.00)  -0.986 (0.56)  -0.986 (0.56)  -5.286 (1.40)  -5.635 (1.45) 
      3: 42.444 (0.00)  -4.493 (0.33)  -4.493 (0.33)  -1.495 (0.98)  -2.595 (0.59) 
                    Day
                 <char>
      1:  0.258 (2.14) 
      2: -0.986 (0.56) 
      3: -4.493 (0.33) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.18 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.411 (3.37)    8.305 (3.82)  17.463 (3.50)  -5.635 (0.19) 
      2: 42.444 (0.00)   2.376 (1.47)   -3.309 (1.07)  -5.286 (1.24)  -5.635 (1.02) 
      3: 42.444 (0.00)   3.834 (3.22)  -18.574 (5.10)  -1.495 (2.37)  -2.595 (0.83) 
                    Day
                 <char>
      1: -3.121 (3.24) 
      2: -2.025 (1.13) 
      3:  1.261 (4.44) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.079 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.467 (0.21)    8.284 (0.98)  17.485 (0.01)  -5.635 (0.12) 
      2: 42.444 (0.00)   2.320 (0.75)   -3.331 (0.11)  -5.264 (0.01)  -5.635 (0.39) 
      3: 42.444 (0.00)   3.778 (0.47)  -18.596 (1.70)  -1.473 (0.01)  -2.595 (0.34) 
                    Day
                 <char>
      1: -3.065 (1.02) 
      2: -1.969 (0.67) 
      3:  1.317 (1.77) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.541 (0.05)    8.330 (0.80)  17.491 (0.02)  -5.585 (0.02) 
      2: 42.444 (0.00)   2.246 (0.05)   -3.285 (0.10)  -5.258 (0.02)  -5.585 (0.02) 
      3: 42.444 (0.00)   3.704 (0.05)  -18.549 (1.40)  -1.467 (0.02)  -2.545 (0.02) 
                    Day
                 <char>
      1: -3.093 (0.80) 
      2: -1.997 (0.10) 
      3:  1.289 (1.40) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.541   8.330 17.491 -5.585 -3.093
      2:          2 42.44   2.246  -3.285 -5.258 -5.585 -1.997
      3:          3 42.44   3.704 -18.549 -1.467 -2.545  1.289

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
                   none               A               B              C
                 <char>          <char>          <char>         <char>
      1: 42.444 (0.00)    0.772 (2.66)   13.337 (3.49)  -1.507 (3.31) 
      2: 42.444 (0.00)    0.601 (2.97)  -13.440 (3.32)  -1.040 (2.77) 
      3: 42.444 (0.00)  -18.368 (3.91)    0.127 (3.95)   0.673 (0.12) 
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
      Current convergence measure: 0.19 [needs 0.1]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.591 (2.23)    8.215 (3.14)  17.463 (5.65)  -5.545 (3.30) 
      2: 42.444 (0.00)   2.196 (1.45)   -3.399 (0.45)  -5.286 (1.14)  -5.545 (1.04) 
      3: 42.444 (0.00)   3.654 (0.94)  -18.664 (4.32)  -1.495 (1.14)  -2.505 (3.75) 
                    Day
                 <char>
      1: -2.940 (4.17) 
      2: -1.845 (1.51) 
      3:  1.442 (2.14) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.14 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.591 (0.76)    8.215 (2.20)  17.463 (4.64)  -5.545 (2.14) 
      2: 42.444 (0.00)   2.196 (0.98)   -3.399 (0.47)  -5.286 (0.76)  -5.545 (0.98) 
      3: 42.444 (0.00)   3.654 (1.12)  -18.664 (3.06)  -1.495 (0.82)  -2.505 (2.55) 
                    Day
                 <char>
      1: -2.940 (4.54) 
      2: -1.845 (1.11) 
      3:  1.442 (1.96) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 14 coalitions:
      Current convergence measure: 0.14 [needs 0.1]
      Estimated remaining coalitions: 10
      (Concervatively) adding 20% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.570 (0.87)    8.236 (1.92)  17.463 (4.97)  -5.593 (1.32) 
      2: 42.444 (0.00)   2.217 (0.66)   -3.378 (0.33)  -5.286 (0.86)  -5.593 (0.26) 
      3: 42.444 (0.00)   3.675 (0.52)  -18.643 (3.19)  -1.495 (0.72)  -2.553 (1.19) 
                    Day
                 <char>
      1: -2.934 (4.68) 
      2: -1.839 (1.06) 
      3:  1.448 (3.00) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 16 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.538 (1.90)    8.268 (0.56)  17.523 (3.29)  -5.589 (0.04) 
      2: 42.444 (0.00)   2.249 (0.66)   -3.347 (0.09)  -5.227 (0.77)  -5.589 (0.04) 
      3: 42.444 (0.00)   3.707 (0.45)  -18.611 (1.01)  -1.435 (0.58)  -2.549 (0.04) 
                    Day
                 <char>
      1: -3.061 (2.86) 
      2: -1.966 (0.50) 
      3:  1.321 (1.06) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.538   8.268 17.523 -5.589 -3.061
      2:          2 42.44   2.249  -3.347 -5.227 -5.589 -1.966
      3:          3 42.44   3.707 -18.611 -1.435 -2.549  1.321

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
      1:          1 42.44  -4.531   8.202 17.504 -5.549 -3.024
      2:          2 42.44   2.256  -3.412 -5.246 -5.549 -1.928
      3:          3 42.44   3.714 -18.677 -1.454 -2.509  1.358

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
      1:          1 42.44  -4.531   8.202 17.504 -5.549 -3.024
      2:          2 42.44   2.256  -3.412 -5.246 -5.549 -1.928
      3:          3 42.44   3.714 -18.677 -1.454 -2.509  1.358

# output_verbose_1

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
<<<<<<< HEAD
      * iterative estimation: TRUE
=======
      * Iterative estimation: TRUE
>>>>>>> origin/shapr-1.0.0
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
      1:          1 42.44  -8.534   7.868  14.3146  0.8504 -1.8969
      2:          2 42.44   4.919  -4.878 -11.9086 -0.8405 -1.1714
      3:          3 42.44   7.447 -25.748   0.0324 -0.1976  0.8978

# output_verbose_1_3

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
<<<<<<< HEAD
      * iterative estimation: TRUE
=======
      * Iterative estimation: TRUE
>>>>>>> origin/shapr-1.0.0
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.33 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.2 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.077 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.046 [needs 0.02]
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
      1:          1 42.44  -8.534   7.868  14.3146  0.8504 -1.8969
      2:          2 42.44   4.919  -4.878 -11.9086 -0.8405 -1.1714
      3:          3 42.44   7.447 -25.748   0.0324 -0.1976  0.8978

# output_verbose_1_3_4

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
<<<<<<< HEAD
      * iterative estimation: TRUE
=======
      * Iterative estimation: TRUE
>>>>>>> origin/shapr-1.0.0
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.33 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind            Temp          Month
                 <char>         <char>         <char>          <char>         <char>
      1: 42.444 (0.00)  -1.428 (1.74)  -1.428 (1.74)   15.197 (5.43)   1.688 (0.97) 
      2: 42.444 (0.00)  -0.914 (1.10)  -0.914 (1.10)  -10.815 (3.23)  -0.321 (0.19) 
      3: 42.444 (0.00)  -5.807 (0.72)  -5.807 (0.72)    0.168 (1.95)  -0.316 (1.71) 
                    Day
                 <char>
      1: -1.428 (1.74) 
      2: -0.914 (1.10) 
      3: -5.807 (0.72) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.2 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none         Solar.R            Wind            Temp
                 <char>          <char>          <char>          <char>
      1: 42.444 (0.00)  -10.984 (4.19)    6.696 (3.77)   15.197 (4.21) 
      2: 42.444 (0.00)    2.151 (2.02)   -6.851 (2.61)  -10.815 (2.04) 
      3: 42.444 (0.00)    6.820 (4.76)  -26.009 (7.25)    0.168 (3.47) 
                  Month           Day
                 <char>        <char>
      1:  1.688 (1.57)  0.006 (3.61) 
      2: -0.321 (0.33)  1.957 (2.22) 
      3: -0.316 (0.90)  1.769 (6.40) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.077 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -9.803 (1.62)    7.155 (0.72)   14.738 (0.31)   1.688 (0.48) 
      2: 42.444 (0.00)   4.188 (1.34)   -6.060 (0.82)  -11.606 (0.54)  -0.321 (0.16) 
      3: 42.444 (0.00)   7.531 (1.13)  -25.733 (2.34)   -0.109 (0.19)  -0.316 (0.31) 
                    Day
                 <char>
      1: -1.175 (1.69) 
      2: -0.080 (1.41) 
      3:  1.057 (2.57) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.046 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -8.850 (0.50)    7.165 (0.77)   14.627 (0.34)   1.200 (0.24) 
      2: 42.444 (0.00)   4.909 (0.49)   -5.670 (0.76)  -11.676 (0.54)  -0.592 (0.19) 
      3: 42.444 (0.00)   7.453 (0.17)  -25.529 (1.87)   -0.083 (0.18)  -0.223 (0.09) 
                    Day
                 <char>
      1: -1.541 (0.65) 
      2: -0.851 (0.60) 
      3:  0.814 (1.89) 
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -8.534 (0.45)    7.868 (0.36)   14.315 (0.27)   0.850 (0.37) 
      2: 42.444 (0.00)   4.919 (0.36)   -4.878 (0.53)  -11.909 (0.38)  -0.841 (0.23) 
      3: 42.444 (0.00)   7.447 (0.16)  -25.748 (0.16)    0.032 (0.13)  -0.198 (0.07) 
                    Day
                 <char>
      1: -1.897 (0.19) 
      2: -1.171 (0.25) 
      3:  0.898 (0.12) 
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.534   7.868  14.3146  0.8504 -1.8969
      2:          2 42.44   4.919  -4.878 -11.9086 -0.8405 -1.1714
      3:          3 42.44   7.447 -25.748   0.0324 -0.1976  0.8978

# output_verbose_1_3_4_5

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      * Model class: <lm>
      * Approach: gaussian
<<<<<<< HEAD
      * iterative estimation: TRUE
=======
      * Iterative estimation: TRUE
>>>>>>> origin/shapr-1.0.0
      * Number of feature-wise Shapley values: 5
      * Number of observations to explain: 3
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 5 of 32 coalitions, 5 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.33 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind            Temp          Month
                 <char>         <char>         <char>          <char>         <char>
      1: 42.444 (0.00)  -1.428 (1.74)  -1.428 (1.74)   15.197 (5.43)   1.688 (0.97) 
      2: 42.444 (0.00)  -0.914 (1.10)  -0.914 (1.10)  -10.815 (3.23)  -0.321 (0.19) 
      3: 42.444 (0.00)  -5.807 (0.72)  -5.807 (0.72)    0.168 (1.95)  -0.316 (1.71) 
                    Day
                 <char>
      1: -1.428 (1.74) 
      2: -0.914 (1.10) 
      3: -5.807 (0.72) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.2 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none         Solar.R            Wind            Temp
                 <char>          <char>          <char>          <char>
      1: 42.444 (0.00)  -10.984 (4.19)    6.696 (3.77)   15.197 (4.21) 
      2: 42.444 (0.00)    2.151 (2.02)   -6.851 (2.61)  -10.815 (2.04) 
      3: 42.444 (0.00)    6.820 (4.76)  -26.009 (7.25)    0.168 (3.47) 
                  Month           Day
                 <char>        <char>
      1:  1.688 (1.57)  0.006 (3.61) 
      2: -0.321 (0.33)  1.957 (2.22) 
      3: -0.316 (0.90)  1.769 (6.40) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.077 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -9.803 (1.62)    7.155 (0.72)   14.738 (0.31)   1.688 (0.48) 
      2: 42.444 (0.00)   4.188 (1.34)   -6.060 (0.82)  -11.606 (0.54)  -0.321 (0.16) 
      3: 42.444 (0.00)   7.531 (1.13)  -25.733 (2.34)   -0.109 (0.19)  -0.316 (0.31) 
                    Day
                 <char>
      1: -1.175 (1.69) 
      2: -0.080 (1.41) 
      3:  1.057 (2.57) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.046 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -8.850 (0.50)    7.165 (0.77)   14.627 (0.34)   1.200 (0.24) 
      2: 42.444 (0.00)   4.909 (0.49)   -5.670 (0.76)  -11.676 (0.54)  -0.592 (0.19) 
      3: 42.444 (0.00)   7.453 (0.17)  -25.529 (1.87)   -0.083 (0.18)  -0.223 (0.09) 
                    Day
                 <char>
      1: -1.541 (0.65) 
      2: -0.851 (0.60) 
      3:  0.814 (1.89) 
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -8.534 (0.45)    7.868 (0.36)   14.315 (0.27)   0.850 (0.37) 
      2: 42.444 (0.00)   4.919 (0.36)   -4.878 (0.53)  -11.909 (0.38)  -0.841 (0.23) 
      3: 42.444 (0.00)   7.447 (0.16)  -25.748 (0.16)    0.032 (0.13)  -0.198 (0.07) 
                    Day
                 <char>
      1: -1.897 (0.19) 
      2: -1.171 (0.25) 
      3:  0.898 (0.12) 
         explain_id  none Solar.R    Wind     Temp   Month     Day
              <int> <num>   <num>   <num>    <num>   <num>   <num>
      1:          1 42.44  -8.534   7.868  14.3146  0.8504 -1.8969
      2:          2 42.44   4.919  -4.878 -11.9086 -0.8405 -1.1714
      3:          3 42.44   7.447 -25.748   0.0324 -0.1976  0.8978

