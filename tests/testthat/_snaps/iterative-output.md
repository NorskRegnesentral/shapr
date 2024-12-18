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
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.3 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)  -0.580 (2.02)  -0.580 (2.02)  17.463 (5.72)  -0.580 (2.02) 
      2: 42.444 (0.00)  -2.189 (0.33)  -2.189 (0.33)  -5.286 (0.98)  -2.189 (0.33) 
      3: 42.444 (0.00)  -5.778 (0.77)  -5.778 (0.77)  -1.495 (1.36)  -5.778 (0.77) 
                    Day
                 <char>
      1: -3.121 (0.85) 
      2: -2.025 (0.05) 
      3:  1.261 (2.36) 
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
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -5.006 (2.08)    8.243 (2.94)  17.492 (3.64)  -5.006 (2.08) 
      2: 42.444 (0.00)  -1.613 (0.31)   -3.371 (0.43)  -5.258 (0.61)  -1.613 (0.31) 
      3: 42.444 (0.00)   0.636 (1.88)  -18.636 (4.36)  -1.466 (2.25)   0.636 (1.88) 
                    Day
                 <char>
      1: -3.121 (0.65) 
      2: -2.025 (0.13) 
      3:  1.261 (0.55) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.12 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -5.021 (1.34)    8.258 (1.16)  17.485 (2.64)  -5.021 (1.34) 
      2: 42.444 (0.00)  -1.627 (0.21)   -3.357 (0.18)  -5.265 (0.43)  -1.627 (0.21) 
      3: 42.444 (0.00)   0.622 (0.48)  -18.622 (1.88)  -1.473 (1.23)   0.622 (0.48) 
                    Day
                 <char>
      1: -3.099 (0.36) 
      2: -2.004 (0.07) 
      3:  1.283 (0.54) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.048 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.575 (0.10)    8.263 (0.68)  17.479 (0.01)  -5.461 (0.70) 
      2: 42.444 (0.00)   2.212 (0.84)   -3.351 (0.10)  -5.270 (0.01)  -5.461 (0.86) 
      3: 42.444 (0.00)   3.670 (0.67)  -18.616 (0.81)  -1.478 (0.01)  -2.421 (1.08) 
                    Day
                 <char>
      1: -3.105 (0.12) 
      2: -2.009 (0.17) 
      3:  1.277 (0.18) 
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.556 (0.02)    8.252 (0.01)  17.512 (0.01)  -5.535 (0.02) 
      2: 42.444 (0.00)   2.231 (0.02)   -3.363 (0.01)  -5.238 (0.01)  -5.535 (0.02) 
      3: 42.444 (0.00)   3.689 (0.02)  -18.628 (0.01)  -1.446 (0.01)  -2.495 (0.02) 
                    Day
                 <char>
      1: -3.070 (0.01) 
      2: -1.975 (0.01) 
      3:  1.312 (0.01) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.556   8.252 17.512 -5.535 -3.070
      2:          2 42.44   2.231  -3.363 -5.238 -5.535 -1.975
      3:          3 42.44   3.689 -18.628 -1.446 -2.495  1.312

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
      Current convergence measure: 0.16 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)   1.919 (1.56)   1.919 (1.56)  17.492 (3.79)  -5.607 (3.04) 
      2: 42.444 (0.00)  -0.495 (0.47)  -0.495 (0.47)  -5.258 (0.77)  -5.607 (0.59) 
      3: 42.444 (0.00)  -7.398 (1.37)  -7.398 (1.37)  -1.466 (0.89)  -2.567 (0.50) 
                    Day
                 <char>
      1: -3.121 (1.68) 
      2: -2.025 (0.47) 
      3:  1.261 (2.69) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.2 [needs 0.1]
      Estimated remaining coalitions: 14
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.467 (3.07)    8.305 (3.28)  17.491 (3.69)  -5.607 (3.27) 
      2: 42.444 (0.00)   2.320 (1.58)   -3.309 (1.14)  -5.258 (0.55)  -5.607 (0.78) 
      3: 42.444 (0.00)   3.778 (4.80)  -18.574 (5.68)  -1.467 (0.52)  -2.567 (0.64) 
                    Day
                 <char>
      1: -3.121 (2.44) 
      2: -2.025 (0.40) 
      3:  1.261 (4.23) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 14 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.425 (2.17)    8.241 (2.30)  17.490 (0.02)  -5.609 (0.06) 
      2: 42.444 (0.00)   2.362 (1.10)   -3.373 (0.99)  -5.260 (0.02)  -5.609 (0.40) 
      3: 42.444 (0.00)   3.820 (3.85)  -18.638 (4.09)  -1.468 (0.02)  -2.569 (0.32) 
                    Day
                 <char>
      1: -3.094 (0.67) 
      2: -1.999 (0.33) 
      3:  1.288 (1.16) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.425   8.241 17.490 -5.609 -3.094
      2:          2 42.44   2.362  -3.373 -5.260 -5.609 -1.999
      3:          3 42.44   3.820 -18.638 -1.468 -2.569  1.288

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
      Current convergence measure: 0.16 [needs 0.001]
      Estimated remaining coalitions: 20
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)   1.919 (1.56)   1.919 (1.56)  17.492 (3.79)  -5.607 (3.04) 
      2: 42.444 (0.00)  -0.495 (0.47)  -0.495 (0.47)  -5.258 (0.77)  -5.607 (0.59) 
      3: 42.444 (0.00)  -7.398 (1.37)  -7.398 (1.37)  -1.466 (0.89)  -2.567 (0.50) 
                    Day
                 <char>
      1: -3.121 (1.68) 
      2: -2.025 (0.47) 
      3:  1.261 (2.69) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.2 [needs 0.001]
      Estimated remaining coalitions: 18
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.467 (3.07)    8.305 (3.28)  17.491 (3.69)  -5.607 (3.27) 
      2: 42.444 (0.00)   2.320 (1.58)   -3.309 (1.14)  -5.258 (0.55)  -5.607 (0.78) 
      3: 42.444 (0.00)   3.778 (4.80)  -18.574 (5.68)  -1.467 (0.52)  -2.567 (0.64) 
                    Day
                 <char>
      1: -3.121 (2.44) 
      2: -2.025 (0.40) 
      3:  1.261 (4.23) 
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
      1: 42.444 (0.00)  -4.425 (2.17)    8.241 (2.30)  17.490 (0.02)  -5.609 (0.06) 
      2: 42.444 (0.00)   2.362 (1.10)   -3.373 (0.99)  -5.260 (0.02)  -5.609 (0.40) 
      3: 42.444 (0.00)   3.820 (3.85)  -18.638 (4.09)  -1.468 (0.02)  -2.569 (0.32) 
                    Day
                 <char>
      1: -3.094 (0.67) 
      2: -1.999 (0.33) 
      3:  1.288 (1.16) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.056 [needs 0.001]
      Estimated remaining coalitions: 14
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.464 (1.27)    8.264 (0.63)  17.503 (1.10)  -5.596 (0.11) 
      2: 42.444 (0.00)   2.323 (0.84)   -3.351 (0.29)  -5.247 (0.38)  -5.596 (0.68) 
      3: 42.444 (0.00)   3.781 (1.26)  -18.616 (1.12)  -1.455 (0.27)  -2.556 (0.55) 
                    Day
                 <char>
      1: -3.105 (0.07) 
      2: -2.009 (0.22) 
      3:  1.277 (0.13) 
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 18 coalitions:
      Current convergence measure: 0.0021 [needs 0.001]
      Estimated remaining coalitions: 12
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.534 (0.05)    8.272 (0.02)  17.518 (0.03)  -5.581 (0.05) 
      2: 42.444 (0.00)   2.253 (0.05)   -3.343 (0.02)  -5.232 (0.03)  -5.581 (0.05) 
      3: 42.444 (0.00)   3.711 (0.05)  -18.607 (0.02)  -1.440 (0.03)  -2.541 (0.05) 
                    Day
                 <char>
      1: -3.073 (0.02) 
      2: -1.977 (0.02) 
      3:  1.309 (0.02) 
    Message
      
      -- Iteration 6 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 20 coalitions:
      Current convergence measure: 0.014 [needs 0.001]
      Estimated remaining coalitions: 10
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.538 (0.06)    8.273 (0.02)  17.522 (0.02)  -5.585 (0.06) 
      2: 42.444 (0.00)   2.249 (0.39)   -3.341 (0.02)  -5.228 (0.02)  -5.585 (0.39) 
      3: 42.444 (0.00)   3.707 (0.31)  -18.606 (0.02)  -1.436 (0.02)  -2.545 (0.32) 
                    Day
                 <char>
      1: -3.070 (0.02) 
      2: -1.975 (0.02) 
      3:  1.312 (0.02) 
    Message
      
      -- Iteration 7 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 22 coalitions:
      Current convergence measure: 0.0012 [needs 0.001]
      Estimated remaining coalitions: 6
      (Concervatively) adding 0.001% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.536 (0.03)    8.276 (0.02)  17.518 (0.01)  -5.583 (0.02) 
      2: 42.444 (0.00)   2.251 (0.03)   -3.338 (0.02)  -5.231 (0.01)  -5.583 (0.02) 
      3: 42.444 (0.00)   3.709 (0.03)  -18.603 (0.02)  -1.440 (0.01)  -2.543 (0.02) 
                    Day
                 <char>
      1: -3.073 (0.02) 
      2: -1.978 (0.02) 
      3:  1.309 (0.02) 
    Message
      
      -- Iteration 8 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 24 coalitions:
      Convergence tolerance reached!
      Maximum number of iterations reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.535 (0.02)    8.277 (0.01)  17.520 (0.01)  -5.585 (0.01) 
      2: 42.444 (0.00)   2.252 (0.02)   -3.338 (0.01)  -5.230 (0.01)  -5.585 (0.01) 
      3: 42.444 (0.00)   3.710 (0.02)  -18.602 (0.01)  -1.438 (0.01)  -2.545 (0.01) 
                    Day
                 <char>
      1: -3.075 (0.01) 
      2: -1.979 (0.01) 
      3:  1.307 (0.01) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.535   8.277 17.520 -5.585 -3.075
      2:          2 42.44   2.252  -3.338 -5.230 -5.585 -1.979
      3:          3 42.44   3.710 -18.602 -1.438 -2.545  1.307

# output_lm_numeric_indep_conv_max_n_coalitions

    Code
      (out <- code)
    Message
      
      -- Iteration 1 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.3 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)  -0.580 (2.02)  -0.580 (2.02)  17.463 (5.72)  -0.580 (2.02) 
      2: 42.444 (0.00)  -2.189 (0.33)  -2.189 (0.33)  -5.286 (0.98)  -2.189 (0.33) 
      3: 42.444 (0.00)  -5.778 (0.77)  -5.778 (0.77)  -1.495 (1.36)  -5.778 (0.77) 
                    Day
                 <char>
      1: -3.121 (0.85) 
      2: -2.025 (0.05) 
      3:  1.261 (2.36) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.17 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -5.006 (2.08)    8.243 (2.94)  17.492 (3.64)  -5.006 (2.08) 
      2: 42.444 (0.00)  -1.613 (0.31)   -3.371 (0.43)  -5.258 (0.61)  -1.613 (0.31) 
      3: 42.444 (0.00)   0.636 (1.88)  -18.636 (4.36)  -1.466 (2.25)   0.636 (1.88) 
                    Day
                 <char>
      1: -3.121 (0.65) 
      2: -2.025 (0.13) 
      3:  1.261 (0.55) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.12 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -5.021 (1.34)    8.258 (1.16)  17.485 (2.64)  -5.021 (1.34) 
      2: 42.444 (0.00)  -1.627 (0.21)   -3.357 (0.18)  -5.265 (0.43)  -1.627 (0.21) 
      3: 42.444 (0.00)   0.622 (0.48)  -18.622 (1.88)  -1.473 (1.23)   0.622 (0.48) 
                    Day
                 <char>
      1: -3.099 (0.36) 
      2: -2.004 (0.07) 
      3:  1.283 (0.54) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.048 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.575 (0.10)    8.263 (0.68)  17.479 (0.01)  -5.461 (0.70) 
      2: 42.444 (0.00)   2.212 (0.84)   -3.351 (0.10)  -5.270 (0.01)  -5.461 (0.86) 
      3: 42.444 (0.00)   3.670 (0.67)  -18.616 (0.81)  -1.478 (0.01)  -2.421 (1.08) 
                    Day
                 <char>
      1: -3.105 (0.12) 
      2: -2.009 (0.17) 
      3:  1.277 (0.18) 
    Message
      
      -- Iteration 5 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 20 coalitions:
      Convergence tolerance reached!
      Maximum number of coalitions reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.568 (0.01)    8.261 (0.01)  17.506 (0.01)  -5.511 (0.03) 
      2: 42.444 (0.00)   2.219 (0.01)   -3.353 (0.01)  -5.243 (0.01)  -5.511 (0.03) 
      3: 42.444 (0.00)   3.677 (0.01)  -18.618 (0.01)  -1.452 (0.01)  -2.471 (0.03) 
                    Day
                 <char>
      1: -3.086 (0.01) 
      2: -1.991 (0.01) 
      3:  1.296 (0.01) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.568   8.261 17.506 -5.511 -3.086
      2:          2 42.44   2.219  -3.353 -5.243 -5.511 -1.991
      3:          3 42.44   3.677 -18.618 -1.452 -2.471  1.296

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
      1: 42.444 (0.00)    1.037 (2.52)   12.949 (3.35)  -1.385 (3.20) 
      2: 42.444 (0.00)    0.866 (3.11)  -13.828 (3.46)  -0.917 (2.88) 
      3: 42.444 (0.00)  -18.102 (3.77)   -0.261 (3.86)   0.795 (0.24) 
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   1.0372  12.9494 -1.3845
      2:          2 42.44   0.8661 -13.8283 -0.9173
      3:          3 42.44 -18.1023  -0.2611  0.7951

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
      Current convergence measure: 0.16 [needs 0.1]
      Estimated remaining coalitions: 8
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)   1.919 (1.56)   1.919 (1.56)  17.492 (3.79)  -5.607 (3.04) 
      2: 42.444 (0.00)  -0.495 (0.47)  -0.495 (0.47)  -5.258 (0.77)  -5.607 (0.59) 
      3: 42.444 (0.00)  -7.398 (1.37)  -7.398 (1.37)  -1.466 (0.89)  -2.567 (0.50) 
                    Day
                 <char>
      1: -3.121 (1.68) 
      2: -2.025 (0.47) 
      3:  1.261 (2.69) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.2 [needs 0.1]
      Estimated remaining coalitions: 14
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.467 (3.07)    8.305 (3.28)  17.491 (3.69)  -5.607 (3.27) 
      2: 42.444 (0.00)   2.320 (1.58)   -3.309 (1.14)  -5.258 (0.55)  -5.607 (0.78) 
      3: 42.444 (0.00)   3.778 (4.80)  -18.574 (5.68)  -1.467 (0.52)  -2.567 (0.64) 
                    Day
                 <char>
      1: -3.121 (2.44) 
      2: -2.025 (0.40) 
      3:  1.261 (4.23) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      
      -- Convergence info 
      v Converged after 14 coalitions:
      Convergence tolerance reached!
      
      -- Final estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.425 (2.17)    8.241 (2.30)  17.490 (0.02)  -5.609 (0.06) 
      2: 42.444 (0.00)   2.362 (1.10)   -3.373 (0.99)  -5.260 (0.02)  -5.609 (0.40) 
      3: 42.444 (0.00)   3.820 (3.85)  -18.638 (4.09)  -1.468 (0.02)  -2.569 (0.32) 
                    Day
                 <char>
      1: -3.094 (0.67) 
      2: -1.999 (0.33) 
      3:  1.288 (1.16) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.425   8.241 17.490 -5.609 -3.094
      2:          2 42.44   2.362  -3.373 -5.260 -5.609 -1.999
      3:          3 42.44   3.820 -18.638 -1.468 -2.569  1.288

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
      1:          1 42.44  -4.553   8.201 17.513 -5.460 -3.098
      2:          2 42.44   2.234  -3.413 -5.237 -5.460 -2.003
      3:          3 42.44   3.692 -18.678 -1.445 -2.421  1.284

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
      1:          1 42.44  -4.553   8.201 17.513 -5.460 -3.098
      2:          2 42.44   2.234  -3.413 -5.237 -5.460 -2.003
      3:          3 42.44   3.692 -18.678 -1.445 -2.421  1.284

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
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
    Output
         explain_id  none Solar.R    Wind      Temp    Month     Day
              <int> <num>   <num>   <num>     <num>    <num>   <num>
      1:          1 42.44  -8.303   8.184  14.50884 -0.05722 -1.7302
      2:          2 42.44   5.018  -4.584 -11.88356 -1.34032 -1.0897
      3:          3 42.44   7.246 -25.606   0.04728 -0.22558  0.9699

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
      Current convergence measure: 0.19 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.12 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.051 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Iteration 5 -----------------------------------------------------------------
      i Using 22 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      v Converged after 22 coalitions:
      Convergence tolerance reached!
    Output
         explain_id  none Solar.R    Wind      Temp    Month     Day
              <int> <num>   <num>   <num>     <num>    <num>   <num>
      1:          1 42.44  -8.303   8.184  14.50884 -0.05722 -1.7302
      2:          2 42.44   5.018  -4.584 -11.88356 -1.34032 -1.0897
      3:          3 42.44   7.246 -25.606   0.04728 -0.22558  0.9699

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
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind            Temp          Month
                 <char>         <char>         <char>          <char>         <char>
      1: 42.444 (0.00)  -0.478 (1.70)  -0.478 (1.70)   15.306 (5.01)  -0.478 (1.70) 
      2: 42.444 (0.00)  -0.790 (1.05)  -0.790 (1.05)  -10.706 (3.15)  -0.790 (1.05) 
      3: 42.444 (0.00)  -6.251 (0.86)  -6.251 (0.86)    0.277 (2.07)  -6.251 (0.86) 
                    Day
                 <char>
      1: -1.271 (0.27) 
      2: -0.804 (0.00) 
      3:  0.909 (2.40) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.19 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -4.343 (1.71)    8.000 (2.76)   14.560 (3.18)  -4.343 (1.71) 
      2: 42.444 (0.00)   1.717 (1.44)   -4.647 (2.04)  -11.863 (2.58)   1.717 (1.44) 
      3: 42.444 (0.00)   3.561 (2.80)  -25.486 (6.46)   -0.112 (3.42)   3.561 (2.80) 
                    Day
                 <char>
      1: -1.271 (0.96) 
      2: -0.804 (0.90) 
      3:  0.909 (1.07) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.12 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -4.281 (1.19)    7.937 (1.20)   14.603 (2.35)  -4.281 (1.19) 
      2: 42.444 (0.00)   1.637 (1.09)   -4.568 (1.34)  -11.876 (2.07)   1.637 (1.09) 
      3: 42.444 (0.00)   3.541 (0.88)  -25.466 (2.74)   -0.114 (1.92)   3.541 (0.88) 
                    Day
                 <char>
      1: -1.376 (0.70) 
      2: -0.710 (0.87) 
      3:  0.930 (0.73) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.051 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -8.277 (0.94)    8.261 (0.58)   14.582 (0.54)  -0.479 (1.16) 
      2: 42.444 (0.00)   4.877 (0.80)   -4.441 (0.44)  -11.753 (0.77)  -1.790 (1.16) 
      3: 42.444 (0.00)   6.997 (0.78)  -25.432 (1.26)   -0.069 (0.25)   0.022 (1.54) 
                    Day
                 <char>
      1: -1.484 (0.30) 
      2: -0.773 (0.20) 
      3:  0.914 (0.08) 
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
      1: 42.444 (0.00)  -8.303 (0.29)    8.184 (0.26)   14.509 (0.30)  -0.057 (0.42) 
      2: 42.444 (0.00)   5.018 (0.24)   -4.584 (0.34)  -11.884 (0.40)  -1.340 (0.60) 
      3: 42.444 (0.00)   7.246 (0.22)  -25.606 (0.16)    0.047 (0.20)  -0.226 (0.36) 
                   Day
                <char>
      1: -1.73 (0.23) 
      2: -1.09 (0.27) 
      3:  0.97 (0.14) 
         explain_id  none Solar.R    Wind      Temp    Month     Day
              <int> <num>   <num>   <num>     <num>    <num>   <num>
      1:          1 42.44  -8.303   8.184  14.50884 -0.05722 -1.7302
      2:          2 42.44   5.018  -4.584 -11.88356 -1.34032 -1.0897
      3:          3 42.44   7.246 -25.606   0.04728 -0.22558  0.9699

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
      i Using 6 of 32 coalitions, 6 new. 
      
      -- Convergence info 
      i Not converged after 6 coalitions:
      Current convergence measure: 0.32 [needs 0.02]
      Estimated remaining coalitions: 24
      (Concervatively) adding 10% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R           Wind            Temp          Month
                 <char>         <char>         <char>          <char>         <char>
      1: 42.444 (0.00)  -0.478 (1.70)  -0.478 (1.70)   15.306 (5.01)  -0.478 (1.70) 
      2: 42.444 (0.00)  -0.790 (1.05)  -0.790 (1.05)  -10.706 (3.15)  -0.790 (1.05) 
      3: 42.444 (0.00)  -6.251 (0.86)  -6.251 (0.86)    0.277 (2.07)  -6.251 (0.86) 
                    Day
                 <char>
      1: -1.271 (0.27) 
      2: -0.804 (0.00) 
      3:  0.909 (2.40) 
    Message
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 10 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 10 coalitions:
      Current convergence measure: 0.19 [needs 0.02]
      Estimated remaining coalitions: 20
      (Concervatively) adding 10% of that (2 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -4.343 (1.71)    8.000 (2.76)   14.560 (3.18)  -4.343 (1.71) 
      2: 42.444 (0.00)   1.717 (1.44)   -4.647 (2.04)  -11.863 (2.58)   1.717 (1.44) 
      3: 42.444 (0.00)   3.561 (2.80)  -25.486 (6.46)   -0.112 (3.42)   3.561 (2.80) 
                    Day
                 <char>
      1: -1.271 (0.96) 
      2: -0.804 (0.90) 
      3:  0.909 (1.07) 
    Message
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 12 of 32 coalitions, 2 new. 
      
      -- Convergence info 
      i Not converged after 12 coalitions:
      Current convergence measure: 0.12 [needs 0.02]
      Estimated remaining coalitions: 18
      (Concervatively) adding 20% of that (4 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -4.281 (1.19)    7.937 (1.20)   14.603 (2.35)  -4.281 (1.19) 
      2: 42.444 (0.00)   1.637 (1.09)   -4.568 (1.34)  -11.876 (2.07)   1.637 (1.09) 
      3: 42.444 (0.00)   3.541 (0.88)  -25.466 (2.74)   -0.114 (1.92)   3.541 (0.88) 
                    Day
                 <char>
      1: -1.376 (0.70) 
      2: -0.710 (0.87) 
      3:  0.930 (0.73) 
    Message
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 32 coalitions, 4 new. 
      
      -- Convergence info 
      i Not converged after 16 coalitions:
      Current convergence measure: 0.051 [needs 0.02]
      Estimated remaining coalitions: 14
      (Concervatively) adding 30% of that (6 coalitions) in the next iteration.
      
      -- Current estimated Shapley values (sd) 
    Output
                   none        Solar.R            Wind            Temp          Month
                 <char>         <char>          <char>          <char>         <char>
      1: 42.444 (0.00)  -8.277 (0.94)    8.261 (0.58)   14.582 (0.54)  -0.479 (1.16) 
      2: 42.444 (0.00)   4.877 (0.80)   -4.441 (0.44)  -11.753 (0.77)  -1.790 (1.16) 
      3: 42.444 (0.00)   6.997 (0.78)  -25.432 (1.26)   -0.069 (0.25)   0.022 (1.54) 
                    Day
                 <char>
      1: -1.484 (0.30) 
      2: -0.773 (0.20) 
      3:  0.914 (0.08) 
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
      1: 42.444 (0.00)  -8.303 (0.29)    8.184 (0.26)   14.509 (0.30)  -0.057 (0.42) 
      2: 42.444 (0.00)   5.018 (0.24)   -4.584 (0.34)  -11.884 (0.40)  -1.340 (0.60) 
      3: 42.444 (0.00)   7.246 (0.22)  -25.606 (0.16)    0.047 (0.20)  -0.226 (0.36) 
                   Day
                <char>
      1: -1.73 (0.23) 
      2: -1.09 (0.27) 
      3:  0.97 (0.14) 
         explain_id  none Solar.R    Wind      Temp    Month     Day
              <int> <num>   <num>   <num>     <num>    <num>   <num>
      1:          1 42.44  -8.303   8.184  14.50884 -0.05722 -1.7302
      2:          2 42.44   5.018  -4.584 -11.88356 -1.34032 -1.0897
      3:          3 42.44   7.246 -25.606   0.04728 -0.22558  0.9699

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
      i Using 6 of 32 coalitions, 6 new. 
      
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
      1:          1 42.44  -4.556   8.252 17.512 -5.535 -3.070
      2:          2 42.44   2.231  -3.363 -5.238 -5.535 -1.975
      3:          3 42.44   3.689 -18.628 -1.446 -2.495  1.312

