# output_lm_numeric_independence_reach_exact

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
      
      Iteration 1
      Not converged after 6 coalitions.
      Estimated remaining coalitions: 970
      Estimated required coalitions: 976
      Using 26 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
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
      
      Iteration 2
      Estimation stopped!
      All (32) coalitions used.
      Maximum number of coalitions (32) reached.
      Final estimated Shapley values:
           none Solar.R    Wind   Temp  Month    Day
         <char>  <char>  <char> <char> <char> <char>
      1: 42.444  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.444   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.444   3.708 -18.610 -1.440 -2.541  1.316
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2:          2 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3:          3 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_independence_converges_tol

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
      
      Iteration 1
      Not converged after 10 coalitions.
      Estimated remaining coalitions: 22
      Estimated required coalitions: 32
      Using 4 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
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
      
      Iteration 2
      Not converged after 14 coalitions.
      Estimated remaining coalitions: 6
      Estimated required coalitions: 20
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.551 (1.13)    8.215 (2.03)  17.507 (2.82)  -5.492 (1.92) 
      2: 42.444 (0.00)   2.236 (1.23)   -3.399 (0.29)  -5.243 (0.56)  -5.492 (1.21) 
      3: 42.444 (0.00)   3.694 (0.96)  -18.664 (2.48)  -1.451 (0.44)  -2.452 (2.55) 
                    Day
                 <char>
      1: -3.077 (2.68) 
      2: -1.982 (0.63) 
      3:  1.305 (0.94) 
      
      Iteration 3
      Estimation stopped!
      Convergence tolerance reached after 16 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.540 (1.27)    8.232 (1.15)  17.504 (3.41)  -5.514 (1.50) 
      2: 42.444 (0.00)   2.247 (0.46)   -3.383 (0.32)  -5.245 (0.62)  -5.514 (0.16) 
      3: 42.444 (0.00)   3.705 (1.14)  -18.648 (1.58)  -1.454 (0.49)  -2.474 (1.14) 
                    Day
                 <char>
      1: -3.080 (3.11) 
      2: -1.984 (0.49) 
      3:  1.302 (0.42) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.540   8.232 17.504 -5.514 -3.080
      2:          2 42.44   2.247  -3.383 -5.245 -5.514 -1.984
      3:          3 42.44   3.705 -18.648 -1.454 -2.474  1.302

# output_lm_numeric_independence_converges_maxit

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
      
      Iteration 1
      Not converged after 10 coalitions.
      Estimated remaining coalitions: 303432
      Estimated required coalitions: 303442
      Using 4 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
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
      
      Iteration 2
      Not converged after 14 coalitions.
      Estimated remaining coalitions: 181016
      Estimated required coalitions: 181030
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.551 (1.13)    8.215 (2.03)  17.507 (2.82)  -5.492 (1.92) 
      2: 42.444 (0.00)   2.236 (1.23)   -3.399 (0.29)  -5.243 (0.56)  -5.492 (1.21) 
      3: 42.444 (0.00)   3.694 (0.96)  -18.664 (2.48)  -1.451 (0.44)  -2.452 (2.55) 
                    Day
                 <char>
      1: -3.077 (2.68) 
      2: -1.982 (0.63) 
      3:  1.305 (0.94) 
      
      Iteration 3
      Not converged after 16 coalitions.
      Estimated remaining coalitions: 88350
      Estimated required coalitions: 88366
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.540 (1.27)    8.232 (1.15)  17.504 (3.41)  -5.514 (1.50) 
      2: 42.444 (0.00)   2.247 (0.46)   -3.383 (0.32)  -5.245 (0.62)  -5.514 (0.16) 
      3: 42.444 (0.00)   3.705 (1.14)  -18.648 (1.58)  -1.454 (0.49)  -2.474 (1.14) 
                    Day
                 <char>
      1: -3.080 (3.11) 
      2: -1.984 (0.49) 
      3:  1.302 (0.42) 
      
      Iteration 4
      Not converged after 18 coalitions.
      Estimated remaining coalitions: 40900
      Estimated required coalitions: 40918
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.545 (0.06)    8.240 (0.02)  17.512 (1.91)  -5.521 (1.62) 
      2: 42.444 (0.00)   2.242 (0.39)   -3.375 (0.02)  -5.238 (0.17)  -5.521 (0.39) 
      3: 42.444 (0.00)   3.700 (0.31)  -18.640 (0.02)  -1.446 (0.16)  -2.481 (0.32) 
                    Day
                 <char>
      1: -3.084 (1.03) 
      2: -1.988 (0.16) 
      3:  1.298 (0.14) 
      
      Iteration 5
      Not converged after 20 coalitions.
      Estimated remaining coalitions: 6960
      Estimated required coalitions: 6980
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.539 (0.08)    8.252 (0.02)  17.500 (0.02)  -5.542 (0.08) 
      2: 42.444 (0.00)   2.248 (0.55)   -3.363 (0.02)  -5.249 (0.02)  -5.542 (0.55) 
      3: 42.444 (0.00)   3.706 (0.44)  -18.627 (0.02)  -1.458 (0.02)  -2.502 (0.44) 
                    Day
                 <char>
      1: -3.069 (0.02) 
      2: -1.973 (0.02) 
      3:  1.313 (0.02) 
      
      Iteration 6
      Not converged after 22 coalitions.
      Estimated remaining coalitions: 46
      Estimated required coalitions: 68
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.531 (0.04)    8.263 (0.02)  17.512 (0.02)  -5.580 (0.04) 
      2: 42.444 (0.00)   2.256 (0.04)   -3.351 (0.02)  -5.238 (0.02)  -5.580 (0.04) 
      3: 42.444 (0.00)   3.714 (0.04)  -18.616 (0.02)  -1.446 (0.02)  -2.541 (0.04) 
                    Day
                 <char>
      1: -3.061 (0.02) 
      2: -1.965 (0.02) 
      3:  1.321 (0.02) 
      
      Iteration 7
      Not converged after 24 coalitions.
      Estimated remaining coalitions: 4368
      Estimated required coalitions: 4392
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.529 (0.06)    8.261 (0.01)  17.514 (0.02)  -5.579 (0.06) 
      2: 42.444 (0.00)   2.258 (0.39)   -3.354 (0.01)  -5.236 (0.02)  -5.579 (0.39) 
      3: 42.444 (0.00)   3.716 (0.31)  -18.619 (0.01)  -1.444 (0.02)  -2.539 (0.32) 
                    Day
                 <char>
      1: -3.064 (0.02) 
      2: -1.969 (0.02) 
      3:  1.318 (0.02) 
      
      Iteration 8
      Estimation stopped!
      Maximum number of iterations reached after 26 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.527 (0.03)    8.262 (0.01)  17.516 (0.01)  -5.582 (0.02) 
      2: 42.444 (0.00)   2.260 (0.03)   -3.352 (0.01)  -5.233 (0.01)  -5.582 (0.02) 
      3: 42.444 (0.00)   3.718 (0.03)  -18.617 (0.01)  -1.442 (0.01)  -2.542 (0.02) 
                    Day
                 <char>
      1: -3.068 (0.01) 
      2: -1.972 (0.01) 
      3:  1.314 (0.01) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.527   8.262 17.516 -5.582 -3.068
      2:          2 42.44   2.260  -3.352 -5.233 -5.582 -1.972
      3:          3 42.44   3.718 -18.617 -1.442 -2.542  1.314

# output_lm_numeric_indep_conv_max_n_coalitions

    Code
      (out <- code)
    Output
      
      Iteration 1
      Not converged after 6 coalitions.
      Estimated remaining coalitions: 970
      Estimated required coalitions: 976
      Using 14 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
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
      
      Iteration 2
      Estimation stopped!
      Convergence tolerance reached after 20 coalitions.
      Maximum number of coalitions (20) reached.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.497 (0.03)    8.266 (0.02)  17.516 (0.01)  -5.599 (0.03) 
      2: 42.444 (0.00)   2.290 (0.03)   -3.349 (0.02)  -5.234 (0.01)  -5.599 (0.03) 
      3: 42.444 (0.00)   3.748 (0.03)  -18.614 (0.02)  -1.442 (0.01)  -2.559 (0.03) 
                    Day
                 <char>
      1: -3.084 (0.02) 
      2: -1.988 (0.02) 
      3:  1.298 (0.02) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.497   8.266 17.516 -5.599 -3.084
      2:          2 42.44   2.290  -3.349 -5.234 -5.599 -1.988
      3:          3 42.44   3.748 -18.614 -1.442 -2.559  1.298

# output_lm_numeric_gaussian_group_converges_tol

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 8, 
      and is therefore set to 2^n_groups = 8.
      
    Output
      
      Iteration 1
      Not converged after 6 coalitions.
      Estimated remaining coalitions: 18
      Estimated required coalitions: 24
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none               A               B              C
                 <char>          <char>          <char>         <char>
      1: 42.444 (0.00)    0.772 (2.66)   13.337 (3.49)  -1.507 (3.31) 
      2: 42.444 (0.00)    0.601 (2.97)  -13.440 (3.32)  -1.040 (2.77) 
      3: 42.444 (0.00)  -18.368 (3.91)    0.127 (3.95)   0.673 (0.12) 
      
      Iteration 2
      Estimation stopped!
      All (8) coalitions used.
      Maximum number of coalitions (8) reached.
      Final estimated Shapley values:
           none       A       B      C
         <char>  <char>  <char> <char>
      1: 42.444   0.599  13.682 -1.679
      2: 42.444   0.515 -13.269 -1.126
      3: 42.444 -18.164  -0.281  0.877
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   0.5994  13.6818 -1.6791
      2:          2 42.44   0.5146 -13.2685 -1.1256
      3:          3 42.44 -18.1640  -0.2808  0.8766

# output_lm_numeric_independence_converges_tol_paired

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
      
      Iteration 1
      Not converged after 10 coalitions.
      Estimated remaining coalitions: 22
      Estimated required coalitions: 32
      Using 4 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
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
      
      Iteration 2
      Not converged after 14 coalitions.
      Estimated remaining coalitions: 6
      Estimated required coalitions: 20
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.551 (1.13)    8.215 (2.03)  17.507 (2.82)  -5.492 (1.92) 
      2: 42.444 (0.00)   2.236 (1.23)   -3.399 (0.29)  -5.243 (0.56)  -5.492 (1.21) 
      3: 42.444 (0.00)   3.694 (0.96)  -18.664 (2.48)  -1.451 (0.44)  -2.452 (2.55) 
                    Day
                 <char>
      1: -3.077 (2.68) 
      2: -1.982 (0.63) 
      3:  1.305 (0.94) 
      
      Iteration 3
      Estimation stopped!
      Convergence tolerance reached after 16 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.540 (1.27)    8.232 (1.15)  17.504 (3.41)  -5.514 (1.50) 
      2: 42.444 (0.00)   2.247 (0.46)   -3.383 (0.32)  -5.245 (0.62)  -5.514 (0.16) 
      3: 42.444 (0.00)   3.705 (1.14)  -18.648 (1.58)  -1.454 (0.49)  -2.474 (1.14) 
                    Day
                 <char>
      1: -3.080 (3.11) 
      2: -1.984 (0.49) 
      3:  1.302 (0.42) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.540   8.232 17.504 -5.514 -3.080
      2:          2 42.44   2.247  -3.383 -5.245 -5.514 -1.984
      3:          3 42.44   3.705 -18.648 -1.454 -2.474  1.302

# output_lm_numeric_independence_saving_and_cont_est

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
    Output
      
      Iteration 6
      Not converged after 15 coalitions.
      Estimated remaining coalitions: 49
      Estimated required coalitions: 64
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.508 (0.04)    8.185 (0.05)  17.475 (0.04)  -5.507 (0.05) 
      2: 42.444 (0.00)   2.279 (0.04)   -3.429 (0.05)  -5.274 (0.04)  -5.507 (0.05) 
      3: 42.444 (0.00)   3.737 (0.04)  -18.694 (0.05)  -1.483 (0.04)  -2.467 (0.05) 
                    Day
                 <char>
      1: -3.044 (0.04) 
      2: -1.948 (0.04) 
      3:  1.338 (0.04) 
      
      Iteration 7
      Not converged after 16 coalitions.
      Estimated remaining coalitions: 39
      Estimated required coalitions: 55
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.531 (0.04)    8.202 (0.03)  17.505 (0.04)  -5.551 (0.04) 
      2: 42.444 (0.00)   2.256 (0.04)   -3.412 (0.03)  -5.245 (0.04)  -5.551 (0.04) 
      3: 42.444 (0.00)   3.714 (0.04)  -18.677 (0.03)  -1.453 (0.04)  -2.511 (0.04) 
                    Day
                 <char>
      1: -3.023 (0.04) 
      2: -1.927 (0.04) 
      3:  1.359 (0.04) 
      
      Iteration 8
      Estimation stopped!
      Maximum number of iterations reached after 17 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.531 (0.05)    8.202 (0.03)  17.504 (0.04)  -5.549 (0.03) 
      2: 42.444 (0.00)   2.256 (0.05)   -3.412 (0.03)  -5.246 (0.04)  -5.549 (0.03) 
      3: 42.444 (0.00)   3.714 (0.05)  -18.677 (0.03)  -1.454 (0.04)  -2.509 (0.03) 
                    Day
                 <char>
      1: -3.024 (0.03) 
      2: -1.928 (0.03) 
      3:  1.358 (0.03) 
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
      
      Iteration 6
      Not converged after 15 coalitions.
      Estimated remaining coalitions: 49
      Estimated required coalitions: 64
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.508 (0.04)    8.185 (0.05)  17.475 (0.04)  -5.507 (0.05) 
      2: 42.444 (0.00)   2.279 (0.04)   -3.429 (0.05)  -5.274 (0.04)  -5.507 (0.05) 
      3: 42.444 (0.00)   3.737 (0.04)  -18.694 (0.05)  -1.483 (0.04)  -2.467 (0.05) 
                    Day
                 <char>
      1: -3.044 (0.04) 
      2: -1.948 (0.04) 
      3:  1.338 (0.04) 
      
      Iteration 7
      Not converged after 16 coalitions.
      Estimated remaining coalitions: 39
      Estimated required coalitions: 55
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.531 (0.04)    8.202 (0.03)  17.505 (0.04)  -5.551 (0.04) 
      2: 42.444 (0.00)   2.256 (0.04)   -3.412 (0.03)  -5.245 (0.04)  -5.551 (0.04) 
      3: 42.444 (0.00)   3.714 (0.04)  -18.677 (0.03)  -1.453 (0.04)  -2.511 (0.04) 
                    Day
                 <char>
      1: -3.023 (0.04) 
      2: -1.927 (0.04) 
      3:  1.359 (0.04) 
      
      Iteration 8
      Estimation stopped!
      Maximum number of iterations reached after 17 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.531 (0.05)    8.202 (0.03)  17.504 (0.04)  -5.549 (0.03) 
      2: 42.444 (0.00)   2.256 (0.05)   -3.412 (0.03)  -5.246 (0.04)  -5.549 (0.03) 
      3: 42.444 (0.00)   3.714 (0.05)  -18.677 (0.03)  -1.454 (0.04)  -2.509 (0.03) 
                    Day
                 <char>
      1: -3.024 (0.03) 
      2: -1.928 (0.03) 
      3:  1.358 (0.03) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.531   8.202 17.504 -5.549 -3.024
      2:          2 42.44   2.256  -3.412 -5.246 -5.549 -1.928
      3:          3 42.44   3.714 -18.677 -1.454 -2.509  1.358

