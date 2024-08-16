# output_lm_numeric_independence_reach_exact

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      Setting parameter 'n_batches' to 2 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Not converged after 5 coalitions.
      Estimated remaining coalitions: 447
      Estimated required coalitions: 450
      Using 29 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.501 (3.14)    8.305 (0.93)   6.004 (1.99)   6.004 (1.99) 
      2: 42.444 (0.00)   2.286 (2.34)   -3.309 (0.61)  -5.370 (0.96)  -5.370 (0.96) 
      3: 42.444 (0.00)   3.744 (1.85)  -18.574 (5.18)  -1.955 (3.04)  -1.955 (3.04) 
                    Day
                 <char>
      1: -3.211 (2.77) 
      2: -2.115 (0.93) 
      3:  1.171 (1.21) 
      
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
      
      Setting parameter 'n_batches' to 2 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Estimation stopped!
      Convergence tolerance reached after 10 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.03)    8.265 (1.63)  17.654 (0.47)  -5.685 (0.04) 
      2: 42.444 (0.00)   2.266 (0.03)   -3.349 (0.22)  -5.096 (0.11)  -5.685 (0.04) 
      3: 42.444 (0.00)   3.724 (0.03)  -18.614 (2.83)  -1.304 (0.86)  -2.645 (0.04) 
                    Day
                 <char>
      1: -3.111 (1.55) 
      2: -2.015 (0.19) 
      3:  1.271 (2.71) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.521   8.265 17.654 -5.685 -3.111
      2:          2 42.44   2.266  -3.349 -5.096 -5.685 -2.015
      3:          3 42.44   3.724 -18.614 -1.304 -2.645  1.271

# output_lm_numeric_independence_converges_maxit

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 32, 
      and is therefore set to 2^n_features = 32.
      
      Setting parameter 'n_batches' to 2 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Not converged after 10 coalitions.
      Estimated remaining coalitions: 39245
      Estimated required coalitions: 39253
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.03)    8.265 (1.63)  17.654 (0.47)  -5.685 (0.04) 
      2: 42.444 (0.00)   2.266 (0.03)   -3.349 (0.22)  -5.096 (0.11)  -5.685 (0.04) 
      3: 42.444 (0.00)   3.724 (0.03)  -18.614 (2.83)  -1.304 (0.86)  -2.645 (0.04) 
                    Day
                 <char>
      1: -3.111 (1.55) 
      2: -2.015 (0.19) 
      3:  1.271 (2.71) 
      
      Iteration 2
      Not converged after 11 coalitions.
      Estimated remaining coalitions: 49439
      Estimated required coalitions: 49448
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.516 (0.04)    8.318 (1.72)  17.566 (0.09)  -5.652 (0.06) 
      2: 42.444 (0.00)   2.271 (0.04)   -3.297 (0.22)  -5.184 (0.09)  -5.652 (0.06) 
      3: 42.444 (0.00)   3.729 (0.04)  -18.562 (3.00)  -1.392 (0.09)  -2.612 (0.06) 
                    Day
                 <char>
      1: -3.113 (1.72) 
      2: -2.017 (0.21) 
      3:  1.269 (2.99) 
      
      Iteration 3
      Not converged after 12 coalitions.
      Estimated remaining coalitions: 61363
      Estimated required coalitions: 61373
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.510 (0.64)    8.312 (1.82)  17.561 (0.47)  -5.645 (0.05) 
      2: 42.444 (0.00)   2.277 (0.29)   -3.302 (0.36)  -5.189 (0.12)  -5.645 (0.05) 
      3: 42.444 (0.00)   3.735 (1.12)  -18.567 (3.13)  -1.397 (0.87)  -2.605 (0.05) 
                    Day
                 <char>
      1: -3.116 (1.65) 
      2: -2.021 (0.20) 
      3:  1.266 (2.85) 
      
      Iteration 4
      Not converged after 13 coalitions.
      Estimated remaining coalitions: 61721
      Estimated required coalitions: 61732
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.05)    8.335 (1.73)  17.513 (0.05)  -5.615 (0.04) 
      2: 42.444 (0.00)   2.266 (0.05)   -3.279 (0.21)  -5.236 (0.05)  -5.615 (0.04) 
      3: 42.444 (0.00)   3.724 (0.05)  -18.544 (2.98)  -1.444 (0.05)  -2.575 (0.04) 
                    Day
                 <char>
      1: -3.111 (1.72) 
      2: -2.015 (0.21) 
      3:  1.271 (2.99) 
      
      Iteration 5
      Not converged after 14 coalitions.
      Estimated remaining coalitions: 21612
      Estimated required coalitions: 21624
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp         Month
                 <char>         <char>          <char>         <char>        <char>
      1: 42.444 (0.00)  -4.500 (0.03)    8.313 (0.98)  17.537 (0.04)  -5.63 (0.03) 
      2: 42.444 (0.00)   2.287 (0.03)   -3.302 (0.12)  -5.213 (0.04)  -5.63 (0.03) 
      3: 42.444 (0.00)   3.745 (0.03)  -18.567 (1.70)  -1.421 (0.04)  -2.59 (0.03) 
                    Day
                 <char>
      1: -3.118 (0.98) 
      2: -2.022 (0.12) 
      3:  1.264 (1.70) 
      
      Iteration 6
      Not converged after 15 coalitions.
      Estimated remaining coalitions: 25
      Estimated required coalitions: 38
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.505 (0.03)    8.327 (0.03)  17.505 (0.03)  -5.608 (0.02) 
      2: 42.444 (0.00)   2.282 (0.03)   -3.287 (0.03)  -5.245 (0.03)  -5.608 (0.02) 
      3: 42.444 (0.00)   3.740 (0.03)  -18.552 (0.03)  -1.453 (0.03)  -2.568 (0.02) 
                    Day
                 <char>
      1: -3.117 (0.04) 
      2: -2.022 (0.04) 
      3:  1.265 (0.04) 
      
      Iteration 7
      Not converged after 16 coalitions.
      Estimated remaining coalitions: 8564
      Estimated required coalitions: 8578
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.508 (0.03)    8.331 (0.57)  17.500 (0.03)  -5.609 (0.03) 
      2: 42.444 (0.00)   2.279 (0.03)   -3.284 (0.07)  -5.250 (0.03)  -5.609 (0.03) 
      3: 42.444 (0.00)   3.737 (0.03)  -18.549 (0.99)  -1.458 (0.03)  -2.569 (0.03) 
                    Day
                 <char>
      1: -3.111 (0.57) 
      2: -2.015 (0.07) 
      3:  1.271 (0.99) 
      
      Iteration 8
      Estimation stopped!
      Maximum number of iterations reached after 17 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.504 (0.03)    8.335 (0.03)  17.495 (0.03)  -5.614 (0.02) 
      2: 42.444 (0.00)   2.283 (0.03)   -3.280 (0.03)  -5.255 (0.03)  -5.614 (0.02) 
      3: 42.444 (0.00)   3.741 (0.03)  -18.545 (0.03)  -1.463 (0.03)  -2.574 (0.02) 
                    Day
                 <char>
      1: -3.109 (0.04) 
      2: -2.014 (0.04) 
      3:  1.273 (0.04) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.504   8.335 17.495 -5.614 -3.109
      2:          2 42.44   2.283  -3.280 -5.255 -5.614 -2.014
      3:          3 42.44   3.741 -18.545 -1.463 -2.574  1.273

# output_lm_numeric_independence_converges_max_n_coalitions

    Code
      (out <- code)
    Message
      Setting parameter 'n_batches' to 2 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Not converged after 5 coalitions.
      Estimated remaining coalitions: 447
      Estimated required coalitions: 450
      Using 15 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.501 (3.14)    8.305 (0.93)   6.004 (1.99)   6.004 (1.99) 
      2: 42.444 (0.00)   2.286 (2.34)   -3.309 (0.61)  -5.370 (0.96)  -5.370 (0.96) 
      3: 42.444 (0.00)   3.744 (1.85)  -18.574 (5.18)  -1.955 (3.04)  -1.955 (3.04) 
                    Day
                 <char>
      1: -3.211 (2.77) 
      2: -2.115 (0.93) 
      3:  1.171 (1.21) 
      
      Iteration 2
      Estimation stopped!
      Convergence tolerance reached after 20 coalitions.
      Maximum number of coalitions (20) reached.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.588 (0.03)    8.265 (0.02)  17.518 (0.02)  -5.502 (0.04) 
      2: 42.444 (0.00)   2.199 (0.03)   -3.349 (0.02)  -5.232 (0.02)  -5.502 (0.04) 
      3: 42.444 (0.00)   3.657 (0.03)  -18.614 (0.02)  -1.440 (0.02)  -2.462 (0.04) 
                    Day
                 <char>
      1: -3.091 (0.02) 
      2: -1.995 (0.02) 
      3:  1.291 (0.02) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.588   8.265 17.518 -5.502 -3.091
      2:          2 42.44   2.199  -3.349 -5.232 -5.502 -1.995
      3:          3 42.44   3.657 -18.614 -1.440 -2.462  1.291

# output_lm_numeric_gaussian_group_converges_tol

    Code
      (out <- code)
    Message
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 8, 
      and is therefore set to 2^n_groups = 8.
      
      Setting parameter 'n_batches' to 7 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Not converged after 5 coalitions.
      Estimated remaining coalitions: 10
      Estimated required coalitions: 13
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none               A               B              C
                 <char>          <char>          <char>         <char>
      1: 42.444 (0.00)   -2.653 (0.98)   14.045 (3.22)   1.210 (3.35) 
      2: 42.444 (0.00)    2.196 (1.21)  -12.992 (1.37)  -3.084 (1.94) 
      3: 42.444 (0.00)  -13.864 (2.38)   -0.294 (3.31)  -3.411 (4.07) 
      
      Iteration 2
      Not converged after 6 coalitions.
      Estimated remaining coalitions: 21
      Estimated required coalitions: 25
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none               A               B              C
                 <char>          <char>          <char>         <char>
      1: 42.444 (0.00)    1.025 (2.55)   14.045 (3.29)  -2.468 (4.09) 
      2: 42.444 (0.00)    0.854 (0.98)  -12.992 (1.55)  -1.742 (1.88) 
      3: 42.444 (0.00)  -18.115 (3.98)   -0.294 (3.01)   0.840 (4.86) 
      
      Iteration 3
      Estimation stopped!
      Maximum number of coalitions (8) reached.
      Final estimated Shapley values (sd):
                   none               A               B              C
                 <char>          <char>          <char>         <char>
      1: 42.444 (0.00)    0.664 (2.02)   13.684 (2.08)  -1.746 (1.77) 
      2: 42.444 (0.00)    0.579 (0.79)  -13.266 (0.68)  -1.192 (0.78) 
      3: 42.444 (0.00)  -18.100 (2.29)   -0.279 (2.33)   0.810 (1.87) 
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   0.6638  13.6838 -1.7456
      2:          2 42.44   0.5791 -13.2665 -1.1920
      3:          3 42.44 -18.0996  -0.2788  0.8101

