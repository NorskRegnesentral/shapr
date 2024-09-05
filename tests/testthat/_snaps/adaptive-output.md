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
      Estimated remaining coalitions: 445
      Estimated required coalitions: 450
      Using 27 new coalitions in the next iteration.
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
      1: 42.444 (0.00)  -4.521 (0.03)    8.265 (1.64)  17.654 (0.47)  -5.685 (0.04) 
      2: 42.444 (0.00)   2.266 (0.03)   -3.349 (0.22)  -5.096 (0.11)  -5.685 (0.04) 
      3: 42.444 (0.00)   3.724 (0.03)  -18.614 (2.82)  -1.304 (0.86)  -2.645 (0.04) 
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
      Estimated remaining coalitions: 39340
      Estimated required coalitions: 39350
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.03)    8.265 (1.64)  17.654 (0.47)  -5.685 (0.04) 
      2: 42.444 (0.00)   2.266 (0.03)   -3.349 (0.22)  -5.096 (0.11)  -5.685 (0.04) 
      3: 42.444 (0.00)   3.724 (0.03)  -18.614 (2.82)  -1.304 (0.86)  -2.645 (0.04) 
                    Day
                 <char>
      1: -3.111 (1.55) 
      2: -2.015 (0.19) 
      3:  1.271 (2.71) 
      
      Iteration 2
      Not converged after 11 coalitions.
      Estimated remaining coalitions: 49495
      Estimated required coalitions: 49506
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.516 (0.04)    8.318 (1.72)  17.566 (0.08)  -5.652 (0.06) 
      2: 42.444 (0.00)   2.271 (0.04)   -3.297 (0.22)  -5.184 (0.08)  -5.652 (0.06) 
      3: 42.444 (0.00)   3.729 (0.04)  -18.562 (2.99)  -1.392 (0.08)  -2.612 (0.06) 
                    Day
                 <char>
      1: -3.113 (1.72) 
      2: -2.017 (0.21) 
      3:  1.269 (2.99) 
      
      Iteration 3
      Not converged after 12 coalitions.
      Estimated remaining coalitions: 61398
      Estimated required coalitions: 61410
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.510 (0.64)    8.312 (1.82)  17.561 (0.47)  -5.645 (0.05) 
      2: 42.444 (0.00)   2.277 (0.29)   -3.302 (0.36)  -5.189 (0.12)  -5.645 (0.05) 
      3: 42.444 (0.00)   3.735 (1.12)  -18.567 (3.12)  -1.397 (0.87)  -2.605 (0.05) 
                    Day
                 <char>
      1: -3.116 (1.65) 
      2: -2.021 (0.20) 
      3:  1.266 (2.85) 
      
      Iteration 4
      Not converged after 13 coalitions.
      Estimated remaining coalitions: 61949
      Estimated required coalitions: 61962
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.05)    8.335 (1.74)  17.513 (0.06)  -5.615 (0.05) 
      2: 42.444 (0.00)   2.266 (0.05)   -3.279 (0.21)  -5.236 (0.06)  -5.615 (0.05) 
      3: 42.444 (0.00)   3.724 (0.05)  -18.544 (2.98)  -1.444 (0.06)  -2.575 (0.05) 
                    Day
                 <char>
      1: -3.111 (1.73) 
      2: -2.015 (0.21) 
      3:  1.271 (2.99) 
      
      Iteration 5
      Not converged after 14 coalitions.
      Estimated remaining coalitions: 21678
      Estimated required coalitions: 21692
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp         Month
                 <char>         <char>          <char>         <char>        <char>
      1: 42.444 (0.00)  -4.500 (0.03)    8.313 (0.98)  17.537 (0.05)  -5.63 (0.04) 
      2: 42.444 (0.00)   2.287 (0.03)   -3.302 (0.12)  -5.213 (0.05)  -5.63 (0.04) 
      3: 42.444 (0.00)   3.745 (0.03)  -18.567 (1.70)  -1.421 (0.05)  -2.59 (0.04) 
                    Day
                 <char>
      1: -3.118 (0.98) 
      2: -2.022 (0.13) 
      3:  1.264 (1.70) 
      
      Iteration 6
      Not converged after 15 coalitions.
      Estimated remaining coalitions: 47
      Estimated required coalitions: 62
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.505 (0.03)    8.327 (0.04)  17.505 (0.05)  -5.608 (0.03) 
      2: 42.444 (0.00)   2.282 (0.03)   -3.287 (0.04)  -5.245 (0.05)  -5.608 (0.03) 
      3: 42.444 (0.00)   3.740 (0.03)  -18.552 (0.04)  -1.453 (0.05)  -2.568 (0.03) 
                    Day
                 <char>
      1: -3.117 (0.04) 
      2: -2.022 (0.04) 
      3:  1.265 (0.04) 
      
      Iteration 7
      Not converged after 16 coalitions.
      Estimated remaining coalitions: 8614
      Estimated required coalitions: 8630
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.508 (0.03)    8.331 (0.57)  17.500 (0.04)  -5.609 (0.04) 
      2: 42.444 (0.00)   2.279 (0.03)   -3.284 (0.07)  -5.250 (0.04)  -5.609 (0.04) 
      3: 42.444 (0.00)   3.737 (0.03)  -18.549 (0.99)  -1.458 (0.04)  -2.569 (0.04) 
                    Day
                 <char>
      1: -3.111 (0.57) 
      2: -2.015 (0.08) 
      3:  1.271 (0.99) 
      
      Iteration 8
      Estimation stopped!
      Maximum number of iterations reached after 17 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.504 (0.03)    8.335 (0.03)  17.495 (0.04)  -5.614 (0.03) 
      2: 42.444 (0.00)   2.283 (0.03)   -3.280 (0.03)  -5.255 (0.04)  -5.614 (0.03) 
      3: 42.444 (0.00)   3.741 (0.03)  -18.545 (0.03)  -1.463 (0.04)  -2.574 (0.03) 
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
      Estimated remaining coalitions: 445
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
      1: -3.091 (0.03) 
      2: -1.995 (0.03) 
      3:  1.291 (0.03) 
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
      Estimated remaining coalitions: 9
      Estimated required coalitions: 14
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none               A               B              C
                 <char>          <char>          <char>         <char>
      1: 42.444 (0.00)   -2.653 (1.02)   14.045 (3.30)   1.210 (3.37) 
      2: 42.444 (0.00)    2.196 (1.21)  -12.992 (1.38)  -3.084 (1.94) 
      3: 42.444 (0.00)  -13.864 (2.44)   -0.294 (3.40)  -3.411 (4.08) 
      
      Iteration 2
      Not converged after 6 coalitions.
      Estimated remaining coalitions: 20
      Estimated required coalitions: 26
      Using 2 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none               A               B              C
                 <char>          <char>          <char>         <char>
      1: 42.444 (0.00)    1.025 (2.60)   14.045 (3.36)  -2.468 (4.10) 
      2: 42.444 (0.00)    0.854 (1.01)  -12.992 (1.56)  -1.742 (1.87) 
      3: 42.444 (0.00)  -18.115 (3.99)   -0.294 (3.13)   0.840 (4.91) 
      
      Iteration 3
      Estimation stopped!
      All (8) coalitions used.
      Maximum number of coalitions (8) reached.
      Final estimated Shapley values:
           none       A       B      C
         <char>  <char>  <char> <char>
      1: 42.444   0.664  13.684 -1.746
      2: 42.444   0.579 -13.266 -1.192
      3: 42.444 -18.100  -0.279  0.810
         explain_id  none        A        B       C
              <int> <num>    <num>    <num>   <num>
      1:          1 42.44   0.6638  13.6838 -1.7456
      2:          2 42.44   0.5791 -13.2665 -1.1920
      3:          3 42.44 -18.0996  -0.2788  0.8101

# output_lm_numeric_independence_converges_tol_paired

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
                   none        Solar.R            Wind           Temp         Month
                 <char>         <char>          <char>         <char>        <char>
      1: 42.444 (0.00)  -4.546 (1.13)    8.215 (2.03)  17.508 (2.82)  -5.50 (1.92) 
      2: 42.444 (0.00)   2.241 (1.23)   -3.399 (0.29)  -5.241 (0.56)  -5.50 (1.21) 
      3: 42.444 (0.00)   3.699 (0.96)  -18.664 (2.48)  -1.449 (0.44)  -2.46 (2.55) 
                    Day
                 <char>
      1: -3.076 (2.68) 
      2: -1.980 (0.63) 
      3:  1.307 (0.94) 
      
      Iteration 3
      Estimation stopped!
      Convergence tolerance reached after 16 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.533 (1.27)    8.226 (1.15)  17.506 (3.41)  -5.519 (1.50) 
      2: 42.444 (0.00)   2.254 (0.46)   -3.389 (0.32)  -5.244 (0.62)  -5.519 (0.16) 
      3: 42.444 (0.00)   3.712 (1.14)  -18.654 (1.58)  -1.452 (0.49)  -2.479 (1.14) 
                    Day
                 <char>
      1: -3.078 (3.11) 
      2: -1.983 (0.49) 
      3:  1.304 (0.42) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.533   8.226 17.506 -5.519 -3.078
      2:          2 42.44   2.254  -3.389 -5.244 -5.519 -1.983
      3:          3 42.44   3.712 -18.654 -1.452 -2.479  1.304

