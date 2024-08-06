# output_lm_numeric_independence_reach_exact

    Code
      (out <- code)
    Message
      Setting parameter 'n_batches' to 2 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Not converged after 4 coalitions.
      Estimated remaining coalitions: 492
      Estimated required coalitions: 494
      Using 30 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R           Wind           Temp          Month
                 <char>         <char>         <char>         <char>         <char>
      1: 42.444 (0.00)   0.258 (2.13)   0.258 (2.13)  17.373 (5.59)  -5.545 (1.81) 
      2: 42.444 (0.00)  -0.986 (0.56)  -0.986 (0.56)  -5.376 (1.43)  -5.545 (1.42) 
      3: 42.444 (0.00)  -4.493 (0.32)  -4.493 (0.32)  -1.585 (0.95)  -2.505 (0.62) 
                    Day
                 <char>
      1:  0.258 (2.13) 
      2: -0.986 (0.56) 
      3: -4.493 (0.32) 
      
      Iteration 2
      Estimation stopped!
      All (32) coalitions used.
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
      Setting parameter 'n_batches' to 2 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Estimation stopped!
      Convergence tolerance reached after 10 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.03)    8.265 (2.17)  17.654 (1.53)  -5.685 (1.36) 
      2: 42.444 (0.00)   2.266 (0.03)   -3.349 (0.31)  -5.096 (0.21)  -5.685 (0.13) 
      3: 42.444 (0.00)   3.724 (0.03)  -18.614 (3.49)  -1.304 (1.89)  -2.645 (0.80) 
                    Day
                 <char>
      1: -3.111 (1.72) 
      2: -2.015 (0.21) 
      3:  1.271 (3.00) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.521   8.265 17.654 -5.685 -3.111
      2:          2 42.44   2.266  -3.349 -5.096 -5.685 -2.015
      3:          3 42.44   3.724 -18.614 -1.304 -2.645  1.271

# output_lm_numeric_independence_converges_maxit

    Code
      (out <- code)
    Message
      Setting parameter 'n_batches' to 2 as a fair trade-off between memory consumption and computation time.
      Reducing 'n_batches' typically reduces the computation time at the cost of increased memory consumption.
      
    Output
      
      Iteration 1
      Not converged after 10 coalitions.
      Estimated remaining coalitions: 69380
      Estimated required coalitions: 69388
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.03)    8.265 (2.17)  17.654 (1.53)  -5.685 (1.36) 
      2: 42.444 (0.00)   2.266 (0.03)   -3.349 (0.31)  -5.096 (0.21)  -5.685 (0.13) 
      3: 42.444 (0.00)   3.724 (0.03)  -18.614 (3.49)  -1.304 (1.89)  -2.645 (0.80) 
                    Day
                 <char>
      1: -3.111 (1.72) 
      2: -2.015 (0.21) 
      3:  1.271 (3.00) 
      
      Iteration 2
      Not converged after 11 coalitions.
      Estimated remaining coalitions: 36246
      Estimated required coalitions: 36255
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.516 (0.03)    8.318 (1.47)  17.566 (0.09)  -5.652 (0.05) 
      2: 42.444 (0.00)   2.271 (0.03)   -3.297 (0.19)  -5.184 (0.09)  -5.652 (0.05) 
      3: 42.444 (0.00)   3.729 (0.03)  -18.562 (2.54)  -1.392 (0.09)  -2.612 (0.05) 
                    Day
                 <char>
      1: -3.113 (1.46) 
      2: -2.017 (0.18) 
      3:  1.269 (2.55) 
      
      Iteration 3
      Not converged after 12 coalitions.
      Estimated remaining coalitions: 60342
      Estimated required coalitions: 60352
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.510 (0.04)    8.312 (1.80)  17.561 (0.07)  -5.645 (0.04) 
      2: 42.444 (0.00)   2.277 (0.04)   -3.302 (0.22)  -5.189 (0.07)  -5.645 (0.04) 
      3: 42.444 (0.00)   3.735 (0.04)  -18.567 (3.12)  -1.397 (0.07)  -2.605 (0.04) 
                    Day
                 <char>
      1: -3.116 (1.80) 
      2: -2.021 (0.21) 
      3:  1.266 (3.12) 
      
      Iteration 4
      Not converged after 13 coalitions.
      Estimated remaining coalitions: 50485
      Estimated required coalitions: 50496
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.521 (0.05)    8.335 (1.56)  17.513 (0.05)  -5.615 (0.04) 
      2: 42.444 (0.00)   2.266 (0.05)   -3.279 (0.20)  -5.236 (0.05)  -5.615 (0.04) 
      3: 42.444 (0.00)   3.724 (0.05)  -18.544 (2.70)  -1.444 (0.05)  -2.575 (0.04) 
                    Day
                 <char>
      1: -3.111 (1.57) 
      2: -2.015 (0.19) 
      3:  1.271 (2.70) 
      
      Iteration 5
      Not converged after 14 coalitions.
      Estimated remaining coalitions: 35224
      Estimated required coalitions: 35236
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp         Month
                 <char>         <char>          <char>         <char>        <char>
      1: 42.444 (0.00)  -4.500 (0.03)    8.313 (1.26)  17.537 (0.06)  -5.63 (0.03) 
      2: 42.444 (0.00)   2.287 (0.03)   -3.302 (0.15)  -5.213 (0.06)  -5.63 (0.03) 
      3: 42.444 (0.00)   3.745 (0.03)  -18.567 (2.17)  -1.421 (0.06)  -2.59 (0.03) 
                    Day
                 <char>
      1: -3.118 (1.25) 
      2: -2.022 (0.15) 
      3:  1.264 (2.18) 
      
      Iteration 6
      Not converged after 15 coalitions.
      Estimated remaining coalitions: 56
      Estimated required coalitions: 69
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.505 (0.03)    8.327 (0.04)  17.505 (0.05)  -5.608 (0.04) 
      2: 42.444 (0.00)   2.282 (0.03)   -3.287 (0.04)  -5.245 (0.05)  -5.608 (0.04) 
      3: 42.444 (0.00)   3.740 (0.03)  -18.552 (0.04)  -1.453 (0.05)  -2.568 (0.04) 
                    Day
                 <char>
      1: -3.117 (0.05) 
      2: -2.022 (0.05) 
      3:  1.265 (0.05) 
      
      Iteration 7
      Not converged after 16 coalitions.
      Estimated remaining coalitions: 40
      Estimated required coalitions: 54
      Using 1 new coalitions in the next iteration.
      Current estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.508 (0.03)    8.331 (0.03)  17.500 (0.04)  -5.609 (0.03) 
      2: 42.444 (0.00)   2.279 (0.03)   -3.284 (0.03)  -5.250 (0.04)  -5.609 (0.03) 
      3: 42.444 (0.00)   3.737 (0.03)  -18.549 (0.03)  -1.458 (0.04)  -2.569 (0.03) 
                    Day
                 <char>
      1: -3.111 (0.04) 
      2: -2.015 (0.04) 
      3:  1.271 (0.04) 
      
      Iteration 8
      Estimation stopped!
      Maximum number iterations reached after 17 coalitions.
      Final estimated Shapley values (sd):
                   none        Solar.R            Wind           Temp          Month
                 <char>         <char>          <char>         <char>         <char>
      1: 42.444 (0.00)  -4.504 (0.03)    8.335 (0.58)  17.495 (0.04)  -5.614 (0.04) 
      2: 42.444 (0.00)   2.283 (0.03)   -3.280 (0.07)  -5.255 (0.04)  -5.614 (0.04) 
      3: 42.444 (0.00)   3.741 (0.03)  -18.545 (0.99)  -1.463 (0.04)  -2.574 (0.04) 
                    Day
                 <char>
      1: -3.109 (0.57) 
      2: -2.014 (0.08) 
      3:  1.273 (0.99) 
         explain_id  none Solar.R    Wind   Temp  Month    Day
              <int> <num>   <num>   <num>  <num>  <num>  <num>
      1:          1 42.44  -4.504   8.335 17.495 -5.614 -3.109
      2:          2 42.44   2.283  -3.280 -5.255 -5.614 -2.014
      3:          3 42.44   3.741 -18.545 -1.463 -2.574  1.273

