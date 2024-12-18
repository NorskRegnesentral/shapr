# forecast_output_ar_numeric

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 4, 
      and is therefore set to 2^n_features = 4.
      
      * Model class: <ar>
      * Approach: empirical
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 2
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 4 of 4 coalitions. 
    Output
         explain_idx horizon  none  Temp.1  Temp.2
               <int>   <int> <num>   <num>   <num>
      1:         152       1 77.88 -0.3972 -1.3912
      2:         153       1 77.88 -6.6177 -0.1835
      3:         152       2 77.88 -0.3285 -1.2034
      4:         153       2 77.88 -6.0208 -0.3371
      5:         152       3 77.88 -0.2915 -1.0552
      6:         153       3 77.88 -5.2122 -0.2553

# forecast_output_arima_numeric

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 128, 
      and is therefore set to 2^n_features = 128.
      
      * Model class: <Arima>
      * Approach: empirical
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 7
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 128 of 128 coalitions. 
    Output
         explain_idx horizon  none  Temp.1 Temp.2  Wind.1  Wind.2 Wind.F1 Wind.F2
               <int>   <int> <num>   <num>  <num>   <num>   <num>   <num>   <num>
      1:         149       1 77.88 -0.9588 -5.044  1.0543 -2.8958 -2.6627      NA
      2:         150       1 77.88  1.1553 -3.137 -2.8802  0.7196 -1.4930      NA
      3:         149       2 77.88  0.1327 -5.048  0.3337 -2.8249 -2.3014 -1.1764
      4:         150       2 77.88  1.6007 -2.399 -2.8146  0.4646 -0.7938  0.4662
      5:         149       3 77.88 -1.3878 -5.014  0.7964 -1.3881 -1.9652 -0.3295
      6:         150       3 77.88  1.6690 -2.556 -2.3821  0.3835 -0.8644 -0.1648
         Wind.F3
           <num>
      1:      NA
      2:      NA
      3:      NA
      4:      NA
      5:  0.5630
      6: -0.7615

# forecast_output_arima_numeric_iterative

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      * Model class: <Arima>
      * Approach: empirical
      * Iterative estimation: TRUE
      * Number of feature-wise Shapley values: 9
      * Number of observations to explain: 2
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 10 of 512 coalitions, 10 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 46 of 512 coalitions, 36 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 94 of 512 coalitions, 48 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 150 of 512 coalitions, 56 new. 
    Output
         explain_idx horizon  none Temp.1  Temp.2 Temp.3 Wind.1   Wind.2   Wind.3
               <int>   <int> <num>  <num>   <num>  <num>  <num>    <num>    <num>
      1:         149       1 77.88 -3.333 -4.2596 -1.521  1.758 -1.63002  0.32717
      2:         150       1 77.88  3.770 -0.4804 -4.732 -2.059  0.79071 -2.48058
      3:         149       2 77.88 -3.566 -3.9036 -1.878  1.430 -1.83639  0.29285
      4:         150       2 77.88  3.947 -1.3009 -5.090 -2.519  1.21404 -2.22629
      5:         149       3 77.88 -3.741 -3.2359 -1.813  1.714 -1.48851  0.05599
      6:         150       3 77.88  2.449 -0.7169 -5.218 -2.039  0.09699 -0.57541
         Wind.F1 Wind.F2 Wind.F3
           <num>   <num>   <num>
      1: -1.8486      NA      NA
      2: -0.4436      NA      NA
      3: -1.6574  0.2345      NA
      4:  0.5138  1.9850      NA
      5: -1.7012  0.4813  1.0023
      6:  0.3795  0.4402  0.5076

# forecast_output_arima_numeric_iterative_groups

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 16, 
      and is therefore set to 2^n_groups = 16.
      
      * Model class: <Arima>
      * Approach: empirical
      * Iterative estimation: TRUE
      * Number of group-wise Shapley values: 4
      * Number of observations to explain: 2
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 10 of 16 coalitions, 10 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 12 of 16 coalitions, 2 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 14 of 16 coalitions, 2 new. 
    Output
         explain_idx horizon  none   Temp    Wind Solar.R   Ozone
               <int>   <int> <num>  <num>   <num>   <num>   <num>
      1:         149       1 77.88 -3.896 -4.2285 -0.3807 -0.7759
      2:         150       1 77.88 -2.011 -3.9476  1.4200 -0.6295
      3:         149       2 77.88 -6.391 -4.6396  3.0143 -2.0175
      4:         150       2 77.88 -3.561 -0.2485  1.2504 -1.7844
      5:         149       3 77.88 -7.540  0.9041  0.9074 -3.4811
      6:         150       3 77.88 -2.881 -1.8988  3.1327 -2.2814

# forecast_output_arima_numeric_no_xreg

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 4, 
      and is therefore set to 2^n_features = 4.
      
      * Model class: <Arima>
      * Approach: empirical
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 2
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 4 of 4 coalitions. 
    Output
         explain_idx horizon  none  Temp.1 Temp.2
               <int>   <int> <num>   <num>  <num>
      1:         149       1 77.88 -1.7273 -7.033
      2:         150       1 77.88 -0.2229 -4.492
      3:         149       2 77.88 -1.7273 -7.033
      4:         150       2 77.88 -0.2229 -4.492
      5:         149       3 77.88 -1.7273 -7.033
      6:         150       3 77.88 -0.2229 -4.492

# forecast_output_forecast_ARIMA_group_numeric

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_groups = 4, 
      and is therefore set to 2^n_groups = 4.
      
      * Model class: <forecast_ARIMA/ARIMA/Arima>
      * Approach: empirical
      * Iterative estimation: FALSE
      * Number of group-wise Shapley values: 2
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 4 of 4 coalitions. 
    Output
         explain_idx horizon  none    Temp   Wind
               <int>   <int> <num>   <num>  <num>
      1:         149       1 77.88 -5.3063 -5.201
      2:         150       1 77.88 -1.4435 -4.192
      3:         149       2 77.88 -3.6824 -7.202
      4:         150       2 77.88 -0.2568 -3.220
      5:         149       3 77.88 -6.5216 -2.204
      6:         150       3 77.88 -1.2125 -3.463

# forecast_output_arima_numeric_no_lags

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      Success with message:
      max_n_coalitions is NULL or larger than or 2^n_features = 8, 
      and is therefore set to 2^n_features = 8.
      
      * Model class: <Arima>
      * Approach: independence
      * Iterative estimation: FALSE
      * Number of feature-wise Shapley values: 3
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 8 of 8 coalitions. 
    Output
         explain_idx horizon  none Wind.F1 Wind.F2 Wind.F3
               <int>   <int> <num>   <num>   <num>   <num>
      1:         149       1 77.88 -10.507      NA      NA
      2:         150       1 77.88  -5.635      NA      NA
      3:         149       2 77.88  -4.696  -6.189      NA
      4:         150       2 77.88  -2.071  -1.405      NA
      5:         149       3 77.88  -3.133  -3.133   -2.46
      6:         150       3 77.88  -1.383  -1.383   -1.91

