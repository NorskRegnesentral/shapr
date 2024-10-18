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
      i Using 30 of 512 coalitions, 4 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 78 of 512 coalitions, 6 new. 
    Output
         explain_idx horizon  none Temp.1  Temp.2 Temp.3 Wind.1  Wind.2  Wind.3
               <int>   <int> <num>  <num>   <num>  <num>  <num>   <num>   <num>
      1:         149       1 77.88 -2.795 -4.5597 -1.114  1.564 -1.8995  0.2087
      2:         150       1 77.88  4.024 -0.5774 -4.589 -2.234  0.1985 -2.2827
      3:         149       2 77.88 -3.701 -4.2427 -1.326  1.465 -1.9227  0.7060
      4:         150       2 77.88  3.460 -0.9158 -5.264 -2.452  0.7709 -1.7864
      5:         149       3 77.88 -4.721 -3.4208 -1.503  1.172 -0.4564 -0.6058
      6:         150       3 77.88  2.811  0.4206 -5.361 -1.388  0.0752 -0.2130
         Wind.F1 Wind.F2 Wind.F3
           <num>   <num>   <num>
      1: -1.9118      NA      NA
      2: -0.1747      NA      NA
      3: -1.1883 -0.6744      NA
      4:  0.7128  1.9982      NA
      5: -1.5436 -0.5418  2.8952
      6: -0.6202 -0.8545  0.4549

# forecast_output_arima_numeric_iterative_groups

    Code
      (out <- code)
    Message
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
      * Model class: <Arima>
      * Approach: empirical
      * Iterative estimation: TRUE
      * Number of group-wise Shapley values: 10
      * Number of observations to explain: 2
      
      -- iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 10 of 1024 coalitions, 10 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 28 of 1024 coalitions, 2 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 56 of 1024 coalitions, 12 new. 
    Output
         explain_idx horizon  none   Temp    Wind Solar.R  Ozone
               <int>   <int> <num>  <num>   <num>   <num>  <num>
      1:         149       1 77.88 -4.680 -3.6712  0.3230 -1.253
      2:         150       1 77.88 -2.487 -3.6317  1.8415 -0.891
      3:         149       2 77.88 -6.032 -4.1973  2.5973 -2.402
      4:         150       2 77.88 -3.124  0.1986  0.8258 -2.245
      5:         149       3 77.88 -7.777  1.1382  0.6962 -3.267
      6:         150       3 77.88 -3.142 -1.6674  2.9047 -2.024

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
      max_n_coalitions is NULL or larger than or 2^n_groups = 16, 
      and is therefore set to 2^n_groups = 16.
      
      * Model class: <forecast_ARIMA/ARIMA/Arima>
      * Approach: empirical
      * Iterative estimation: FALSE
      * Number of group-wise Shapley values: 4
      * Number of observations to explain: 2
      
      -- Main computation started --
      
      i Using 16 of 16 coalitions. 
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

