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
         explain_idx horizon  none Temp.1  Temp.2 Temp.3  Wind.1  Wind.2   Wind.3
               <int>   <int> <num>  <num>   <num>  <num>   <num>   <num>    <num>
      1:         149       1 77.88 -3.327 -4.2474 -1.499  1.7231 -1.6078  0.31502
      2:         150       1 77.88  3.781 -0.4774 -4.727 -2.0583  0.7566 -2.46020
      3:         149       2 77.88 -3.475 -3.2166 -1.508  1.2872 -1.9965  0.02242
      4:         150       2 77.88  3.491 -1.2376 -5.132 -1.8843  0.4237 -1.22244
      5:         149       3 77.88 -4.394 -4.4730 -2.371  1.8591 -0.8661  0.55908
      6:         150       3 77.88  3.131 -1.4060 -5.881 -0.8523  0.6045 -1.06403
         Wind.F1 Wind.F2  Wind.F3
           <num>   <num>    <num>
      1: -1.8643      NA       NA
      2: -0.4508      NA       NA
      3: -1.5325 -0.4656       NA
      4:  0.7020  1.3828       NA
      5: -1.1902  0.5009  1.64924
      6:  0.1562  0.6957 -0.06019

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
         explain_idx horizon  none   Temp     Wind Solar.R   Ozone
               <int>   <int> <num>  <num>    <num>   <num>   <num>
      1:         149       1 77.88 -5.219 -4.05598 -0.2082  0.2019
      2:         150       1 77.88 -2.384 -3.44580  1.9219 -1.2607
      3:         149       2 77.88 -5.807 -4.26464  2.1290 -2.0917
      4:         150       2 77.88 -2.771  0.02034  0.5277 -2.1210
      5:         149       3 77.88 -7.566  0.92425  0.9333 -3.5013
      6:         150       3 77.88 -2.914 -1.92467  3.1653 -2.2555

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

