# forecast_output_ar_numeric

    Code
      print({
        out <- code
      }, digits = digits)
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
      print({
        out <- code
      }, digits = digits)
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
      print({
        out <- code
      }, digits = digits)
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
         explain_idx horizon  none Temp.1  Temp.2 Temp.3 Wind.1  Wind.2  Wind.3
               <int>   <int> <num>  <num>   <num>  <num>  <num>   <num>   <num>
      1:         149       1 77.88 -3.330 -4.2545 -1.512  1.743 -1.6209  0.3222
      2:         150       1 77.88  3.775 -0.4792 -4.730 -2.059  0.7765 -2.4722
      3:         149       2 77.88 -3.209 -4.1088 -1.567  1.059 -1.8442  0.8195
      4:         150       2 77.88  3.917 -0.9780 -5.590 -2.271  1.4698 -1.8701
      5:         149       3 77.88 -4.298 -3.8666 -2.047  1.725 -1.8183  0.1713
      6:         150       3 77.88  2.991 -0.9145 -5.456 -2.146  0.1396 -1.1337
         Wind.F1 Wind.F2 Wind.F3
           <num>   <num>   <num>
      1: -1.8552      NA      NA
      2: -0.4466      NA      NA
      3: -1.8192 -0.2149      NA
      4:  0.4641  1.3828      NA
      5: -1.1984  0.7946  1.8118
      6:  0.6710  0.7106  0.4627

# forecast_output_arima_numeric_iterative_groups

    Code
      print({
        out <- code
      }, digits = digits)
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
      3:         149       2 77.88 -6.547 -4.28292  3.6084 -2.8124
      4:         150       2 77.88 -3.311  0.05915  1.6234 -2.7149
      5:         149       3 77.88 -7.805  1.09541  0.6762 -3.1756
      6:         150       3 77.88 -3.532 -2.01359  3.2416 -1.6250

# forecast_output_arima_numeric_no_xreg

    Code
      print({
        out <- code
      }, digits = digits)
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
      print({
        out <- code
      }, digits = digits)
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
      print({
        out <- code
      }, digits = digits)
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

# forecast_output_forecast_ARIMA_manual_group_numeric

    Code
      print({
        out <- code
      }, digits = digits)
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

# forecast_output_forecast_ARIMA_manual_group_numeric2

    Code
      print({
        out <- code
      }, digits = digits)
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
         explain_idx horizon  none  Group1 Group2
               <int>   <int> <num>   <num>  <num>
      1:         149       1 77.88 -2.5593 -7.948
      2:         150       1 77.88 -0.5681 -5.067
      3:         149       2 77.88 -2.1223 -8.762
      4:         150       2 77.88  0.7271 -4.203

