# forecast_output_ar_numeric

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 4`, and is therefore set to `2^n_features = 4`.
      
      -- Explanation overview --
      
      * Model class: <ar>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
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
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 128`, and is therefore set to `2^n_features = 128`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
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
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 9
      * Number of observations to explain: 2
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 10 of 512 coalitions, 10 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 60 of 512 coalitions, 50 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 150 of 512 coalitions, 90 new. 
    Output
         explain_idx horizon  none Temp.1  Temp.2 Temp.3 Wind.1  Wind.2   Wind.3
               <int>   <int> <num>  <num>   <num>  <num>  <num>   <num>    <num>
      1:         149       1 77.88 -3.331 -4.2562 -1.515  1.748 -1.6239  0.32386
      2:         150       1 77.88  3.773 -0.4796 -4.731 -2.059  0.7812 -2.47505
      3:         149       2 77.88 -3.220 -4.1919 -1.392  1.506 -2.4946  1.00722
      4:         150       2 77.88  3.822 -0.9555 -5.185 -2.452  0.7095 -1.73152
      5:         149       3 77.88 -3.788 -4.1981 -1.995  2.084 -1.1141  0.07542
      6:         150       3 77.88  3.261 -0.5533 -5.712 -1.802  0.9030 -1.47742
         Wind.F1 Wind.F2 Wind.F3
           <num>   <num>   <num>
      1: -1.8530      NA      NA
      2: -0.4456      NA      NA
      3: -1.9276 -0.1705      NA
      4:  0.7845  1.5313      NA
      5: -1.7927  0.3282  1.6758
      6: -0.1910  0.1644  0.7316

# forecast_output_arima_numeric_iterative_groups

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 16`, and is therefore set to `2^n_groups = 16`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 4
      * Number of observations to explain: 2
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 10 of 16 coalitions, 10 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 12 of 16 coalitions, 2 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 14 of 16 coalitions, 2 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 16 of 16 coalitions, 2 new. 
    Output
         explain_idx horizon  none   Temp    Wind Solar.R   Ozone
               <int>   <int> <num>  <num>   <num>   <num>   <num>
      1:         149       1 77.88 -4.688 -3.7146  0.3129 -1.1907
      2:         150       1 77.88 -2.496 -3.6594  1.8379 -0.8509
      3:         149       2 77.88 -6.001 -4.2811  2.6461 -2.3982
      4:         150       2 77.88 -3.146  0.1632  0.8301 -2.1910
      5:         149       3 77.88 -7.790  1.1422  0.7702 -3.3314
      6:         150       3 77.88 -3.164 -1.7441  2.9928 -2.0133

# forecast_output_arima_numeric_no_xreg

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 4`, and is therefore set to `2^n_features = 4`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
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
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <fc_model>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
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
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_features = 8`, and is therefore set to `2^n_features = 8`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
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
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <fc_model>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 2
      * Feature groups: Temp: {"Temp.1", "Temp.2"}; Wind.1: {"Wind.1", "Wind.2",
      "Wind.F1"}; Wind.2: {"Wind.1", "Wind.2", "Wind.F1", "Wind.F2"}
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
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contain `NA`.
        Consistency checks between model and data are therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <fc_model>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of group-wise Shapley values: 2
      * Feature groups: Group1.1: {"Wind.1", "Temp.1"}; Group1.2: {"Wind.1",
      "Temp.1", "Wind.F2"}; Group2: {"Wind.2", "Temp.2", "Wind.F1"}
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

