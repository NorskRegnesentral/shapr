# forecast_output_ar_numeric

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 4`, and is therefore set to `2^n_features = 4`.
      
      -- Explanation overview --
      
      * Model class: <ar>
      * Approach: empirical
      * Procedure: Non-iterative
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
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 128`, and is therefore set to `2^n_features = 128`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * Approach: empirical
      * Procedure: Non-iterative
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
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * Approach: empirical
      * Procedure: Iterative
      * Number of feature-wise Shapley values: 9
      * Number of observations to explain: 2
      
      -- Iterative computation started --
      
      -- Iteration 1 -----------------------------------------------------------------
      i Using 10 of 512 coalitions, 10 new. 
      
      -- Iteration 2 -----------------------------------------------------------------
      i Using 46 of 512 coalitions, 36 new. 
      
      -- Iteration 3 -----------------------------------------------------------------
      i Using 140 of 512 coalitions, 94 new. 
      
      -- Iteration 4 -----------------------------------------------------------------
      i Using 150 of 512 coalitions, 10 new. 
    Output
         explain_idx horizon  none Temp.1  Temp.2 Temp.3 Wind.1  Wind.2   Wind.3
               <int>   <int> <num>  <num>   <num>  <num>  <num>   <num>    <num>
      1:         149       1 77.88 -3.323 -4.2395 -1.485  1.701 -1.5930  0.30677
      2:         150       1 77.88  3.788 -0.4755 -4.723 -2.058  0.7345 -2.44627
      3:         149       2 77.88 -3.523 -3.2878 -1.729  1.193 -1.7219 -0.01723
      4:         150       2 77.88  3.640 -1.0120 -5.431 -2.265  0.9635 -1.01114
      5:         149       3 77.88 -4.582 -3.9153 -1.782  1.538 -0.5771  0.36957
      6:         150       3 77.88  3.364 -1.5522 -5.396 -0.977  0.4458 -0.64450
         Wind.F1 Wind.F2 Wind.F3
           <num>   <num>   <num>
      1: -1.8743      NA      NA
      2: -0.4556      NA      NA
      3: -1.3708 -0.4275      NA
      4:  0.5539  1.0857      NA
      5: -1.1117  0.4598  0.8754
      6:  0.2497  0.2212 -0.3865

# forecast_output_arima_numeric_iterative_groups

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Starting `shapr::explain_forecast()` ----------------------------------------
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_groups = 16`, and is therefore set to `2^n_groups = 16`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * Approach: empirical
      * Procedure: Iterative
      * Number of group-wise Shapley values: 4
      * Feature groups: Temp: "Temp.1", "Temp.2", "Temp.3"; Wind.1: "Wind.1",
      "Wind.2", "Wind.3", "Wind.F1"; Wind.2: "Wind.1", "Wind.2", "Wind.3", "Wind.F1",
      "Wind.F2"; Wind.3: "Wind.1", "Wind.2", "Wind.3", "Wind.F1", "Wind.F2",
      "Wind.F3"; Solar.R.1: "Solar.R.1", "Solar.R.2", "Solar.R.3", "Solar.R.F1";
      Solar.R.2: "Solar.R.1", "Solar.R.2", "Solar.R.3", "Solar.R.F1", "Solar.R.F2";
      Solar.R.3: "Solar.R.1", "Solar.R.2", "Solar.R.3", "Solar.R.F1", "Solar.R.F2",
      "Solar.R.F3"; Ozone.1: "Ozone.1", "Ozone.2", "Ozone.3", "Ozone.F1"; Ozone.2:
      "Ozone.1", "Ozone.2", "Ozone.3", "Ozone.F1", "Ozone.F2"; Ozone.3: "Ozone.1",
      "Ozone.2", "Ozone.3", "Ozone.F1", "Ozone.F2", "Ozone.F3"
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
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 4`, and is therefore set to `2^n_features = 4`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * Approach: empirical
      * Procedure: Non-iterative
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
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <forecast_ARIMA>
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of group-wise Shapley values: 2
      * Feature groups: Temp: "Temp.1", "Temp.2"; Wind.1: "Wind.1", "Wind.2",
      "Wind.F1"; Wind.2: "Wind.1", "Wind.2", "Wind.F1", "Wind.F2"; Wind.3: "Wind.1",
      "Wind.2", "Wind.F1", "Wind.F2", "Wind.F3"
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
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_features = 8`, and is therefore set to `2^n_features = 8`.
      
      -- Explanation overview --
      
      * Model class: <Arima>
      * Approach: independence
      * Procedure: Non-iterative
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
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <forecast_ARIMA>
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of group-wise Shapley values: 2
      * Feature groups: Temp: "Temp.1", "Temp.2"; Wind.1: "Wind.1", "Wind.2",
      "Wind.F1"; Wind.2: "Wind.1", "Wind.2", "Wind.F1", "Wind.F2"
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
      i Feature names extracted from the model contains `NA`.
        Consistency checks between model and data is therefore disabled.
      i `max_n_coalitions` is `NULL` or larger than or `2^n_groups = 4`, and is therefore set to `2^n_groups = 4`.
      
      -- Explanation overview --
      
      * Model class: <forecast_ARIMA>
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of group-wise Shapley values: 2
      * Feature groups: Group1.1: "Wind.1", "Temp.1"; Group1.2: "Wind.1", "Temp.1",
      "Wind.F2"; Group2: "Wind.2", "Temp.2", "Wind.F1"
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

