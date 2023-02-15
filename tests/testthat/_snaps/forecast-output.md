# forecast_output_ar_numeric

    Code
      (out <- code)
    Message <simpleMessage>
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Output
         explain_idx horizon  none  Temp.1  Temp.2
      1:         152       1 77.88 -0.3972 -1.3912
      2:         153       1 77.88 -6.6177 -0.1835
      3:         152       2 77.88 -0.3285 -1.2034
      4:         153       2 77.88 -6.0208 -0.3371
      5:         152       3 77.88 -0.2915 -1.0552
      6:         153       3 77.88 -5.2122 -0.2553

# forecast_output_arima_numeric

    Code
      (out <- code)
    Message <simpleMessage>
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Output
         explain_idx horizon  none  Temp.1 Temp.2  Wind.1  Wind.2 Wind.F1  Wind.F2
      1:         149       1 77.88 -1.1168 -9.139  1.3714 -0.9526 -2.7547       NA
      2:         150       1 77.88  0.8647 -6.727 -1.7107  0.8565 -0.8984       NA
      3:         149       2 77.88 -0.1253 -9.349  0.8574 -1.6337 -1.8951 -0.82325
      4:         150       2 77.88  1.2315 -5.253 -1.7923  0.7609 -0.6036  0.20064
      5:         149       3 77.88 -1.1233 -9.491  1.2840 -0.8459 -1.6030 -0.01238
      6:         150       3 77.88  1.1982 -5.608 -1.2576  0.5160 -0.7735 -0.44522
         Wind.F3
      1:      NA
      2:      NA
      3:      NA
      4:      NA
      5:  0.9819
      6: -0.2849

# forecast_output_forecast_ARIMA_group_numeric

    Code
      (out <- code)
    Message <simpleMessage>
      Note: Feature names extracted from the model contains NA.
      Consistency checks between model and data is therefore disabled.
      
    Output
         explain_idx horizon  none   Temp   Wind
      1:         149       1 77.88 -8.458 -4.134
      2:         150       1 77.88 -4.082 -3.533
      3:         149       2 77.88 -6.382 -6.587
      4:         150       2 77.88 -1.913 -3.542
      5:         149       3 77.88 -9.830 -0.980
      6:         150       3 77.88 -2.624 -4.032

