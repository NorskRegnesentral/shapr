# summary_explain

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Summary of Shapley value explanation ----------------------------------------
      * Computed with `shapr::explain()`
      * Model class: <lm>
      * v(S) estimation class: Monte Carlo integration
      * Approach: independence
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 5
      * Number of coalitions used: 32 (of total 32)
      * Number of observations to explain: 3
      
      -- Estimated Shapley values 
         explain_id   none Solar.R   Wind   Temp    Day Month_factor
              <int> <char>  <char> <char> <char> <char>       <char>
      1:          1  42.44   -4.73   7.75  17.75  -2.60        -7.59
      2:          2  42.44    2.34  -3.15  -5.31  -1.68        -7.59
      3:          3  42.44    3.86 -17.47  -1.47   1.10         3.38
      
      -- Estimated MSEv 
      Estimated MSE of v(S) = 115 (with sd = 20)

# summary_explain_forecast

    Code
      print({
        out <- code
      }, digits = digits)
    Message
      
      -- Summary of Shapley value explanation ----------------------------------------
      * Computed with `shapr::explain_forecast()`
      * Model class: <ar>
      * v(S) estimation class: Monte Carlo integration
      * Approach: empirical
      * Procedure: Non-iterative
      * Number of Monte Carlo integration samples: 1000
      * Number of feature-wise Shapley values: 2
      * Number of coalitions used: 4 (of total 4)
      * Number of observations to explain: 2
      
      -- Estimated Shapley values 
         explain_idx horizon   none Temp.1 Temp.2
               <int>   <int> <char> <char> <char>
      1:         152       1  77.88  -0.40  -1.39
      2:         153       1  77.88  -6.62  -0.18
      3:         152       2  77.88  -0.33  -1.20
      4:         153       2  77.88  -6.02  -0.34
      5:         152       3  77.88  -0.29  -1.06
      6:         153       3  77.88  -5.21  -0.26

