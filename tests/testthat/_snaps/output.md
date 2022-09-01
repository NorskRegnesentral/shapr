# output_lm_numeric_independence

    Code
      (out <- code)
    Output
           none  Solar.R   Wind   Temp    Month     Day
      1: 40.752  4.42903 19.814 12.271  0.77897 -4.0031
      2: 40.752 -0.69895 10.523 25.541 -5.30094 -4.0031

# output_lm_numeric_empirical

    Code
      (out <- code)
    Output
           none Solar.R    Wind     Temp  Month     Day
      1: 40.752  7.2098 22.0306  0.41757 5.8050 -2.1733
      2: 40.752  5.5946 -1.7351 18.17269 1.3886  2.6399

# output_lm_numeric_empirical_independence

    Code
      (out <- code)
    Warning <simpleWarning>
      Using empirical.type = 'independence' for approach = 'empirical' is deprecated.
      Please use approach = 'independence' instead.
    Message <simpleMessage>
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
    Output
           none  Solar.R   Wind   Temp    Month     Day
      1: 40.752  4.42903 19.814 12.271  0.77897 -4.0031
      2: 40.752 -0.69895 10.523 25.541 -5.30094 -4.0031

# output_lm_numeric_empirical_AICc_each

    Code
      (out <- code)
    Output
           none Solar.R   Wind    Temp   Month     Day
      1: 40.752  9.7136 27.305 -8.9187 13.4754 -8.2860
      2: 40.752  5.8227 15.143  5.0823 -4.8571  4.8696

# output_lm_numeric_empirical_AICc_full

    Code
      (out <- code)
    Output
           none Solar.R   Wind    Temp   Month     Day
      1: 40.752  9.9617 25.455 -9.0014 15.9878 -9.1131
      2: 40.752  5.1766 15.687  5.2976 -5.8801  5.7797

# output_lm_numeric_gaussian

    Code
      (out <- code)
    Output
           none Solar.R    Wind    Temp   Month     Day
      1: 40.752  6.3579 24.4431  4.7647 0.36934 -2.6453
      2: 40.752 -2.2563  9.8937 20.8231 0.74187 -3.1416

# output_lm_numeric_copula

    Code
      (out <- code)
    Output
           none Solar.R   Wind    Temp    Month     Day
      1: 40.752  5.9289 24.815  5.2796  0.37793 -3.1117
      2: 40.752 -1.8044 10.121 21.7839 -0.46705 -3.5726

# output_lm_numeric_comb1

    Code
      (out <- code)
    Output
           none Solar.R    Wind    Temp   Month     Day
      1: 40.752  6.3245 24.6634  4.5405 0.29326 -2.5319
      2: 40.752 -1.9993  9.1793 20.4181 0.96189 -2.4993

# output_lm_numeric_comb3

    Code
      (out <- code)
    Output
           none  Solar.R   Wind   Temp    Month     Day
      1: 40.752  4.42903 19.814 12.271  0.77897 -4.0031
      2: 40.752 -0.69895 10.523 25.541 -5.30094 -4.0031

# output_lm_mixed_independence

    Code
      (out <- code)
    Output
           none  Solar.R    Wind   Temp     Day Month_factor
      1: 40.752  4.61196 18.6061 12.444 -3.3478     -0.72777
      2: 40.752 -0.72853  9.8887 25.898 -3.3478     -7.34468

# output_custom_lm_numeric_independence_1

    Code
      (out <- code)
    Output
           none  Solar.R   Wind   Temp    Month     Day
      1: 40.752  4.42903 19.814 12.271  0.77897 -4.0031
      2: 40.752 -0.69895 10.523 25.541 -5.30094 -4.0031

# output_custom_lm_numeric_independence_2

    Code
      (out <- code)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
           none  Solar.R   Wind   Temp    Month     Day
      1: 40.752  4.42903 19.814 12.271  0.77897 -4.0031
      2: 40.752 -0.69895 10.523 25.541 -5.30094 -4.0031

