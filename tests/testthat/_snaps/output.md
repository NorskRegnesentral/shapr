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
           none Solar.R    Wind   Temp   Month    Day
      1: 40.752 -18.549 19.4454 10.011 19.5361 2.8456
      2: 40.752 -17.486  6.4034 28.922  4.8873 3.3335

# output_lm_numeric_empirical_AICc_full

    Code
      (out <- code)
    Output
           none Solar.R    Wind   Temp   Month    Day
      1: 40.752 -19.515 18.8463 10.479 20.7526 2.7263
      2: 40.752 -17.621  4.7818 29.694  6.1072 3.0986

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

# output_lm_numeric_ctree

    Code
      (out <- code)
    Output
           none Solar.R    Wind    Temp   Month     Day
      1: 40.752 5.69022 24.4988  2.3756  4.6042 -3.8790
      2: 40.752 0.29536  8.3215 24.1690 -2.3619 -4.3632

# output_lm_numeric_comb1

    Code
      (out <- code)
    Output
           none Solar.R    Wind    Temp   Month     Day
      1: 40.752  6.3245 24.6634  4.5405 0.29326 -2.5319
      2: 40.752 -1.9993  9.1793 20.4181 0.96189 -2.4993

# output_lm_numeric_comb2

    Code
      (out <- code)
    Output
           none Solar.R   Wind    Temp   Month     Day
      1: 40.752 5.75944 24.394  2.4967  4.5293 -3.8896
      2: 40.752 0.16416  8.372 24.1075 -2.3507 -4.2323

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

# output_lm_mixed_ctree

    Code
      (out <- code)
    Output
           none  Solar.R    Wind    Temp     Day Month_factor
      1: 40.752  4.99231 22.6295  1.6231 -3.5155       5.8569
      2: 40.752 -0.25185  7.8194 26.8071 -1.1590      -8.8504

# output_lm_mixed_comb

    Code
      (out <- code)
    Output
           none  Solar.R    Wind    Temp      Day Month_factor
      1: 40.752  5.00933 22.6405  1.5599 -3.48455       5.8612
      2: 40.752 -0.49171  8.1241 26.3643 -0.74807      -8.8834

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

# output_custom_xgboost_mixed_dummy_ctree

    Code
      (out <- code)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
           none  Solar.R   Wind   Temp     Day Month_factor
      1: 40.752 13.96512 76.943 12.957 -3.6138       3.2694
      2: 40.752 -0.86312  1.618 34.588 -9.4797      -5.5401

# output_lm_numeric_interaction

    Code
      (out <- code)
    Output
           none Solar.R   Wind
      1: 40.752 15.3512 36.655
      2: 40.752 -2.6185 16.095

