# output_lm_numeric_independence

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp   Month     Day
      1: 42.444  -4.537   8.2692 17.5175 -5.5812 -3.0665
      2: 42.444   2.250  -3.3453 -5.2319 -5.5812 -1.9710
      3: 42.444   3.708 -18.6102 -1.4404 -2.5412  1.3155

# output_lm_numeric_empirical

    Code
      (out <- code)
    Output
           none  Solar.R     Wind    Temp     Month     Day
      1: 42.444 -13.2521  15.5408 12.8258 -5.771786  3.2593
      2: 42.444   2.7580  -3.3247 -7.9923 -7.127997  1.8075
      3: 42.444   6.8046 -22.1255  3.7300 -0.092345 -5.8849

# output_lm_numeric_empirical_n_combinations

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp   Month     Day
      1: 42.444 -5.7946  15.3199  8.5572 -7.5465  2.0660
      2: 42.444  3.2660  -3.2518 -7.6927 -7.6628  1.4620
      3: 42.444  4.2901 -24.3947  6.7387 -1.0058 -3.1965

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
           none Solar.R     Wind    Temp   Month     Day
      1: 42.444  -4.537   8.2692 17.5175 -5.5812 -3.0665
      2: 42.444   2.250  -3.3453 -5.2319 -5.5812 -1.9710
      3: 42.444   3.708 -18.6102 -1.4404 -2.5412  1.3155

# output_lm_numeric_empirical_AICc_each

    Code
      (out <- code)
    Output
           none Solar.R     Wind      Temp     Month      Day
      1: 42.444 -15.661   6.8230  17.50921   0.24632  3.68474
      2: 42.444  10.697  -1.0634 -10.68038 -13.03054  0.19829
      3: 42.444  14.648 -19.9462   0.96752  -7.34330 -5.89456

# output_lm_numeric_empirical_AICc_full

    Code
      (out <- code)
    Output
           none Solar.R      Wind      Temp     Month      Day
      1: 42.444 -14.975   6.31705  17.41032   0.28761  3.56230
      2: 42.444  12.419   0.14816 -10.23384 -16.40958  0.19667
      3: 42.444  15.741 -19.72496   0.99925  -8.69501 -5.88863

# output_lm_numeric_gaussian

    Code
      (out <- code)
    Output
           none Solar.R     Wind      Temp    Month      Day
      1: 42.444 -8.5451   7.7793  14.58571  0.44747 -1.66534
      2: 42.444  4.8261  -4.2948 -11.65488 -1.12497 -1.63091
      3: 42.444  7.1628 -25.4913   0.36802 -0.54551  0.93769

# output_lm_numeric_copula

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp    Month     Day
      1: 42.444 -6.3711   7.3549 14.4696 -0.61077 -2.2407
      2: 42.444  4.1149  -4.1593 -9.9804 -1.93783 -1.9168
      3: 42.444  5.9322 -25.0855  1.8573 -1.36243  1.0902

# output_lm_numeric_ctree

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp    Month     Day
      1: 42.444 -9.1239   9.5091 17.1391 -1.47110 -3.4511
      2: 42.444  5.3416  -6.0971 -8.2321 -2.81286 -2.0790
      3: 42.444  6.9013 -21.0785 -4.6865  0.14938  1.1461

# output_lm_categorical_ctree

    Code
      (out <- code)
    Output
           none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
      1: 42.444      -6.2061             15.375        -6.7052     -2.9727
      2: 42.444      -5.7645            -17.713        21.8663    -13.2194
      3: 42.444       7.1013            -21.782         1.7300     -5.4132

# output_lm_categorical_categorical

    Code
      (out <- code)
    Output
           none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
      1: 42.444      13.6562            -19.729         4.3687    -16.6592
      2: 42.444      -5.4477             11.306       -11.4448      5.0777
      3: 42.444      -7.4925            -12.267        19.6723    -14.7440

# output_lm_categorical_independence

    Code
      (out <- code)
    Output
           none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
      1: 42.444      -5.2522             13.951       -7.04069     -2.1667
      2: 42.444      -5.2522            -15.615       20.08562    -14.0497
      3: 42.444       4.8331            -15.615        0.59599     -8.1782

# output_lm_ts_timeseries

    Code
      (out <- code)
    Output
           none       S1      S2        S3       S4
      1: 4.8953 -0.52607 0.78309 -0.210234 -0.38846
      2: 4.8953 -0.63103 1.62881 -0.044985 -2.92975

# output_lm_numeric_comb1

    Code
      (out <- code)
    Output
           none Solar.R     Wind      Temp    Month      Day
      1: 42.444 -8.5797   7.9995  14.36083  0.37186 -1.55051
      2: 42.444  5.0779  -5.0142 -12.06444 -0.89628 -0.98246
      3: 42.444  7.2759 -25.4485   0.39529 -0.38682  0.59586

# output_lm_numeric_comb2

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp       Month     Day
      1: 42.444 -8.9566   9.4004 17.0610 -1.46782098 -3.4350
      2: 42.444  5.3075  -5.9321 -8.1260 -2.93112830 -2.1977
      3: 42.444  6.9132 -20.9687 -4.5388 -0.00042251  1.0265

# output_lm_numeric_comb3

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp   Month     Day
      1: 42.444  -4.537   8.2692 17.5175 -5.5812 -3.0665
      2: 42.444   2.250  -3.3453 -5.2319 -5.5812 -1.9710
      3: 42.444   3.708 -18.6102 -1.4404 -2.5412  1.3155

# output_lm_mixed_independence

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp     Day Month_factor
      1: 42.444 -4.7299   7.7495 17.7533 -2.6010      -7.5881
      2: 42.444  2.3383  -3.1472 -5.3100 -1.6761      -7.5881
      3: 42.444  3.8567 -17.4686 -1.4661  1.0987       3.3792

# output_lm_mixed_ctree

    Code
      (out <- code)
    Output
           none Solar.R    Wind    Temp      Day Month_factor
      1: 42.444 -9.1653  11.815 13.1837 -0.44731      -4.8021
      2: 42.444  3.6525  -5.782 -6.5237 -0.43490      -6.2950
      3: 42.444  6.2682 -21.441 -7.3226  1.63305      10.2624

# output_lm_mixed_comb

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp      Day Month_factor
      1: 42.444 -9.1534  11.4919 13.3405 -0.22906      -4.8662
      2: 42.444  3.9355  -5.7652 -6.4033 -0.44621      -6.7038
      3: 42.444  6.1291 -21.4164 -7.2078  1.55138      10.3436

# output_custom_lm_numeric_independence_1

    Code
      (out <- code)
    Output
           none Solar.R     Wind    Temp   Month     Day
      1: 42.444  -4.537   8.2692 17.5175 -5.5812 -3.0665
      2: 42.444   2.250  -3.3453 -5.2319 -5.5812 -1.9710
      3: 42.444   3.708 -18.6102 -1.4404 -2.5412  1.3155

# output_custom_lm_numeric_independence_2

    Code
      (out <- code)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
           none Solar.R     Wind    Temp   Month     Day
      1: 42.444  -4.537   8.2692 17.5175 -5.5812 -3.0665
      2: 42.444   2.250  -3.3453 -5.2319 -5.5812 -1.9710
      3: 42.444   3.708 -18.6102 -1.4404 -2.5412  1.3155

# output_custom_xgboost_mixed_dummy_ctree

    Code
      (out <- code)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
           none Solar.R    Wind    Temp      Day Month_factor
      1: 42.444 -5.6026  13.054  20.434 0.085081      -0.2664
      2: 42.444  4.6447 -12.570 -16.653 1.291334      -2.1574
      3: 42.444  5.4512 -14.006 -19.719 1.325035       6.3851

# output_lm_numeric_interaction

    Code
      (out <- code)
    Output
           none  Solar.R     Wind
      1: 42.444 -13.8181  10.5789
      2: 42.444   4.6423  -6.2871
      3: 42.444   4.4519 -34.6024

