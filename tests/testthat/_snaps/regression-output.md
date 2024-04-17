# output_lm_numeric_lm_separate

    Code
      (out <- code)
    Output
          none Solar.R    Wind     Temp   Month     Day
         <num>   <num>   <num>    <num>   <num>   <num>
      1: 42.44  -8.577   7.845  14.4756  0.6251 -1.7664
      2: 42.44   4.818  -4.811 -11.6350 -1.0423 -1.2086
      3: 42.44   7.406 -25.587   0.3353 -0.4718  0.7491

# output_lm_numeric_lm_separate_n_comb

    Code
      (out <- code)
    Output
          none Solar.R    Wind    Temp Month     Day
         <num>   <num>   <num>   <num> <num>   <num>
      1: 42.44  -7.806  14.811   5.751 4.056 -4.2111
      2: 42.44   5.056  -7.055 -16.887 5.976 -0.9692
      3: 42.44   7.020 -33.059   2.395 3.782  2.2943

# output_lm_categorical_lm_separate

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
         <num>        <num>              <num>          <num>       <num>
      1: 42.44       -9.806              18.60        -11.788       2.489
      2: 42.44       -7.256             -18.88         24.751     -13.445
      3: 42.44       15.594             -26.01          5.887     -13.834

# output_lm_mixed_lm_separate

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp     Day Month_factor
         <num>   <num>   <num>  <num>   <num>        <num>
      1: 42.44  -8.782   8.165 20.389 -1.2383       -7.950
      2: 42.44   4.623  -3.551 -6.199 -0.9110       -9.345
      3: 42.44   8.029 -25.200 -4.821  0.4172       10.975

# output_lm_mixed_splines_separate

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Day Month_factor
         <num>   <num>   <num>  <num>  <num>        <num>
      1: 42.44  -8.083   7.102 18.732  1.483       -8.651
      2: 42.44   6.147  -4.314 -6.445 -2.136       -8.635
      3: 42.44   7.536 -22.504 -5.081 -2.170       11.619

# output_lm_mixed_decision_tree_cv_separate

    Code
      (out <- code)
    Output
          none Solar.R   Wind    Temp     Day Month_factor
         <num>   <num>  <num>   <num>   <num>        <num>
      1: 42.44  -8.131 12.303   9.935  1.6221       -5.145
      2: 42.44   2.907 -5.119  -7.128  1.7841       -7.827
      3: 42.44   6.237 -9.010 -17.927 -0.6915       10.791

# output_lm_mixed_decision_tree_cv_separate_parallel

    Code
      (out <- code)
    Output
          none Solar.R   Wind    Temp     Day Month_factor
         <num>   <num>  <num>   <num>   <num>        <num>
      1: 42.44  -8.131 12.303   9.935  1.6221       -5.145
      2: 42.44   2.907 -5.119  -7.128  1.7841       -7.827
      3: 42.44   6.237 -9.010 -17.927 -0.6915       10.791

# output_lm_mixed_xgboost_separate

    Code
      (out <- code)
    Output
          none Solar.R    Wind    Temp    Day Month_factor
         <num>   <num>   <num>   <num>  <num>        <num>
      1: 42.44 -13.991  14.352  16.490   1.82       -8.088
      2: 42.44   8.183  -1.463 -16.499   3.63       -9.233
      3: 42.44   3.364 -14.946   0.401 -11.32       11.905

# output_lm_numeric_lm_surrogate

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp   Month     Day
         <num>   <num>   <num>  <num>   <num>   <num>
      1: 42.44  -9.273   9.578 16.536 -1.2690 -2.9707
      2: 42.44   2.623  -5.766 -6.717 -1.4694 -2.5496
      3: 42.44   6.801 -24.090 -1.295  0.1202  0.8953

# output_lm_numeric_lm_surrogate_n_comb

    Code
      (out <- code)
    Output
          none Solar.R     Wind     Temp   Month     Day
         <num>   <num>    <num>    <num>   <num>   <num>
      1: 42.44 -9.6804  12.2171  11.4871 0.74529 -2.1671
      2: 42.44  0.6882   0.3332 -12.8835 1.93235 -3.9496
      3: 42.44  7.8022 -26.0731  -0.2148 0.04831  0.8691

# output_lm_numeric_lm_surrogate_reg_surr_n_comb

    Code
      (out <- code)
    Output
          none Solar.R     Wind     Temp   Month     Day
         <num>   <num>    <num>    <num>   <num>   <num>
      1: 42.44 -9.6804  12.2171  11.4871 0.74529 -2.1671
      2: 42.44  0.6882   0.3332 -12.8835 1.93235 -3.9496
      3: 42.44  7.8022 -26.0731  -0.2148 0.04831  0.8691

# output_lm_categorical_lm_surrogate

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
         <num>        <num>              <num>          <num>       <num>
      1: 42.44       -7.137              16.29         -9.895      0.2304
      2: 42.44       -6.018             -16.28         23.091    -15.6258
      3: 42.44       10.042             -18.58          2.415    -12.2431

# output_lm_mixed_lm_surrogate

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp     Day Month_factor
         <num>   <num>   <num>  <num>   <num>        <num>
      1: 42.44  -7.427  10.831 16.477 -0.6280       -8.669
      2: 42.44   3.916  -4.232 -4.849 -0.8776       -9.341
      3: 42.44   5.629 -24.012 -2.274 -0.4774       10.534

# output_lm_mixed_decision_tree_cv_surrogate

    Code
      (out <- code)
    Output
          none Solar.R   Wind   Temp    Day Month_factor
         <num>   <num>  <num>  <num>  <num>        <num>
      1: 42.44  -4.219 -4.219 27.460 -4.219       -4.219
      2: 42.44  -3.077 -3.077 -3.077 -3.077       -3.077
      3: 42.44  -6.716 -6.716 -6.716 -6.716       16.262

# output_lm_mixed_xgboost_surrogate

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Day Month_factor
         <num>   <num>   <num>  <num>  <num>        <num>
      1: 42.44 -11.165   8.002  20.61  2.030       -8.896
      2: 42.44   4.143  -1.515 -11.23  2.025       -8.806
      3: 42.44   6.515 -18.268  -4.06 -3.992        9.204

