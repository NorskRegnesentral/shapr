# output_lm_numeric_independence

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Month    Day
      1: 42.44 -13.252  15.541 12.826 -5.77179  3.259
      2: 42.44   2.758  -3.325 -7.992 -7.12800  1.808
      3: 42.44   6.805 -22.126  3.730 -0.09235 -5.885

# output_lm_numeric_empirical_n_combinations

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
      1: 42.44  -5.795  15.320  8.557 -7.547  2.066
      2: 42.44   3.266  -3.252 -7.693 -7.663  1.462
      3: 42.44   4.290 -24.395  6.739 -1.006 -3.197

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
          none Solar.R    Wind   Temp  Month    Day
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical_AICc_each

    Code
      (out <- code)
    Output
          none Solar.R    Wind     Temp    Month     Day
      1: 42.44  -15.66   6.823  17.5092   0.2463  3.6847
      2: 42.44   10.70  -1.063 -10.6804 -13.0305  0.1983
      3: 42.44   14.65 -19.946   0.9675  -7.3433 -5.8946

# output_lm_numeric_empirical_AICc_full

    Code
      (out <- code)
    Output
          none Solar.R     Wind     Temp    Month     Day
      1: 42.44  -14.98   6.3170  17.4103   0.2876  3.5623
      2: 42.44   12.42   0.1482 -10.2338 -16.4096  0.1967
      3: 42.44   15.74 -19.7250   0.9992  -8.6950 -5.8886

# output_lm_numeric_gaussian

    Code
      (out <- code)
    Output
          none Solar.R    Wind    Temp   Month     Day
      1: 42.44  -8.545   7.779  14.586  0.4475 -1.6653
      2: 42.44   4.826  -4.295 -11.655 -1.1250 -1.6309
      3: 42.44   7.163 -25.491   0.368 -0.5455  0.9377

# output_lm_numeric_copula

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp   Month    Day
      1: 42.44  -6.371   7.355 14.470 -0.6108 -2.241
      2: 42.44   4.115  -4.159 -9.980 -1.9378 -1.917
      3: 42.44   5.932 -25.086  1.857 -1.3624  1.090

# output_lm_numeric_ctree

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp   Month    Day
      1: 42.44  -9.124   9.509 17.139 -1.4711 -3.451
      2: 42.44   5.342  -6.097 -8.232 -2.8129 -2.079
      3: 42.44   6.901 -21.079 -4.687  0.1494  1.146

# output_lm_categorical_ctree

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
      1: 42.44       -6.206              15.38         -6.705      -2.973
      2: 42.44       -5.764             -17.71         21.866     -13.219
      3: 42.44        7.101             -21.78          1.730      -5.413

# output_lm_categorical_categorical

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
      1: 42.44       13.656             -19.73          4.369     -16.659
      2: 42.44       -5.448              11.31        -11.445       5.078
      3: 42.44       -7.493             -12.27         19.672     -14.744

# output_lm_categorical_independence

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
      1: 42.44       -5.252              13.95         -7.041      -2.167
      2: 42.44       -5.252             -15.61         20.086     -14.050
      3: 42.44        4.833             -15.61          0.596      -8.178

# output_lm_ts_timeseries

    Code
      (out <- code)
    Output
          none      S1     S2       S3      S4
      1: 4.895 -0.5261 0.7831 -0.21023 -0.3885
      2: 4.895 -0.6310 1.6288 -0.04498 -2.9298

# output_lm_numeric_comb1

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp   Month     Day
      1: 42.44  -8.809   9.149 15.503 -2.8888 -0.3522
      2: 42.44   3.146  -4.566 -7.727 -4.3771 -0.3559
      3: 42.44   6.655 -22.559 -1.645  0.5634 -0.5832

# output_lm_numeric_comb2

    Code
      (out <- code)
    Output
          none Solar.R    Wind    Temp  Month     Day
      1: 42.44  -9.302   9.454 17.2106 -1.767 -2.9936
      2: 42.44   5.189  -5.352 -8.5382 -2.854 -2.3245
      3: 42.44   6.388 -22.748  0.0177 -1.441  0.2159

# output_lm_numeric_comb3

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month      Day
      1: 42.44  -6.940  10.773 12.187 -3.692  0.27495
      2: 42.44   2.628  -2.656 -8.569 -5.313  0.03032
      3: 42.44   5.827 -22.183  3.440 -2.954 -1.69839

# output_lm_mixed_independence

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Day Month_factor
      1: 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2: 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3: 42.44   3.857 -17.469 -1.466  1.099        3.379

# output_lm_mixed_ctree

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp     Day Month_factor
      1: 42.44  -9.165  11.815 13.184 -0.4473       -4.802
      2: 42.44   3.652  -5.782 -6.524 -0.4349       -6.295
      3: 42.44   6.268 -21.441 -7.323  1.6330       10.262

# output_lm_mixed_comb

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp     Day Month_factor
      1: 42.44  -7.886  10.511 16.292 -0.9519       -7.382
      2: 42.44   5.001  -4.925 -7.015 -1.0954       -7.349
      3: 42.44   5.505 -20.583 -4.328  0.7825        8.023

# output_custom_lm_numeric_independence_1

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_custom_lm_numeric_independence_2

    Code
      (out <- code)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
          none Solar.R    Wind   Temp  Month    Day
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_custom_xgboost_mixed_dummy_ctree

    Code
      (out <- code)
    Message <simpleMessage>
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
          none Solar.R   Wind   Temp     Day Month_factor
      1: 42.44  -5.603  13.05  20.43 0.08508      -0.2664
      2: 42.44   4.645 -12.57 -16.65 1.29133      -2.1574
      3: 42.44   5.451 -14.01 -19.72 1.32503       6.3851

# output_lm_numeric_interaction

    Code
      (out <- code)
    Output
          none Solar.R    Wind
      1: 42.44 -13.818  10.579
      2: 42.44   4.642  -6.287
      3: 42.44   4.452 -34.602

# output_lm_numeric_ctree_parallelized

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp   Month    Day
      1: 42.44  -9.124   9.509 17.139 -1.4711 -3.451
      2: 42.44   5.342  -6.097 -8.232 -2.8129 -2.079
      3: 42.44   6.901 -21.079 -4.687  0.1494  1.146

# output_lm_numeric_independence_more_batches

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical_progress

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Month    Day
      1: 42.44 -13.252  15.541 12.826 -5.77179  3.259
      2: 42.44   2.758  -3.325 -7.992 -7.12800  1.808
      3: 42.44   6.805 -22.126  3.730 -0.09235 -5.885

# output_lm_numeric_independence_keep_samp_for_vS

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

