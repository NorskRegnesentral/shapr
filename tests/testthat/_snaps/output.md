# output_lm_numeric_independence

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_independence_MSEv_Shapley_weights

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Month    Day
         <num>   <num>   <num>  <num>    <num>  <num>
      1: 42.44 -13.252  15.541 12.826 -5.77179  3.259
      2: 42.44   2.758  -3.325 -7.992 -7.12800  1.808
      3: 42.44   6.805 -22.126  3.730 -0.09235 -5.885

# output_lm_numeric_empirical_n_combinations

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -5.795  15.320  8.557 -7.547  2.066
      2: 42.44   3.266  -3.252 -7.693 -7.663  1.462
      3: 42.44   4.290 -24.395  6.739 -1.006 -3.197

# output_lm_numeric_empirical_independence

    Code
      (out <- code)
    Condition
      Warning in `setup_approach.empirical()`:
      Using empirical.type = 'independence' for approach = 'empirical' is deprecated.
      Please use approach = 'independence' instead.
    Message
      
      Success with message:
      empirical.eta force set to 1 for empirical.type = 'independence'
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical_AICc_each

    Code
      (out <- code)
    Output
          none Solar.R    Wind     Temp    Month     Day
         <num>   <num>   <num>    <num>    <num>   <num>
      1: 42.44  -15.66   6.823  17.5092   0.2463  3.6847
      2: 42.44   10.70  -1.063 -10.6804 -13.0305  0.1983
      3: 42.44   14.65 -19.946   0.9675  -7.3433 -5.8946

# output_lm_numeric_empirical_AICc_full

    Code
      (out <- code)
    Output
          none Solar.R     Wind     Temp    Month     Day
         <num>   <num>    <num>    <num>    <num>   <num>
      1: 42.44  -14.98   6.3170  17.4103   0.2876  3.5623
      2: 42.44   12.42   0.1482 -10.2338 -16.4096  0.1967
      3: 42.44   15.74 -19.7250   0.9992  -8.6950 -5.8886

# output_lm_numeric_gaussian

    Code
      (out <- code)
    Output
          none Solar.R    Wind     Temp   Month     Day
         <num>   <num>   <num>    <num>   <num>   <num>
      1: 42.44  -8.117   7.438  14.0026  0.8602 -1.5813
      2: 42.44   5.278  -5.219 -12.1079 -0.8073 -1.0235
      3: 42.44   7.867 -25.995  -0.1377 -0.2368  0.9342

# output_lm_numeric_copula

    Code
      (out <- code)
    Output
          none Solar.R    Wind    Temp  Month    Day
         <num>   <num>   <num>   <num>  <num>  <num>
      1: 42.44  -5.960   7.046  13.863 -0.274 -2.074
      2: 42.44   4.482  -4.892 -10.491 -1.659 -1.319
      3: 42.44   6.587 -25.533   1.279 -1.043  1.142

# output_lm_numeric_ctree

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp   Month    Day
         <num>   <num>   <num>  <num>   <num>  <num>
      1: 42.44  -9.124   9.509 17.139 -1.4711 -3.451
      2: 42.44   5.342  -6.097 -8.232 -2.8129 -2.079
      3: 42.44   6.901 -21.079 -4.687  0.1494  1.146

# output_lm_numeric_vaeac

    Code
      (out <- code)
    Output
          none Solar.R    Wind    Temp  Month     Day
         <num>   <num>   <num>   <num>  <num>   <num>
      1: 42.44  -6.534   9.146 18.8166 -5.238 -3.5884
      2: 42.44   1.421  -5.329 -6.8472 -3.668  0.5436
      3: 42.44   7.073 -18.914 -0.6391 -6.038  0.9493

# output_lm_categorical_ctree

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
         <num>        <num>              <num>          <num>       <num>
      1: 42.44       -6.206              15.38         -6.705      -2.973
      2: 42.44       -5.764             -17.71         21.866     -13.219
      3: 42.44        7.101             -21.78          1.730      -5.413

# output_lm_categorical_vaeac

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
         <num>        <num>              <num>          <num>       <num>
      1: 42.44        1.795              10.32         -6.919      -5.704
      2: 42.44       -2.438             -18.15         20.755     -14.999
      3: 42.44        8.299             -23.71          8.751     -11.708

# output_lm_categorical_categorical

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
         <num>        <num>              <num>          <num>       <num>
      1: 42.44       13.656             -19.73          4.369     -16.659
      2: 42.44       -5.448              11.31        -11.445       5.078
      3: 42.44       -7.493             -12.27         19.672     -14.744

# output_lm_categorical_independence

    Code
      (out <- code)
    Output
          none Month_factor Ozone_sub30_factor Solar.R_factor Wind_factor
         <num>        <num>              <num>          <num>       <num>
      1: 42.44       -5.252              13.95         -7.041      -2.167
      2: 42.44       -5.252             -15.61         20.086     -14.050
      3: 42.44        4.833             -15.61          0.596      -8.178

# output_lm_ts_timeseries

    Code
      (out <- code)
    Output
          none      S1     S2       S3      S4
         <num>   <num>  <num>    <num>   <num>
      1: 4.895 -0.5261 0.7831 -0.21023 -0.3885
      2: 4.895 -0.6310 1.6288 -0.04498 -2.9298

# output_lm_numeric_comb1

    Code
      (out <- code)
    Output
          none Solar.R   Wind   Temp  Month     Day
         <num>   <num>  <num>  <num>  <num>   <num>
      1: 42.44  -8.746   9.03 15.366 -2.619 -0.4293
      2: 42.44   3.126  -4.50 -7.789 -4.401 -0.3161
      3: 42.44   7.037 -22.86 -1.837  0.607 -0.5181

# output_lm_numeric_comb2

    Code
      (out <- code)
    Output
          none Solar.R    Wind     Temp  Month     Day
         <num>   <num>   <num>    <num>  <num>   <num>
      1: 42.44  -9.294   9.327 17.31641 -1.754 -2.9935
      2: 42.44   5.194  -5.506 -8.45049 -2.935 -2.1810
      3: 42.44   6.452 -22.967 -0.09553 -1.310  0.3519

# output_lm_numeric_comb3

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month      Day
         <num>   <num>   <num>  <num>  <num>    <num>
      1: 42.44  -6.952  10.777 12.160 -3.641  0.25767
      2: 42.44   2.538  -2.586 -8.503 -5.376  0.04789
      3: 42.44   5.803 -22.122  3.362 -2.926 -1.68514

# output_lm_mixed_independence

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Day Month_factor
         <num>   <num>   <num>  <num>  <num>        <num>
      1: 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2: 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3: 42.44   3.857 -17.469 -1.466  1.099        3.379

# output_lm_mixed_ctree

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp     Day Month_factor
         <num>   <num>   <num>  <num>   <num>        <num>
      1: 42.44  -9.165  11.815 13.184 -0.4473       -4.802
      2: 42.44   3.652  -5.782 -6.524 -0.4349       -6.295
      3: 42.44   6.268 -21.441 -7.323  1.6330       10.262

# output_lm_mixed_vaeac

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp     Day Month_factor
         <num>   <num>   <num>  <num>   <num>        <num>
      1: 42.44  -3.629   8.898 17.330 -2.5409      -9.4742
      2: 42.44   3.938  -3.933 -8.190  0.6284      -7.8259
      3: 42.44   5.711 -15.928 -3.216  2.2431       0.5899

# output_lm_mixed_comb

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp     Day Month_factor
         <num>   <num>   <num>  <num>   <num>        <num>
      1: 42.44  -7.886  10.511 16.292 -0.9519       -7.382
      2: 42.44   5.001  -4.925 -7.015 -1.0954       -7.349
      3: 42.44   5.505 -20.583 -4.328  0.7825        8.023

# output_custom_lm_numeric_independence_1

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_custom_lm_numeric_independence_2

    Code
      (out <- code)
    Message
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_custom_xgboost_mixed_dummy_ctree

    Code
      (out <- code)
    Message
      Note: You passed a model to explain() which is not natively supported, and did not supply a 'get_model_specs' function to explain().
      Consistency checks between model and data is therefore disabled.
      
    Output
          none Solar.R   Wind   Temp     Day Month_factor
         <num>   <num>  <num>  <num>   <num>        <num>
      1: 42.44  -5.603  13.05  20.43 0.08508      -0.2664
      2: 42.44   4.645 -12.57 -16.65 1.29133      -2.1574
      3: 42.44   5.451 -14.01 -19.72 1.32503       6.3851

# output_lm_numeric_interaction

    Code
      (out <- code)
    Output
          none Solar.R    Wind
         <num>   <num>   <num>
      1: 42.44 -13.818  10.579
      2: 42.44   4.642  -6.287
      3: 42.44   4.452 -34.602

# output_lm_numeric_ctree_parallelized

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp   Month    Day
         <num>   <num>   <num>  <num>   <num>  <num>
      1: 42.44  -9.124   9.509 17.139 -1.4711 -3.451
      2: 42.44   5.342  -6.097 -8.232 -2.8129 -2.079
      3: 42.44   6.901 -21.079 -4.687  0.1494  1.146

# output_lm_numeric_independence_more_batches

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

# output_lm_numeric_empirical_progress

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp    Month    Day
         <num>   <num>   <num>  <num>    <num>  <num>
      1: 42.44 -13.252  15.541 12.826 -5.77179  3.259
      2: 42.44   2.758  -3.325 -7.992 -7.12800  1.808
      3: 42.44   6.805 -22.126  3.730 -0.09235 -5.885

# output_lm_numeric_independence_keep_samp_for_vS

    Code
      (out <- code)
    Output
          none Solar.R    Wind   Temp  Month    Day
         <num>   <num>   <num>  <num>  <num>  <num>
      1: 42.44  -4.537   8.269 17.517 -5.581 -3.066
      2: 42.44   2.250  -3.345 -5.232 -5.581 -1.971
      3: 42.44   3.708 -18.610 -1.440 -2.541  1.316

