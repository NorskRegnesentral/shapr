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
    Output
      $calling_function
      [1] "explain"
      
      $proglang
      [1] "R"
      
      $approach
      [1] "independence"
      
      $shapley_est
         explain_id  none Solar.R    Wind   Temp    Day Month_factor
              <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:          1 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2:          2 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3:          3 42.44   3.857 -17.469 -1.466  1.099        3.379
      
      $shapley_sd
         explain_id  none Solar.R  Wind  Temp   Day Month_factor
              <int> <num>   <num> <num> <num> <num>        <num>
      1:          1     0       0     0     0     0            0
      2:          2     0       0     0     0     0            0
      3:          3     0       0     0     0     0            0
      
      $pred_explain
      p_hat1_1 p_hat1_2 p_hat1_3 
         53.03    27.06    31.84 
      
      $MSEv
          MSEv MSEv_sd
         <num>   <num>
      1: 115.3   20.08
      
      $MSEv_explicand
            id   MSEv
         <int>  <num>
      1:     1 150.63
      2:     2  81.08
      3:     3 114.22
      
      $MSEv_coalition
          id_coalition coalitions    MSEv MSEv_sd
                 <int>     <list>   <num>   <num>
       1:            2          1 250.638  30.495
       2:            3          2  68.050  40.495
       3:            4          3  75.807  14.349
       4:            5          4 164.839  16.024
       5:            6          5 195.087  80.362
       6:            7        1,2  92.575  59.787
       7:            8        1,3 107.235  51.118
       8:            9        1,4 272.309  26.972
       9:           10        1,5 314.686 124.197
      10:           11        2,3 112.206  53.795
      11:           12        2,4  58.185  25.349
      12:           13        2,5  48.271  31.585
      13:           14        3,4  63.408  23.915
      14:           15        3,5  53.087  50.182
      15:           16        4,5 232.080 115.817
      16:           17      1,2,3  69.058  24.663
      17:           18      1,2,4  91.078  46.768
      18:           19      1,2,5  93.293  70.784
      19:           20      1,3,4 103.205  56.829
      20:           21      1,3,5 105.013  80.105
      21:           22      1,4,5 360.048 168.788
      22:           23      2,3,4  76.639  37.323
      23:           24      2,3,5  26.156  15.000
      24:           25      2,4,5  62.097  54.870
      25:           26      3,4,5  64.380  59.357
      26:           27    1,2,3,4  41.860  15.076
      27:           28    1,2,3,5   3.506   1.571
      28:           29    1,2,4,5 115.488 100.875
      29:           30    1,3,4,5 124.674  90.703
      30:           31    2,3,4,5  14.281   4.729
          id_coalition coalitions    MSEv MSEv_sd
      
      $iterative_info
          exact compute_sd n_coalitions new_n_coalitions n_sampled_coalitions
         <lgcl>     <lgcl>        <int>            <num>                <num>
      1:   TRUE      FALSE           32               32                   30
         n_batches converged converged_exact converged_sd converged_max_iter
             <num>    <lgcl>          <lgcl>       <lgcl>             <lgcl>
      1:        10      TRUE            TRUE        FALSE               TRUE
         converged_max_n_coalitions est_required_coal_samp est_remaining_coal_samp
                             <lgcl>                 <lgcl>                  <lgcl>
      1:                       TRUE                     NA                      NA
         overall_conv_measure
                       <lgcl>
      1:                   NA
      
      $iterative_shapley_est
          iter explain_id  none Solar.R    Wind   Temp    Day Month_factor
         <int>      <int> <num>   <num>   <num>  <num>  <num>        <num>
      1:     1          1 42.44  -4.730   7.750 17.753 -2.601       -7.588
      2:     1          2 42.44   2.338  -3.147 -5.310 -1.676       -7.588
      3:     1          3 42.44   3.857 -17.469 -1.466  1.099        3.379
      
      $iterative_shapley_sd
          iter explain_id  none Solar.R  Wind  Temp   Day Month_factor
         <int>      <int> <num>   <num> <num> <num> <num>        <num>
      1:     1          1     0       0     0     0     0            0
      2:     1          2     0       0     0     0     0            0
      3:     1          3     0       0     0     0     0            0
      
      $saving_path
      NULL
      
      $timing_summary
      NULL
      
      $timing_details
      NULL
      
      $parameters
      $parameters$approach
      [1] "independence"
      
      $parameters$phi0
      [1] 42.44
      
      $parameters$max_n_coalitions
      [1] 32
      
      $parameters$group
      NULL
      
      $parameters$n_MC_samples
      [1] 1000
      
      $parameters$seed
      [1] 1
      
      $parameters$is_python
      [1] FALSE
      
      $parameters$output_size
      [1] 1
      
      $parameters$type
      [1] "regular"
      
      $parameters$verbose
      [1] "basic"
      
      $parameters$iterative
      [1] FALSE
      
      $parameters$iterative_args
      $parameters$iterative_args$initial_n_coalitions
      [1] 32
      
      $parameters$iterative_args$fixed_n_coalitions_per_iter
      NULL
      
      $parameters$iterative_args$max_n_coalitions
      [1] 32
      
      $parameters$iterative_args$max_iter
      [1] 1
      
      $parameters$iterative_args$convergence_tol
      NULL
      
      $parameters$iterative_args$n_coal_next_iter_factor_vec
      NULL
      
      
      $parameters$output_args
      $parameters$output_args$keep_samp_for_vS
      [1] FALSE
      
      $parameters$output_args$MSEv_uniform_comb_weights
      [1] TRUE
      
      
      $parameters$extra_computation_args
      $parameters$extra_computation_args$paired_shap_sampling
      [1] TRUE
      
      $parameters$extra_computation_args$semi_deterministic_sampling
      [1] FALSE
      
      $parameters$extra_computation_args$kernelSHAP_reweighting
      [1] "on_all_cond"
      
      $parameters$extra_computation_args$compute_sd
      [1] FALSE
      
      $parameters$extra_computation_args$n_boot_samps
      [1] 100
      
      $parameters$extra_computation_args$vS_batching_method
      [1] "future"
      
      $parameters$extra_computation_args$max_batch_size
      [1] 10
      
      $parameters$extra_computation_args$min_n_batches
      [1] 10
      
      
      $parameters$asymmetric
      [1] FALSE
      
      $parameters$causal_ordering
      $parameters$causal_ordering[[1]]
      [1] 1 2 3 4 5
      
      
      $parameters$confounding
      NULL
      
      $parameters$model_class
      [1] "lm"
      
      $parameters$testing
      [1] TRUE
      
      $parameters$regression
      [1] FALSE
      
      $parameters$n_features
      [1] 5
      
      $parameters$n_explain
      [1] 3
      
      $parameters$n_train
      [1] 108
      
      $parameters$feature_names
      [1] "Solar.R"      "Wind"         "Temp"         "Day"          "Month_factor"
      
      $parameters$is_groupwise
      [1] FALSE
      
      $parameters$shap_names
      [1] "Solar.R"      "Wind"         "Temp"         "Day"          "Month_factor"
      
      $parameters$n_shapley_values
      [1] 5
      
      $parameters$n_approaches
      [1] 1
      
      $parameters$n_unique_approaches
      [1] 1
      
      $parameters$causal_ordering_names
      $parameters$causal_ordering_names[[1]]
      [1] "Solar.R"      "Wind"         "Temp"         "Day"          "Month_factor"
      
      
      $parameters$causal_ordering_names_string
      [1] "{{Solar.R, Wind, Temp, Day, Month_factor}}"
      
      $parameters$causal_sampling
      [1] FALSE
      
      $parameters$n_coal_each_size
      [1]  5 10 10  5
      
      $parameters$exact
      [1] TRUE
      
      
      $x_train
           Solar.R  Wind  Temp   Day Month_factor
             <int> <num> <int> <int>       <fctr>
        1:      78  18.4    57    18            5
        2:       7   6.9    74    21            7
        3:     225   2.3    94    29            8
        4:     220  10.3    78     8            9
        5:     259   9.7    73    10            9
       ---                                       
      104:     201   8.0    82    20            9
      105:     256   9.7    69    12            5
      106:      13  12.0    67    28            5
      107:     137  10.3    76    20            6
      108:     285   6.3    84    18            7
      
      $x_explain
         Solar.R  Wind  Temp   Day Month_factor
           <int> <num> <int> <int>       <fctr>
      1:      95   7.4    87     5            9
      2:     230  10.9    75     9            9
      3:     259  15.5    77    21            8
      
      $dt_vS
      Index: <id_coalition>
          id_coalition p_hat1_1 p_hat1_2 p_hat1_3
                 <num>    <num>    <num>    <num>
       1:            1    42.44    42.44    42.44
       2:            2    37.54    44.61    46.13
       3:            3    50.02    39.13    24.81
       4:            4    60.03    36.96    40.81
       5:            5    39.67    40.60    43.37
       6:            6    34.69    34.69    45.65
       7:            7    45.34    41.51    28.71
       8:            8    55.34    39.35    44.71
       9:            9    34.99    42.98    47.27
      10:           10    30.00    37.07    49.55
      11:           11    67.82    33.86    23.38
      12:           12    47.47    37.49    25.95
      13:           13    42.48    31.58    28.23
      14:           14    57.47    35.33    41.95
      15:           15    52.48    29.42    44.23
      16:           16    32.13    33.05    46.79
      17:           17    63.13    36.24    27.28
      18:           18    42.78    39.87    29.85
      19:           19    37.79    33.96    32.13
      20:           20    52.78    37.71    45.85
      21:           21    47.79    31.80    48.13
      22:           22    27.44    35.43    50.69
      23:           23    65.26    32.23    24.52
      24:           24    60.27    26.31    26.80
      25:           25    39.92    29.95    29.37
      26:           26    49.92    27.79    45.37
      27:           27    60.57    34.61    28.42
      28:           28    55.59    28.70    30.70
      29:           29    35.23    32.33    33.27
      30:           30    45.24    30.17    49.27
      31:           31    57.72    24.68    27.95
      32:           32    53.03    27.06    31.84
          id_coalition p_hat1_1 p_hat1_2 p_hat1_3
      
      $dt_samp_for_vS
      NULL
      
      $dt_used_coalitions
      Index: <coalition_size>
          id_coalition coalitions coalitions_str coalition_size     N shapley_weight
                 <int>     <list>         <char>          <int> <int>          <num>
       1:            1                                        0     1      1.000e+06
       2:            2          1              1              1     5      2.000e-01
       3:            3          2              2              1     5      2.000e-01
       4:            4          3              3              1     5      2.000e-01
       5:            5          4              4              1     5      2.000e-01
       6:            6          5              5              1     5      2.000e-01
       7:            7        1,2            1 2              2    10      6.667e-02
       8:            8        1,3            1 3              2    10      6.667e-02
       9:            9        1,4            1 4              2    10      6.667e-02
      10:           10        1,5            1 5              2    10      6.667e-02
      11:           11        2,3            2 3              2    10      6.667e-02
      12:           12        2,4            2 4              2    10      6.667e-02
      13:           13        2,5            2 5              2    10      6.667e-02
      14:           14        3,4            3 4              2    10      6.667e-02
      15:           15        3,5            3 5              2    10      6.667e-02
      16:           16        4,5            4 5              2    10      6.667e-02
      17:           17      1,2,3          1 2 3              3    10      6.667e-02
      18:           18      1,2,4          1 2 4              3    10      6.667e-02
      19:           19      1,2,5          1 2 5              3    10      6.667e-02
      20:           20      1,3,4          1 3 4              3    10      6.667e-02
      21:           21      1,3,5          1 3 5              3    10      6.667e-02
      22:           22      1,4,5          1 4 5              3    10      6.667e-02
      23:           23      2,3,4          2 3 4              3    10      6.667e-02
      24:           24      2,3,5          2 3 5              3    10      6.667e-02
      25:           25      2,4,5          2 4 5              3    10      6.667e-02
      26:           26      3,4,5          3 4 5              3    10      6.667e-02
      27:           27    1,2,3,4        1 2 3 4              4     5      2.000e-01
      28:           28    1,2,3,5        1 2 3 5              4     5      2.000e-01
      29:           29    1,2,4,5        1 2 4 5              4     5      2.000e-01
      30:           30    1,3,4,5        1 3 4 5              4     5      2.000e-01
      31:           31    2,3,4,5        2 3 4 5              4     5      2.000e-01
      32:           32  1,2,3,4,5      1 2 3 4 5              5     1      1.000e+06
          id_coalition coalitions coalitions_str coalition_size     N shapley_weight
          sample_freq  features     approach
               <lgcl>    <list>       <char>
       1:          NA                   <NA>
       2:          NA         1 independence
       3:          NA         2 independence
       4:          NA         3 independence
       5:          NA         4 independence
       6:          NA         5 independence
       7:          NA       1,2 independence
       8:          NA       1,3 independence
       9:          NA       1,4 independence
      10:          NA       1,5 independence
      11:          NA       2,3 independence
      12:          NA       2,4 independence
      13:          NA       2,5 independence
      14:          NA       3,4 independence
      15:          NA       3,5 independence
      16:          NA       4,5 independence
      17:          NA     1,2,3 independence
      18:          NA     1,2,4 independence
      19:          NA     1,2,5 independence
      20:          NA     1,3,4 independence
      21:          NA     1,3,5 independence
      22:          NA     1,4,5 independence
      23:          NA     2,3,4 independence
      24:          NA     2,3,5 independence
      25:          NA     2,4,5 independence
      26:          NA     3,4,5 independence
      27:          NA   1,2,3,4 independence
      28:          NA   1,2,3,5 independence
      29:          NA   1,2,4,5 independence
      30:          NA   1,3,4,5 independence
      31:          NA   2,3,4,5 independence
      32:          NA 1,2,3,4,5         <NA>
          sample_freq  features     approach
      
      $dt_valid_causal_coalitions
      NULL
      
      $dt_coal_samp_info
         max_fixed_coal_size n_coal_max n_coal_determ weight_determ weight_sample
                       <num>      <num>         <num>         <num>         <num>
      1:                   0         32             2             0             1
         coal_sizes_sample coal_sizes_sample_prob
                    <list>                 <list>
      1:           1,2,3,4        0.3,0.2,0.2,0.3
      

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
    Output
      $calling_function
      [1] "explain_forecast"
      
      $proglang
      [1] "R"
      
      $approach
      [1] "empirical"
      
      $shapley_est
         explain_idx horizon  none  Temp.1  Temp.2
               <int>   <int> <num>   <num>   <num>
      1:         152       1 77.88 -0.3972 -1.3912
      2:         153       1 77.88 -6.6177 -0.1835
      3:         152       2 77.88 -0.3285 -1.2034
      4:         153       2 77.88 -6.0208 -0.3371
      5:         152       3 77.88 -0.2915 -1.0552
      6:         153       3 77.88 -5.2122 -0.2553
      
      $shapley_sd
         explain_idx horizon  none Temp.1 Temp.2
               <num>   <num> <num>  <num>  <num>
      1:           0       0     0      0      0
      2:           0       0     0      0      0
      3:           0       0     0      0      0
      4:           0       0     0      0      0
      5:           0       0     0      0      0
      6:           0       0     0      0      0
      
      $pred_explain
      explain_idx_152_horizon_1 explain_idx_153_horizon_1 explain_idx_152_horizon_2 
                          76.09                     71.08                     76.35 
      explain_idx_153_horizon_2 explain_idx_152_horizon_3 explain_idx_153_horizon_3 
                          71.52                     76.54                     72.41 
      
      $MSEv
      NULL
      
      $MSEv_explicand
      NULL
      
      $MSEv_coalition
      NULL
      
      $iterative_info
          exact compute_sd n_coalitions new_n_coalitions n_sampled_coalitions
         <lgcl>     <lgcl>        <int>            <num>                <num>
      1:   TRUE      FALSE            4                4                    2
         n_batches converged converged_exact converged_sd converged_max_iter
             <num>    <lgcl>          <lgcl>       <lgcl>             <lgcl>
      1:         4      TRUE            TRUE        FALSE               TRUE
         converged_max_n_coalitions est_required_coal_samp est_remaining_coal_samp
                             <lgcl>                 <lgcl>                  <lgcl>
      1:                       TRUE                     NA                      NA
         overall_conv_measure
                       <lgcl>
      1:                   NA
      
      $iterative_shapley_est
          iter explain_idx horizon  none  Temp.1  Temp.2
         <int>       <int>   <int> <num>   <num>   <num>
      1:     1         152       1 77.88 -0.3972 -1.3912
      2:     1         153       1 77.88 -6.6177 -0.1835
      3:     1         152       2 77.88 -0.3285 -1.2034
      4:     1         153       2 77.88 -6.0208 -0.3371
      5:     1         152       3 77.88 -0.2915 -1.0552
      6:     1         153       3 77.88 -5.2122 -0.2553
      
      $iterative_shapley_sd
          iter explain_idx horizon  none Temp.1 Temp.2
         <int>       <num>   <num> <num>  <num>  <num>
      1:     1           0       0     0      0      0
      2:     1           0       0     0      0      0
      3:     1           0       0     0      0      0
      4:     1           0       0     0      0      0
      5:     1           0       0     0      0      0
      6:     1           0       0     0      0      0
      
      $saving_path
      NULL
      
      $timing_summary
      NULL
      
      $timing_details
      NULL
      
      $parameters
      $parameters$approach
      [1] "empirical"
      
      $parameters$phi0
      [1] 77.88 77.88 77.88
      
      $parameters$max_n_coalitions
      [1] 4
      
      $parameters$group
      NULL
      
      $parameters$n_MC_samples
      [1] 1000
      
      $parameters$seed
      [1] 1
      
      $parameters$is_python
      [1] FALSE
      
      $parameters$output_size
      [1] 3
      
      $parameters$type
      [1] "forecast"
      
      $parameters$verbose
      [1] "basic"
      
      $parameters$iterative
      [1] FALSE
      
      $parameters$iterative_args
      $parameters$iterative_args$initial_n_coalitions
      [1] 4
      
      $parameters$iterative_args$fixed_n_coalitions_per_iter
      NULL
      
      $parameters$iterative_args$max_n_coalitions
      [1] 4
      
      $parameters$iterative_args$max_iter
      [1] 1
      
      $parameters$iterative_args$convergence_tol
      NULL
      
      $parameters$iterative_args$n_coal_next_iter_factor_vec
      NULL
      
      
      $parameters$output_args
      $parameters$output_args$keep_samp_for_vS
      [1] FALSE
      
      $parameters$output_args$MSEv_uniform_comb_weights
      [1] TRUE
      
      
      $parameters$extra_computation_args
      $parameters$extra_computation_args$paired_shap_sampling
      [1] TRUE
      
      $parameters$extra_computation_args$semi_deterministic_sampling
      [1] FALSE
      
      $parameters$extra_computation_args$kernelSHAP_reweighting
      [1] "on_all_cond"
      
      $parameters$extra_computation_args$compute_sd
      [1] FALSE
      
      $parameters$extra_computation_args$n_boot_samps
      [1] 100
      
      $parameters$extra_computation_args$vS_batching_method
      [1] "future"
      
      $parameters$extra_computation_args$max_batch_size
      [1] 10
      
      $parameters$extra_computation_args$min_n_batches
      [1] 10
      
      
      $parameters$asymmetric
      [1] FALSE
      
      $parameters$causal_ordering
      NULL
      
      $parameters$confounding
      NULL
      
      $parameters$model_class
      [1] "ar"
      
      $parameters$testing
      [1] TRUE
      
      $parameters$horizon
      [1] 3
      
      $parameters$train_idx
        [1]   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19
       [19]  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37
       [37]  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55
       [55]  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73
       [73]  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91
       [91]  92  93  94  95  96  97  98  99 100 101 102 103 104 105 106 107 108 109
      [109] 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127
      [127] 128 129 130 131 132 133 134 135 136 137 138 139 140 141 142 143 144 145
      [145] 146 147 148 149 150 151
      
      $parameters$explain_idx
      [1] 152 153
      
      $parameters$group_lags
      [1] FALSE
      
      $parameters$output_labels
           explain_idx horizon
      [1,]         152       1
      [2,]         153       1
      [3,]         152       2
      [4,]         153       2
      [5,]         152       3
      [6,]         153       3
      
      $parameters$explain_lags
      $parameters$explain_lags$y
      [1] 2
      
      $parameters$explain_lags$xreg
      [1] 2
      
      
      $parameters$regression
      [1] FALSE
      
      $parameters$horizon_features
      $parameters$horizon_features[[1]]
      [1] "Temp.1" "Temp.2"
      
      $parameters$horizon_features[[2]]
      [1] "Temp.1" "Temp.2"
      
      $parameters$horizon_features[[3]]
      [1] "Temp.1" "Temp.2"
      
      
      $parameters$n_features
      [1] 2
      
      $parameters$n_explain
      [1] 2
      
      $parameters$n_train
      [1] 150
      
      $parameters$feature_names
      [1] "Temp.1" "Temp.2"
      
      $parameters$is_groupwise
      [1] FALSE
      
      $parameters$shap_names
      [1] "Temp.1" "Temp.2"
      
      $parameters$n_shapley_values
      [1] 2
      
      $parameters$n_approaches
      [1] 1
      
      $parameters$n_unique_approaches
      [1] 1
      
      $parameters$causal_sampling
      [1] FALSE
      
      $parameters$exact
      [1] TRUE
      
      $parameters$empirical.eta
      [1] 0.95
      
      $parameters$empirical.type
      [1] "fixed_sigma"
      
      $parameters$empirical.fixed_sigma
      [1] 0.1
      
      $parameters$empirical.n_samples_aicc
      [1] 1000
      
      $parameters$empirical.eval_max_aicc
      [1] 20
      
      $parameters$empirical.start_aicc
      [1] 0.1
      
      $parameters$empirical.cov_mat
             Temp.1 Temp.2
      Temp.1  89.90  73.77
      Temp.2  73.77  90.65
      
      
      $x_train
           Temp.1 Temp.2
            <int>  <int>
        1:     72     67
        2:     74     72
        3:     62     74
        4:     56     62
        5:     66     56
       ---              
      146:     69     81
      147:     63     69
      148:     70     63
      149:     77     70
      150:     75     77
      
      $x_explain
         Temp.1 Temp.2
          <int>  <int>
      1:     76     75
      2:     68     76
      
      $dt_vS
      Index: <id_coalition>
         id_coalition p_hat1_1 p_hat1_2 p_hat2_1 p_hat2_2 p_hat3_1 p_hat3_2
                <num>    <num>    <num>    <num>    <num>    <num>    <num>
      1:            1    77.88    77.88    77.88    77.88    77.88    77.88
      2:            2    76.33    69.63    76.50    70.58    76.68    71.52
      3:            3    75.33    76.06    75.63    76.26    75.92    76.47
      4:            4    76.09    71.08    76.35    71.52    76.54    72.41
      
      $dt_samp_for_vS
      NULL
      
      $dt_used_coalitions
         id_coalition coalitions coalitions_str coalition_size     N shapley_weight
                <int>     <list>         <char>          <int> <int>          <num>
      1:            1                                        0    NA          1e+06
      2:            2          1              1              1    NA          5e-01
      3:            3          2              2              1    NA          5e-01
      4:            4        1,2            1 2              2    NA          1e+06
         sample_freq features  approach
              <lgcl>   <list>    <char>
      1:          NA               <NA>
      2:          NA        1 empirical
      3:          NA        2 empirical
      4:          NA      1,2      <NA>
      
      $dt_valid_causal_coalitions
      NULL
      
      $dt_coal_samp_info
      NULL
      

