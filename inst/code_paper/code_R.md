

``` r
library(here)
path_source <- "source"

#source(file.path(path_source,"prep_data_and_model.R")) # Already run, and data/models are saved to ensure reproducibility

source(file.path(path_source,"code_sec_3.R"))
```

```
## Error in globalCallingHandlers(condition = global_progression_handler): should not be called with handlers on the stack
```

``` r
source(file.path(path_source,"code_sec_4.R"))
```

```
## Error in globalCallingHandlers(condition = global_progression_handler): should not be called with handlers on the stack
```

``` r
source(file.path(path_source,"code_sec_6.R"))
```

```
## Error in globalCallingHandlers(condition = global_progression_handler): should not be called with handlers on the stack
```

``` r
sessionInfo()
```

```
## R version 4.4.1 (2024-06-14)
## Platform: x86_64-pc-linux-gnu
## Running under: Ubuntu 20.04.6 LTS
## 
## Matrix products: default
## BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
## LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/liblapack.so.3;  LAPACK version 3.9.0
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C               LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8     LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8    LC_PAPER=en_US.UTF-8       LC_NAME=C                  LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## time zone: Europe/Oslo
## tzcode source: system (glibc)
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] here_1.0.1        ggpubr_0.6.0      ggplot2_3.5.1     progressr_0.15.1  future_1.34.0     shapr_1.0.3.9000  data.table_1.17.0 xgboost_1.7.9.1  
## 
## loaded via a namespace (and not attached):
##  [1] generics_0.1.3    tidyr_1.3.1       rstatix_0.7.2     lattice_0.22-6    listenv_0.9.1     digest_0.6.37     magrittr_2.0.3    evaluate_1.0.3    grid_4.4.1        parsnip_1.3.1     rprojroot_2.0.4   jsonlite_1.9.1    Matrix_1.7-0      backports_1.5.0   Formula_1.2-5    
## [16] gridExtra_2.3     purrr_1.0.4       scales_1.3.0      codetools_0.2-20  abind_1.4-8       cli_3.6.4         rlang_1.1.5       hardhat_1.4.1     parallelly_1.43.0 cowplot_1.1.3     munsell_0.5.1     withr_3.0.2       ggbeeswarm_0.7.2  tools_4.4.1       parallel_4.4.1   
## [31] ggsignif_0.6.4    dplyr_1.1.4       colorspace_2.1-1  globals_0.16.3    broom_1.0.7       vctrs_0.6.5       R6_2.6.1          lifecycle_1.0.4   car_3.1-3         vipor_0.4.7       pkgconfig_2.0.3   beeswarm_0.4.0    pillar_1.10.1     gtable_0.3.6      glue_1.8.0       
## [46] Rcpp_1.0.14       xfun_0.51         tibble_3.2.1      tidyselect_1.2.1  knitr_1.50        rstudioapi_0.17.1 carData_3.0-5     compiler_4.4.1
```

