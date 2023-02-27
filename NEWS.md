
# shapr 0.2.1

* Patch to fix warning from development version of data.table due to the use of nomatch argument in merge(),
as requested from [data.table developers](https://github.com/NorskRegnesentral/shapr/issues/322).


# shapr 0.2.0

* Minor CRAN release
* Added the new dependence modeling approach "ctree" which handles categorical features in addition
  to numerical ones. For more information see our paper https://doi.org/10.1007/978-3-030-57321-8_7
* Added support to explain models which take as input categorical features for model classes like xgboost
  which originally takes only numeric input. On the user side, an additional call to the new *make_dummies*
  function is required. See the vignette for details. 
* Slight change in the user procedure for explaining predictions from custom models. This now requires 
only a single function *predict_model*. 
* Introduced a thorough system for extracting and checking the feature information in the model and the data 
  passed to *shapr* and *explain*. The features in the data are checked for consistency with what can be extracted
  from the model object. If the model object is missing some of the necessary information, the info from the data
  is used instead. The system checks feature labels, classes, and any factor levels.
* Due to the previous point, the *feature_labels* option previously used for custom models is removed.
* Added a manual testing script for custom model (currently cannot be handled by testthat due to environment issues).
* A few under-the-hood changes for checking in the *shapr* function.

# shapr 0.1.4

* Patch to fulfill CRAN policy of using packages under Suggests conditionally (in tests and examples)

# shapr 0.1.3

* Fix installation error on Solaris
* Updated README with CRAN installation instructions and badges

# shapr 0.1.2

* CRAN release
* Removed unused clustering code
* Removed several package dependencies
* Moved automatic check and pkgdown site build from Circle CI to GitHub actions
* Some minor efficiency fixes
* Changed stopping threshold from 12 to 13 features for none-sampling version of 
  KernelSHAP for consistency with our recommendation
* Changed package title (shortened)
* Minor fixes to fulfill CRAN policy
* Improved documentation
* Revised internal/external and exported/non-exported functions, leading to far
  fewer external functions and a cleaner manual. 

# shapr 0.1.1

* Journal of Open Source Software release
* Improved installation instructions and community guidelines in README 
* Improved documentation
* Some minor bugfixes

# shapr 0.1.0

* Support for custom models
* Improved documentation
* Automated testing using [testthat](https://github.com/r-lib/testthat)
* Added vignette that gives an introduction to the package
* Added webpage for package using [pkgdown](https://github.com/r-lib/pkgdown)
* Improved API for end user
* Various bugfixes

# shapr 0.0.0.9000

* First version of the package. Currently under development.
