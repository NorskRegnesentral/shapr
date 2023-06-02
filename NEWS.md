# shapr (development version)

* Complete restructuring motivated by introducing Python wrapper  (`shaprpyr`, [#325](https://github.com/NorskRegnesentral/shapr/pull/325)) for explaining predictions from Python models (from Python) utilizing almost all functionality of `shapr`. The restructuring splits the explanation tasks into smaller pieces, allowing the Python wrapper to move back and forth between Python and R, to do the prediction in Python, and almost everything else in R. This simplifies maintenance of `shaprpy` significantly. 
* As part of the restructuring, we also did a number of design changes, resulting in a series of breaking changes described below.

## Breaking changes

* Moved from explaining predictions using *two* functions (`shapr()` for initial setup + `explain()` for explanation for specific observations), to a single function call (also named `explain()`)
* New names for several input arguments
* Prediction and checking functions for custom models are now passed directly as arguments to `explain()` instead of being defined as functions of a specific class in the global env.

## New features

* Introduce batch computation of conditional expectations ([#244](https://github.com/NorskRegnesentral/shapr/issues/244)). This essentially compute $v(S)$ for a portion of the $S$-subsets at a time, to reduce the amount of data needed to be held in memory. The user can control the number of batches herself, but we set a reasonable value by default ([#327](https://github.com/NorskRegnesentral/shapr/pull/327))
This allows models with large number of features to be explained with a significantly lower RAM consumption (at the cost os a slight increase in the computation time)
* Parallelization over batches ([#38](https://github.com/NorskRegnesentral/shapr/issues/38))
* Progress bar
* Categorical method ([#256](https://github.com/NorskRegnesentral/shapr/issues/256), [#307](https://github.com/NorskRegnesentral/shapr/pull/307)).
* Time series method from groupShapley paper
* Unique sampling of Shapley value subsets ([#227](https://github.com/NorskRegnesentral/shapr/issues/227))
* New function `explain_forecast` to explain forecasts from time series models, at various prediction horizons. ([#328](https://github.com/NorskRegnesentral/shapr/pull/328)). Uses a different set of input argument which is more appropriate for these models. 
* Re-implementation of `method = 'independence'` method providing significantly faster computation (no longer a special case of the `empirical` method). Also allow the method to be used on models with categorical data  ([#315](https://github.com/NorskRegnesentral/shapr/pull/315)).
* New plots and improved visualization capabilities.

## Under the hood

* The test base have been completely rewritten ([#249](https://github.com/NorskRegnesentral/shapr/issues/249)). Now heavily utilizing [snapshots](https://testthat.r-lib.org/articles/snapshotting.html) on a large set of benchmark calls to `explain`. Test functions are only written for exported core functions. Internal functions are only tested through the exported ones. 
* Update GHA ([#335](https://github.com/NorskRegnesentral/shapr/pull/335))


## Minor improvements and bug fixes

* The vignette/readme/tests now uses the `datasets::airquality` dataset, which avoids including a new package just for the dataset ([#248](https://github.com/NorskRegnesentral/shapr/issues/248))
* Allows lm/glm/gam models with interactions ([#303](https://github.com/NorskRegnesentral/shapr/pull/303)). Previously, this was not possible with the prediction functions defined internally due to a bug.
* Sampling of group subsets implemented also for grouping, not only features

## Documentation improvements

* The vignette `vignette("understanding_shapr", "shapr")` has been updated to reflect the new way to explain predictions, and all the new package features/functionality.

# shapr 0.2.3 (GitHub only)

* Development version
* Added support for groupSHAP, including check of appropriate groups, examples and tests
* Various modifications to input of internal functions to reflect that Shapley values may be
  computed both feature-wise and group-wise
* Fixed bug when passing non-named data to shapr() or explain() (e.g. ```shapr(data[,1:5],model...)```

# shapr 0.2.2

* Patch to fix failing CRAN-tests on R-devel due to changed behavior of `attach()`: Fixed by changing how we simluate adding a function to .GlobalEnv in the failing test. Actual package not affected.

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
* Due to the previous point, the *feature_names* option previously used for custom models is removed.
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
