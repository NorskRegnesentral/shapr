# In this file, we demonstrate how to use the regression-based method from Python.
# For more details, we refer the reader to the vignette. There are two differences
# between the Python and R version.
# First, in R, the parameter names have the structure 'regression.parameter_name',
# while in Python they have the structure 'regression_parameter_name'. This means
# that we should use e.g., 'regression_recipe_func' in Python and NOT 'regression.recipe_func'.
# Second, the parameters 'regression_model', 'regression_recipe_func', and 'regression_tune_values'.
# must be provided as strings of functional R code. The latter is only needed to be
# a string if it is a function, i.e., it can also be, e.g., a pandas data frame.

import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

# Fit model
model = xgb.XGBRegressor()
model.fit(dfx_train, dfy_train.values.flatten())

# List to store the explanations
explanation_list = {}

# Explain the model using the empirical approach
explanation_list["empirical"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='empirical',
    iterative = False,
    phi0=dfy_train.mean().item(),
    seed = 1
)

# Explain the model using several separate regression methods
# Linear regression
explanation_list["sep_lm"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    phi0=dfy_train.mean().item(),
    regression_model='parsnip::linear_reg()',
    seed = 1
)

# Principal component regression with (up to) three principal components
explanation_list["sep_pca"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    phi0=dfy_train.mean().item(),
    regression_model='parsnip::linear_reg()',
    regression_recipe_func='''function(regression_recipe) {
        return(recipes::step_ns(regression_recipe, recipes::all_numeric_predictors(), deg_free = 3))
    }''',
    seed = 1
)

#  GAM with splines with (up to) three degrees of freedom
explanation_list["sep_splines"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    phi0=dfy_train.mean().item(),
    regression_model='parsnip::linear_reg()',
    regression_recipe_func='''function(regression_recipe) {
        return(recipes::step_ns(regression_recipe, recipes::all_numeric_predictors(), deg_free = 3))
    }''',
    seed = 1
)

# Decision tree with cross validated tree depth
explanation_list["sep_tree_cv"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    phi0=dfy_train.mean().item(),
    regression_model="parsnip::decision_tree(tree_depth = hardhat::tune(), engine = 'rpart', mode = 'regression')",
    regression_tune_values='dials::grid_regular(dials::tree_depth(), levels = 4)',
    regression_vfold_cv_para={'v': 5},
    seed = 1
)

# XGboost with default parameters
explanation_list["sep_xgboost"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    phi0=dfy_train.mean().item(),
    regression_model="parsnip::boost_tree(engine = 'xgboost', mode = 'regression')",
    seed = 1
)

# XGboost with cross validated number of trees
explanation_list["sep_xgboost_cv"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    phi0=dfy_train.mean().item(),
    regression_model="parsnip::boost_tree(trees = hardhat::tune(), engine = 'xgboost', mode = 'regression')",
    regression_tune_values='expand.grid(trees = c(10, 15, 25, 50, 100, 500))',
    regression_vfold_cv_para={'v': 5},
    seed = 1
)

# Explain the model using several surrogate regression methods
# Linear regression
explanation_list["sur_lm"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_surrogate',
    phi0=dfy_train.mean().item(),
    regression_model='parsnip::linear_reg()',
    seed = 1
)

# Using random forest with default parameters as the surrogate model
explanation_list["sur_rf"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_surrogate',
    phi0=dfy_train.mean().item(),
    regression_model="parsnip::rand_forest(engine = 'ranger', mode = 'regression')",
    seed = 1
)

# Using random forest with parameters tuned by cross-validation as the surrogate model
explanation_list["sur_rf_cv"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_surrogate',
    phi0=dfy_train.mean().item(),
    regression_model="""parsnip::rand_forest(
        mtry = hardhat::tune(), trees = hardhat::tune(), engine = 'ranger', mode = 'regression'
    )""",
    regression_tune_values="""dials::grid_regular(
      dials::mtry(c(1, 8)),
      dials::trees(c(50, 750)),
      levels = 4
    )""",
    regression_vfold_cv_para={'v': 4},
    seed = 1
)

# Print the MSEv evaluation criterion scores
print("Method", "MSEv", "Elapsed time (seconds)")
for i, (method, explanation) in enumerate(explanation_list.items()):
    print(method, round(explanation.get_results("MSEv")["MSEv"].iloc[0], 3), round(explanation.get_results("timing_summary")["total_time_secs"].iloc[0], 3))

"""
Method MSEv Elapsed time (seconds)
empirical 0.826 4.921
sep_lm 1.619 7.438
sep_pca 1.6 7.486
sep_splines 1.6 7.707
sep_tree_cv 1.435 286.691
sep_xgboost 0.765 31.646
sep_xgboost_cv 0.805 705.618
sur_lm 1.772 0.474
sur_rf 0.865 26.951
sur_rf_cv 0.971 688.114
"""


explanation_list["sep_xgboost"].print()

"""
   explain_id  none  MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>   <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -0.4974   0.1910   -0.077     0.016    -0.2204  -0.3132
2:          2  2.21 -0.1630   0.0129   -0.418    -0.111     0.0761   0.1502
3:          3  2.21  0.5871   0.2621    0.100    -0.666     0.3485   0.8652
4:          4  2.21  0.3158  -0.1084    0.210     0.041    -0.1805  -0.0598
5:          5  2.21  0.0808  -0.1532   -0.118     0.104    -0.1044   0.4279
   Latitude Longitude
      <num>     <num>
1:  -0.4399  -0.35942
2:  -0.4824  -0.32147
3:   0.2621   0.95638
4:   0.0263   0.04382
5:  -0.2551   0.00761
"""
