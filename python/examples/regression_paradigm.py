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
    print(method, round(explanation["MSEv"]["MSEv"].iloc[0].iloc[0], 3), round(explanation["timing"]["summary"]["total_time_secs"].iloc[0], 3))

"""
Method MSEv Elapsed time (seconds)
empirical 0.826 5.238
sep_lm 1.619 10.242
sep_pca 1.6 9.77
sep_splines 1.6 10.245
sep_tree_cv 1.435 231.503
sep_xgboost 0.765 58.948
sep_xgboost_cv 0.805 54613.817
sur_lm 1.772 0.54
sur_rf 0.865 26.759
sur_rf_cv 0.975 693.178
"""


print(explanation_list["sep_xgboost"]["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205937 -0.497444  0.190998 -0.076956   0.015962   -0.220396
2           2  2.205938 -0.162954  0.012904 -0.418309  -0.111319    0.076074
3           3  2.205938  0.587087  0.262129  0.100346  -0.666143    0.348501
4           4  2.205938  0.315756 -0.108360  0.209570   0.040982   -0.180546
5           5  2.205938  0.080827 -0.153198 -0.118224   0.103920   -0.104418

   AveOccup  Latitude  Longitude
1 -0.313245 -0.439860  -0.359420
2  0.150190 -0.482375  -0.321474
3  0.865160  0.262089   0.956384
4 -0.059770  0.026272   0.043815
5  0.427907 -0.255125   0.007613
"""
