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
    prediction_zero=dfy_train.mean().item()
)

# Explain the model using several separate regression methods
# Linear regression
explanation_list["sep_lm"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model='parsnip::linear_reg()'
)

# Principal component regression with (up to) three principal components
explanation_list["sep_pca"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model='parsnip::linear_reg()',
    regression_recipe_func='''function(regression_recipe) {
        return(recipes::step_ns(regression_recipe, recipes::all_numeric_predictors(), deg_free = 3))
    }'''
)

#  GAM with splines with (up to) three degrees of freedom
explanation_list["sep_splines"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model='parsnip::linear_reg()',
    regression_recipe_func='''function(regression_recipe) {
        return(recipes::step_ns(regression_recipe, recipes::all_numeric_predictors(), deg_free = 3))
    }'''
)

# Decision tree with cross validated tree depth
explanation_list["sep_tree_cv"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model="parsnip::decision_tree(tree_depth = hardhat::tune(), engine = 'rpart', mode = 'regression')",
    regression_tune_values='dials::grid_regular(dials::tree_depth(), levels = 4)',
    regression_vfold_cv_para={'v': 5}
)

# XGboost with default parameters
explanation_list["sep_xgboost"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model="parsnip::boost_tree(engine = 'xgboost', mode = 'regression')"
)

# XGboost with cross validated number of trees
explanation_list["sep_xgboost_cv"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_separate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model="parsnip::boost_tree(trees = hardhat::tune(), engine = 'xgboost', mode = 'regression')",
    regression_tune_values='expand.grid(trees = c(10, 15, 25, 50, 100, 500))',
    regression_vfold_cv_para={'v': 5}
)

# Explain the model using several surrogate regression methods
# Linear regression
explanation_list["sur_lm"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_surrogate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model='parsnip::linear_reg()'
)

# Using random forest with default parameters as the surrogate model
explanation_list["sur_rf"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_surrogate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model="parsnip::rand_forest(engine = 'ranger', mode = 'regression')"
)

# Using random forest with parameters tuned by cross-validation as the surrogate model
explanation_list["sur_rf_cv"] = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='regression_surrogate',
    prediction_zero=dfy_train.mean().item(),
    verbose=2,
    n_batches=1,
    regression_model="""parsnip::rand_forest(
        mtry = hardhat::tune(), trees = hardhat::tune(), engine = 'ranger', mode = 'regression'
    )""",
    regression_tune_values="""dials::grid_regular(
      dials::mtry(c(1, 8)),
      dials::trees(c(50, 750)),
      levels = 4
    )""",
    regression_vfold_cv_para={'v': 4}
)

# Print the MSEv evaluation criterion scores
print("Method", "MSEv", "Elapsed time (seconds)")
for i, (explanation_name, explanation) in enumerate(explanation_list.items()):
    print(explanation_name, round(explanation[4]["MSEv"]["MSEv"][0], 3), round(explanation[3]["total_time_secs"][0], 3))


"""
Method         MSEv     Elapsed time (seconds)
sep_tree_cv    2.243
sep_xgboost_cv 0.804
empirical      0.826
sep_lm         1.623
sep_pca        1.626
sep_splines    1.626
sep_xgboost    0.769
sur_lm         1.772
sur_rf         0.886
"""