import xgboost as xgb
import warnings
import numpy as np
import pandas as pd
from typing import Callable
from datetime import datetime
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.rinterface import NULL, NA
from shaprpy.utils import r2py, py2r, recurse_r_tree
from rpy2.robjects.vectors import StrVector, ListVector
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = xgb.XGBRegressor()
model.fit(dfx_train, dfy_train.values.flatten())

from shaprpy import explain
from shaprpy.utils import r2py, py2r, recurse_r_tree


## Shapr
shapley_values, shapley_values_sd, pred_explain, MSEv, iterative_results, saving_path, rinternal = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'gaussian',
    prediction_zero = dfy_train.mean().item(),
    max_n_coalitions=30
)


shapley_values
shapley_values_sd
pred_explain
MSEv
iterative_results["dt_iter_shapley_sd"]
saving_path
rinternal



shapley_values, shapley_values_sd, pred_explain, MSEv, iterative_results, saving_path, rinternal = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'gaussian',
    prediction_zero = dfy_train.mean().item(),
    max_n_coalitions=100,
    iterative = False
)

shapley_values, shapley_values_sd, pred_explain, MSEv, iterative_results, saving_path, rinternal = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'gaussian',
    prediction_zero = dfy_train.mean().item(),
    max_n_coalitions=100,
    iterative = True,
    verbose = ["basic", "progress"]
)
