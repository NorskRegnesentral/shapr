'''Running shapr.explain from python'''

import numpy as np
import pandas as pd
from sklearn.datasets import fetch_california_housing
from sklearn.model_selection import train_test_split

import rpy2.robjects as ro
import rpy2.robjects.pandas2ri
import rpy2.robjects.numpy2ri
from rpy2.robjects.conversion import localconverter
from rpy2.robjects.packages import importr
data_table = importr('data.table')
xgboost = importr('xgboost')
shapr = importr('shapr')
base = importr('base')

def py2r(obj):
  with localconverter(ro.default_converter + ro.numpy2ri.converter + ro.pandas2ri.converter):
    robj = ro.conversion.py2rpy(obj)
  return robj

def r2py(robj):
  with localconverter(ro.default_converter + ro.numpy2ri.converter + ro.pandas2ri.converter):
      obj = ro.conversion.rpy2py(robj)
  return obj

housing = fetch_california_housing()
dfx = pd.DataFrame(housing.data, columns=housing.feature_names)
dfy = pd.DataFrame({'target': housing.target})
dfx_train, dfx_test, dfy_train, dfy_test = train_test_split(dfx, dfy, test_size=0.99, random_state=42)
dfx_test, dfy_test = dfx_test[:5], dfy_test[:5] # To reduce computational load
rx_train = base.as_matrix(py2r(dfx_train))
ry_train = base.as_matrix(py2r(dfy_train))
rx_test = base.as_matrix(py2r(dfx_test))
ry_test = base.as_matrix(py2r(dfy_test))

## Fit model

rmodel = xgboost.xgboost(
  data = rx_train,
  label = ry_train,
  nround = 20,
  verbose = False,
)

## Shapr

rexplanation = shapr.explain(
  x_train = rx_train,
  x_explain = rx_test,
  model = rmodel,
  approach = "empirical",
  prediction_zero = base.mean(ry_train),
)

rdf_shapley = base.as_data_frame(rexplanation.rx2('shapley_values'))
df_shapley = r2py(rdf_shapley)
print(df_shapley)