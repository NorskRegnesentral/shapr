import numpy as np
import pandas as pd
from sklearn.datasets import fetch_california_housing, load_iris
from sklearn.model_selection import train_test_split
import rpy2.robjects as ro
import rpy2.robjects.pandas2ri
import rpy2.robjects.numpy2ri
from rpy2.robjects.conversion import localconverter
from rpy2.rinterface import NULL, NA
from rpy2.robjects.vectors import DataFrame, FloatVector, IntVector, BoolVector, StrVector, ListVector, FloatMatrix, Matrix


def py2r(obj):
  with localconverter(ro.default_converter + ro.numpy2ri.converter + ro.pandas2ri.converter):
    robj = ro.conversion.py2rpy(obj)
  return robj


def r2py(robj):
  with localconverter(ro.default_converter + ro.numpy2ri.converter + ro.pandas2ri.converter):
      obj = ro.conversion.rpy2py(robj)
  return obj


def load_california_housing():
  housing = fetch_california_housing()
  dfx = pd.DataFrame(housing.data, columns=housing.feature_names)
  dfy = pd.DataFrame({'target': housing.target})
  dfx_train, dfx_test, dfy_train, dfy_test = train_test_split(dfx, dfy, test_size=0.99, random_state=42)
  dfx_test, dfy_test = dfx_test[:5], dfy_test[:5] # To reduce computational load
  return dfx_train, dfx_test, dfy_train, dfy_test


def load_binary_iris():
  bcancer = load_iris()
  dfx = pd.DataFrame(bcancer.data, columns=bcancer.feature_names).iloc[bcancer.target<2] # Turning it into a binary classification problem
  dfy = pd.DataFrame({'target': bcancer.target}).iloc[bcancer.target<2] # Turning it into a binary classification problem
  dfx_train, dfx_test, dfy_train, dfy_test = train_test_split(dfx, dfy, test_size=5, random_state=42)
  return dfx_train, dfx_test, dfy_train, dfy_test


def recurse_r_tree(data):
  if data == NULL:
      return None
  elif type(data) == DataFrame:
      try:
          return r2py(data)
      except Exception as e:
          # The column "features" in internal$objects$X is known to cause problems
          d = {}
          for col in data.names:
              try:
                  d[col] = r2py(data.rx2(col))
              except:
                  # We manually convert the elements of the column "features" in internal$objects$X
                  d[col] = [r2py(d) for d in data.rx2(col)]
          return pd.DataFrame(d, index=data.rownames)
  elif type(data) in [FloatVector, IntVector, BoolVector, FloatMatrix, Matrix]:
      return np.array(data)
  elif type(data) == ListVector:
      return dict(zip(data.names, [recurse_r_tree(d) for d in data]))
  elif type(data) == StrVector:
      return [recurse_r_tree(d) for d in data]
  else:
      if hasattr(data, "rclass"):  # An unsupported r class
          raise KeyError('Could not proceed, type {} is not defined'
                          'to add support for this type, just add it to the imports '
                          'and to the appropriate type list above'.format(type(data)))
      else:
          return data  # We reached the end of recursion