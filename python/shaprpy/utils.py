import numpy as np
import pandas as pd
from rpy2.robjects.conversion import localconverter
from rpy2.robjects import default_converter
from rpy2.robjects.numpy2ri import converter as np_converter
from rpy2.robjects.pandas2ri import converter as pd_converter
from rpy2.robjects.pandas2ri import _to_pandas_factor
from rpy2.rinterface import NULL, NA
from rpy2.robjects.vectors import DataFrame, FloatVector, IntVector, BoolVector, StrVector, ListVector, FactorVector, FloatMatrix, Matrix


@pd_converter.rpy2py.register(FactorVector)
def rpt2py_factorvector(obj):
    return _to_pandas_factor(obj)


def py2r(obj):
  with localconverter(default_converter + np_converter + pd_converter) as converter:
    robj = converter.py2rpy(obj)
  return robj


def r2py(robj):
  converter = default_converter + np_converter + pd_converter
  obj = converter.rpy2py(robj)
  return obj


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
  elif type(data) == FactorVector:
      return _to_pandas_factor(data)
  elif type(data) == ListVector:
      return dict(zip(data.names, [recurse_r_tree(d) for d in data]))
  elif type(data) == StrVector:
      return [recurse_r_tree(d) for d in data]
  else:
      if hasattr(data, "rclass"): # An unsupported r class
          raise KeyError(f'Could not proceed, unknown data type {type(data)}')
      else:
          return data  # We reached the end of recursion