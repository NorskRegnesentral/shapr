import warnings
from typing import Any

import numpy as np
import pandas as pd
from rpy2.rinterface import NULL
from rpy2.robjects import Formula, default_converter
from rpy2.robjects.conversion import localconverter
from rpy2.robjects.functions import SignatureTranslatedFunction
from rpy2.robjects.numpy2ri import converter as np_converter
from rpy2.robjects.pandas2ri import _to_pandas_factor
from rpy2.robjects.pandas2ri import converter as pd_converter
from rpy2.robjects.vectors import (
    BoolVector,
    DataFrame,
    FactorVector,
    FloatMatrix,
    FloatVector,
    IntVector,
    ListVector,
    Matrix,
    POSIXct,
    StrVector,
)


@pd_converter.rpy2py.register(FactorVector)
def rpt2py_factorvector(obj: FactorVector) -> pd.Categorical:
    return _to_pandas_factor(obj)


def py2r(obj: Any) -> Any:
  with localconverter(default_converter + np_converter + pd_converter) as converter:
    robj = converter.py2rpy(obj)
  return robj


def _convert_dataframe_with_factors(robj: DataFrame) -> pd.DataFrame:
  """
  Convert R DataFrame to pandas DataFrame, preserving factor columns as categorical.

  This function efficiently handles DataFrames with factors by:
  1. Identifying which columns are factors
  2. Bulk-converting non-factor columns (fast, no warnings)
  3. Individually converting factor columns to pandas Categorical
  4. Combining and reordering to match original
  """
  # Identify factor and non-factor columns
  factor_cols = []
  non_factor_cols = []

  for col_name in robj.names:
    col_data = robj.rx2(col_name)
    if isinstance(col_data, FactorVector):
      factor_cols.append(col_name)
    else:
      non_factor_cols.append(col_name)

  # Converter for standard columns
  converter = default_converter + np_converter + pd_converter

  # Convert non-factor columns using standard (fast) method
  if non_factor_cols:
    non_factor_df = robj.rx(True, StrVector(non_factor_cols))
    result_df = converter.rpy2py(non_factor_df)
  else:
    result_df = pd.DataFrame(index=robj.rownames)

  # Convert factor columns individually to preserve as categorical
  for col_name in factor_cols:
    col_data = robj.rx2(col_name)
    result_df[col_name] = _to_pandas_factor(col_data)

  # Reorder columns to match original order
  result_df = result_df[list(robj.names)]

  return result_df


def r2py(robj: Any) -> Any:
  # Special handling for DataFrames with factor columns to preserve them as categorical
  if isinstance(robj, DataFrame):
    # Check if any columns are factors
    has_factors = any(isinstance(robj.rx2(col_name), FactorVector) for col_name in robj.names)

    if has_factors:
      return _convert_dataframe_with_factors(robj)
    else:
      # No factors, use standard DataFrame conversion (faster, no warnings)
      converter = default_converter + np_converter + pd_converter
      return converter.rpy2py(robj)
  else:
    # Not a DataFrame, use standard conversion
    converter = default_converter + np_converter + pd_converter
    obj = converter.rpy2py(robj)
    return obj


def recurse_r_tree(data: Any) -> Any:
  if data == NULL:
      return None
  elif type(data) == DataFrame:
      try:
          return r2py(data)
      except Exception:
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
  elif type(data) == POSIXct:
      with warnings.catch_warnings():
        warnings.simplefilter("ignore")
        tmp = r2py(data).strftime("%Y-%m-%d %H:%M:%S")[0]
      return tmp
  elif type(data) == SignatureTranslatedFunction or type(data) == Formula:
      return str(data)
  elif type(data) == ListVector:
      if type(data.names) == type(NULL):
          data.names = [f"element_{i+1}" for i in range(len(data))]
      return dict(zip(data.names, [recurse_r_tree(d) for d in data]))
  elif type(data) == StrVector:
      return [recurse_r_tree(d) for d in data]
  else:
      return data  # We reached the end of recursion (if not converted below, return the object as is)
