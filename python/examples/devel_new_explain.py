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

from shaprpy import explain, devel
from shaprpy.utils import r2py, py2r, recurse_r_tree

## devel

data_table = importr('data.table')
shapr = importr('shapr')
utils = importr('utils')
base = importr('base')


rinternal = devel(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'gaussian',
    prediction_zero = dfy_train.mean().item(),
    max_n_coalitions=30
)

shapr.cli_iter("basic", rinternal, 1)

rinternal.rx2['timing_list'] = ro.ListVector({'init': base.Sys_time()})

# Setup the Shapley framework
rinternal = shapr.shapley_setup(rinternal)

# Only actually called for approach in ["regression_surrogate", "vaeac"]
rinternal = shapr.setup_approach(rinternal)
iter = 1


 #  S_batch <- internal$iter_list[[iter]]$S_batch
 S_batch = rinternal.rx2('iter_list')[iter-1].rx2('S_batch')
  
  # verbose
  shapr.cli_compute_vS(rinternal)
  
  vS_list = ro.ListVector({})
  for i, S in enumerate(S_batch):
    vS_list.rx2[i] = batch_compute_vS(S=S, rinternal=rinternal, model=model, predict_model=predict_model)
    
  #### Adds v_S output above to any vS_list already computed ####
  vS_list = shapr.append_vS_list(vS_list,rinternal)
    


      # Compute the vS
vS_list = compute_vS(rinternal, model, predict_model)

      # Compute Shapley value estimates and bootstrapped standard deviations
      rinternal = shapr.compute_estimates(rinternal, vS_list)

      # Check convergence based on estimates and standard deviations (and thresholds)
      rinternal = shapr.check_convergence(rinternal)

      # Save intermediate results
      shapr.save_results(rinternal)

      # Preparing parameters for next iteration (does not do anything if already converged)
      rinternal = shapr.prepare_next_iteration(rinternal)

      # Printing iteration information
      shapr.print_iter(rinternal)

      # Setting globals to simplify the loop
      converged = rinternal.rx2('iter_list')[iter].rx2('converged')[0]

      rinternal.rx2['timing_list'].rx2['postprocess_res'] = base.Sys_time()

      rinternal.rx2['iter_timing_list'][iter] = rinternal.rx2['timing_list']

      iter += 1







#### END DEVEL ######
from shaprpy import explain, devel

## Shapr
test = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'gaussian',
    prediction_zero = dfy_train.mean().item(),
    max_n_coalitions=30
)

test

test = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'vaeac',
    prediction_zero = dfy_train.mean().item(),
    max_n_coalitions=30,
    verbose = ["basic", "progress"]
)

test



def remove_nulls_from_listvector(lv):
    if isinstance(lv, ListVector):
        cleaned_lv = {}
        for k, v in lv.items():
            if isinstance(v, ListVector):
                cleaned_lv[k] = remove_nulls_from_listvector(v)
            elif v != NULL:
                cleaned_lv[k] = v
        return ListVector(cleaned_lv)
    return lv

cleaned_parameters = remove_nulls_from_listvector(test.rx2('parameters'))



def subset_listvector_by_index(lv, indices):
    if isinstance(lv, ListVector):
        keys = list(lv.names)
        return ListVector({keys[i]: lv[keys[i]] for i in indices if i < len(keys)})
    return lv

# Example usage: specify the indices of the elements you want to keep
indices_to_keep = [0, 1]  # Adjusted to keep the first two elements (indices start from 0)
subset_parameters = subset_listvector_by_index(cleaned_parameters, indices_to_keep)

recurse_r_tree(cleaned_parameters)

parameters = recurse_r_tree(test.rx2('parameters'))

parameters.rx(1, 2, 3)

def rlist_to_dict(rlist):
    if isinstance(rlist, ListVector):
        return {key: rlist_to_dict(value) for key, value in rlist.items()}
    elif isinstance(rlist, StrVector):
        return list(rlist)
    elif rlist in (NULL, NA):
        return None
    else:
        return rlist

parameters_dict = recurse_r_tree(parameters)

parameters = recurse_r_tree(test.rx2('parameters'))
test

