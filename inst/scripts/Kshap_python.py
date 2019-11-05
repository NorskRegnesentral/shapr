#### Python code ####
import xgboost as xgb
import shap
import numpy as np
import pandas as pd
import time

model = xgb.Booster()  # init model
model.load_model("inst/model_objects/xgboost_model_object_raw")

## kernel shap sends data as numpy array which has no column names, so we fix it
def xgb_predict(data_asarray):
  data_asDmatrix =  xgb.DMatrix(data_asarray)
  return model.predict(data_asDmatrix)

py_pred_test = xgb_predict(r.x_test) # Test predictions in python

#

#### Applying kernelshap

time_py_start = time.perf_counter()

shap_kernel_explainer = shap.KernelExplainer(xgb_predict, r.x_train)
Kshap_shap0 = shap_kernel_explainer.shap_values(r.x_test,nsamples = int(100000),l1_reg=0)

time_py_end = time.perf_counter()

time_py = time_py_end-time_py_start

getattr(shap_kernel_explainer,'expected_value') # This is phi0, not used at all below

Kshap_shap = pd.DataFrame(Kshap_shap0,columns = r.x_var)

Kshap_shap.insert(0,"none",getattr(shap_kernel_explainer,'expected_value'),True) # Adding the none column
