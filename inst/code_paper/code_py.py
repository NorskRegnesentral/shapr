import os
import xgboost as xgb
import pandas as pd
from shaprpy import explain
from shap import plots as shap_plt
import matplotlib.pyplot as plt



# Read data
x_train = pd.read_csv("data_and_models/" + "x_train.csv")
x_explain = pd.read_csv("data_and_models/" + "x_explain.csv")
y_train = pd.read_csv("data_and_models/" + "y_train.csv")

# Load the XGBoost model from the raw format and add feature names
model = xgb.Booster()
model.load_model("data_and_models/" + "xgb.model")
model.feature_names = x_train.columns.tolist()

# Predict x_explain using the XGBoost model
exp_40_ctree = explain(model = model,
                       x_train = x_train,
                       x_explain = x_explain,
                       approach = "ctree",
                       phi0 = y_train.mean().item(),
                       verbose = None,
                       max_n_coalitions=40,
                       ctree_sample = False,
                       seed = 1)


# Print the Shapley values
exp_40_ctree.print()

# Print the MSE of the v(S)
exp_40_ctree.print(what = "MSEv")

# Display a "force plot" of the eight observation using the shap package ##

exp_40_ctree_shap = exp_40_ctree.to_shap() # Convert to shap class

shap_plt.force(exp_40_ctree_shap[8-1], matplotlib = True,show = True)

# Generate another one for saving to disk
plt.figure(figsize=(16, 4), dpi=300)
shap_plt.force(exp_40_ctree_shap[8-1], matplotlib = True,show = False)
plt.tight_layout()
os.makedirs("Py_paper_figures", exist_ok=True)
plt.savefig(fname="Py_paper_figures/py_force_plot.pdf")
plt.close()



# Print the session information
import session_info
session_info.show(html=False)
