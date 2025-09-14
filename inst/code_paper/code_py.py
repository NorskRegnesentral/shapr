import xgboost as xgb
import pandas as pd
from shaprpy import explain

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
print(exp_40_ctree["shapley_values_est"].iloc[:, 1:].round(2))


# Print the session information
import session_info
import os
session_info.show(html=False)
