import pytest 
import numpy as np
from shaprpy.explain import explain
from rpy2.rinterface_lib.embedded import RRuntimeError

# @pytest.mark.parametrize("model_fixture, simulated_data_fixture", [
#     ("model_logistic", "simulated_classification_data"),
#     ("model_xgb_classifier", "simulated_classification_data"),
#     ("model_linear", "simulated_regression_data"),
#     ("model_xgb_regressor", "simulated_regression_data"),
# ])
# def test_setup_valid_model(request, model_fixture, simulated_data_fixture):
#     model = request.getfixturevalue(model_fixture)
#     xtrain, xtest, ytrain, _ = request.getfixturevalue(simulated_data_fixture)

#     phi0 = float(np.mean(ytrain))

#     explain(
#         model=model,
#         x_train=xtrain,
#         x_explain=xtest,
#         approach="gaussian",
#         phi0=phi0,
#     )

@pytest.mark.filterwarnings("ignore::UserWarning")
@pytest.mark.filterwarnings("ignore::DeprecationWarning")
def test_setup_invalid_model(simulated_regression_data):
    xtrain, xtest, ytrain, _ = simulated_regression_data
    phi0 = float(np.mean(ytrain))

    with pytest.raises(ValueError, match="No pre-built predict_model"):
        explain(model="not a model",
            x_train=xtrain, 
            x_explain=xtest, 
            approach="gaussian", 
            phi0=phi0)
        
# def test_setup_sage(simulated_regression_data, model_xgb_regressor):
#     xtrain, _, ytrain, _ = simulated_regression_data
#     phi0 = float(np.mean(ytrain))

#     explain(
#         model=model_xgb_regressor,
#         x_train=xtrain,
#         x_explain=xtrain,
#         approach="gaussian",
#         phi0=phi0,
#         sage=True, 
#         response=ytrain
#     )

def test_setup_sage_missing_response(simulated_regression_data, model_xgb_regressor):
    xtrain, _, ytrain, _ = simulated_regression_data
    phi0 = float(np.mean(ytrain))

    with pytest.raises(RRuntimeError, match="Error in check_data"):
        explain(
            model=model_xgb_regressor,
            x_train=xtrain,
            x_explain=xtrain,
            approach="gaussian",
            phi0=phi0,
            sage=True    
            )
