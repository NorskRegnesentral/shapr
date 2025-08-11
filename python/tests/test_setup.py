import pytest 
import numpy as np
from shaprpy.explain import explain
from rpy2.rinterface_lib.embedded import RRuntimeError

'''
UserWarning: No pre-built get_model_specs for model of type <class 'str'>, disabling checks.
    warnings.warn(f'No pre-built get_model_specs for model of type {type(model)}, disabling checks.')
DeprecationWarning: The following error was raised: 'str' object has no attribute '__sklearn_tags__'. 
'''
@pytest.mark.filterwarnings("ignore::UserWarning")
@pytest.mark.filterwarnings("ignore::DeprecationWarning")
def test_setup_invalid_model(house_data):
    xtrain, xtest, ytrain, _ = house_data
    phi0 = float(np.mean(ytrain))

    invalid_model = "not a model"

    with pytest.raises(ValueError, match="No pre-built predict_model"):
        explain(
            model=invalid_model,
            x_train=xtrain, 
            x_explain=xtest, 
            approach="gaussian", 
            phi0=phi0
        )

def test_setup_invalid_xtrain(house_data, model_xgb):
    _, xtest, ytrain, _ = house_data
    phi0 = float(np.mean(ytrain))

    invalid_xtrain = [["a", "b"], [1, 2]]

    with pytest.raises(RRuntimeError):
        explain(
            model=model_xgb,
            x_train=invalid_xtrain, 
            x_explain=xtest, 
            approach="gaussian", 
            phi0=phi0
        )

def test_setup_missing_colnames_xexplain(house_data, model_xgb):
    xtrain, xtest, ytrain, _ = house_data
    phi0 = float(np.mean(ytrain))

    invalid_xtest = xtest.copy()
    invalid_xtest.columns = [''] * len(xtrain.columns)

    with pytest.raises(ValueError, match = "The name cannot be an empty string"):
        explain(
            model=model_xgb,
            x_train=xtrain, 
            x_explain=invalid_xtest, 
            approach="gaussian", 
            phi0=phi0
        )

def test_setup_sage_not_boolean(house_data, model_xgb):
    xtrain, _, ytrain, _ = house_data
    phi0 = float(np.mean(ytrain))

    invalid_sage = 4

    with pytest.raises(RRuntimeError, match="Error in get_parameters"):
        explain(
            model=model_xgb,
            x_train=xtrain,
            x_explain=xtrain,
            approach="gaussian",
            phi0=phi0,
            sage=invalid_sage    
        )

def test_setup_sage_missing_response(house_data, model_xgb):
    xtrain, _, ytrain, _ = house_data
    phi0 = float(np.mean(ytrain))

    with pytest.raises(RRuntimeError, match="Error in check_data"):
        explain(
            model=model_xgb,
            x_train=xtrain,
            x_explain=xtrain,
            approach="gaussian",
            phi0=phi0,
            sage=True    
        )
