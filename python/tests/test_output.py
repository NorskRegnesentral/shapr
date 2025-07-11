from shaprpy.explain import explain
import numpy as np
from rpy2.robjects import r

def test_output_xgb_gaussian(simulated_regression_data, model_xgb_regressor, snapshot):
    xtrain, xtest, ytrain, _ = simulated_regression_data
    phi0 = float(np.mean(ytrain))

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_xgb_regressor,
        x_train=xtrain,
        x_explain=xtest,
        approach="gaussian",
        phi0=phi0, 
        seed = 42
    )

    snapshot.assert_match(explanation, 'test_output_xgb_gaussian')


def test_output_sage_xgb_gaussian(simulated_regression_data, model_xgb_regressor, snapshot):
    xtrain, _, ytrain, _ = simulated_regression_data
    phi0 = float(np.mean(ytrain))

    # snapshot.assert_match(ytrain, "ytrain")

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_xgb_regressor,
        x_train=xtrain,
        x_explain=xtrain,
        approach="gaussian",
        phi0=phi0, 
        seed = 42, 
        sage = True, 
        response = ytrain
    )
    
    snapshot.assert_match(explanation, 'test_output_sage_xgb_gaussian')

