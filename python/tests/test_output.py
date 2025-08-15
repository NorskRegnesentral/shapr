import pytest
from shaprpy.explain import explain
import numpy as np
from rpy2.robjects import r
import xgboost as xgb


def test_output_xgb_gaussian(house_data, model_xgb, snapshot):
    xtrain, xtest, ytrain, _ = house_data
    phi0 = float(ytrain.mean().item())

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_xgb,
        x_train=xtrain,
        x_explain=xtest,
        approach="gaussian",
        phi0=phi0, 
        seed = 42
    )

    snapshot.assert_match(explanation["shapley_values_est"], 'test_output_xgb_gaussian_est.txt')
    snapshot.assert_match(explanation["shapley_values_sd"], 'test_output_xgb_gaussian_sd.txt')

def test_output_xgb_independence(house_data, model_xgb, snapshot):
    xtrain, xtest, ytrain, _ = house_data
    phi0 = float(ytrain.mean().item())

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_xgb,
        x_train=xtrain,
        x_explain=xtest,
        approach="independence",
        phi0=phi0, 
        seed = 42
    )

    snapshot.assert_match(explanation["shapley_values_est"], 'test_output_xgb_independence_est')
    snapshot.assert_match(explanation["shapley_values_sd"], 'test_output_xgb_independence_sd')

def test_output_lm_empirical(iris_data, model_lm, snapshot):
    xtrain, xtest, ytrain, _ = iris_data
    phi0 = float(ytrain.mean().item())

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_lm,
        x_train=xtrain,
        x_explain=xtest,
        approach="empirical",
        phi0=phi0, 
        seed = 42
    )

    snapshot.assert_match(explanation["shapley_values_est"], 'test_output_lm_empirical_est')
    snapshot.assert_match(explanation["shapley_values_sd"], 'test_output_lm_empirical_sd')

def test_output_lm_copula(iris_data, model_lm, snapshot):
    xtrain, xtest, ytrain, _ = iris_data
    phi0 = float(ytrain.mean().item())

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_lm,
        x_train=xtrain,
        x_explain=xtest,
        approach="copula",
        phi0=phi0, 
        seed = 42
    )

    snapshot.assert_match(explanation["shapley_values_est"], 'test_output_lm_copula_est')
    snapshot.assert_match(explanation["shapley_values_sd"], 'test_output_lm_copula_sd')

def test_output_sage_xgb_gaussian(house_data, model_xgb, snapshot):
    xtrain, xtest, ytrain, _ = house_data
    phi0 = float(ytrain.mean().item())

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_xgb,
        x_train=xtrain,
        x_explain=xtrain,
        approach="gaussian",
        phi0=phi0, 
        seed = 42, 
        sage = True, 
        response = ytrain.squeeze()
    )
    
    snapshot.assert_match(explanation["shapley_values_est"], 'test_output_sage_xgb_gaussian_est')
    snapshot.assert_match(explanation["shapley_values_sd"], 'test_output_sage_xgb_gaussian_sd')

def test_output_sage_lm_copula(house_data, model_xgb, snapshot):
    xtrain, xtest, ytrain, _ = house_data
    phi0 = float(ytrain.mean().item())

    r('set.seed(42)')

    explanation = explain(
        testing = True,
        model=model_xgb,
        x_train=xtrain,
        x_explain=xtrain,
        approach="copula",
        phi0=phi0, 
        seed = 42, 
        sage = True, 
        response = ytrain.squeeze()
    )
    
    snapshot.assert_match(explanation["shapley_values_est"], 'test_output_sage_lm_copula_est')
    snapshot.assert_match(explanation["shapley_values_sd"], 'test_output_sage_lm_copula_sd')


