"""
Shared pytest fixtures for shaprpy tests.
"""
import pytest
import numpy as np
import pandas as pd
import os
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
import xgboost as xgb
from shaprpy.datasets import load_california_housing, load_binary_iris


# Set seeds for reproducibility
@pytest.fixture(scope="session", autouse=True)
def set_random_seeds():
    """Set random seeds for reproducible tests."""
    np.random.seed(42)
    # Set environment variables for R reproducibility
    os.environ['R_SEED'] = '1'
    os.environ['OMP_NUM_THREADS'] = '1'  # For consistent threading
    os.environ['OPENBLAS_NUM_THREADS'] = '1'
    os.environ['MKL_NUM_THREADS'] = '1'


@pytest.fixture(scope="session")
def california_housing_data():
    """Load California housing dataset for regression tests."""
    return load_california_housing()


@pytest.fixture(scope="session")
def binary_iris_data():
    """Load binary iris dataset for classification tests."""
    return load_binary_iris()


@pytest.fixture(scope="session")
def trained_rf_regressor(california_housing_data):
    """Trained RandomForest regressor on California housing data."""
    dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data
    model = RandomForestRegressor(random_state=1, n_estimators=100)
    model.fit(dfx_train, dfy_train.values.flatten())
    return model


@pytest.fixture(scope="session")
def trained_rf_classifier(binary_iris_data):
    """Trained RandomForest classifier on binary iris data."""
    dfx_train, dfx_test, dfy_train, dfy_test = binary_iris_data
    model = RandomForestClassifier(random_state=1, n_estimators=100)
    model.fit(dfx_train, dfy_train.values.flatten())
    return model


@pytest.fixture(scope="session")
def trained_xgb_regressor(california_housing_data):
    """Trained XGBoost regressor on California housing data."""
    dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data
    model = xgb.XGBRegressor(random_state=1, n_estimators=100, n_jobs=1)
    model.fit(dfx_train, dfy_train.values.flatten())
    return model


@pytest.fixture
def regression_group_config():
    """Group configuration for regression tests."""
    return {
        'A': ['MedInc', 'HouseAge', 'AveRooms'],
        'B': ['AveBedrms', 'Population', 'AveOccup'],
        'C': ['Latitude', 'Longitude']
    }


@pytest.fixture
def causal_ordering_config():
    """Causal ordering configuration for asymmetric Shapley values."""
    return {
        "A": ["MedInc"],
        "B": ["HouseAge", "AveRooms"],
        "C": ["AveBedrms", "Population", "AveOccup", "Latitude", "Longitude"]
    }


@pytest.fixture
def extract_shapley_outputs():
    """
    Extract only the shapley_values_est and shapley_values_sd from explanation results.
    This is what we'll use for snapshot testing.
    """
    def _extract(explanation):
        # Convert DataFrames to dictionaries for stable snapshot comparison
        result = {
            "shapley_values_est": explanation["shapley_values_est"].to_dict(),
            "shapley_values_sd": explanation["shapley_values_sd"].to_dict()
        }
        return result
    return _extract
