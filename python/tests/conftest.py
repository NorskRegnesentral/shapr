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


@pytest.fixture(scope="session")
def california_housing_categorical_data():
    """Load California housing dataset with added categorical features."""
    dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

    # Create categorical features by binning continuous variables
    # Income category: Low, Medium, High
    income_bins = [0, 3, 6, 15]
    income_labels = ['Low', 'Medium', 'High']
    dfx_train['IncomeCategory'] = pd.cut(dfx_train['MedInc'], bins=income_bins, labels=income_labels)
    dfx_test['IncomeCategory'] = pd.cut(dfx_test['MedInc'], bins=income_bins, labels=income_labels)

    # House age category: New, Mid, Old
    age_bins = [0, 15, 35, 60]
    age_labels = ['New', 'Mid', 'Old']
    dfx_train['AgeCategory'] = pd.cut(dfx_train['HouseAge'], bins=age_bins, labels=age_labels)
    dfx_test['AgeCategory'] = pd.cut(dfx_test['HouseAge'], bins=age_bins, labels=age_labels)

    # Location type based on latitude: North, Central, South
    lat_bins = [32, 34, 37, 42]
    lat_labels = ['South', 'Central', 'North']
    dfx_train['LocationType'] = pd.cut(dfx_train['Latitude'], bins=lat_bins, labels=lat_labels)
    dfx_test['LocationType'] = pd.cut(dfx_test['Latitude'], bins=lat_bins, labels=lat_labels)

    # Convert categorical columns to pandas categorical dtype with consistent categories
    # This ensures rpy2 will convert them to R factors properly and both train/test have same levels
    cat_cols = ['IncomeCategory', 'AgeCategory', 'LocationType']
    for col in cat_cols:
        # Fill any NaN values with a placeholder before converting to string (pandas 3.0 compatibility)
        dfx_train[col] = dfx_train[col].cat.add_categories(['Unknown']).fillna('Unknown')
        dfx_test[col] = dfx_test[col].cat.add_categories(['Unknown']).fillna('Unknown')

        # Convert to string
        dfx_train[col] = dfx_train[col].astype(str)
        dfx_test[col] = dfx_test[col].astype(str)

        # Get all unique categories from both train and test
        all_categories = pd.unique(pd.concat([dfx_train[col], dfx_test[col]]).values)
        # Ensure no null/nan values in categories list (pandas 3.0 requirement)
        all_categories = [cat for cat in all_categories if pd.notna(cat)]

        # Create categorical with explicit categories for both
        dfx_train[col] = pd.Categorical(dfx_train[col], categories=all_categories)
        dfx_test[col] = pd.Categorical(dfx_test[col], categories=all_categories)

    return dfx_train, dfx_test, dfy_train, dfy_test


@pytest.fixture(scope="session")
def trained_rf_regressor_categorical(california_housing_categorical_data):
    """Trained RandomForest regressor on California housing data with categorical features."""
    from sklearn.compose import ColumnTransformer
    from sklearn.preprocessing import OneHotEncoder
    from sklearn.pipeline import Pipeline

    dfx_train, dfx_test, dfy_train, dfy_test = california_housing_categorical_data

    # Define numeric and categorical features
    numeric_features = ['MedInc', 'HouseAge', 'AveRooms', 'AveBedrms', 'Population', 'AveOccup', 'Latitude', 'Longitude']
    categorical_features = ['IncomeCategory', 'AgeCategory', 'LocationType']

    # Create preprocessing pipeline
    preprocessor = ColumnTransformer(
        transformers=[
            ('num', 'passthrough', numeric_features),
            ('cat', OneHotEncoder(handle_unknown='ignore', sparse_output=False), categorical_features)
        ]
    )

    # Create full pipeline with RandomForest
    pipeline = Pipeline(
        steps=[
            ('preprocessor', preprocessor),
            ('model', RandomForestRegressor(random_state=1, n_estimators=100))
        ]
    )

    pipeline.fit(dfx_train, dfy_train.values.flatten())
    return pipeline


@pytest.fixture
def categorical_group_config():
    """Group configuration for categorical regression tests."""
    return {
        'NumericA': ['MedInc', 'HouseAge', 'AveRooms'],
        'NumericB': ['AveBedrms', 'Population', 'AveOccup'],
        'Location': ['Latitude', 'Longitude'],
        'Categories': ['IncomeCategory', 'AgeCategory', 'LocationType']
    }


@pytest.fixture
def extract_shapley_outputs():
    """
    Extract only the shapley_values_est and shapley_values_sd from explanation results.
    This is what we'll use for snapshot testing.
    """
    def _extract(explanation):
        # Convert DataFrames to markdown format for stable snapshot comparison
        # Round to 5 decimal places to avoid numerical precision issues across environments
        result = {
            "shapley_values_est": explanation.get_results("shapley_est").round(5).to_markdown(),
            "shapley_values_sd": explanation.get_results("shapley_sd").round(5).to_markdown()
        }
        return result
    return _extract


# Custom model fixtures for testing non-standard model types
class SimpleLinearModel:
    """
    Simple linear regression model implemented from scratch.
    Used for testing custom model support without sklearn.
    """
    def __init__(self):
        self.coef_ = None
        self.intercept_ = None
        self.is_fitted_ = False

    def fit(self, X, y):
        """Fit linear model using normal equations: (X'X)^-1 X'y"""
        X_array = X.values if hasattr(X, 'values') else X
        y_array = y.values.flatten() if hasattr(y, 'values') else y.flatten()

        # Add intercept column
        X_with_intercept = np.column_stack([np.ones(len(X_array)), X_array])

        # Solve normal equations
        params = np.linalg.lstsq(X_with_intercept, y_array, rcond=None)[0]
        self.intercept_ = params[0]
        self.coef_ = params[1:]
        self.is_fitted_ = True
        return self

    def predict(self, X):
        """Predict using the linear model"""
        if not self.is_fitted_:
            raise ValueError("Model must be fitted before calling predict")
        X_array = X.values if hasattr(X, 'values') else X
        return self.intercept_ + X_array @ self.coef_


@pytest.fixture(scope="session")
def trained_custom_regressor(california_housing_data):
    """Trained custom linear model on California housing data."""
    dfx_train, _, dfy_train, _ = california_housing_data
    model = SimpleLinearModel()
    model.fit(dfx_train, dfy_train)
    return model


@pytest.fixture
def custom_predict_model():
    """Custom predict_model function for SimpleLinearModel."""
    def predict_fn(model, x):
        return model.predict(x).flatten()
    return predict_fn
