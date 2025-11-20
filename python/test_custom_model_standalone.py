"""
Standalone test script for custom model with predict_model.
Run this to test the custom model functionality independently.
"""
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from shaprpy import explain
from shaprpy.datasets import load_california_housing


# Custom model class
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


# Custom predict_model function
def custom_predict_model(model, x):
    """Custom predict_model function for SimpleLinearModel."""
    return model.predict(x).flatten()


# Load data
print("Loading California housing data...")
dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

# Train custom model
print("Training custom linear model...")
model = SimpleLinearModel()
model.fit(dfx_train, dfy_train)

# Test predictions work
print(f"Testing predictions on first row: {model.predict(dfx_test.iloc[:1])[0]:.4f}")

# Run explanation
print("\nRunning shapr explanation with custom model...")
explanation = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach='empirical',
    predict_model=custom_predict_model,
    phi0=dfy_train.mean().item(),
    max_n_coalitions=50,
    seed=1
)

# Print results
print("\n" + "="*80)
print("RESULTS")
print("="*80)
explanation.print()

print("\nShapley standard deviations:")
explanation.print("shapley_sd")

print("\nMSEv evaluation criterion:")
explanation.print("MSEv")
