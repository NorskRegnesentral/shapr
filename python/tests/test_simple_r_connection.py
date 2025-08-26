"""
Simple test to verify R connection and model predictions reproducibility.

This test uses the existing fixtures to create models and make predictions,
checking for consistency between runs.
"""
import pytest
import rpy2.robjects as robjects
import numpy as np


class TestSimpleRConnectionAndModels:
    """Simple test for R connection and model predictions reproducibility."""

    def test_r_random_numbers_reproducibility(self):
        """Test that R produces reproducible random numbers."""
        # Set seed in R for reproducibility
        robjects.r('set.seed(123)')
        
        # Sample 5 random numbers from uniform distribution
        result1 = robjects.r('runif(5)')
        
        # Convert to numpy array for easier comparison
        numbers1 = np.array(result1)
        
        # Reset seed and sample again - should get same numbers
        robjects.r('set.seed(123)')
        result2 = robjects.r('runif(5)')
        numbers2 = np.array(result2)
        
        # Check that we get the same numbers
        np.testing.assert_array_almost_equal(numbers1, numbers2, decimal=10)
        
        # Also check that we get exactly 5 numbers
        assert len(numbers1) == 5
        assert len(numbers2) == 5
        
        # Check against expected values (these are the exact values we should get with seed 123)
        expected_numbers = np.array([0.28757752, 0.78830514, 0.40897692, 0.8830174, 0.94046728])
        np.testing.assert_array_almost_equal(numbers1, expected_numbers, decimal=7)
        
        print(f"Sampled numbers: {numbers1}")
        print(f"Expected numbers: {expected_numbers}")
        print(f"Numbers match expected: {np.allclose(numbers1, expected_numbers)}")
        print(f"Numbers are reproducible: {np.allclose(numbers1, numbers2)}")

    def test_rf_regressor_predictions_reproducibility(self, california_housing_data, trained_rf_regressor):
        """Test that RandomForest regressor predictions are reproducible."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data
        
        # Make predictions on first 5 test samples
        test_sample = dfx_test.head(5)
        predictions1 = trained_rf_regressor.predict(test_sample)
        
        # Create a new model with same parameters and data - should get same predictions
        from sklearn.ensemble import RandomForestRegressor
        model2 = RandomForestRegressor(random_state=1, n_estimators=100)
        model2.fit(dfx_train, dfy_train.values.flatten())
        predictions2 = model2.predict(test_sample)
        
        # Check reproducibility
        np.testing.assert_array_almost_equal(predictions1, predictions2, decimal=10)
        
        print(f"RF regressor predictions (first 5): {predictions1}")
        print(f"Predictions are reproducible: {np.allclose(predictions1, predictions2)}")
        
        # Basic sanity checks
        assert len(predictions1) == 5
        assert len(predictions2) == 5
        assert all(np.isfinite(predictions1))
        assert all(np.isfinite(predictions2))

    def test_xgb_regressor_predictions_reproducibility(self, california_housing_data, trained_xgb_regressor):
        """Test that XGBoost regressor predictions are reproducible."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data
        
        # Make predictions on first 5 test samples
        test_sample = dfx_test.head(5)
        predictions1 = trained_xgb_regressor.predict(test_sample)
        
        # Create a new model with same parameters and data
        import xgboost as xgb
        model2 = xgb.XGBRegressor(random_state=1, n_estimators=100, n_jobs=1)
        model2.fit(dfx_train, dfy_train.values.flatten())
        predictions2 = model2.predict(test_sample)
        
        # Check reproducibility
        np.testing.assert_array_almost_equal(predictions1, predictions2, decimal=10)
        
        print(f"XGB regressor predictions (first 5): {predictions1}")
        print(f"Predictions are reproducible: {np.allclose(predictions1, predictions2)}")
        
        # Basic sanity checks
        assert len(predictions1) == 5
        assert len(predictions2) == 5
        assert all(np.isfinite(predictions1))
        assert all(np.isfinite(predictions2))

    def test_rf_classifier_predictions_reproducibility(self, binary_iris_data, trained_rf_classifier):
        """Test that RandomForest classifier predictions are reproducible."""
        dfx_train, dfx_test, dfy_train, dfy_test = binary_iris_data
        
        # Make predictions on first 5 test samples
        test_sample = dfx_test.head(5)
        predictions1 = trained_rf_classifier.predict_proba(test_sample)
        
        # Create a new model with same parameters and data
        from sklearn.ensemble import RandomForestClassifier
        model2 = RandomForestClassifier(random_state=1, n_estimators=100)
        model2.fit(dfx_train, dfy_train.values.flatten())
        predictions2 = model2.predict_proba(test_sample)
        
        # Check reproducibility
        np.testing.assert_array_almost_equal(predictions1, predictions2, decimal=10)
        
        print(f"RF classifier predictions (first 5, probabilities): {predictions1}")
        print(f"Predictions are reproducible: {np.allclose(predictions1, predictions2)}")
        
        # Basic sanity checks
        assert predictions1.shape == (5, 2)  # 5 samples, 2 classes
        assert predictions2.shape == (5, 2)
        assert all(np.isfinite(predictions1.flatten()))
        assert all(np.isfinite(predictions2.flatten()))

    def test_r_version_info(self):
        """Test that we can get R version information."""
        version_result = robjects.r('R.version.string')
        version = str(version_result)
        print(f"R version: {version}")
        
        # Just check that we got some version string
        assert isinstance(version, str)
        assert len(version) > 0
        assert 'R version' in version