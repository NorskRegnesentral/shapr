"""
Snapshot tests for classification models using Syrupy.

These tests ensure that the shapley_values_est and shapley_values_sd outputs
remain consistent across changes to the codebase.
"""
import pytest
import pandas as pd
from shaprpy import explain


@pytest.mark.integration
class TestClassificationSnapshots:
    """Snapshot tests for classification model explanations."""

    def test_rf_classifier_empirical_basic(self, binary_iris_data, trained_rf_classifier, extract_shapley_outputs):
        """Test RandomForest classifier with empirical approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = binary_iris_data

        explanation = explain(
            model=trained_rf_classifier,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='empirical',
            phi0=dfy_train.mean().item(),
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Basic assertions for now - will convert to snapshots later
        assert isinstance(result['shapley_values_est'], pd.DataFrame)
        assert isinstance(result['shapley_values_sd'], pd.DataFrame)
        assert len(result['shapley_values_est']) == len(dfx_test)
        assert len(result['shapley_values_sd']) == len(dfx_test)

        # Check that we have the expected columns (features + none + explain_id)
        expected_features = list(dfx_train.columns) + ['none', 'explain_id']
        assert all(col in result['shapley_values_est'].columns for col in expected_features)

    def test_rf_classifier_independence_basic(self, binary_iris_data, trained_rf_classifier, extract_shapley_outputs):
        """Test RandomForest classifier with independence approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = binary_iris_data

        explanation = explain(
            model=trained_rf_classifier,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='independence',
            phi0=dfy_train.mean().item(),
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Basic assertions for now - will convert to snapshots later
        assert isinstance(result['shapley_values_est'], pd.DataFrame)
        assert isinstance(result['shapley_values_sd'], pd.DataFrame)
        assert len(result['shapley_values_est']) == len(dfx_test)
        assert len(result['shapley_values_sd']) == len(dfx_test)
