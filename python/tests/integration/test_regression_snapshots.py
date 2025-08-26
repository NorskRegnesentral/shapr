"""
Snapshot tests for regression models using Syrupy.

These tests ensure that the shapley_values_est and shapley_values_sd outputs
remain consistent across changes to the codebase.
"""
import pytest
import pandas as pd
from shaprpy import explain


@pytest.mark.integration
class TestRegressionSnapshots:
    """Snapshot tests for regression model explanations."""

    @pytest.mark.snapshot
    def test_rf_regressor_empirical_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with empirical approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data
        
        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='empirical',
            phi0=dfy_train.mean().item(),
            seed=1
        )
        
        result = extract_shapley_outputs(explanation)
        
        # Use syrupy for snapshot testing
        assert result == snapshot

    def test_rf_regressor_independence_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs):
        """Test RandomForest regressor with independence approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
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

    def test_rf_regressor_empirical_with_groups(self, california_housing_data, trained_rf_regressor, 
                                                regression_group_config, extract_shapley_outputs):
        """Test RandomForest regressor with empirical approach and feature groups."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='empirical',
            phi0=dfy_train.mean().item(),
            group=regression_group_config,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Basic assertions for grouped results
        assert isinstance(result['shapley_values_est'], pd.DataFrame)
        assert isinstance(result['shapley_values_sd'], pd.DataFrame)
        assert len(result['shapley_values_est']) == len(dfx_test)

        # Check that we have group columns instead of individual features
        expected_groups = list(regression_group_config.keys()) + ['none', 'explain_id']
        assert all(col in result['shapley_values_est'].columns for col in expected_groups)

    @pytest.mark.slow
    def test_xgb_regressor_empirical_basic(self, california_housing_data, trained_xgb_regressor, extract_shapley_outputs):
        """Test XGBoost regressor with empirical approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_xgb_regressor,
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
