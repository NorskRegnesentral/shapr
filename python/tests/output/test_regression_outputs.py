"""
Output tests for regression models using Syrupy.

These tests ensure that the shapley_values_est and shapley_values_sd outputs
remain consistent across changes to the codebase.
"""
import pytest
import pandas as pd
from shaprpy import explain


class TestRegressionOutputs:
    """Output tests for regression model explanations."""

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

    @pytest.mark.snapshot
    def test_rf_regressor_independence_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
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

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_empirical_with_groups(self, california_housing_data, trained_rf_regressor,
                                                regression_group_config, extract_shapley_outputs, snapshot):
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

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.slow
    @pytest.mark.snapshot
    def test_xgb_regressor_empirical_basic(self, california_housing_data, trained_xgb_regressor, extract_shapley_outputs, snapshot):
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

        # Use syrupy for snapshot testing
        assert result == snapshot
