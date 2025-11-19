"""
Output tests for models with categorical features using Syrupy.

These tests ensure that shapley_values_est and shapley_values_sd outputs
remain consistent for mixed numeric/categorical data.
"""
import pytest
import pandas as pd
from shaprpy import explain


class TestCategoricalOutputs:
    """Output tests for explanations with categorical features."""

    @pytest.mark.snapshot
    def test_rf_regressor_ctree_categorical_basic(self, california_housing_categorical_data,
                                                   trained_rf_regressor_categorical,
                                                   extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with ctree approach - mixed numeric/categorical data."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_categorical_data

        explanation = explain(
            model=trained_rf_regressor_categorical,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='ctree',
            phi0=dfy_train.mean().item(),
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_ctree_categorical_with_groups(self, california_housing_categorical_data,
                                                        trained_rf_regressor_categorical,
                                                        categorical_group_config,
                                                        extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with ctree approach - categorical data with feature groups."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_categorical_data

        explanation = explain(
            model=trained_rf_regressor_categorical,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='ctree',
            phi0=dfy_train.mean().item(),
            group=categorical_group_config,
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

