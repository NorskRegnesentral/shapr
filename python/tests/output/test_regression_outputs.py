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

    @pytest.mark.snapshot
    def test_rf_regressor_gaussian_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with gaussian approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='gaussian',
            phi0=dfy_train.mean().item(),
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.slow
    @pytest.mark.snapshot
    def test_rf_regressor_ctree_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with ctree approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
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
    def test_rf_regressor_gaussian_with_groups(self, california_housing_data, trained_rf_regressor,
                                              regression_group_config, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with gaussian approach and feature groups."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='gaussian',
            phi0=dfy_train.mean().item(),
            group=regression_group_config,
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.slow
    @pytest.mark.snapshot
    def test_rf_regressor_empirical_with_causal_ordering(self, california_housing_data, trained_rf_regressor,
                                                        causal_ordering_config, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with empirical approach and causal ordering."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='empirical',
            phi0=dfy_train.mean().item(),
            asymmetric=True,
            causal_ordering=causal_ordering_config,
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_regression_separate_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with regression_separate approach - basic linear regression."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='regression_separate',
            phi0=dfy_train.mean().item(),
            regression_model='parsnip::linear_reg()',
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_regression_surrogate_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with regression_surrogate approach - decision tree."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='regression_surrogate',
            phi0=dfy_train.mean().item(),
            regression_model="parsnip::decision_tree(engine = 'rpart', mode = 'regression')",
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_gaussian_iterative_low_coalitions(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with gaussian approach - iterative with low n_coalitions."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='gaussian',
            phi0=dfy_train.mean().item(),
            iterative=True,
            max_n_coalitions=20,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_empirical_iterative(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with empirical approach - explicit iterative mode."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='empirical',
            phi0=dfy_train.mean().item(),
            iterative=True,
            max_n_coalitions=100,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_copula_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with copula approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='copula',
            phi0=dfy_train.mean().item(),
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.local
    @pytest.mark.snapshot
    def test_rf_regressor_vaeac_basic(self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with vaeac approach - basic case with short runtime."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='vaeac',
            phi0=dfy_train.mean().item(),
            max_n_coalitions=20,
            extra_computation_args={'vaeac.epochs': 10, 'vaeac.width': 16, 'vaeac.depth': 2},
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot
