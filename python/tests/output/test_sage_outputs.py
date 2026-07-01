"""
Output tests for SAGE (Shapley Additive Global importancE) values using Syrupy.

These tests ensure that the SAGE shapley_values_est and shapley_values_sd outputs
remain consistent across changes to the codebase, and that a custom Python
global_loss_func can be bridged into the R computation.
"""

import numpy as np
import pandas as pd
import pytest

from pyshapr import explain


@pytest.fixture(scope="module")
def sage_explanation_object(california_housing_data, trained_rf_regressor):
    """Create a single SAGE explanation object to test the SAGE-specific accessors."""
    dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

    explanation = explain(
        model=trained_rf_regressor,
        x_train=dfx_train,
        x_explain=dfx_explain,
        approach="independence",
        phi0=dfy_train.mean().item(),
        scope="global",
        y_explain=dfy_explain.values.flatten(),
        seed=1,
    )
    return explanation


class TestSageOutputs:
    """Output tests for SAGE value explanations."""

    @pytest.mark.snapshot
    def test_sage_regressor_default_loss(
        self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot
    ):
        """Test SAGE values for a regressor with the default (MSE) loss."""
        dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_explain,
            approach="independence",
            phi0=dfy_train.mean().item(),
            scope="global",
            y_explain=dfy_explain.values.flatten(),
            seed=1,
        )

        result = extract_shapley_outputs(explanation)

        assert result == snapshot

    @pytest.mark.snapshot
    def test_sage_regressor_custom_loss(
        self, california_housing_data, trained_rf_regressor, extract_shapley_outputs, snapshot
    ):
        """Test SAGE values for a regressor with a custom Python loss_func bridged into R."""
        dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

        def mse_loss(y, pred):
            return float(np.mean((np.asarray(y) - np.asarray(pred)) ** 2))

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_explain,
            approach="independence",
            phi0=dfy_train.mean().item(),
            scope="global",
            y_explain=dfy_explain.values.flatten(),
            extra_computation_args={"global_loss_func": mse_loss},
            seed=1,
        )

        result = extract_shapley_outputs(explanation)

        assert result == snapshot

    @pytest.mark.snapshot
    def test_sage_regressor_grouped_default_loss(
        self, california_housing_data, trained_rf_regressor, regression_group_config, extract_shapley_outputs, snapshot
    ):
        """Test group-wise SAGE values for a regressor with the default (MSE) loss."""
        dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_explain,
            approach="independence",
            phi0=dfy_train.mean().item(),
            group=regression_group_config,
            scope="global",
            y_explain=dfy_explain.values.flatten(),
            seed=1,
        )

        result = extract_shapley_outputs(explanation)

        assert result == snapshot

    @pytest.mark.snapshot
    def test_sage_classifier_default_loss(
        self, binary_iris_data, trained_rf_classifier, extract_shapley_outputs, snapshot
    ):
        """Test SAGE values for a binary classifier with the default (logistic) loss."""
        dfx_train, dfx_explain, dfy_train, dfy_explain = binary_iris_data

        explanation = explain(
            model=trained_rf_classifier,
            x_train=dfx_train,
            x_explain=dfx_explain,
            approach="independence",
            phi0=dfy_train.mean().item(),
            scope="global",
            y_explain=dfy_explain.values.flatten(),
            seed=1,
        )

        result = extract_shapley_outputs(explanation)

        assert result == snapshot


class TestSageClassMethods:
    """Tests for the SAGE-specific Shapr accessors and conversions."""

    def test_get_shap_values_est_returns_per_observation_values(self, sage_explanation_object):
        """get_shap_values_est() returns the per-observation Shapley values for a SAGE explanation."""
        shap_values_est = sage_explanation_object.get_shap_values_est()

        assert isinstance(shap_values_est, pd.DataFrame)
        # One row per explained observation (5 California housing rows in the fixture).
        assert len(shap_values_est) == 5

    def test_get_shap_values_est_requires_sage(self, california_housing_data, trained_rf_regressor):
        """get_shap_values_est() raises for a non-SAGE explanation."""
        dfx_train, dfx_explain, dfy_train, _ = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_explain.iloc[:3],
            approach="independence",
            phi0=dfy_train.mean().item(),
            seed=1,
        )

        with pytest.raises(ValueError):
            explanation.get_shap_values_est()

    def test_to_shap_returns_single_global_explanation(self, sage_explanation_object):
        """to_shap() returns a single-row global explanation for a SAGE explanation."""
        try:
            import shap
        except ImportError:
            pytest.skip("SHAP library not installed")

        shap_exp = sage_explanation_object.to_shap()

        assert isinstance(shap_exp, shap.Explanation)
        # SAGE yields a single global explanation row across the 8 features.
        assert shap_exp.values.shape == (1, 8)
        assert shap_exp.data is None
