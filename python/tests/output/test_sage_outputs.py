"""
Output tests for SAGE (Shapley Additive Global importancE) values using Syrupy.

These tests ensure that the SAGE shapley_values_est and shapley_values_sd outputs
remain consistent across changes to the codebase, and that a custom Python
loss_func can be bridged into the R computation.
"""

import numpy as np
import pytest

from pyshapr import explain


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
            sage=True,
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
            sage=True,
            y_explain=dfy_explain.values.flatten(),
            sage_args={"loss_func": mse_loss},
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
            sage=True,
            y_explain=dfy_explain.values.flatten(),
            seed=1,
        )

        result = extract_shapley_outputs(explanation)

        assert result == snapshot
