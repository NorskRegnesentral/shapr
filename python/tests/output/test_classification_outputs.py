"""
Output tests for classification models using Syrupy.

These tests ensure that the shapley_values_est and shapley_values_sd outputs
remain consistent across changes to the codebase.
"""
import pytest
import pandas as pd
from shaprpy import explain


class TestClassificationOutputs:
    """Output tests for classification model explanations."""

    @pytest.mark.snapshot
    def test_rf_classifier_empirical_basic(self, binary_iris_data, trained_rf_classifier, extract_shapley_outputs, snapshot):
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

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_classifier_independence_basic(self, binary_iris_data, trained_rf_classifier, extract_shapley_outputs, snapshot):
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

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_classifier_gaussian_basic(self, binary_iris_data, trained_rf_classifier, extract_shapley_outputs, snapshot):
        """Test RandomForest classifier with gaussian approach - basic case."""
        dfx_train, dfx_test, dfy_train, dfy_test = binary_iris_data

        explanation = explain(
            model=trained_rf_classifier,
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
