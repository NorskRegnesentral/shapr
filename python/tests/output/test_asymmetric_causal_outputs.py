"""
Output tests for causal ordering and asymmetric Shapley values.

These tests ensure that Shapley value computations with causal ordering
produce consistent results across different approaches, with and without
asymmetric mode and confounding.
"""
import pytest

from shaprpy import explain


class TestCausalOrderingOutputs:
    """Output tests for causal ordering with various configurations."""

    @pytest.mark.snapshot
    def test_rf_regressor_gaussian_causal_symmetric(self, california_housing_data, trained_rf_regressor,
                                                     causal_ordering_config, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with gaussian approach - symmetric with causal ordering."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='gaussian',
            phi0=dfy_train.mean().item(),
            asymmetric=False,
            causal_ordering=causal_ordering_config,
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_gaussian_causal_asymmetric(self, california_housing_data, trained_rf_regressor,
                                                      causal_ordering_config, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with gaussian approach - asymmetric with causal ordering, no confounding."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='gaussian',
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
    def test_rf_regressor_copula_causal_asymmetric_confounding(self, california_housing_data, trained_rf_regressor,
                                                                causal_ordering_config, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with copula approach - asymmetric with causal ordering and confounding."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='copula',
            phi0=dfy_train.mean().item(),
            asymmetric=True,
            causal_ordering=causal_ordering_config,
            confounding=[True, False, True],  # Confounding for components A, B, C
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot

    @pytest.mark.snapshot
    def test_rf_regressor_ctree_causal_symmetric(self, california_housing_data, trained_rf_regressor,
                                                  causal_ordering_config, extract_shapley_outputs, snapshot):
        """Test RandomForest regressor with ctree approach - symmetric with causal ordering."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test,
            approach='ctree',
            phi0=dfy_train.mean().item(),
            asymmetric=False,
            causal_ordering=causal_ordering_config,
            max_n_coalitions=50,
            seed=1
        )

        result = extract_shapley_outputs(explanation)

        # Use syrupy for snapshot testing
        assert result == snapshot
