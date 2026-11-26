"""
Tests for the Shapr explanation class methods.

These tests verify that the Shapr class methods work correctly and
produce consistent output.
"""

import numpy as np
import pandas as pd
import pytest

from pyshapr import explain
from pyshapr.explanation import Shapr


@pytest.fixture(scope="class")
def explanation_object(california_housing_data, trained_rf_regressor):
    """Create a single Shapr explanation object to test all methods."""
    dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

    explanation = explain(
        model=trained_rf_regressor,
        x_train=dfx_train,
        x_explain=dfx_explain.iloc[:3],  # Use only 3 observations for faster tests
        approach="empirical",
        phi0=dfy_train.mean().item(),
        max_n_coalitions=50,
        seed=1,
    )
    return explanation


@pytest.fixture(scope="class")
def grouped_explanation(california_housing_data, trained_rf_regressor):
    dfx_train, dfx_explain, dfy_train, _ = california_housing_data
    group = {
        "MedInc": ["MedInc", "HouseAge", "AveRooms"],
        "B": ["AveBedrms", "Population", "AveOccup"],
        "C": ["Latitude", "Longitude"],
    }
    explanation = explain(
        model=trained_rf_regressor,
        x_train=dfx_train,
        x_explain=dfx_explain.iloc[:3],
        approach="independence",
        phi0=dfy_train.mean().item(),
        group=group,
        seed=1,
    )
    return explanation, group, dfx_explain.iloc[:3]


class TestShaprClassMethods:
    """Tests for Shapr class methods and functionality."""

    @pytest.mark.snapshot
    def test_str_representation(self, explanation_object, snapshot):
        """Test __str__ method returns correct string representation."""
        result = str(explanation_object)
        assert result == snapshot

    @pytest.mark.snapshot
    def test_repr_representation(self, explanation_object, snapshot):
        """Test __repr__ method returns correct representation."""
        result = repr(explanation_object)
        assert result == snapshot

    @pytest.mark.snapshot
    def test_print_shapley_est(self, explanation_object, snapshot, capsys):
        """Test print() method with default shapley_est output."""
        explanation_object.print()
        captured = capsys.readouterr()
        assert captured.out == snapshot

    @pytest.mark.snapshot
    def test_print_shapley_sd(self, explanation_object, snapshot, capsys):
        """Test print() method with shapley_sd output."""
        explanation_object.print(what="shapley_sd")
        captured = capsys.readouterr()
        assert captured.out == snapshot

    @pytest.mark.snapshot
    def test_print_msev(self, explanation_object, snapshot, capsys):
        """Test print() method with MSEv output."""
        explanation_object.print(what="MSEv")
        captured = capsys.readouterr()
        assert captured.out == snapshot

    @pytest.mark.snapshot
    def test_print_timing_summary(self, explanation_object, snapshot, capsys):
        """Test print() method with timing_summary output."""
        explanation_object.print(what="timing_summary")
        captured = capsys.readouterr()
        # Timing values vary, so we just check that output was produced
        assert len(captured.out) > 0
        assert "timing" in captured.out.lower() or "time" in captured.out.lower()

    def test_get_explanation_dict(self, explanation_object):
        """Test get_explanation_dict() returns a dictionary with expected keys."""
        exp_dict = explanation_object.get_explanation_dict()

        assert isinstance(exp_dict, dict)
        expected_keys = [
            "shapley_values_est",
            "shapley_values_sd",
            "pred_explain",
            "MSEv",
            "internal",
            "timing",
        ]
        for key in expected_keys:
            assert key in exp_dict, f"Expected key '{key}' not found in explanation dict"

    def test_get_r_object(self, explanation_object):
        """Test get_r_object() returns an R object."""
        r_obj = explanation_object.get_r_object()
        assert r_obj is not None
        # Check it's an R object by checking for typical R object attributes
        assert hasattr(r_obj, "rx2")

    def test_get_results_default(self, explanation_object):
        """Test get_results() with no arguments returns all components."""
        results = explanation_object.get_results()
        assert isinstance(results, dict)

    @pytest.mark.snapshot
    def test_get_results_single_component(self, explanation_object, snapshot):
        """Test get_results() with single component request."""
        shapley_est = explanation_object.get_results("shapley_est")

        assert isinstance(shapley_est, pd.DataFrame)
        result = shapley_est.round(6).to_markdown()
        assert result == snapshot

    def test_get_results_multiple_components(self, explanation_object):
        """Test get_results() with multiple component requests."""
        results = explanation_object.get_results(["shapley_est", "shapley_sd", "MSEv"])

        assert isinstance(results, dict)
        assert "shapley_est" in results
        assert "shapley_sd" in results
        assert "MSEv" in results

    def test_summary(self, explanation_object):
        """Test summary() method returns ShaprSummary object."""
        from pyshapr.explanation import ShaprSummary

        result = explanation_object.summary(digits=3)

        # Should return a ShaprSummary object
        assert isinstance(result, ShaprSummary)

        # Check string representation
        summary_str = str(result)
        assert len(summary_str) > 0
        assert "explain" in summary_str.lower() or "shapley" in summary_str.lower()

        # Check keys() method works
        keys = result.keys()
        assert len(list(keys)) > 0

        # Check bracket access works
        assert "approach" in keys
        approach = result["approach"]
        assert approach is not None

    def test_to_shap_all_observations(self, explanation_object):
        """Test to_shap() conversion for all observations."""
        try:
            import shap
        except ImportError:
            pytest.skip("SHAP library not installed")

        shap_exp = explanation_object.to_shap()

        # Check it's a SHAP Explanation object
        assert isinstance(shap_exp, shap.Explanation)
        assert shap_exp.values is not None
        assert shap_exp.base_values is not None
        assert shap_exp.data is not None
        assert len(shap_exp.values) == 3  # We used 3 observations
        assert shap_exp.values.shape[1] == 8  # 8 features
        np.testing.assert_array_equal(
            explanation_object.to_shap(include_group_feature_means=True).data, shap_exp.data
        )

    def test_to_shap_single_observation(self, explanation_object):
        """Test to_shap() conversion for single observation."""
        try:
            import shap
        except ImportError:
            pytest.skip("SHAP library not installed")

        shap_exp = explanation_object.to_shap(idx=0)

        # Check it's a SHAP Explanation object
        assert isinstance(shap_exp, shap.Explanation)
        assert shap_exp.values.shape == (1, 8)  # 1 observation, 8 features

    def test_to_shap_slice(self, explanation_object):
        """Test to_shap() conversion for slice of observations."""
        try:
            import shap
        except ImportError:
            pytest.skip("SHAP library not installed")

        shap_exp = explanation_object.to_shap(idx=slice(0, 2))

        # Check it's a SHAP Explanation object
        assert isinstance(shap_exp, shap.Explanation)
        assert shap_exp.values.shape == (2, 8)  # 2 observations, 8 features

    def test_to_shap_grouped(self, grouped_explanation):
        import matplotlib.pyplot as plt
        import shap

        explanation, group, x_explain = grouped_explanation
        result = explanation.get_explanation_dict()["shapley_values_est"]
        shap_exp = explanation.to_shap()
        assert shap_exp.data is None
        assert shap_exp.feature_names == list(group)
        np.testing.assert_allclose(shap_exp.values, result[list(group)].to_numpy())
        np.testing.assert_allclose(shap_exp.base_values, result["none"].to_numpy())

        shap.plots.waterfall(shap_exp[0], show=False)
        plt.close()
        shap.plots.waterfall(explanation.to_shap(idx=1)[0], show=False)
        plt.close()
        shap.plots.beeswarm(shap_exp, show=False)
        plt.close()
        shap.plots.beeswarm(explanation.to_shap(include_group_feature_means=True), show=False)
        plt.close()

        expected_means = np.column_stack(
            [x_explain[columns].mean(axis=1).to_numpy() for columns in group.values()]
        )
        for idx, expected_rows in [
            (None, slice(None)),
            (1, slice(1, 2)),
            (slice(1, 3), slice(1, 3)),
        ]:
            with_means = explanation.to_shap(idx=idx, include_group_feature_means=True)
            np.testing.assert_allclose(with_means.data, expected_means[expected_rows])
            np.testing.assert_allclose(
                with_means.values, result[list(group)].to_numpy()[expected_rows]
            )
            np.testing.assert_allclose(
                with_means.base_values, result["none"].to_numpy()[expected_rows]
            )
            assert explanation.to_shap(idx=idx).data is None

        internal = explanation.get_explanation_dict()["internal"]
        missing_data = internal["data"]["x_explain"].copy()
        missing_data.loc[missing_data.index[0], group["MedInc"][0]] = np.nan
        missing_data.loc[missing_data.index[1], group["MedInc"]] = np.nan
        missing = Shapr(
            {
                "shapley_values_est": result,
                "internal": {**internal, "data": {**internal["data"], "x_explain": missing_data}},
            },
            None,
        )
        means = missing.to_shap(include_group_feature_means=True).data
        np.testing.assert_allclose(
            means[:, 0], missing_data[group["MedInc"]].mean(axis=1), equal_nan=True
        )

    def test_to_shap_group_means_reject_non_numeric(self, grouped_explanation):
        explanation, _, _ = grouped_explanation
        result = explanation.get_explanation_dict()
        internal = result["internal"]
        mixed_data = internal["data"]["x_explain"].copy()
        mixed_data["MedInc"] = pd.Categorical(mixed_data["MedInc"])
        mixed = Shapr(
            {
                "shapley_values_est": result["shapley_values_est"],
                "internal": {
                    "parameters": internal["parameters"],
                    "data": {"x_explain": mixed_data},
                    "objects": {"feature_specs": {"classes": ["numeric", "factor"]}},
                },
            },
            None,
        )
        assert mixed.to_shap().data is None
        with pytest.raises(ValueError, match="non-numeric"):
            mixed.to_shap(include_group_feature_means=True)
        with pytest.raises(TypeError, match="must be a bool"):
            mixed.to_shap(include_group_feature_means="yes")
