"""
Tests for the Shapr explanation class methods.

These tests verify that the Shapr class methods work correctly and
produce consistent output.
"""
import pytest
import pandas as pd
from shaprpy import explain


class TestShaprClassMethods:
    """Tests for Shapr class methods and functionality."""

    @pytest.fixture(scope="class")
    def explanation_object(self, california_housing_data, trained_rf_regressor):
        """Create a single Shapr explanation object to test all methods."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test.iloc[:3],  # Use only 3 observations for faster tests
            approach='empirical',
            phi0=dfy_train.mean().item(),
            max_n_coalitions=50,
            seed=1
        )
        return explanation

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
        expected_keys = ['shapley_values_est', 'shapley_values_sd', 'pred_explain',
                        'MSEv', 'internal', 'timing']
        for key in expected_keys:
            assert key in exp_dict, f"Expected key '{key}' not found in explanation dict"

    def test_get_r_object(self, explanation_object):
        """Test get_r_object() returns an R object."""
        r_obj = explanation_object.get_r_object()
        assert r_obj is not None
        # Check it's an R object by checking for typical R object attributes
        assert hasattr(r_obj, 'rx2')

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
        from shaprpy.explanation import ShaprSummary

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
