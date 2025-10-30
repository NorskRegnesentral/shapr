"""
Unit tests for input validation and error handling in shaprpy.

These tests ensure that the explain function properly validates inputs
and raises appropriate errors for invalid configurations.
"""
import pytest
import numpy as np
import pandas as pd
from shaprpy import explain


class TestInputValidation:
    """Unit tests for input validation."""

    def test_invalid_approach_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that invalid approach parameter raises appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        with pytest.raises((ValueError, Exception)):  # Using Exception for now, can be more specific later
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test,
                approach='invalid_approach',
                phi0=dfy_train.mean().item(),
                seed=1
            )

    def test_mismatched_feature_names_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that mismatched feature names between train and explain raise error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        # Create test data with different column names
        dfx_test_bad = dfx_test.copy()
        dfx_test_bad.columns = ['BadCol' + str(i) for i in range(len(dfx_test_bad.columns))]

        with pytest.raises((ValueError, KeyError, Exception)):  # Can be more specific later
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test_bad,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                seed=1
            )

    def test_empty_explain_data_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that empty x_explain data raises appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        # Create empty explain data
        dfx_test_empty = dfx_test.iloc[0:0]  # Empty DataFrame with same columns

        with pytest.raises((ValueError, Exception)):  # Can be more specific later
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test_empty,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                seed=1
            )


class TestBasicFunctionality:
    """Basic functionality tests to ensure the library works as expected."""

    def test_explain_returns_expected_keys(self, california_housing_data, trained_rf_regressor):
        """Test that explain function returns expected dictionary keys."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        explanation = explain(
            model=trained_rf_regressor,
            x_train=dfx_train,
            x_explain=dfx_test.iloc[:1],  # Just one row for speed
            approach='independence',  # Fastest approach
            phi0=dfy_train.mean().item(),
            seed=1
        )

        # Check that the explanation contains expected keys
        expected_keys = ['shapley_est', 'shapley_sd']

        res = explanation.get_results(expected_keys)

        for key in expected_keys:
            assert key in res, f"Expected key '{key}' not found in explanation"

        # Check that shapley values are DataFrames with expected structure
        assert isinstance(res['shapley_est'], pd.DataFrame)
        assert isinstance(res['shapley_sd'], pd.DataFrame)

        # Check that we have the right number of rows (should match x_explain)
        assert len(res['shapley_est']) == 1
        assert len(res['shapley_sd']) == 1
