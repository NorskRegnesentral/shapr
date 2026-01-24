"""
Unit tests for input validation and error handling in shaprpy.

These tests ensure that the explain function properly validates inputs
and raises appropriate errors for invalid configurations.
"""
import numpy as np
import pandas as pd
import pytest

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

    def test_nan_in_x_train_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that NaN values in x_train raise appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        # Inject NaN values into training data
        dfx_train_with_nan = dfx_train.copy()
        dfx_train_with_nan.iloc[0, 0] = np.nan

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train_with_nan,
                x_explain=dfx_test,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                seed=1
            )

    def test_nan_in_x_explain_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that NaN values in x_explain raise appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        # Inject NaN values into explain data
        dfx_test_with_nan = dfx_test.copy()
        dfx_test_with_nan.iloc[0, 0] = np.nan

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test_with_nan,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                seed=1
            )

    def test_mismatched_feature_dimensions_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that different number of features between train and explain raises error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        # Drop a column from x_explain
        dfx_test_fewer_cols = dfx_test.iloc[:, :-1]

        with pytest.raises((ValueError, KeyError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test_fewer_cols,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                seed=1
            )

    def test_invalid_max_n_coalitions_negative_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that negative max_n_coalitions raises appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                max_n_coalitions=-10,
                seed=1
            )

    def test_invalid_max_n_coalitions_zero_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that zero max_n_coalitions raises appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                max_n_coalitions=0,
                seed=1
            )

    def test_invalid_group_specification_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that invalid group specification raises appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        # Create invalid group specification (e.g., group index out of bounds)
        invalid_group = [1, 1, 2, 2, 2, 2, 2, 99]  # 99 is invalid

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                group=invalid_group,
                seed=1
            )

    def test_invalid_group_wrong_length_raises_error(self, california_housing_data, trained_rf_regressor):
        """Test that group specification with wrong length raises appropriate error."""
        dfx_train, dfx_test, dfy_train, dfy_test = california_housing_data

        # Create group specification with wrong length
        invalid_group = [1, 1, 2]  # Should match number of features

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_test,
                approach='empirical',
                phi0=dfy_train.mean().item(),
                group=invalid_group,
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
