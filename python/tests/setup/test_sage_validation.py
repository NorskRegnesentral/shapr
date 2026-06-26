"""
Unit tests for SAGE-specific input validation in pyshapr.

These tests ensure that the explain function properly validates the SAGE-related
inputs (`y_explain` and `sage_args`) and raises appropriate errors.
"""

import numpy as np
import pytest

from pyshapr import explain


class TestSageValidation:
    """Unit tests for SAGE input validation."""

    def test_sage_requires_y_explain(self, california_housing_data, trained_rf_regressor):
        """Test that `sage=True` without `y_explain` raises an error."""
        dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_explain,
                approach="independence",
                phi0=dfy_train.mean().item(),
                sage=True,
                seed=1,
            )

    def test_sage_y_explain_wrong_length(self, california_housing_data, trained_rf_regressor):
        """Test that a `y_explain` of the wrong length raises an error."""
        dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

        y_explain_bad = dfy_explain.values.flatten()[:-1]  # one element too few

        with pytest.raises((ValueError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_explain,
                approach="independence",
                phi0=dfy_train.mean().item(),
                sage=True,
                y_explain=y_explain_bad,
                seed=1,
            )

    def test_sage_invalid_loss_func(self, california_housing_data, trained_rf_regressor):
        """Test that a `loss_func` with the wrong number of arguments raises an error."""
        dfx_train, dfx_explain, dfy_train, dfy_explain = california_housing_data

        def bad_loss(y):  # only one argument; R requires exactly two
            return float(np.mean(np.asarray(y)))

        with pytest.raises((ValueError, TypeError, Exception)):
            explain(
                model=trained_rf_regressor,
                x_train=dfx_train,
                x_explain=dfx_explain,
                approach="independence",
                phi0=dfy_train.mean().item(),
                sage=True,
                y_explain=dfy_explain.values.flatten(),
                sage_args={"loss_func": bad_loss},
                seed=1,
            )
