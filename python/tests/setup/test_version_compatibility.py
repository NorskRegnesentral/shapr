"""Tests for pyshapr and shapr version compatibility checks."""

import warnings
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

import pyshapr
from pyshapr import _rutils


@pytest.mark.parametrize("installed_version", ["1.0.5", "1.0.8"])
def test_shapr_version_without_full_support_warns(monkeypatch, installed_version):
    """A limited shapr installation reports unavailable functionality."""
    monkeypatch.setattr(_rutils, "version", lambda package: "0.5.1")
    shapr_package = SimpleNamespace(__version__=installed_version)

    expected_message = (
        "pyshapr 0.5.1 was developed with shapr 1.0.8.9005, but shapr "
        f"{installed_version} is installed. ARF and SAGE require a shapr version newer than "
        "1.0.8 and are unavailable; other functionality may still work. "
        "Update shapr from R with "
        "pak::pak('NorskRegnesentral/shapr')."
    )
    with pytest.warns(_rutils.ShaprVersionWarning) as recorded_warnings:
        _rutils._warn_if_shapr_version_lacks_full_support(shapr_package)

    assert str(recorded_warnings[0].message) == expected_message


@pytest.mark.parametrize("installed_version", ["1.0.8.9000", "1.0.8-9001", "1.0.9"])
def test_newer_shapr_version_does_not_warn(installed_version):
    """Development and later release versions load without a general warning."""
    shapr_package = SimpleNamespace(__version__=installed_version)

    with warnings.catch_warnings(record=True) as recorded_warnings:
        _rutils._warn_if_shapr_version_lacks_full_support(shapr_package)

    assert not recorded_warnings


def test_sage_requires_supported_shapr_capability():
    """SAGE fails before an old backend can silently return local SHAP values."""
    setup = Mock()
    setup.formals.return_value = SimpleNamespace(names=["x_train", "x_explain", "..."])
    shapr_package = SimpleNamespace(__version__="1.0.8", setup=setup)

    _rutils._check_shapr_feature_support(shapr_package, scope="local")

    with pytest.raises(_rutils.ShaprVersionError, match="does not support SAGE"):
        _rutils._check_shapr_feature_support(shapr_package, scope="global")


@pytest.mark.parametrize("scope", ["local", "global"])
def test_supported_shapr_capability_accepts_all_scopes(scope):
    """A SAGE-capable backend accepts local and global explanations."""
    setup = Mock()
    setup.formals.return_value = SimpleNamespace(names=["x_train", "scope", "..."])
    shapr_package = SimpleNamespace(__version__="development", setup=setup)

    _rutils._check_shapr_feature_support(shapr_package, scope=scope)


def test_importr_checks_only_shapr(monkeypatch):
    """The shared importer applies the compatibility check only to shapr."""
    imported_package = SimpleNamespace(__version__="1.0.8.9001")
    importr = Mock(return_value=imported_package)
    version_warning = Mock()
    robjects = SimpleNamespace(r=lambda expression: [])
    monkeypatch.setattr(_rutils, "_warn_if_shapr_version_lacks_full_support", version_warning)
    monkeypatch.setattr(_rutils, "_checked_shapr_versions", set())

    assert _rutils._importr("shapr", robjects, importr) is imported_package
    version_warning.assert_called_once_with(imported_package)

    version_warning.reset_mock()
    assert _rutils._importr("shapr", robjects, importr) is imported_package
    version_warning.assert_not_called()

    assert _rutils._importr("utils", robjects, importr) is imported_package
    version_warning.assert_not_called()


def test_accessing_explain_initializes_r(monkeypatch):
    """Importing the public explain attribute initializes the R backend."""
    explain_impl = Mock()
    ensure_r_ready = Mock()
    monkeypatch.setattr(pyshapr, "_explain_impl", explain_impl)
    monkeypatch.setattr(pyshapr, "ensure_r_ready", ensure_r_ready)

    assert pyshapr.__getattr__("explain") is explain_impl
    ensure_r_ready.assert_called_once_with()
