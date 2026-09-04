"""Tests for pyshapr and shapr version compatibility checks."""

import subprocess
import sys
import tomllib
import warnings
from importlib.resources import files
from pathlib import Path
from types import SimpleNamespace
from unittest.mock import Mock

import pytest

import pyshapr
from pyshapr import _rutils

PYTHON_ROOT = Path(__file__).resolve().parents[2]


@pytest.mark.parametrize("installed_version", ["1.0.5", "1.0.8"])
def test_shapr_version_without_full_support_warns(monkeypatch, installed_version):
    """A limited shapr installation reports unavailable functionality."""
    monkeypatch.setattr(_rutils, "version", lambda package: "0.5.1")
    shapr_package = SimpleNamespace(__version__=installed_version)

    expected_message = (
        "pyshapr 0.5.1 was developed with shapr 1.1.0, but shapr "
        f"{installed_version} is installed. ARF and SAGE require a shapr version newer than "
        "1.0.8 and are unavailable; other functionality may still work. "
        "Update shapr from R with install.packages('shapr')."
    )
    with pytest.warns(_rutils.ShaprVersionWarning) as recorded_warnings:
        _rutils._warn_if_shapr_version_lacks_full_support(shapr_package)

    assert str(recorded_warnings[0].message) == expected_message


@pytest.mark.parametrize("installed_version", ["1.0.8.9000", "1.0.8-9001", "1.0.9", "1.1.0"])
def test_newer_shapr_version_does_not_warn(installed_version):
    """Development and later release versions load without a general warning."""
    shapr_package = SimpleNamespace(__version__=installed_version)

    with warnings.catch_warnings(record=True) as recorded_warnings:
        _rutils._warn_if_shapr_version_lacks_full_support(shapr_package)

    assert not recorded_warnings


def test_compatibility_policy_is_available_as_package_data():
    """The installed package exposes the policy used by runtime checks."""
    policy_resource = files("pyshapr").joinpath("compatibility.toml")

    with policy_resource.open("rb") as policy_file:
        policy = tomllib.load(policy_file)

    assert policy["schema_version"] == 1
    assert policy["shapr"]["developed_with"] == _rutils.SHAPR_VERSION_USED_FOR_DEVELOPMENT
    assert (
        policy["shapr"]["recent_features_require_newer_than"]
        == _rutils.RECENT_FEATURES_REQUIRE_NEWER_THAN
    )


def test_compatibility_policy_is_in_distribution_configuration():
    """Wheel and source distribution configuration include the policy."""
    with (PYTHON_ROOT / "pyproject.toml").open("rb") as pyproject_file:
        pyproject = tomllib.load(pyproject_file)

    package_data = pyproject["tool"]["setuptools"]["package-data"]["pyshapr"]
    manifest = (PYTHON_ROOT / "MANIFEST.in").read_text(encoding="utf-8")

    assert "compatibility.toml" in package_data
    assert "recursive-include src/pyshapr *.toml" in manifest


def test_readme_compatibility_table_is_current():
    """The committed README table matches the packaged compatibility policy."""
    result = subprocess.run(
        [sys.executable, "scripts/update_compatibility_table.py", "--check"],
        cwd=PYTHON_ROOT,
        check=False,
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stderr


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


def test_calling_explain_initializes_r(monkeypatch):
    """Calling the public explain proxy initializes R and delegates the call."""
    explain_impl = Mock()
    ensure_r_ready = Mock()
    monkeypatch.setattr(pyshapr, "_explain_impl", explain_impl)
    monkeypatch.setattr(pyshapr, "ensure_r_ready", ensure_r_ready)

    pyshapr.explain("model", scope="local")

    ensure_r_ready.assert_called_once_with()
    explain_impl.assert_called_once_with("model", scope="local")


def test_importing_explain_does_not_initialize_r(monkeypatch):
    """Importing the public explain proxy remains independent of R."""
    ensure_r_ready = Mock()
    monkeypatch.setattr(pyshapr, "ensure_r_ready", ensure_r_ready)

    from pyshapr import explain

    assert explain is pyshapr.explain
    ensure_r_ready.assert_not_called()
