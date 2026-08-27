from __future__ import annotations

import re
import warnings
from collections.abc import Sequence
from importlib.metadata import PackageNotFoundError, version
from pathlib import Path
from typing import Any

RECENT_FEATURES_REQUIRE_NEWER_THAN = "1.0.8"
SHAPR_VERSION_USED_FOR_DEVELOPMENT = "1.0.8.9005"
_checked_shapr_versions: set[str] = set()


class ShaprVersionWarning(UserWarning):
    """Warning raised when the installed shapr version lacks pyshapr functionality."""


class ShaprVersionError(RuntimeError):
    """Error raised when shapr cannot safely provide requested pyshapr functionality."""


def _version_tuple(package_version: str) -> tuple[int, ...]:
    return tuple(int(part) for part in re.split(r"[.-]", package_version))


def _warn_if_shapr_version_lacks_full_support(shapr_package: Any) -> None:
    installed_version = str(shapr_package.__version__)
    if _version_tuple(installed_version) > _version_tuple(RECENT_FEATURES_REQUIRE_NEWER_THAN):
        return

    try:
        pyshapr_version = version("pyshapr")
    except PackageNotFoundError:
        pyshapr_version = "development version"

    warnings.warn(
        f"pyshapr {pyshapr_version} was developed with shapr "
        f"{SHAPR_VERSION_USED_FOR_DEVELOPMENT}, but shapr {installed_version} is installed. "
        f"ARF and SAGE require a shapr version newer than "
        f"{RECENT_FEATURES_REQUIRE_NEWER_THAN} and are unavailable; other "
        "functionality may still work. "
        "Update shapr from R with pak::pak('NorskRegnesentral/shapr').",
        ShaprVersionWarning,
        stacklevel=3,
    )


def _check_shapr_feature_support(shapr_package: Any, scope: str) -> None:
    if scope != "global" or "scope" in shapr_package.setup.formals().names:
        return

    installed_version = str(shapr_package.__version__)
    raise ShaprVersionError(
        f"shapr {installed_version} does not support SAGE (`scope='global'`). Without this "
        "check, it can return local SHAP values instead of SAGE values. Update shapr from R with "
        "pak::pak('NorskRegnesentral/shapr')."
    )


def get_non_empty_libpaths(robjects_module) -> list[str] | None:
    """Return a list of non-empty R library paths for use with importr(lib_loc=...)."""
    try:
        lib_paths: Sequence[str] = [str(p) for p in robjects_module.r(".libPaths()")]
    except Exception:
        return None

    non_empty: list[str] = []

    for lib_path in lib_paths:
        path = Path(lib_path)

        if not path.exists() or not path.is_dir():
            continue

        try:
            next(path.iterdir())
        except StopIteration:
            continue
        except OSError:
            non_empty.append(lib_path)
        else:
            non_empty.append(lib_path)

    return non_empty or None


def get_package_lib_loc(robjects_module, package: str) -> str | None:
    """Find a library path containing the given package, or return a best-effort fallback."""
    lib_paths = get_non_empty_libpaths(robjects_module)
    if not lib_paths:
        return None

    for lib_path in lib_paths:
        pkg_dir = Path(lib_path) / package
        if pkg_dir.exists():
            return lib_path

    return lib_paths[0]


def _importr(package: str, robjects_module=None, importr_func=None):
    if robjects_module is None:
        import rpy2.robjects as robjects_module

    if importr_func is None:
        from rpy2.robjects.packages import importr as importr_func

    lib_loc = get_package_lib_loc(robjects_module, package)
    imported_package = importr_func(package, lib_loc=lib_loc) if lib_loc else importr_func(package)

    if package == "shapr":
        installed_version = str(imported_package.__version__)
        if installed_version not in _checked_shapr_versions:
            _warn_if_shapr_version_lacks_full_support(imported_package)
            _checked_shapr_versions.add(installed_version)

    return imported_package
