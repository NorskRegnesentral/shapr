from __future__ import annotations

from collections.abc import Sequence
from pathlib import Path


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
    if lib_loc:
        return importr_func(package, lib_loc=lib_loc)
    return importr_func(package)
