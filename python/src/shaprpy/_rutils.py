from __future__ import annotations

from pathlib import Path
from typing import Optional, Sequence


def get_non_empty_libpaths(robjects_module) -> Optional[list[str]]:
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


def get_package_lib_loc(robjects_module, package: str) -> Optional[str]:
    """Find a library path containing the given package, or return a best-effort fallback."""
    lib_paths = get_non_empty_libpaths(robjects_module)
    if not lib_paths:
        return None

    for lib_path in lib_paths:
        pkg_dir = Path(lib_path) / package
        if pkg_dir.exists():
            return lib_path

    return lib_paths[0]
