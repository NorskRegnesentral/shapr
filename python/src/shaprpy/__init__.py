def _check_r_env():
    try:
        import rpy2.robjects as ro
        from rpy2.robjects.packages import importr
    except Exception as e:
        raise ImportError(
            "shaprpy requires rpy2 and a working R installation.\n"
            "Install R and ensure R_HOME or PATH is set. See README."
        ) from e

    try:
        importr("shapr")
    except Exception as e:
        raise ImportError(
            "The R package 'shapr' is not installed. In R, run:\n"
            "    install.packages('shapr')"
        ) from e

# Call on first use (recommended) rather than at import if import speed matters.

_check_r_env()

from .explain import explain
from . import datasets
from importlib.metadata import version, PackageNotFoundError

__all__ = ["explain", "datasets"]

try:  # Prefer installed package metadata
    __version__ = version("shaprpy")
except PackageNotFoundError:  # Fallback for editable/source tree before build/install
    __version__ = "0.0.0+local"

