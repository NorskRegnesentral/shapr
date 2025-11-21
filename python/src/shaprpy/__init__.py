from importlib import import_module
from importlib.metadata import PackageNotFoundError, version

# Lightweight public re-export (no R dependency)
from . import datasets
from ._rutils import get_package_lib_loc

__all__ = ["Shapr", "datasets", "ensure_r_ready", "explain"]

try:
    __version__ = version("shaprpy")
except PackageNotFoundError:
    __version__ = "0.0.0+local"

_r_ready = False
_explain_impl = None


def ensure_r_ready() -> bool:
    """Ensure rpy2 and the R package 'shapr' are available, then bind the real explain() (idempotent)."""
    global _r_ready, _explain_impl
    if _r_ready:
        return True

    try:
        import rpy2.robjects as _ro
        from rpy2.robjects.packages import importr
    except Exception as e:
        raise ImportError(
            "shaprpy requires rpy2 and a working R installation.\n"
            "Install R and rpy2, and ensure R is on PATH/R_HOME. See README."
        ) from e

    try:
        lib_loc = get_package_lib_loc(_ro, "shapr")
        if lib_loc:
            importr("shapr", lib_loc=lib_loc)
        else:
            importr("shapr")
    except Exception as e:
        raise ImportError(
            "The R package 'shapr' is not installed or not found.\n"
            "In an R session, run: install.packages('shapr')"
        ) from e

    # Import the implementation from a private module to avoid name collision
    _explain_mod = import_module(__name__ + "._explain")
    _explain_impl = _explain_mod.explain
    _r_ready = True
    return True


def explain(*args, **kwargs):
    """Lazily initialize R/shapr then call the real explain()."""
    ensure_r_ready()
    return _explain_impl(*args, **kwargs)


# Import the Shapr class (lazy import to avoid R dependency issues)
def _import_shapr():
    from .explanation import Shapr

    return Shapr


# Make Shapr available when the module is imported
Shapr = None
try:
    Shapr = _import_shapr()
except ImportError:
    pass
