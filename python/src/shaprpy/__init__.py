from importlib.metadata import version, PackageNotFoundError

# Public re-export: datasets is lightweight (no R dependency)
from . import datasets  # noqa: E402

__all__ = ["explain", "datasets", "ensure_r_ready"]

try:  # Prefer installed package metadata
    __version__ = version("shaprpy")
except PackageNotFoundError:  # Fallback for editable/source tree before build/install
    __version__ = "0.0.0+local"

_r_ready = False
_explain_impl = None


def ensure_r_ready():
    """Load rpy2 + R 'shapr' package and the heavy explain implementation (idempotent)."""
    global _r_ready, _explain_impl
    if _r_ready:
        return True
    try:
        import rpy2.robjects as ro  # noqa: F401
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
    # Defer heavy module import until R environment confirmed
    from . import explain as _explain_mod  # noqa: E402
    _explain_impl = _explain_mod.explain
    _r_ready = True
    return True


def explain(*args, **kwargs):  # noqa: D401
    """Wrapper that lazily initializes R/shapr then calls the real explain()."""
    ensure_r_ready()
    return _explain_impl(*args, **kwargs)

