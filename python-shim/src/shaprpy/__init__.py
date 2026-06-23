"""Compatibility shim for the renamed ``pyshapr`` package.

The Python wrapper previously published as ``shaprpy`` has been renamed to
``pyshapr``. This package re-exports everything from ``pyshapr`` so existing
``import shaprpy`` code keeps working, while emitting a ``DeprecationWarning``
to encourage migration. It is maintained only for a transition period and
contains no functionality of its own.
"""

from __future__ import annotations

import sys
import warnings

import pyshapr
from pyshapr import _rutils, datasets, explanation, utils

warnings.warn(
    "The 'shaprpy' package has been renamed to 'pyshapr'. "
    "Please run 'pip install pyshapr' and update your imports to 'import pyshapr'. "
    "The 'shaprpy' name will keep forwarding to 'pyshapr' for a transition period.",
    DeprecationWarning,
    stacklevel=2,
)

# Forward the top-level public API. These are lazy in pyshapr, so importing
# shaprpy does not start an R session on its own.
explain = pyshapr.explain
ensure_r_ready = pyshapr.ensure_r_ready
Shapr = pyshapr.Shapr
__version__ = pyshapr.__version__

# Make ``import shaprpy.<sub>`` and ``from shaprpy.<sub> import ...`` resolve to
# the corresponding ``pyshapr`` submodules.
sys.modules[__name__ + ".datasets"] = datasets
sys.modules[__name__ + ".explanation"] = explanation
sys.modules[__name__ + ".utils"] = utils
sys.modules[__name__ + "._rutils"] = _rutils

__all__ = ["Shapr", "datasets", "ensure_r_ready", "explain"]
