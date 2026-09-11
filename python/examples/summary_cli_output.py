"""Print only a pyshapr summary using a selected R package installation."""

from __future__ import annotations

import argparse
from pathlib import Path

import numpy as np
import pandas as pd
import rpy2.robjects as ro
from sklearn.linear_model import LinearRegression

import pyshapr
from pyshapr._rutils import _importr


def main() -> None:
    """Create a small explanation silently and print its R-backed summary.

    Returns
    -------
    None
        Prints the summary text captured from R.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--r-library", type=Path, required=True)
    args = parser.parse_args()
    library = args.r_library.expanduser().resolve()
    if not (library / "shapr" / "DESCRIPTION").is_file():
        parser.error(f"No installed shapr package found in {library}")

    ro.r[".libPaths"](ro.StrVector([str(library), *ro.r[".libPaths"]()]))
    ro.r["options"](**{
        "width": 100, "cli.width": 100, "cli.num_colors": 1, "progressr.enable": False,
    })
    _importr("shapr")
    loaded_path = Path(str(ro.r["find.package"]("shapr")[0])).resolve()
    if loaded_path != library / "shapr":
        raise RuntimeError(f"Requested {library / 'shapr'}, but R loaded {loaded_path}")

    rng = np.random.default_rng(17)
    x_train = pd.DataFrame(rng.normal(size=(30, 3)), columns=["first", "second", "third"])
    y_train = x_train.to_numpy() @ np.array([1.0, 2.0, -1.0])
    model = LinearRegression().fit(x_train, y_train)

    explanation = pyshapr.explain(
        model=model,
        x_train=x_train,
        x_explain=x_train,
        approach="independence",
        phi0=float(y_train.mean()),
        n_MC_samples=10,
        iterative=False,
        seed=1,
        verbose=None,
        testing=True,
    )
    print(explanation.summary())


if __name__ == "__main__":
    main()