"""Inspect pyshapr console output using an explicitly selected R installation.

Run in a fresh Python process for each R library; embedded R cannot reliably
switch an already loaded package to another installation.
"""

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
    """Run a small, reproducible explanation and display its R-backed output.

    Returns
    -------
    None
        Prints the selected installation, explanation, and summary.
    """
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--r-library", type=Path, required=True,
        help="R library directory containing the selected shapr package.",
    )
    parser.add_argument("--width", type=int, default=85, help="R console width (default: 85).")
    parser.add_argument("--approach", choices=("independence", "vaeac"), default="independence")
    parser.add_argument("--epochs", type=int, default=12, help="VAEAC epochs (default: 12).")
    parser.add_argument(
        "--verbose", choices=("basic", "shapley", "details", "quiet"), default="shapley",
        help="Explanation verbosity; explicit print/summary calls still run in quiet mode.",
    )
    args = parser.parse_args()
    library = args.r_library.expanduser().resolve()
    if not (library / "shapr" / "DESCRIPTION").is_file():
        parser.error(f"No installed shapr package found in {library}")
    if not 10 <= args.width <= 10000:
        parser.error("--width must be between 10 and 10000")
    if args.epochs < 3:
        parser.error("--epochs must be at least 3 (initialization uses two epochs)")

    ro.r[".libPaths"](ro.StrVector([str(library), *ro.r[".libPaths"]()]))
    ro.r["options"](**{"width": args.width, "cli.width": args.width, "cli.num_colors": 1})
    shapr = _importr("shapr")
    loaded_path = Path(str(ro.r["find.package"]("shapr")[0])).resolve()
    if loaded_path != library / "shapr":
        raise RuntimeError(f"Requested {library / 'shapr'}, but R loaded {loaded_path}")

    migrated = bool(ro.r('any(grepl("cli::cli_verbatim", deparse(shapr:::print_iter)))')[0])
    print(f"Python wrapper: {pyshapr.__file__}")
    print(f"R shapr {shapr.__version__}: {loaded_path}")
    print(f"Iteration output API: {'cli::cli_verbatim' if migrated else 'not migrated'}")

    rng = np.random.default_rng(17)
    n_train = 128 if args.approach == "vaeac" else 30
    x_train = pd.DataFrame(rng.normal(size=(n_train, 3)), columns=["first", "second", "third"])
    y_train = x_train.to_numpy() @ np.array([1.0, 2.0, -1.0])
    model = LinearRegression().fit(x_train, y_train)
    verbose = {
        "basic": "basic",
        "shapley": ["basic", "shapley"],
        "details": ["basic", "vS_details", "shapley"],
        "quiet": None,
    }[args.verbose]
    approach_args = {}
    if args.approach == "vaeac":
        torch = _importr("torch")
        torch.torch_set_num_threads(1)
        torch.torch_set_num_interop_threads(1)
        approach_args = {
            "vaeac_epochs": args.epochs,
            "vaeac_n_vaeacs_initialize": 2,
            "vaeac_width": 16,
            "vaeac_depth": 2,
            "vaeac_extra_parameters": ro.ListVector({"vaeac.cuda": False}),
        }

    print("\n=== explain() output ===", flush=True)
    explanation = pyshapr.explain(
        model=model,
        x_train=x_train,
        x_explain=x_train.iloc[:2],
        approach=args.approach,
        phi0=float(y_train.mean()),
        n_MC_samples=10,
        iterative=False,
        seed=1,
        verbose=verbose,
        testing=True,
        **approach_args,
    )
    print("\n=== print(explanation) ===")
    print(explanation)
    print("\n=== explanation.print() ===")
    explanation.print()
    print("\n=== print(explanation.summary()) ===")
    print(explanation.summary())


if __name__ == "__main__":
    main()