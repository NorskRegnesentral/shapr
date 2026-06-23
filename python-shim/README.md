# shaprpy (renamed to pyshapr)

> **This package has been renamed to [`pyshapr`](https://pypi.org/project/pyshapr/).**

`shaprpy` is now a thin compatibility shim. Installing it pulls in `pyshapr` and
forwards all imports to it, so existing code that does `import shaprpy` keeps
working. Importing `shaprpy` emits a `DeprecationWarning`.

This shim is maintained only for a transition period and contains no
functionality of its own.

## Migrate

Replace your installation:

```bash
pip uninstall shaprpy
pip install pyshapr
```

Replace your imports:

```python
# Old
from shaprpy import explain
from shaprpy.datasets import load_california_housing

# New
from pyshapr import explain
from pyshapr.datasets import load_california_housing
```

See the [pyshapr documentation](https://norskregnesentral.github.io/shapr/pyshapr.html)
for installation instructions and examples.
