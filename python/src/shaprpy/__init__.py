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
