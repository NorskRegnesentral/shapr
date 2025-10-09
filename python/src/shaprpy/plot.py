from shap import plots, Explanation
import matplotlib
import warnings
import pandas as pd

def plot(shaprpy_obj: dict, plot_type: str = "bar", plot_mean: bool = False, idx: slice | int = None, **kwargs):
    """
    Generate a plot from a SHAPR explanation object based on the specified plot type.

    Parameters
    -----------
    shaprpy_obj : dict
        A dictionary representing the SHAPR explanation object containing
        internal parameters and explanation data.
    plot_type : str, optional
        The type of plot to generate. Defaults to "bar".
        Supported plot types are: "bar", "beeswarm", "heatmap", "scatter", "violin", "waterfall"
    plot_mean : bool, optional
        Wether the plot should SHAP values for the individual predictions or plot the mean.
        Defaults to False.
        Only applicaple for the plot_types '', when 'sage' is False and the number of explanations is 10 or less.
    idx: slice or int, optional
        The indices of which observations to be plotted.
        When providing a range the slice format should be used, f.ex. 'slice(10)' for the first 10 observations.
    **kwargs:
        Additional keyword arguments passed to the underlying plot function from `shap.plots`.

    Returns
    --------
    None

    Raises an error if the plot type is not supported for the given SHAPR explanation.
    """

    # Checking for a valid plot_type
    if plot_type not in ("bar", "beeswarm", "heatmap", "scatter", "violin", "waterfall"):
        raise TypeError(
            "ERROR in shaprpy.plot: Unknown or unsupported plot type.\n"
            "See the documentation for supported types.")

    explanation = prep_data(shaprpy_obj, idx=idx)

    n_explain = explanation.shape[0]

    if plot_type == "waterfall" and n_explain > 1:
        raise ValueError(
            "The waterfall plot can currently only plot a single explanation, but"
            f"{n_explain} explanations was passed! Perhaps try "
            "indexing with 'idx'."
        )

    if not plot_mean and n_explain > 10:
        warnings.warn("Too many observations to plot together; 10 or less oberservations is required to plot single observations.\n"
          "Plotting mean.", UserWarning)
        plot_mean = True

    if (plot_mean or n_explain==1):
        if plot_type == "waterfall":
            explanation = explanation[0]
        ax = getattr(plots, plot_type)(explanation, **kwargs)
    else:
        n_rows = (n_explain + 1) // 2  # ceil division
        fig_height = n_rows * 2        # 4 inches per row
        fig, axes = matplotlib.pyplot.subplots(n_rows, 2, figsize=(12, fig_height))
        axes = axes.flatten()

        for i in range(n_explain):
            ax = axes[i]

            getattr(plots, plot_type)(explanation[i], show = False, ax = ax, **kwargs)

            ax.yaxis.set_tick_params(pad=20)

        if(n_explain % 2 == 1):
            fig.delaxes(axes[n_explain])

        matplotlib.pyplot.tight_layout()
        matplotlib.pyplot.show()


# Helper function to convert data from a shapr object to a SHAP object
def prep_data(shaprpy_obj: dict, idx = None):
    shap_values_df = shaprpy_obj['shapley_values_est']
    feature_names = shap_values_df.columns.drop(['explain_id', 'none'])
    data = shaprpy_obj["internal"]["data"]["x_explain"]

    if isinstance(idx, int):
        shap_values_df = shap_values_df.iloc[[idx]]
        data = data.iloc[[idx]]
        base_values = shap_values_df["none"]
    elif isinstance(idx, slice):
        shap_values_df = shap_values_df.iloc[idx]
        data = data.iloc[idx]
        base_values = shap_values_df["none"].values
    elif not idx:
        base_values = shap_values_df["none"].values
    else:
        raise TypeError("'idx' must be an int or slice.")

    shap_vals = shap_values_df[feature_names].values
    data = data[feature_names].values

    explanation = Explanation(
        values=shap_vals,
        base_values=base_values,
        data=data,
        feature_names=feature_names.tolist()
    )

    return explanation

