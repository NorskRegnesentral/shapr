from shap import plots, Explanation
import matplotlib
import re
import warnings
import pandas as pd

def plot(shaprpy_obj: dict, plot_type: str = "bar", plot_mean: bool = False, idx: slice | int = None, indirect_values: bool = False, **kwargs): 
    """
    Generate a plot from a SHAPR explanation object based on the specified plot type.

    Parameters
    -----------
    shaprpy_obj : dict
        A dictionary representing the SHAPR explanation object containing
        internal parameters and explanation data.
    plot_type : str, optional
        The type of plot to generate. Defaults to "bar".
        Supported plot types depend on the 'sage' paramter within 'shaprpy_obj':
          - If 'sage' is True: "bar", "waterfall"
          - If 'sage' is False: "bar", "beeswarm", "heatmap", "scatter", "violin", "waterfall"
    plot_mean : bool, optional
        Wether the plot should SHAP values for the individual predictions or plot the mean.
        Defaults to False. 
        Only applicaple for the plot_types '', when 'sage' is False and the number of explanations is 10 or less. 
    idx: slice or int, optional
        The indices of which observations to be plotted.
        When providing a range the slice format should be used, f.ex. 'slice(10)' for the first 10 observations.
    indirect_values: bool, optional
        When False (default), plots the given object as ordinary. 
        When True, shapley_values are collected from 'shaprpy_obj["internal"]["output"]["shap_values_est"]'.
        Mainly useful when the given object is from a SAGE-computation. 
    **kwargs:
        Additional keyword arguments passed to the underlying plot function from `shap.plots`.

    Returns
    --------
    None

    Raises an error if the plot type is not supported for the given SHAPR explanation.
    """

    sage = bool(shaprpy_obj['internal']['parameters']['sage'])

    if indirect_values: 
        sage = False
    
    # Checking for a valid plot_type
    if sage and not (plot_type in ("bar", "waterfall")): 
        raise TypeError(
            "ERROR in shaprpy.plot: Unknown or unsupported plot type.\n"
            "For SAGE values the only supported plot types are 'bar' and 'waterfall'.")
    elif plot_type not in ("bar", "beeswarm", "heatmap", "scatter", "violin", "waterfall"):
        raise TypeError(
            "ERROR in shaprpy.plot: Unknown or unsupported plot type.\n"
            "See the documentation for supported types.")

    # Checking for SAGE indexing
    if sage and idx:
        warnings.warn("SAGE values are assumed singular and does not allow for indexing. Ignoring idx.", UserWarning)
        idx = None

    explanation = prep_data(shaprpy_obj, sage=sage, idx=idx, indirect_values=indirect_values)
    
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

    if (sage): 
        match plot_type:
            case "bar": 
                ax = plots.bar(explanation, show = False, show_data = False, **kwargs)
                ax.set_xlabel("SAGE values")
                matplotlib.pyplot.show()
            case "waterfall": 
                plot_waterfall_sage(explanation[0], **kwargs)
                matplotlib.pyplot.show()
    elif (plot_mean or n_explain==1): 
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
def prep_data(shaprpy_obj: dict, sage: bool = False, idx = None, includeTestData: bool = True, indirect_values: bool = False): 
    if indirect_values: 
        shap_values_df = shaprpy_obj["internal"]["output"]["shap_values_est"]
        feature_names = shap_values_df.columns.drop(['none'])
    else: 
        shap_values_df = shaprpy_obj['shapley_values_est']
        feature_names = shap_values_df.columns.drop(['explain_id', 'none'])
    data = shaprpy_obj["internal"]["data"]["x_explain"]

    if sage: 
        idx = 0

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

    if sage:
        explanation = Explanation(
            values=shap_vals,
            base_values=base_values,
            feature_names=feature_names.tolist()
        ) 
    else:
        explanation = Explanation(
            values=shap_vals,
            base_values=base_values,
            data=data,
            feature_names=feature_names.tolist()
        )
    return explanation


# Helper function for waterfall plotting of SAGE-values
def plot_waterfall_sage(explanation, **kwargs): 
    ax = plots.waterfall(explanation, show=False, **kwargs)
    fig = matplotlib.pyplot.gcf()

    while len(fig.axes) > 1:
        fig.delaxes(fig.axes[-1])

    base_values = explanation.base_values
    values = explanation.values
    fx = base_values + values.sum()

    ax.set_xlabel("SAGE value")

    #The rest of the function is altered from SHAP.plots.waterfall:
    def format_value(s, format_str):
        if not issubclass(type(s), str):
            s = format_str % s
        s = re.sub(r"\.?0+$", "", s)
        if s[0] == "-":
            s = "\u2212" + s[1:]
        return s

    # draw the E[f(X)] tick mark
    xmin, xmax = ax.get_xlim()
    ax2 = ax.twiny()
    ax2.set_xlim(xmin, xmax)
    ax2.set_xticks(
        [base_values, base_values + min(1e-8, xmax * 1e-10)]
    )  # The 1e-8 is so matplotlib 3.3 doesn't try and collapse the ticks
    # However, for very small values, 1e-8 is disruptively large, so xmax * 1e-10 is used instead
    ax2.spines["right"].set_visible(False)
    ax2.spines["top"].set_visible(False)
    ax2.set_xticklabels(["\n$\mathcal{L}(\emptyset)$", "\n$ = " + format_value(base_values, "%0.03f") + "$"], fontsize=12, ha="left")
    ax2.spines["left"].set_visible(False)

    # draw the f(x) tick mark
    ax3 = ax2.twiny()
    ax3.set_xlim(xmin, xmax)
    ax3.set_xticks(
        [base_values + values.sum(), base_values + values.sum() + min(1e-8, xmax * 1e-10)]
    )  # The 1e-8 is so matplotlib 3.3 doesn't try and collapse the ticks
    # However, for very small values, 1e-8 is disruptively large, so xmax * 1e-10 is used instead
    ax3.set_xticklabels(["$\mathcal{L}(f(x))$", "$ = " + format_value(fx, "%0.03f") + "$"], fontsize=12, ha="left")
    tick_labels = ax3.xaxis.get_majorticklabels()
    tick_labels[0].set_transform(
        tick_labels[0].get_transform() + matplotlib.transforms.ScaledTranslation(-17 / 72.0, 0, fig.dpi_scale_trans)
    )
    tick_labels[1].set_transform(
        tick_labels[1].get_transform() + matplotlib.transforms.ScaledTranslation(18 / 72.0, 0, fig.dpi_scale_trans)
    )
    ax3.spines["right"].set_visible(False)
    ax3.spines["top"].set_visible(False)
    ax3.spines["left"].set_visible(False)

    # adjust the position of the L(Ø) label and value (a bit more space)
    tick_labels = ax2.xaxis.get_majorticklabels()
    tick_labels[0].set_transform(
        tick_labels[0].get_transform() + matplotlib.transforms.ScaledTranslation(-15 / 72.0, 0, fig.dpi_scale_trans)
    )
    tick_labels[1].set_transform(
        tick_labels[1].get_transform()
        + matplotlib.transforms.ScaledTranslation(16 / 72.0, -1 / 72.0, fig.dpi_scale_trans)
    )
