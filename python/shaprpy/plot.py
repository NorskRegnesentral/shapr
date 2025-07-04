from shap import plots, Explanation
import logging
import matplotlib
import re

def plot(shaprpy_obj: dict, plot_type: str = "bar", **kwargs): 
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
    **kwargs:
        Additional keyword arguments passed to the underlying plot function from `shap.plots`.

    Returns
    --------
    None

    Logs an error if the plot type is not supported for the given SHAPR explanation.
    """
    sage = bool(shaprpy_obj['internal']['parameters']['sage'])
    explanation = prep_data(shaprpy_obj, sage=sage)

    logger = logging.getLogger(__name__)

    if (sage): 
        match plot_type:
            case "bar": 
                ax = plots.bar(explanation, show = False, **kwargs)
                ax.set_xlabel("SAGE values")
                matplotlib.pyplot.show()
            case "waterfall": 
                plot_waterfall_sage(explanation, **kwargs)
            case _: 
                logger.error(
                    "ERROR in shaprpy.plot: Unknown or unsupported plot type.\n"
                    "For SAGE values the only supported plot types are 'bar' and 'waterfall'."
                )
    else:
        supported_plots = ("bar", "beeswarm", "heatmap", "scatter", "violin", "waterfall")

        if plot_type in supported_plots:
            ax = getattr(plots, plot_type)(explanation, **kwargs)
        else:
            logger.error(
                "ERROR in shaprpy.plot: Unknown or unsupported plot type.\n"
                "See the documentation for supported types."
            )

def plot_waterfall_sage(explanation, **kwargs): 
    def format_value(s, format_str):
        if not issubclass(type(s), str):
            s = format_str % s
        s = re.sub(r"\.?0+$", "", s)
        if s[0] == "-":
            s = "\u2212" + s[1:]
        return s

    ax = plots.waterfall(explanation, show=False, **kwargs)
    fig = matplotlib.pyplot.gcf()

    while len(fig.axes) > 1:
        fig.delaxes(fig.axes[-1])

    base_values = explanation.base_values
    values = explanation.values
    fx = base_values + values.sum()

    ax.set_xlabel("SAGE values")

    #The rest of the function is altered from SHAP.plots.waterfall:
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

    matplotlib.pyplot.show()


def prep_data(shaprpy_obj: dict, sage: bool = False): 
    """
    Transform output from shaprpy into a 'shap.Explanation' object for plotting.

    Note: Since this creates an object of SHAP's primary explainer interface,
    it is intended mainly for plotting purposes and likely does not support
    other use cases.

    Parameters
    ----------
    shaprpy_obj : dict
        The results from a call to 'shaprpy.explain()'.

    Returns
    -------
    shap.Explanation
        An object containing SHAP values, base values, feature data, and metadata,
        suitable for plotting with `shap.plots.*`.
    """

    shap_values_df = shaprpy_obj['shapley_values_est']
    feature_names = shap_values_df.columns.drop(['explain_id', 'none'])

    if sage:
        # Select the first row for sage case
        shap_values_row = shap_values_df.iloc[0]
        shap_vals = shap_values_row[feature_names].values
        base_values = shap_values_row["none"]
        data = shap_values_row[feature_names].values  # or X_display.iloc[0].values
    else:
        # Use all rows
        shap_vals = shap_values_df[feature_names].values
        base_values = shap_values_df["none"].values
        data = shap_values_df[feature_names].values

    explanation = Explanation(
        values=shap_vals,
        base_values=base_values,
        data=data,
        feature_names=feature_names.tolist()
    )
    return explanation
