"""
Shapr explanation class for exploring Shapley value results.
"""
from rpy2 import robjects
from shaprpy._rutils import _importr


class Shapr:
    """
    A class to hold and explore Shapley value explanations using R functions.

    This class wraps the explanation dictionary and R object returned by the
    explain() function, providing direct access to R's shapr functions for
    exploration and analysis.

    Parameters
    ----------
    explanation_dict : dict
        The explanation dictionary returned by explain().
    r_object : R object
        The original R shapr object used for all R function calls.
    """

    def _capture_print_output(self, what="shapley_est", digits=3):
        """Capture the output from R's print.shapr without printing twice."""
        import io
        from contextlib import redirect_stdout
        base = _importr('base')

        buffer = io.StringIO()
        with redirect_stdout(buffer):
            base.print(self._r_object, what=what, digits=digits)
        return buffer.getvalue().rstrip()

    def __init__(self, explanation_dict, r_object):
        """
        Initialize the Shapr explanation object.

        Parameters
        ----------
        explanation_dict : dict
            The explanation dictionary containing Shapley values and metadata.
        r_object : R object
            The original R shapr object for accessing R-specific functionality.
        """
        self._explanation_dict = explanation_dict
        self._r_object = r_object

    def __str__(self):
        """
        Mirror the R print.shapr() output (shapley_est by default).
        """
        return self._capture_print_output()

    def __repr__(self):
        """Provide same output when object is inspected in console."""
        return self.__str__()

    def print(self, what="shapley_est", digits=3):
        """
        Print specific components using R's print.shapr function.

        Parameters
        ----------
        what : str, optional
            Which component to print.
            Options are "shapley_est", "shapley_sd", "MSEv", "MSEv_explicand", "MSEv_coalition", and
            "timing_summary". Defaults to "shapley_est".
            Only one component can be printed at a time.
            See the details section of get_results() for details about each component.
        digits : int, optional
            Number of significant digits to display. Defaults to 3.

        Returns
        -------
        None
            Prints output from R's print.shapr() but returns nothing.
        """
        captured_output = self._capture_print_output(what=what, digits=digits)
        print(captured_output)

    def get_explanation_dict(self):
        """Get the explanation dictionary.

        Returns the original Python dictionary that was returned by explain()
        before being wrapped in this class. This provides direct access to
        all the Python-converted components of the explanation.

        Returns
        -------
        dict
            The explanation dictionary containing keys like 'shapley_values_est',
            'internal', 'timing', etc.

        Examples
        --------
        >>> explanation = shaprpy.explain(model, X_test, X_train)
        >>> exp_dict = explanation.get_explanation_dict()
        >>> shapley_values = exp_dict['shapley_values_est']
        """
        return self._explanation_dict

    def get_r_object(self):
        """
        Get the original R shapr object.

        This allows direct access to R functions like print.shapr(),
        plotting functions, etc.

        Returns
        -------
        R object
            The original R shapr object.
        """
        return self._r_object



    def get_results(self, what=None):
        """
        Extract components from the Shapr explanation object using R's get_results function.

        Parameters
        ----------
        what : str or list of str, optional
            Component(s) to extract. Available options include:
            "calling_function", "approach", "proglang", "shapley_est", "shapley_sd", "pred_explain",
            "MSEv", "MSEv_explicand", "MSEv_coalition", "iterative_info", "iterative_shapley_est",
            "iterative_shapley_sd", "saving_path", "timing_summary", "timing_details", "parameters",
            "x_train", "x_explain", "dt_vS", "dt_samp_for_vS", "dt_used_coalitions",
            "dt_valid_causal_coalitions", "dt_coal_samp_info".
            The default is to return all components. See Notes section for component descriptions.

        Notes
        -----
        This function extracts information related to the computation of Shapley values.
        Available components:

        - **calling_function** : Name of function used to create the shapr object (always explain() in Python)
        - **proglang** : Programming language used to initiate the computations (R or Python).
        - **approach** : Approach used to estimate the conditional expectations
        - **shapley_est** : DataFrame with the estimated Shapley values
        - **shapley_sd** : DataFrame with standard deviations of Shapley values
        - **pred_explain** : Predictions for the explained observations
        - **MSEv/MSEv_explicand/MSEv_coalition** : MSEv evaluation criterion values
        - **iterative_info** : Information about the iterative estimation procedure
        - **iterative_shapley_est/iterative_shapley_sd** : Shapley values/std devs for each iteration
        - **saving_path** : Path where temporary results are saved
        - **timing_summary** : Summary timing information (init_time, end_time, total_time_secs)
        - **timing_details** : Detailed timing information for different computation parts
        - **parameters** : Parameters used in the computation
        - **x_train/x_explain** : Training data and observations to explain
        - **dt_vS** : Contribution function v(S) estimates for each coalition
        - **dt_samp_for_vS** : Samples used in Monte Carlo estimation of v(S)
        - **dt_used_coalitions** : Overview of coalitions used in computation
        - **dt_valid_causal_coalitions** : Valid causal coalitions used
        - **dt_coal_samp_info** : Information about coalition sampling procedure

        Returns
        -------
        object
            Results from R's shapr.get_results function, converted to Python objects.
            If a single component is requested, returns that object. If multiple are requested, returns a named dict.
        """
        from rpy2.robjects import StrVector
        from shaprpy.utils import recurse_r_tree

        shapr = _importr('shapr')

        if what is None:
            # Call R get_results with default arguments
            r_results = shapr.get_results(self._r_object)
        else:
            # Ensure what is a list for R
            if isinstance(what, str):
                what = [what]
            # Call R get_results with specific components
            r_results = shapr.get_results(self._r_object, what=StrVector(what))

        # Convert R results to Python objects
        return recurse_r_tree(r_results)

    def summary(self, digits=2):
        """
        Print a formatted summary of the Shapr explanation object using R's summary.shapr function.

        Parameters
        ----------
        digits : int, optional
            Integer. (Maximum) number of digits to be displayed after the decimal point. Defaults to 2.

        Returns
        -------
        None
            Prints summary (from R's summary.shapr() but returns nothing
        """
        base = _importr('base')

        # Call R summary function just for the printing
        base.summary(self._r_object, digits=digits)

        # Return None explicitly
        return None

    def to_shap(self, idx=None):
        """Convert the Shapr explanation to a SHAP Explanation object.

        This method transforms the Shapley values and associated data into
        a SHAP library compatible format, enabling the use of SHAP's
        visualization functions for plotting and analysis.

        Parameters
        ----------
        idx : int, slice, or None, optional
            Indices of observations to include. If None, includes all observations.
            If int, includes only that observation. If slice, includes the range.

        Returns
        -------
        shap.Explanation
            A SHAP Explanation object containing the Shapley values and
            base values, ready for use with SHAP plotting functions.

        Examples
        --------
        >>> import shap
        >>> explanation = shaprpy.explain(model, X_test, X_train)
        >>> shap_exp = explanation.to_shap()
        >>> shap.plots.waterfall(shap_exp[0])  # Plot first observation
        """

        from shap import Explanation

        shap_values_df = self._explanation_dict['shapley_values_est']
        feature_names = shap_values_df.columns.drop(['explain_id', 'none'])
        data = self._explanation_dict["internal"]["data"]["x_explain"]

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
