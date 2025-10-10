"""
Shapr explanation class for better exploration of results.
"""
from shap import Explanation


class Shapr:
    """
    A class to hold and explore Shapley value explanations using R functions.

    This class wraps the explanation dictionary and R object returned by the
    explain() function, providing direct access to R's shapr functions for
    exploration and analysis.

    Parameters
    ----------
    explanation_dict : dict
        The explanation dictionary returned by explain()
    r_object : R object
        The original R shapr object used for all R function calls
    """

    def __init__(self, explanation_dict, r_object):
        """
        Initialize the Shapr explanation object.

        Parameters
        ----------
        explanation_dict : dict
            The explanation dictionary containing Shapley values and metadata
        r_object : R object
            The original R shapr object for accessing R-specific functionality
        """
        self._data = explanation_dict
        self._r_object = r_object

    def get_data(self):
        """
        Get the raw explanation dictionary.

        Returns
        -------
        dict
            The raw explanation dictionary
        """
        return self._data

    def get_r_object(self):
        """
        Get the original R shapr object.

        This allows direct access to R functions like print.shapr,
        plot functions, etc.

        Returns
        -------
        R object or None
            The original R shapr object if available
        """
        return self._r_object



    def get_results(self, what=None):
        """
        Extract components from the Shapr explanation object using R's get_results function.

        Parameters
        ----------
        what : str or list of str, optional
            Component(s) to extract. Options: "calling_function", "approach", "shapley_est", "shapley_sd", "pred_explain",
            "MSEv", "MSEv_explicand", "MSEv_coalition", "iterative_info", "iterative_shapley_est", "iterative_shapley_sd",
              "saving_path", "timing_summary", "timing_details", "parameters", "x_train", "x_explain", "dt_vS",
                "dt_samp_for_vS", "dt_used_coalitions", "dt_valid_causal_coalitions", "dt_coal_samp_info".
                The default is to return all components.
                See details for what each component contains.

        Details
        -------
        The function extracts a full suite of information related to the computation of the Shapley values from a shapr object.
        The allowed characters in what provides information as follows:

        calling_function
            Name of function called to create the shapr object, (explain() or explain_forecast()).

        approach
            Approach used to estimate the conditional expectations.

        shapley_est
            dataframe with the estimated Shapley values.

        shapley_sd
            dataframe with the standard deviation of the Shapley values reflecting the uncertainty in the coalition sampling part of the kernelSHAP procedure.

        pred_explain
            Numeric vector with the predictions for the explained observations.

        MSEv/MSEv_explicand/MSEv_coalition
            Dataframes with MSEv evaluation criterion values overall/ per explicand/per coalition.
            Smaller values indicate better estimates of v(S).
            See the MSEv evaluation section in the general usage vignette for details.

        iterative_info
            Dataframe with information about the iterative estimation procedure.

        iterative_shapley_est/iterative_shapley_sd
            Dataframes with the estimated Shapley values/their standard deviation for each iteration (when using the iterative estimation procedure).

        saving_path
            Character string with the path where the (temporary) results are saved.

        timing_summary
            Dataframe with one row and three columns: init_time and end_time give the time stamps for the start and end of the computation, respectively, while total_time_secs gives the total time in seconds for the full computation.

        timing_details
            Dict containing timing information for the different parts of the computation. summary contains the information from timing_summary.
            overall_timing_secs gives the time spent on the different parts of the explanation computation.
              main_computation_timing_secs further decomposes the main computation time into the different parts of the computation for each iteration of the iterative estimation routine, if used.

        parameters
            Dict with the parameters used in the computation.

        x_train/x_explain
            Dataframe with the training data used in the computation/observations to explain.

        dt_vS
            Dataframe with the contribution function (v(S)) estimates for each coalition.

        dt_samp_for_vS
            Dataframe with the samples used in the Monte Carlo estimation of the contribution function (v(S)).
            This is only available if output_args_default$keep_samp_for_vS = TRUE (defaults to FALSE) in explain().

        dt_used_coalitions
            Dataframe with an overview of the coalitions used in the computation.

        dt_valid_causal_coalitions
            Dataframe with the valid causal coalitions used in the computation.

        dt_coal_samp_info
            Dataframe with information related to the coalition sampling procedure being used.

        Returns
        -------
        object
            Results from R's shapr.get_results function, converted to Python objects.
            If a single component is requested, returns that object. If multiple are requested, returns a named dict.
        """
        from rpy2.robjects.packages import importr
        from rpy2.robjects import StrVector
        from shaprpy.utils import recurse_r_tree

        shapr = importr('shapr')

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
        from rpy2.robjects.packages import importr
        base = importr('base')

        # Call R summary function just for the printing
        tmp = base.summary(self._r_object, digits=digits)

        # Return None explicitly
        return None

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
        """
        from rpy2.robjects.packages import importr
        base = importr('base')

        # Call R's print.shapr function
        base.print(self._r_object, what=what, digits=digits)

    def to_shap(self, idx=None):
        """
        Convert the Shapr explanation to a SHAP Explanation object.

        This method converts the Shapley values and data to the format expected
        by the SHAP library, allowing us to utilize their plotting infrastructure.

        Parameters
        ----------
        idx : int, slice, or None, optional
            Indices of observations to include. If None, includes all observations.
            If int, includes only that observation. If slice, includes the range.

        Returns
        -------
        shap.Explanation
            SHAP Explanation object suitable for SHAP plotting functions
        """
        shap_values_df = self._data['shapley_values_est']
        feature_names = shap_values_df.columns.drop(['explain_id', 'none'])
        data = self._data["internal"]["data"]["x_explain"]

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