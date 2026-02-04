Notes

# TODO

- run AI formatting
X- Consider fitting more models in the preparation step to showcase different model types.
- reduce training time for the vaeac model as was done in the vignette
- remove vaeac model if it continues to be ustable
X- add explanation of workflow-model
X- add explanation of custom model
X- add regression sep and surr in the first setup
- add forecasting example
- check software paper whether causal shapley values is really what we get when specifying the confounders

- python: just the basic stuff, regression method


### Things to fix (look into) in the future

- custom model with iterative procedure and parallelization throws odd error from future
- why vaeac does not work (on WSL)
- double warning when passing a non-natively supported model:

── Starting `shapr::explain()` at 2026-02-03 19:52:04 ──────────────────
ℹ You passed a model to `shapr::explain()` which is not natively
  supported, and did not supply a `get_model_specs` function to
  `shapr::explain()`.
  Consistency checks between model and data are therefore disabled.
Error in `get_predict_model()`:
! You passed a model to `shapr::explain()` that is not natively
  supported and did not supply a 'predict_model' function to
  `shapr::explain()`. See the documentation of `shapr::explain()` or
  the `vignette(shapr::general_usage)` vignette for more information on
  how to run shapr with custom models.
Run `rlang::last_trace()` to see where the error occurred.

- Error in the reporting of timing of the v(S) computations when using verbose = "progress"

✔ Computing v(S) [4ms]
✔ Computing Shapley value estimates [5ms]
✔ Bootstrapping Shapley value standard deviations [660ms]

- Add a "Computations DONE" in the end of the shapley value computations whenever verbose is not NULL
- plotting for explain_forecast not working properly (beeswarm works, but not waterfall etc)
- check whether the estimated remaining coalitions is really based on the current convergence tol -- changing it, it seems not to affect the estimated remaining coalitions...
- the MSEv values are always very large, are they really normalzied?
- in the MSEv plot function, do we by default include all observations? a message saying top 10 is shown is printed, so i am not sure.
