

# Installation

- Install shapr from the current branch (devtools::install() )



# TODO methodology

- Set cutoff criterion based on current difference between max and min shap values
- Implement and test different methods for adjusting weights when reducing the number of features
- (some time in the future (after the summer)) Implement method for handling multiple test observations 


# Experiments

- Simulations:
  - Use some simulation example and check the performance of the method compared to using as many v(S) with sampling-based regular kernelSHAP
  - Linear model with Gaussian features
- Nonlinear (e.g. xgboost based) model with Gaussian features
- 4 real data examples from Kjersti
