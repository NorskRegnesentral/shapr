# Demo Files

## Files

- **demo_R.R** - R demo script showing how to use shapr
- **demo_py.py** - Python demo script showing how to use shaprpy
- **prep_data_and_models.R** - Script to prepare data and train models
- **Slides - demo - Amsterdam XAI workshop Feb 2026.pdf** - Slides for the initial presentation of the demo from the Amsterdam Workshop: "Methods for Explainable Machine Learning in Health Care" (https://www.vvsor.nl/biometrics/events/methods-explainable-ml/)
- **data_and_models/** - Folder containing training data, test data, and trained models

## Requirements

### R
- xgboost
- data.table
- shapr
- future
- progressr

### Python
- xgboost
- pandas
- shaprpy
- shap

## Usage

The data and models as outputted by `prep_data_and_models.R` are already available in the `data_and_models/` folder. Thus, you may run the demo scripts `demo_R.R` and `demo_py.py` directly.
