
library(here)

# source(file.path("source","R_prep_data_and_model.R")) # Already run, and data/models saved to ensure reproducibility

source(file.path("source","R_setup.R")) # Reads data and models created by R_prep_data_and_model.R

source(file.path("source","R_explain.R")) # Example code in Section 3
source(file.path("source","R_explain_asym_cau_figure.R")) # Generates figure in Section 4
# source(file.path("source","R_explain_asym_cau_example.R")) # Don't run: Code for the generic code example in Section 4
source(file.path("source","R_explain_forecast.R")) # Example code in Section 6

# Make sure any open connections opened in R_setup.R are closed afterwards
if (!inherits(future::plan(), "sequential")) future::plan("sequential")


sessionInfo()


