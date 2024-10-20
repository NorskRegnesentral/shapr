# Installs the required R-packages
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("NorskRegnesentral/shapr", ref = "pytho_iterative_2") # temporary set the ref here
# Installs the development version of shapr from the master branch on CRAN
