# Installs the required R-packages
install.packages("remotes", repos = "https://cloud.r-project.org")
remotes::install_github("NorskRegnesentral/shapr")
# Installs the development version of shapr from the master branch on CRAN
