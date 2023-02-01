# Installs the required R-packages
install.packages("remotes",repos="https://cloud.r-project.org")
remotes::install_github("NorskRegnesentral/shapr",ref = "add_shaprpy") 
# TODO: Update ref to devel, and then remove it once in master, and replace by regular install.packages once on CRAN