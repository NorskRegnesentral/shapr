
# rcmdcheck


#install.packages("rcmdcheck")
#library(rcmdcheck)
#rcmdcheck(path = getwd())

# Run package on Winbuilder

devtools::check_win_devel()
devtools::check_win_oldrelease()
devtools::check_win_release()

# Check pacakge on Rhub

#install.packages("rhub")

#rhub::validate_email(email="Martin.Jullum@nr.no")

#devtools::check_rhub()

