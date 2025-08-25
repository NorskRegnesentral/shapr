# Script used to run tests instead of using `devtools::test()` to avoid the serialization warning in snapshots

library(testthat)
library(shapr)

# Run all tests in the package
files <- list.files(
  path = "tests/testthat",
  pattern = "test-",
  full.names = TRUE,
  recursive = TRUE
)

 #files <- files[grep("summary",files)]

# files = files[14]

for (file in files) {
  print(paste0("Running snapshots for: ", file))
  Sys.sleep(1)
  testthat::test_file(file, package = "shapr")
}


testthat::snapshot_review()

# Get only the part between "test-" and ".R" in the file names
snap_folders <- sub("tests/testthat/test-(.*)\\.R", "\\1", files)


for (folder in snap_folders) {
  print(paste0("Reviewing snapshots for: ", folder))
  Sys.sleep(.5) # To clearly see when we start reviewing a new folder
  snapshot_review_man(paste0(folder, "/"))
}
