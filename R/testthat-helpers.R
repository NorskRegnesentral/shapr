
helper_rds <- function(code,name="tmp.rds"){
  path <- file.path(tempdir(),name)
  testthat::expect_snapshot_output({cat(paste0("Output from ",name,"\n"));code})
  saveRDS(code,file = path)

  path
}

compare_rds <- function(old,new){

  old <- readRDS(old)
  new <- readRDS(new)

  check <- all.equal(old,new,tolerance = 10^(-6)) # Increase tolerance
  ifelse(is.character(check),FALSE,check)
}

expect_snapshot_rds <- function(code,name = "tmp"){


  name_full <- paste0(name,".rds")
  path <- file.path(tempdir(),name_full)

  testthat::announce_snapshot_file(path = path)

  out <- code

  testthat::expect_snapshot_output(out) # Test the printed output
  saveRDS(out,file = path)

  testthat::expect_snapshot_file(path,compare=compare_rds) # Test the returned object
}

