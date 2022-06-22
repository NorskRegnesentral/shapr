
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


#' Helper function for package development
#'
#' This is a manual extension of [testthat::snapshot_review()] which works for the \code{.rds} files used in
#' this package.
#'
#' @param path Character
#' Gives the relative path to the test files to review
#'
#' @export
snapshot_review_man <- function(path,...){
  changed <- testthat:::snapshot_meta(path)
  these_rds <- (tools::file_ext(changed$name)=="rds")
  if(any(these_rds)){
    for(i in which(these_rds)){
      old <- readRDS(changed[i,"cur"])
      new <- readRDS(changed[i,"new"])

      cat(paste0("Difference for check ",changed[i,"name"]," in test ", changed[i,"test"],"\n"))
      print(waldo::compare(old,new,...))
      browser()
    }
  }
}
