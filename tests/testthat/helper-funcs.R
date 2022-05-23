
helper_rds <- function(code,name="tmp.rds"){
  path <- file.path(tempdir(),name)
  testthat::expect_snapshot_output({cat(paste0("Output from ",name,"\n"));code})
  saveRDS(code,file = path)

  path
}

compare_rds <- function(old,new){

  old <- readRDS(old)
  new <- readRDS(new)

  check <- all.equal(old,new)
  ifelse(is.character(check),FALSE,check)
}

expect_snapshot_rds <- function(code,name = "tmp"){
  name_full <- paste0(name,".rds")
  path <- file.path(tempdir(),name_full)

  announce_snapshot_file(name = name_full)

  testthat::expect_snapshot_output({cat(paste0("Output from ",name,"\n"));code}) # Test the printed output
  saveRDS(code,file = path)

  expect_snapshot_file(path,compare=compare_rds) # Test the returned object
}




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
