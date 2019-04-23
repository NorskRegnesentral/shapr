
x <- read.csv("~/test_lintr.txt", sep = ";", header = FALSE, stringsAsFactors = FALSE)
x <- data.table::as.data.table(x)
data.table::setnames(x, "old_name")
x[, new_name := old_name]
x <- unique(x)
x[, num_char := nchar(old_name)]
data.table::setorder(x, -num_char)
x[, num_char := NULL]
x[, id := .I]
data.table::setkey(x, id)
x[, new_name := snakecase::to_snake_case(old_name), id]
data.table::fwrite(x, file = "~/temp_lintr.txt", sep = ";")
file.edit("~/temp_lintr.txt")
x <- data.table::fread("~/temp_lintr.txt")
data.table::fwrite(x, file = "~/temp_lintr2.txt", sep = ";")
x <- data.table::fread("~/temp_lintr2.txt")
x <- as.data.frame(x)


for(i in 1:nrow(x)) {

  print(sprintf("%s out of %s", i, nrow(x)))

  old_name <- x$old_name[i]
  new_name <- x$new_name[i]

  str_pattern_b <- "\\(\\s\\|,\\|(\\|:\\)"
  str_pattern_a <- "\\(\\s\\|,\\|)\\|:\\|\\.\\|(\\|\\[\\|;\\|\\$\\|$\\)"
  if (old_name != new_name) {
    str_cmd_r <- sprintf("sed -i 's/%s%s%s/\\1%s\\2/g' R/*.R", str_pattern_b, old_name, str_pattern_a, new_name)
    str_cmd_s <- sprintf("sed -i 's/%s%s%s/\\1%s\\2/g' src/*.cpp", str_pattern_b, old_name, str_pattern_a, new_name)
    str_cmd_i <- sprintf("sed -i 's/%s%s%s/\\1%s\\2/g' inst/scripts/*.R", str_pattern_b, old_name, str_pattern_a, new_name)
    system(str_cmd_r)
    system(str_cmd_s)
    system(str_cmd_i)
  }
}
