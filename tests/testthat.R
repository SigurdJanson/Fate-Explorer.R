#testthat.R
library(testthat)
#print(getwd())
#print(pkgload::pkg_path("./tests/testthat/"))
#test_dir(path = "./tests/testthat", filter = ".*_test\\.R")
test_dir(path = "./tests/testthat/")
#source("../R/FindPdf_Root.R", verbose = TRUE)