#testthat.R
library(testthat)
#print(getwd())
test_dir(path = "./tests/testthat/", filter = "read") #battle

# test_dir(path = "./tests/testthat/",
#          env = shiny::loadSupport("../../R"),
#          reporter = c("progress", "fail"),
#          filter = "") #battle
