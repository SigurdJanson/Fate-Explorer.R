setwd("..")
source("./R/helpers.R", encoding = "UTF-8")
setwd("./tests")

# Helper Functions ----
test_that("replace_umlauts", {
  # PRECONDITIONS

  # 
  text <- "äöüÄÖÜ"
  o <- replace_umlauts(text)
  e <- "aeoeueAEOEUE"
  expect_identical(o, e)
  
  # 
  text <- "In einen Täxt gehören auch Umlaute"
  o <- replace_umlauts(text)
  e <- "In einen Taext gehoeren auch Umlaute"
  expect_identical(o, e)
  
  #
  text <- "Keine Umlaute hier drin"
  o <- replace_umlauts(text)
  e <- text
  expect_identical(o, e)
  
  #
  text <- ""
  o <- replace_umlauts(text)
  e <- text
  expect_identical(o, e)

})


# 
# test_that("", {
# })
# 
# 
# 
# test_that("", {
# })
# 
# 
# 
# test_that("", {
# })

