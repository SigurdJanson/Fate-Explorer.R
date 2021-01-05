library(shiny)

.testdir <- getwd()
setwd("../..")
source("./R/helpers.R", encoding = "UTF-8")
setwd(.testdir)

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



test_that("RollButtonLabel", {
  Id <- "doButtonA"
  Label <- "TestLabel"
  CssClass <- ""
  
  o <- RollButtonLabel(Id, Label) # , Result = NULL, inProgress = FALSE
  e <- sprintf("<span id=\"lbl%s\" class=\"%s\">%s</span>", Id, CssClass, Label)
  expect_identical(o, e)
  
  # Same as before only explicitely
  o <- RollButtonLabel(Id, Label, Result = NULL, inProgress = FALSE)
  e <- sprintf("<span id=\"lbl%s\" class=\"%s\">%s</span>", Id, CssClass, Label)
  expect_identical(o, e)
  
  # with Result 
  o <- RollButtonLabel(Id, Label, Result = 1) # , inProgress = FALSE
  e <- sprintf("<span id=\"lbl%s\" class=\"%s\">%s (1)</span>", Id, CssClass, Label)
  expect_identical(o, e)
  
  CssClass <- "loading dots"
  o <- RollButtonLabel(Id, Label, Result = 1, inProgress = TRUE)
  e <- sprintf("<span id=\"lbl%s\" class=\"%s\">%s (1)</span>", Id, CssClass, Label)
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

