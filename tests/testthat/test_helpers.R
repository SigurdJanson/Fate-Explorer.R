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


test_that("allTruthy", {
  # 1 - level
  expect_true( allTruthy(1, 2, 3) )
  # 2 - level
  expect_true( allTruthy(1:3, 5:2, letters[1:5]) )

  # 1 - level NA
  expect_false( allTruthy(1, 2, NA) )
  # 1st level NA
  expect_false( allTruthy(1:3, NA, c(2, 3, 4)) )
  # 1st level NA with lists
  expect_false( allTruthy(as.list(1:3), NA, list(2, 3, 4)) )
  # 2nd level NA
  expect_false( allTruthy(1:3, c(2, NA, 4), 3:5) )
  # 2nd level NA with lists
  expect_false( allTruthy(as.list(1:3), list(2, NA, 4), as.list(3:5)) )

  # 1st level NaN
  expect_false( allTruthy(1:3, c(2, 3, 4), NaN) )
  # 2nd level NaN
  expect_false( allTruthy(1:3, c(2, NaN, 4), 3:5) )

  # 1st level integer(0)
  expect_false( allTruthy(1:3, c(2, 3, 4), integer()) )
  # 2nd level integer(0) - lists only because that won't work with vectors
  expect_false( allTruthy(1:3, list(2, integer(), 4), 3:5) )
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

