library(testthat)
library(jsonlite)
library(shiny)

.testdir <- getwd()
setwd("../../R")
.srcdir <- getwd()
source("./weapon.R")
setwd(.testdir)

test_that("", {
  Expected <- "Some String to Test cAllbacK"
  Observed <- NULL
  TestCallback <- function(Weapon) {
    # Weapon is an R6 class
    Observed <<- Expected
  }
  Name <- "Waqqif"
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 11L), 
                  class = "data.frame", row.names = c(NA, -1L))
  ct <- list(CT_3 = 15, CT_9 = 15, CT_12 = 12, CT_14 = 13)
  setwd(.srcdir)
  W <- MeleeWeapon$new(Name, ab, ct)
  setwd(.testdir)
  
  # ERROR: trying to set non-function as callback
  expect_error(W$RegisterOnValueChange("Go home, lunatic!"))
  
  # No registered callback, no effect
  W$Name <- "Waqqif without Callback"
  expect_null(Observed)
  
  # Callback sets a string
  expect_silent(W$RegisterOnValueChange(TestCallback))
  W$Name <- "Dangerous Waqqif"
  expect_identical(Observed, Expected)
  
  # Remove callback
  Observed <- NULL # reset to `NULL` and change `Name`
  expect_silent(W$UnregisterOnValueChange(TestCallback))
  W$Name <- "Awesome Waqqif"
  expect_null(Observed)
})