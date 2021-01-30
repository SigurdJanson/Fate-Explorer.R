library(testthat)
library(jsonlite)
library(shiny)

.testdir <- getwd()
setwd("../../R")
  .srcdir <- getwd()
  source("./rules.R")
  source("./battleground.R")
setwd(.testdir)


# test_that("", {
#   # When there is no vision AT = AT /2 ----> 9 / 2 shall not be 4.5 but 5
#   Check        <- c(at = 9L, pa = 10L, do = 10L)
#   BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], WithObsoletes = FALSE,
#                                                `Environment$Visibility` = .Visibility["NoVision"])
#   setwd(.srcdir)
#   o <- ModifyCheck(Check, BattleGround)
#   setwd(.testdir)
#   e <- c(at = 5L, pa = 1L, do = 1L)
#   expect_identical(o, e, label = names(.Visibility["NoVision"]))
# })


test_that("defaultCombatEnvironment", {
  # A default environment is one without modifiers
  Check        <- c(Attack = 9L, Parry = 8L, Dodge = 7L)
  for (w in names(.WeaponType)) {
    BattleGround <- defaultCombatEnvironment(w)
      setwd(.srcdir)
    o <- ModifyCheck(Check, BattleGround)
      setwd(.testdir)
    expect_identical(o, Check, label = w)
  }
})


