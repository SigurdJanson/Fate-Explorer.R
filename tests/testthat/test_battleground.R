library(testthat)
library(jsonlite)
library(shiny)

.testdir <- getwd()
setwd("../../R")
  .srcdir <- getwd()
  source("./rules.R")
  source("./battleground.R")
setwd(.testdir)

# This test uses the fact that a Combat Environent in it's default configuration
# shall not affect the combat abilities.
test_that("Init class", {
  Check        <- c(at = 9L, pa = 10L, do = 10L)

  for (w in names(.WeaponType)) {
    BattleGround <- CombatEnvironment$new(.WeaponType[w])
    expect_type(BattleGround, "environment")
    expect_s3_class(BattleGround, "R6")

    setwd(.srcdir)###

    CombatEnv <- BattleGround$GetCombatEnvironment(w)
    expect_type(CombatEnv, "list")

    # Content: check only values that are universal across weapon types
    expect_identical(CombatEnv$Hero$WeaponType, .WeaponType[w])
    expect_identical(CombatEnv$Opponent$TargetSize, .TargetSize["Medium"])
    expect_identical(CombatEnv$Environment$Visibility, .Visibility["Clearly"])
    expect_identical(CombatEnv$Environment$UnderWater, .UnderWater["Dry"])


    # `Check` shall be unchanged
    o <- ModifyCheck(Check, CombatEnv)
    expect_identical(o, Check, info = w)

    setwd(.testdir)###
  }
})



test_that("Init class", {
  for (w in names(.WeaponType)) {
    BattleGround <- CombatEnvironment$new(.WeaponType[w])

    setwd(.srcdir)###
    CombatEnv <- BattleGround$GetCombatEnvironment(w)
    setwd(.testdir)###

    if (w == "Ranged") {
      expect_identical(CombatEnv$Hero$MeansOfMovement, .MeansOfMovement["OnFoot"], label = w)
      expect_identical(CombatEnv$Hero$Movement, .Movement["Stationary"], label = w)
      expect_identical(CombatEnv$Opponent$Movement, .Movement["Slow"], label = w)
      expect_identical(CombatEnv$Opponent$Distance, .TargetDistance["Medium"], label = w)

      expect_null(CombatEnv$Hero$CloseCombatRange)
      expect_null(CombatEnv$Opponent$CloseCombatRange)
      expect_null(CombatEnv$Environment$CrampedSpace)
    } else if (w == "Melee") {
      expect_null(CombatEnv$Hero$MeansOfMovement)
      expect_null(CombatEnv$Hero$Movement)
      expect_null(CombatEnv$Opponent$Movement)
      expect_null(CombatEnv$Opponent$Distance)

      expect_identical(CombatEnv$Hero$CloseCombatRange, .CloseCombatRange["Short"], label = w)
      expect_identical(CombatEnv$Opponent$CloseCombatRange, .CloseCombatRange["Short"], label = w)
      expect_identical(CombatEnv$Environment$CrampedSpace, .CrampedSpace["Free"], label = w)
    }
  }
})


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


# test_that("defaultCombatEnvironment", {
#   # A default environment is one without modifiers
#   Check        <- c(Attack = 9L, Parry = 8L, Dodge = 7L)
#   for (w in names(.WeaponType)) {
#     BattleGround <- defaultCombatEnvironment(w)
#       setwd(.srcdir)
#     o <- ModifyCheck(Check, BattleGround)
#       setwd(.testdir)
#     expect_identical(o, Check, label = w)
#   }
# })


