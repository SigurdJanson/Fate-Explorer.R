library(testthat)
library(jsonlite)
library(shiny)

.testdir <- getwd()
setwd("../../R")
  .srcdir <- getwd()
  source("./rules.R")
  source("./battleground.R")
setwd(.testdir)


# Initialised with Defaults ---------------

# This test uses the fact that a Combat Environment in it's default configuration
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



test_that("Init class: GetCombatEnvironment", {
  for (w in names(.WeaponType)) {
    BattleGround <- CombatEnvironment$new(.WeaponType[w])

    setwd(.srcdir)###
    CombatEnv <- BattleGround$GetCombatEnvironment(w)
    setwd(.testdir)###

    if (w == "Ranged") {
      expect_identical(CombatEnv$Hero$MeansOfMovement, .MeansOfMovement["OnFoot"], label = w)
      expect_identical(CombatEnv$Hero$Movement, .Movement["Stationary"], label = w)
      expect_identical(CombatEnv$Opponent$Movement, .Movement["Slow"], label = w)
      expect_identical(CombatEnv$Opponent$TargetDistance, .TargetDistance["Medium"], label = w)
      # obsolete because close combat but there nevertheless
      expect_identical(CombatEnv$Hero$CloseCombatRange, .CloseCombatRange["Short"], label = w)
      expect_identical(CombatEnv$Opponent$CloseCombatRange, .CloseCombatRange["Short"], label = w)
      expect_identical(CombatEnv$Environment$CrampedSpace, .CrampedSpace["Free"], label = w)
    }  else if (w == "Melee") {
      expect_identical(CombatEnv$Hero$CloseCombatRange, .CloseCombatRange["Short"], label = w)
      expect_identical(CombatEnv$Opponent$CloseCombatRange, .CloseCombatRange["Short"], label = w)
      expect_identical(CombatEnv$Environment$CrampedSpace, .CrampedSpace["Free"], label = w)
      # obsolete because ranged combat but there nevertheless
      expect_identical(CombatEnv$Hero$MeansOfMovement, .MeansOfMovement["OnFoot"], label = w)
      expect_identical(CombatEnv$Hero$Movement, .Movement["Stationary"], label = w)
      expect_identical(CombatEnv$Opponent$Movement, .Movement["Stationary"], label = w)
      expect_identical(CombatEnv$Opponent$TargetDistance, .TargetDistance["Close"], label = w)
    }
  }
})


# Directly after initialization the combat environment of the object shall
# be identical to the default generated environment.
test_that("Init class: GetCombatEnvironment = GetDefaultCombatEnvironment", {
  for (w in names(.WeaponType)) {
    BattleGround <- CombatEnvironment$new(.WeaponType[w])

    setwd(.srcdir)###
    CombatEnv  <- BattleGround$GetCombatEnvironment(w)
    DefaultEnv <- BattleGround$GetDefaultCombatEnvironment(w)
    setwd(.testdir)###

    CombatEnvStr  <- capture.output(str(CombatEnv))
    DefaultEnvStr <- capture.output(str(DefaultEnv))

    expect_identical(CombatEnvStr, DefaultEnvStr)
  }
})


# Properties ---------------
test_that("Active Property: WeaponType", {
  BattleGround <- CombatEnvironment$new(.WeaponType[1])
  for (w in names(.WeaponType)) {
    BattleGround$WeaponType <- .WeaponType[w]
    expect_identical(BattleGround$WeaponType, .WeaponType[w])
  }
})

test_that("Active Property: (Close)CombatRange", {
  BattleGround <- CombatEnvironment$new(.WeaponType[1])
  for (cr in names(.CloseCombatRange)) {
    BattleGround$CombatRange <- .CloseCombatRange[cr]
    expect_identical(BattleGround$CombatRange, .CloseCombatRange[cr])
  }
})

test_that("Active Property: TargetSize", {
  BattleGround <- CombatEnvironment$new(.WeaponType[1])
  for (ts in names(.TargetSize)) {
    BattleGround$TargetSize <- .TargetSize[ts]
    expect_identical(BattleGround$TargetSize, .TargetSize[ts])
  }
})

test_that("Active Property: Visibility", {
  BattleGround <- CombatEnvironment$new(.WeaponType[1])
  for (v in names(.Visibility)) {
    BattleGround$Visibility <- .Visibility[v]
    expect_identical(BattleGround$Visibility, .Visibility[v])
  }
})

test_that("Active Property: CrampedSpace", {
  BattleGround <- CombatEnvironment$new(.WeaponType[1])
  for (cs in names(.CrampedSpace)) {
    BattleGround$CrampedSpace <- .CrampedSpace[cs]
    expect_identical(BattleGround$CrampedSpace, .CrampedSpace[cs])
  }
})

test_that("Active Property: UnderWater", {
  BattleGround <- CombatEnvironment$new(.WeaponType[1])
  for (u in names(.UnderWater)) {
    BattleGround$UnderWater <- .UnderWater[u]
    expect_identical(BattleGround$UnderWater, .UnderWater[u])
  }
  # Invalid input
  expect_error(BattleGround$UnderWater <- 99)
  # Input is NA or NULL resets to default
  BattleGround$UnderWater <- NA
  expect_identical(BattleGround$UnderWater, BattleGround$getDefault(groupId = "Environment", "UnderWater"))
  BattleGround$UnderWater <- sample(.UnderWater, 1L)
  BattleGround$UnderWater <- NULL
  expect_identical(BattleGround$UnderWater, BattleGround$getDefault(groupId = "Environment", "UnderWater"))
})


# getValue / getDefault ----------------
test_that("getValue", {
  BattleGround <- CombatEnvironment$new(.WeaponType[1])

  o <- BattleGround$getValue(valueId = "UnderWater") #
  expect_identical(o, .UnderWater["Dry"])

  BattleGround$UnderWater <- tail(.UnderWater, 1L)
  o <- BattleGround$getValue(valueId = "UnderWater") #
  expect_identical(o, tail(.UnderWater, 1L))

  o <- BattleGround$getValue(valueId = "TargetSize") #
  expect_identical(o, .TargetSize["Medium"])

  BattleGround$TargetSize <- head(.TargetSize, 1L)
  o <- BattleGround$getValue(valueId = "TargetSize") #
  expect_identical(o, head(.TargetSize, 1L))

})


test_that("getDefault", {
  BattleGround <- CombatEnvironment$new(.WeaponType["Unarmed"])

  o <- BattleGround$getDefault(valueId = "UnderWater") #
  expect_identical(o, .UnderWater["Dry"])
  # Default other than 1L
  o <- BattleGround$getValue(valueId = "TargetSize") #
  expect_identical(o, .TargetSize["Medium"])

  o <- BattleGround$getDefault(valueId = "This valueId does not exist ... I am sure of it") #
  expect_true(is.na(o))

  # Edge cases
  # "UnderWater" is unambiguous so this works
  o <- BattleGround$getDefault(groupId = "This does not match anything I know", valueId = "UnderWater") #
  expect_false(is.na(o))
  # "CloseCombatRange" is NOT unambiguous so this returns NA
  o <- BattleGround$getDefault(groupId = "This does not match anything I know", valueId = "CloseCombatRange") #
  expect_true(is.na(o))
})

