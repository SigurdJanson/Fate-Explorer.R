library(testthat)
library(jsonlite)
library(shiny)

.testdir <- getwd()
setwd("../../R")
.srcdir <- getwd()
source("./weapon.R")
setwd(.testdir)

test_that("", {
  Name <- "Waqqif"
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 11L), 
                  class = "data.frame", row.names = c(NA, -1L))
  ct <- list(CT_3 = 15, CT_9 = 15, CT_12 = 12, CT_14 = 13)
  setwd(.srcdir)
  W <- MeleeWeapon$new(Name, ab, ct)
  setwd(.testdir)
  
  expect_identical(W$Name, Name)
  expect_identical(W$Type, .WeaponType["Melee"])
  expect_identical(W$Technique, "CT_3")
  #-# Distance
  expect_identical(W$Skill, list(Attack = 16, Parry = 9, Dodge = 8))
  expect_identical(W$Damage$N, 1L)
  expect_identical(W$Damage$DP, 6L)
  expect_identical(W$Damage$Bonus, 2L+2L) # Base value 2 + [ATTR_6 (GE) > 14]

  # Rolling
  for(A in names(.CombatAction))
    for(i in 1:100) {
      W$Roll(A)
      expect_identical(W$LastAction, .CombatAction[A])
      expect_gte(W$LastRoll, 1L)
      expect_lte(W$LastRoll, 20L)
      if(A == "Attack" && W$LastResult %in% .SuccessLevel[c("Critical", "Success")]) {
        expect_gte(W$LastDamage, 5L)
        expect_lte(W$LastDamage, 10L)
      }
      expect_identical(W$LastModifier, 0L)
      if(W$LastRoll == 20) 
        expect_identical(W$LastResult, .SuccessLevel["Fumble"])
      if(W$LastRoll == 1) 
        expect_identical(W$LastResult, .SuccessLevel["Critical"])
      if(W$LastRoll == 20 || W$LastRoll == 1)
        expect_identical(W$ConfirmationMissing, TRUE)
    }
  
  # WAFFENLOS
  Name <- "Waffenlos"
  W <- MeleeWeapon$new(Name, ab, ct)
  expect_identical(W$Name, Name)
  expect_identical(W$Type, .WeaponType["Unarmed"])
  expect_identical(W$Technique, "CT_9")
  expect_identical(W$Damage$N, 1L)
  expect_identical(W$Damage$DP, 6L)
  expect_identical(W$Damage$Bonus, 0L+2L) # Base value 0 + [ATTR_6 (GE) > 14]
  
  # Fake weapon with 1d3 
  W$Damage$DP <- 3L
  for(i in 1:30) {
    W$Roll("Attack")
    expect_identical(W$LastAction, .CombatAction["Attack"])
    expect_gte(W$LastRoll, 1L)
    expect_lte(W$LastRoll, 20L)
    if(W$LastResult %in% .SuccessLevel[c("Critical", "Success")]) {
      expect_gte(W$LastDamage, 3L)
      expect_lte(W$LastDamage, 5L)
    }
  }
})