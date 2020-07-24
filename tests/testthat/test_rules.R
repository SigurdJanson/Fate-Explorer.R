library(testthat)
library(jsonlite)
library(shiny)

.testdir <- getwd()
setwd("../../R")
.srcdir <- getwd()
source("./rules.R")
setwd(.testdir)



# Functions ----
test_that("CombatTechniques", {
  setwd(.srcdir)
  CT <- GetCombatTechniques()
  setwd(.testdir)
  expect_s3_class(CT, "data.frame")
  expect_equal(CT[[1]], paste0("CT_", 1:21))
  expect_equal(nrow(CT), 21)
  expect_equal(ncol(CT), 5)
})


test_that("Abilities", {
  setwd(.srcdir)
  AB <- GetAbilities()
  setwd(.testdir)
  expect_s3_class(AB, "data.frame")
  expect_named(AB, c("attrID", "shortname", "name"))
  expect_equal(nrow(AB), 8)
  expect_equal(ncol(AB), 3)
})

test_that("Skills", {
  setwd(.srcdir)
  SK <- GetSkills()
  setwd(.testdir)
  expect_s3_class(SK, "data.frame")
  expect_named(SK, c("attrID", "name", "class", "classID", "ab1", "ab2", "ab3"))
  expect_equal(nrow(SK), 59)
  expect_equal(ncol(SK), 7)
})



test_that("Weapons", {
  # PRECONDITIONS
  expect_error(GetWeapons(Type = "Blabla"), "'arg' sollte eines  von '“Melee”, “Unarmed”, “Ranged”, “Any”' sein")#".{25}\"Melee\", \"Unarmed\", \"Ranged\", \"Any\".*")
  expect_error(GetWeapons(Which = "All", Type = "Any"), "Invalid combination of arguments.")
  
  # MELEE WEAPONS
  cn <- c("name", "combattech", "primeattr", "threshold", "damage",
          "bonus", "at", "pa", "range", "weight", "price", "sf", 
          "combattechID", "primeattrID", "improvised", "url", "clsrng", 
          "armed", "templateID")
  setwd(.srcdir)
  W <- GetWeapons()
  setwd(.testdir)
  expect_s3_class(W, "data.frame")
  expect_equal(nrow(W), 184)
  expect_equal(ncol(W), length(cn))
  expect_named(W, cn, ignore.order = TRUE)
  
  W <- GetWeapons("Waqqif") # Use name, "Melee" is default
  expect_equal(W$name, "Waqqif")
  expect_equal(W$templateID, "ITEMTPL_7")
  setwd(.srcdir)
  W <- GetWeapons("Waqqif", "Any") # Use name
  setwd(.testdir)
  expect_equal(W$name, "Waqqif")
  expect_equal(W$templateID, "ITEMTPL_7")

  W <- GetWeapons("Waqqif", "Unarmed") # same result as "Melee"
  expect_equal(W$name, "Waqqif")
  expect_equal(W$templateID, "ITEMTPL_7")
  
  W <- GetWeapons("Waqqif", "Ranged") # Use name
  expect_equal(nrow(as.data.frame(W)), 0)
  
  
  W <- GetWeapons("ITEMTPL_549") # Use optholit ID
  expect_equal(W$name, "Nostrisches Langschwert")
  
  # RANGED WEAPONS
  cn <- c("combattech", "loadtime", "damage", "name", 
          "bonus", "ammo", "range", "weight", "price", "sf", 
          "combattechID", "improvised", "url", "clsrng", 
          "armed", "templateID", "primeattr", "primeattrID")
  setwd(.srcdir)
  W <- GetWeapons(Type = "Ranged")
  setwd(.testdir)
  expect_s3_class(W, "data.frame")
  expect_equal(nrow(W), 52)
  expect_equal(ncol(W), length(cn))
  expect_named(W, cn, ignore.order = TRUE)
  
  W <- GetWeapons("Waqqif", "Ranged") # Use name
  expect_equal(nrow(W), NULL) # close combat weapon
  W <- GetWeapons("Fledermaus", "Ranged") 
  expect_equal(W$name, "Fledermaus")
  expect_equal(W$templateID, NA_character_)
  
  W <- GetWeapons("ITEMTPL_539", "Ranged") # Use optholit ID
  expect_equal(W$name, "Windhager Schleuder")
})



test_that("PrimaryWeaponAttribute", {
  o <- GetPrimaryWeaponAttribute("Waqqif")
  e <- "ATTR_6"
  expect_identical(o, e)
  
  o <- GetPrimaryWeaponAttribute("Barbarenschwert")
  e <- c("ATTR_6", "ATTR_8")
  expect_identical(o, e)
  
  o <- GetPrimaryWeaponAttribute("Turnierlanze")
  e <- NA_character_
  expect_identical(o, e)
})


test_that("PrimaryWeaponAttribute", {
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 11L), 
                  class = "data.frame", row.names = c(NA, -1L))
  
  o <- GetHitpointBonus("Waqqif", ab) # thrshold: 14
  e <- 2L
  expect_identical(o, e)
  
  o <- GetHitpointBonus("Barbarenschwert", ab) # thrshold: 15
  e <- 1L
  expect_identical(o, e)
  
  o <- GetHitpointBonus("Turnierlanze", ab) # no threshold
  e <- 0L
  expect_identical(o, e)

  
  # ATTR_6 and 8 are both > 15 but 8 is larger
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 17L), 
                  class = "data.frame", row.names = c(NA, -1L))
  o <- GetHitpointBonus("Barbarenschwert", ab)# thrshold: 15
  e <- 2L
  expect_identical(o, e)

  # ATTR_6 and 8 are both below threshold of 15
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 15L, ATTR_7 = 11L, ATTR_8 = 15L), 
                  class = "data.frame", row.names = c(NA, -1L))
  o <- GetHitpointBonus("Barbarenschwert", ab) # thrshold: 15
  e <- 0L
  expect_identical(o, e)
})



test_that("IsRangedWeapon", {
  # PRECONDITIONS
  expect_error(IsRangedWeapon(), "No arguments to define weapon")
  
  # Melee
  expect_identical(IsRangedWeapon("Shakagra-Krummsäbel"), FALSE)
  expect_identical(IsRangedWeapon("Bratspieß"), FALSE)
  expect_identical(IsRangedWeapon("Ogerschelle"), FALSE)
  expect_identical(IsRangedWeapon("Zwergenschlägel"), FALSE)
  
  expect_identical(IsRangedWeapon("ITEMTPL_15"), FALSE)
  expect_identical(IsRangedWeapon("ITEMTPL_35"), FALSE)
  expect_identical(IsRangedWeapon("ITEMTPL_476"), FALSE)
  expect_identical(IsRangedWeapon("ITEMTPL_584"), FALSE)
  
  # Ranged
  expect_identical(IsRangedWeapon("Schwere Armbrust"), TRUE)
  expect_identical(IsRangedWeapon("Wurfspeer"), TRUE)
  expect_identical(IsRangedWeapon("Feuerspeien"), TRUE)
  expect_identical(IsRangedWeapon("Diskus"), TRUE)
  
  expect_identical(IsRangedWeapon("ITEMTPL_60"), TRUE)
  expect_identical(IsRangedWeapon("ITEMTPL_70"), TRUE)
  expect_identical(IsRangedWeapon("ITEMTPL_465"), TRUE)
  expect_identical(IsRangedWeapon("ITEMTPL_540"), TRUE)
  
  # By combat technique
  ct <- paste0("CT_", c(1:21))
  expect_identical(IsRangedWeapon(CombatTech = ct), 
                   c(TRUE, TRUE, rep(FALSE, 8), 
                     TRUE, rep(FALSE, 2), 
                     TRUE, rep(FALSE, 2), 
                     rep(TRUE, 3), rep(FALSE, 2)))
})



test_that("IsParryWeapon", {
  # PRECONDITIONS
  expect_error(IsParryWeapon(), "No arguments to define weapon")

  expect_identical(IsParryWeapon("Shakagra-Krummsäbel"), TRUE)
  expect_identical(IsParryWeapon("Bratspieß"), TRUE)
  expect_identical(IsParryWeapon("Ogerschelle"), FALSE)
  expect_identical(IsParryWeapon("Zwergenschlägel"), TRUE)
  
  expect_identical(IsParryWeapon("ITEMTPL_15"), TRUE)
  expect_identical(IsParryWeapon("ITEMTPL_24"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_449"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_584"), TRUE)
  
  # Ranged
  expect_identical(IsParryWeapon("Schwere Armbrust"), FALSE)
  expect_identical(IsParryWeapon("Wurfspeer"), FALSE)
  expect_identical(IsParryWeapon("Feuerspeien"), FALSE)
  expect_identical(IsParryWeapon("Diskus"), FALSE)
  
  expect_identical(IsParryWeapon("ITEMTPL_60"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_70"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_465"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_540"), FALSE)
  
  # By combat technique
  ct <- paste0("CT_", c(1:21))
  o <- IsParryWeapon(CombatTech = ct)
  expect_identical(o, 
                   c(FALSE, FALSE, rep(TRUE, 3), #5
                     rep(FALSE, 3), rep(TRUE, 2), #10
                     FALSE, rep(TRUE, 2), #10
                     FALSE, rep(TRUE, 2), 
                     rep(FALSE, 3), TRUE, FALSE))
  expect_identical(IsParryWeapon(CombatTech = "CT_17"), FALSE) # spit fire
  expect_identical(IsParryWeapon(CombatTech = "CT_12"), TRUE) # sword
})