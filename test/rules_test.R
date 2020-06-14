library(testthat)
library(jsonlite)

setwd("..")
source("./src/rules.R")
setwd("./test")



# Functions ----
test_that("CombatTechniques", {
  setwd("../src")
  CT <- GetCombatTechniques()
  setwd("../test")
  expect_s3_class(CT, "data.frame")
  expect_named(CT, paste0("CT_", 1:21))
  expect_equal(nrow(CT), 1)
  expect_equal(ncol(CT), 21)
})


test_that("Abilities", {
  setwd("../src")
  AB <- GetAbilities()
  setwd("../test")
  expect_s3_class(AB, "data.frame")
  expect_named(AB, c("attrID", "shortname", "name"))
  expect_equal(nrow(AB), 8)
  expect_equal(ncol(AB), 3)
})

test_that("Skills", {
  setwd("../src")
  SK <- GetSkills()
  setwd("../test")
  expect_s3_class(SK, "data.frame")
  expect_named(SK, c("attrID", "name", "class", "classID", "ab1", "ab2", "ab3"))
  expect_equal(nrow(SK), 59)
  expect_equal(ncol(SK), 7)
})



test_that("Weapons", {
  cn <- c("name", "technik", "leiteigenschaft", "schwelle", "grundschaden",
          "bonus", "at", "pa", "rw", "gewicht", "preis", "bf", 
          "combattechID", "primeattrID", "improvised", "url")
  setwd("../src")
  W <- GetWeapons()
  setwd("../test")
  expect_s3_class(W, "data.frame")
  expect_equal(nrow(W), 184)
  expect_equal(ncol(W), 16)
  expect_named(W, cn, ignore.order = TRUE)
  
  W <- GetWeapons("Waqqif")
  expect_equal(W$name, "Waqqif")
  
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


