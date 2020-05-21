setwd("..")
source("./src/rules.R")
setwd("./test")

# Functions ----
test_that("CombatTechniques", {
  #setwd("..")
  CT <- GetCombatTechniques()
  #setwd("./test")
  expect_s3_class(CT, "data.frame")
  expect_named(CT, paste0("CT_", 1:21))
  expect_equal(nrow(CT), 1)
  expect_equal(ncol(CT), 21)
})


test_that("Abilities", {
  #setwd("..")
  AB <- GetAbilities()
  #setwd("./test")
  expect_s3_class(AB, "data.frame")
  expect_named(AB, c("attrID", "shortname", "name"))
  expect_equal(nrow(AB), 8)
  expect_equal(ncol(AB), 3)
})


test_that("Weapons", {
  cn <- c("name", "technik", "leiteigenschaft", "schwelle", "grundschaden",
          "bonus", "at", "pa", "rw", "gewicht", "preis", "bf", 
          "combattechID", "primeattrID")
  #setwd("..")
  W <- GetWeapons()
  #setwd("./test")
  expect_s3_class(W, "data.frame")
  expect_equal(nrow(W), 150)
  expect_equal(ncol(W), 14)
  expect_named(W, cn, ignore.order = TRUE)
  
  #setwd("..")
  W <- GetWeapons("Waqqif")
  expect_equal(W$name, "Waqqif")
  #setwd("./test")
  
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