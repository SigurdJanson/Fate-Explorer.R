setwd("..")
source("./src/rules.R")
setwd("./test")

# Functions ----
test_that("CombatTechniques", {
  setwd("..")
  CT <- Rules$CombatTechniques()
  setwd("./test")
  expect_s3_class(CT, "data.frame")
  expect_named(CT, paste0("CT_", 1:21))
  expect_equal(nrow(CT), 1)
  expect_equal(ncol(CT), 21)
})


test_that("Abilities", {
  setwd("..")
  AB <- Rules$Abilities()
  setwd("./test")
  expect_s3_class(AB, "data.frame")
  expect_named(AB, c("attrID", "shortname", "name"))
  expect_equal(nrow(AB), 8)
  expect_equal(ncol(AB), 3)
})


test_that("Weapons", {
  cn <- c("name", "technik", "leiteigenschaft", "schwelle", "grundschaden",
          "bonus", "at", "pa", "rw", "gewicht", "preis", "bf", "combattechID")
  setwd("..")
  W <- Rules$Weapons()
  setwd("./test")
  expect_s3_class(W, "data.frame")
  expect_equal(nrow(W), 150)
  expect_equal(ncol(W), 13)
  expect_named(W, c("at", "pa", "preis"), ignore.order = TRUE)
})
