# app test
library(shinytest)
library(testthat)

# open Shiny app and PhantomJS ----
app <- ShinyDriver$new(path = "../src", seed = 1)

# RECORDING ----
#recordTest("./src", save_dir = file.path(getwd(), "test", "ui_recs"), seed = 1)


# TESTING ----
test_that("Abilities", {

  # Click: expected value = 4
  app$setInputs(doAbilityRoll = "click")
  output <- app$getValue(name = "AbilityRoll")
  expect_identical(grep("game-icon-laurels", output), 1L) 
  expect_identical(grep("Erfolg", output), 1L) 
  
  # Click: expected value = 7
  app$setInputs(doAbilityRoll = "click")
  output <- app$getValue(name = "AbilityRoll")
  expect_identical(grep("game-icon-laurels", output), 1L) 
  expect_identical(grep("Erfolg", output), 1L) 
  
  # Click: expected value = 1
  app$setInputs(doAbilityRoll = "click")
  output <- app$getValue(name = "AbilityRoll")
  expect_identical(grep("game-icon-laurel-crown", output), 1L) 
  expect_identical(grep("Meisterlich", output), 1L) 

  # Click: expected value = 2
  app$setInputs(doAbilityRoll = "click")
  output <- app$getValue(name = "AbilityRoll")
  expect_identical(grep("game-icon-laurels", output), 1L) 
  expect_identical(grep("Erfolg", output), 1L)
  
  # Reduce ability to force a fail
  app$setInputs(inpAbility = 1)
  output <- app$getValue(name = "AbilityRoll")
  expect_identical(grep("game-icon-spectre", output), 1L) 
  expect_identical(grep("Gescheitert", output), 1L)
})


# stop the Shiny app ------------
app$stop()
