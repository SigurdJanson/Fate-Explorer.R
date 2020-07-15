library(shinytest)
library(webdriver)
library(xml2)
library(rvest)

TestSeed <- 1233


test_that("Tabs", {
  app <- ShinyDriver$new(path = "../src")
  expect_equal(app$findWidget("uiTabset")$listTabs(), c("Sei", "Handle", "Kämpfe", "Setup", "Über..."))
  app$stop() # Shiny-App stoppen
})

# ABILITY TAB -------------
ExtractAbilityRoll <- function(app) {
  output <- xml2::read_html( app$getValue(name = "AbilityRoll") )
  ValText <- html_text(html_node(output, ".dr"))
  LevelText <- html_text(html_node(output, ".keyresult"))
  return( list(Value = as.integer(ValText), 
          SuccessLevel = LevelText) )
}


test_that("Ability", {

set.seed(TestSeed)
ExpectedVal <- integer()
for(i in 1:50) ExpectedVal <- c(ExpectedVal, sample.int(20L, 1L))
ExpectedResult <- c("Gescheitert", rep("Erfolg", 3), rep("Meisterlich", 2), rep("Erfolg", 3), "Patzer")

# Test basic random sequence
app <- ShinyDriver$new(path = "../src", seed = TestSeed)
for (i in 1:length(ExpectedVal)) {
  app$setInputs(doAbilityRoll = "click")
  app$waitForValue("AbilityRoll", ignore = list(NULL, ""), iotype = "output")
  
  Result <- ExtractAbilityRoll(app)
  expect_identical(Result[["Value"]], ExpectedVal[i])
  
  KeyResult <- Result[["SuccessLevel"]]
  if (i <= 10 && TestSeed == 1233)
    expect_identical(KeyResult, ExpectedResult[i])
  else
    succeed(message = "Result only defined for TestSeed of 1233")
}

#
# Test reactivity to changes in slider
LastValue <- Result[["Value"]]
for (Ability in 1:20) {
  app$setValue("inpAbility", Ability)
  app$waitForValue("AbilityRoll", ignore = list(NULL, ""), iotype = "output")
  
  Result <- ExtractAbilityRoll(app)
  expect_identical(Result[["Value"]], LastValue)
  if (Ability < Result[["Value"]]) {
    expect_identical(Result[["SuccessLevel"]], "Gescheitert")
  } else {
    expect_identical(Result[["SuccessLevel"]], "Erfolg")
  }
}


#
# Test reactivity to changes of the modifier slider
Ability <- 10
app$setValue("inpAbility", Ability)
app$waitForValue("AbilityRoll", ignore = list(NULL, ""), iotype = "output")

for (m in c(-10, -5, -1, 0, 1, 5)) {
  app$setValue("inpAbilityMod", m)
  app$waitForValue("AbilityRoll", ignore = list(NULL, ""), iotype = "output")
  
  Result <- ExtractAbilityRoll(app)
  expect_identical(Result[["Value"]], LastValue) # shall *not* change
  if (Ability+m < Result[["Value"]]) {
    expect_identical(Result[["SuccessLevel"]], "Gescheitert", label = paste(m, LastValue, Result[["Value"]], Result[["SuccessLevel"]]))
  } else {
    expect_identical(Result[["SuccessLevel"]], "Erfolg", label = paste(m, LastValue, Result[["Value"]], Result[["SuccessLevel"]]))
  }
  
}



app$stop() # Shiny-App stoppen
})



# SKILLS TAB -------------
test_that("Skills", {
  succeed(message = "not yet implemented")
})