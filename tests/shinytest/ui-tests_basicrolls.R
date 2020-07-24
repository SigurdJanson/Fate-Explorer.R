library(shinytest)
library(webdriver)
library(xml2)
library(rvest)

TestSeed <- 1233

# BASIC APP ----------------
test_that("Tabs", {
  app <- ShinyDriver$new(path = "../../R")
  expect_equal(app$findWidget("uiTabset")$listTabs(), 
               c("Sei", "Handle", "Kämpfe", "Setup", "Über..."))
  app$stop() # Shiny-App stoppen
})



# ABILITY TAB ##########################################################
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
  app <- ShinyDriver$new(path = "../../R", seed = TestSeed)
  for (i in 1:length(ExpectedVal)) {
    app$setInputs(doAbilityRoll = "click")
    app$waitForValue("AbilityRoll", ignore = list(NULL, ""), iotype = "output")
    
    Result <- ExtractAbilityRoll(app)
    expect_identical(Result[["Value"]], ExpectedVal[i])
    
    KeyResult <- Result[["SuccessLevel"]]
    if (i <= length(ExpectedResult) && TestSeed == 1233)
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



# SKILLS TAB ##########################################################
ExtractSkillRoll <- function(app) {
  output <- xml2::read_html( app$getValue(name = "SkillRoll") )
  QLText <- html_text(html_node(output, ".ql"))
  LevelText <- html_text(html_node(output, ".keyresult"))
  
  Roll_3d20 <- html_text(html_node(output, "table.table")) # .shiny-table.spacing-s > thead > tr > td
  Roll_3d20 <- unlist(strsplit(Roll_3d20, "\\n"))
  Roll_3d20 <- Roll_3d20[grep("\\d", Roll_3d20)]
  Roll_3d20 <- as.integer(Roll_3d20)
  
  if (length(Roll_3d20) == 8) {
    Ab <- Roll_3d20[1:3]; Sk <- Roll_3d20[4]
    AbRoll <- Roll_3d20[5:7]; SkRoll <- Roll_3d20[8]
  } else if (length(Roll_3d20) == 3) {
    Ab <- NA; Sk <- NA
    AbRoll <- Roll_3d20[1:3]; SkRoll <- NA
  }
  return( list(QL = as.integer(QLText), 
               Roll = as.integer(AbRoll),
               SuccessLevel = LevelText) )
}


test_that("Plain Skill Rolls", {
  #
  # Create testing sequence
  set.seed(TestSeed)
  ExpectedVal <- list()
  for(i in 1:10) ExpectedVal <- c(ExpectedVal, list(sample.int(20L, 3L, replace = TRUE)))
  # default values are 11/11/11 and 4 on the skill
  ExpectedResult <- c("Erfolg", "Meisterlich", "Erfolg", "Patzer", rep("Gescheitert", 2), rep("Erfolg", 2), rep("Gescheitert", 2), "Erfolg")
  ExpectedQuality <- as.integer(c(1, 2, 2, rep(0, 3), rep(1, 2), rep(0, 2)))
  
  #
  #
  for (SkillSource in c('NoSkill', 'ManualSkill')) {
    app <- ShinyDriver$new(path = "../../R", seed = TestSeed)
    # Goto 2. tab
    ts <- app$findWidget("uiTabset")
    ts$setValue("Handle")
    expect_identical(ts$getValue(), "Handle")
    
    #
    # Rolls with default values (Abilities == 11, 11, 11; Skill == 4; Mod == 0)
    #
    app$setValue("rdbSkillSource", SkillSource)
    
    for (i in 1:length(ExpectedVal)) {
      app$setInputs(doSkillRoll = "click")
      app$waitForValue("SkillRoll", ignore = list(NULL, ""), iotype = "output")
      
      Result <- ExtractSkillRoll(app)
      
      expect_identical(Result[["Roll"]], ExpectedVal[[i]], 
                       label = paste("Roll /", SkillSource))
      
      if (SkillSource == "ManualSkill") {
        expect_identical(Result[["QL"]], ExpectedQuality[i], 
                         label = paste("QL /", SkillSource)) # Skill required

        # Only relevant for rolls against a skill value but this is a plain roll
        KeyResult <- Result[["SuccessLevel"]]
        if (i <= length(ExpectedResult) && TestSeed == 1233)
          expect_identical(KeyResult, ExpectedResult[i], 
                           label = paste("Success /", SkillSource))
        else
          succeed(message = "Result only defined for TestSeed of 1233")
      }
    }
    
    #
    # Create a fake roll and change ability, skill value and modifier
    #
    # app$setValue("rdbSkillSource", "ManualSkill")
    # 
    # app$setValue("SkillTrait1", 1)
    # app$setValue("SkillTrait2", 1)
    # app$setValue("SkillTrait3", 1)
    
    app$stop() # Shiny-App stoppen
  }# for(SkillSources...)
  
})


# COMBAT TAB ##########################################################
test_that("Combat Actions", {
  #
  # Create testing sequence
  set.seed(TestSeed)
  ExpectedVal <- list()
  for(i in 1:10) ExpectedVal <- c(ExpectedVal, list(sample.int(20L, 3L, replace = TRUE)))
  # default values are 11/11/11 and 4 on the skill
  ###ExpectedResult <- c("Erfolg", "Meisterlich", "Erfolg", "Patzer", rep("Gescheitert", 2), rep("Erfolg", 2), rep("Gescheitert", 2), "Erfolg")
  ###ExpectedQuality <- as.integer(c(1, 2, 2, rep(0, 3), rep(1, 2), rep(0, 2)))
  
  #
  #
  app <- ShinyDriver$new(path = "../../R", seed = TestSeed)
  for (CombatAction in c("Attack", "Parry", "Dodge")) {
    switch (CombatAction,
            Attack = app$setInputs(doAttackThrow = "click"),
            Parry = app$setInputs(doParryThrow = "click"),
            Dodge = app$setInputs(doDodgeRoll = "click")
    )
    #app$setInputs(doSkillRoll = "click")
    # Action <- list("click")
    # names(Action) <- "doAttackThrow"
    # do.call(app$setInputs, Action)
    #app$waitForValue("uiCombatRoll", ignore = list(NULL, ""), iotype = "output")
    
    #expectUpdate(app, doAttackThrow = 1, output = "uiCombatRoll")
    #expectUpdate(app, doParryThrow = 1, output = "uiCombatRoll")
    #expectUpdate(app, doDodgeRoll = 1, output = "uiCombatRoll")
    
  }# for(SkillSources...)
  app$stop() # Shiny-App stoppen
  
})