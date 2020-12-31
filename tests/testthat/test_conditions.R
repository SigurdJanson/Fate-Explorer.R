#require(jsonlite)

.testdir <- getwd()
  setwd("../../R")
  #.srcdir <- getwd()
  #print(.srcdir)
  source("conditions.R")
setwd(.testdir)

GetConditionTemplate <- function(Name, Id, Url) {
  ## Test condition "encumbrance"
  mods <- data.frame(
    actionID = c("TAL_", "LITURGY_", "SPELL_", "AT", "PA", "AW", "INI"),
    level1   = rep(-1L, 7),
    level2	 = rep(-2L, 7),
    level3   = rep(-3L, 7),
    level4   = rep(-99L, 7)
  )
  carry <- data.frame(
    conditionID = NULL,
    level1      = NULL,
    level2	    = NULL,
    level3      = NULL,
    level4      = NULL
  )
  
  Encumbrance <- list(
    name = Name,
    attrID = Id,
    url = Url,
    levels = paste0("L-", 1:4),
    modifiers = mods,
    carryovers = NULL
  )
  
  return(Encumbrance)
}

GetConditionTemplateJson <- function() {
  JSON <- '{"Encumbrance":{"name":"Belastung","attrID":"COND_5","url":"index.php/Sta_Belastung.html","modifiers":[{"actionID":"TAL_","level1":-1,"level2":-2,"level3":-3,"level4":-99},{"actionID":"LITURGY_","level1":-1,"level2":-2,"level3":-3,"level4":-99},{"actionID":"SPELL_","level1":-1,"level2":-2,"level3":-3,"level4":-99},{"actionID":"AT","level1":-1,"level2":-2,"level3":-3,"level4":-99},{"actionID":"PA","level1":-1,"level2":-2,"level3":-3,"level4":-99},{"actionID":"AW","level1":-1,"level2":-2,"level3":-3,"level4":-99},{"actionID":"INI","level1":-1,"level2":-2,"level3":-3,"level4":-99}],"carryovers":{}}}'
  data <- fromJSON(JSON, simplifyVector = TRUE)
  return(data[[1]])
}


# Constructors -----
test_that("Initialize from data structure", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  
  expect_silent(
    cond <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  )
  
  expect_identical(cond$GetName(), TestName)
  expect_identical(cond$GetId(), TestId)
  expect_identical(cond$GetUrl(), TestUrl)
  expect_identical(cond$GetLevel(), 0L)
  expect_identical(cond$GetLevelName(), "-")
  expect_identical(cond$GetLevelNames(), paste0("L-", 1:4))
})


# This test is needed especially to check if integers are read correctly 
# and not imported as doubles
test_that("Initialize from JSON", {
  expect_silent(
    cond <- ConditionBase$new(GetConditionTemplateJson())
  )
  
  expect_identical(cond$GetName(), "Belastung")
  expect_identical(cond$GetId(), "COND_5")
  expect_identical(cond$GetUrl(), "index.php/Sta_Belastung.html")
})




# Setting Levels -----
test_that("Setting levels: error handling", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  cond <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))

  # Either `to` or `by` but never both
  expect_error(cond$ChangeLevel(to = 1, by = -1), "")
  
  # `to` outside range
  expect_error(cond$ChangeLevel(to = -1), "")
  for (l in 0:4)
    expect_silent(cond$ChangeLevel(to = l))
  expect_error(cond$ChangeLevel(to = 5), "")
  
  # `by` outside range
  expect_error(cond$ChangeLevel(by = -5L), "")
  for (l in -4L:4L)
    expect_silent(cond$ChangeLevel(by = l))
  expect_error(cond$ChangeLevel(by = 5L), "")
  
})


test_that("Setting levels: to", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  cond <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))

  # Initialize all conditions at level 0
  expect_identical(cond$GetLevel(), 0L)
  expect_identical(cond$GetLevelName(), "-")
  
  # explicitly specifying the `to` argument
  expect_identical(cond$ChangeLevel(to = 1L)$GetLevel(), 1L)
  expect_identical(cond$GetLevelName(), "L-1")
  # Implicitely choosing `to`
  expect_identical(cond$ChangeLevel(2L)$GetLevel(), 2L)
  expect_identical(cond$GetLevelName(), "L-2")
})


test_that("Setting levels: by", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  cond <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  
  # Initialize all conditions at level 0
  expect_identical(cond$GetLevel(), 0L)
  
  expect_identical(cond$ChangeLevel(by = +1L)$GetLevel(), 1L)
  expect_identical(cond$ChangeLevel(by = -1L)$GetLevel(), 0L)
})



# Get Modifiers -----

# Test twice, once with data from generated list and 
# once with data from parsed JSON
test_that("Modifiers", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  TestConditions <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  TestConditions <- list(TestConditions, ConditionBase$new(GetConditionTemplateJson()))

  for(cond in TestConditions) {
    for (level in 0L:3L) {
      cond$ChangeLevel(to = level)
      
      Skill <- paste0("TAL_", sample(1:59, 1))
      expect_identical(cond$GetModifier(Skill), -level)
      Chant <- paste0("LITURGY_", sample(1:192, 1))
      expect_identical(cond$GetModifier(Chant), -level)
      expect_identical(cond$GetModifier("UNKNOWN ACTION"), 0L)
    }
    
    level <- 4L
    cond$ChangeLevel(to = level)
    Skill <- paste0("TAL_", sample(1:59, 1))
    expect_identical(cond$GetModifier(Skill), -99L)
    Chant <- paste0("LITURGY_", sample(1:192, 1))
    expect_identical(cond$GetModifier(Chant), -99L)
    expect_identical(cond$GetModifier("UNKNOWN ACTION"), 0L)
  }
})


test_that("ANY Modifiers", {
  # Setup condition
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  Template <- GetConditionTemplate(TestName, TestId, TestUrl)
  Template$modifiers[1, "actionID"] <- c("")
  cond <- ConditionBase$new(Template)
  
  # TEST - any string should work
  cond$ChangeLevel(to = 1)
  expect_identical(cond$GetModifier("UNKNOWN ACTION"), -1L)
  cond$ChangeLevel(to = 2)
  expect_identical(cond$GetModifier("BULLSHIT"), -2L)
})


test_that("ANY Modifiers as second qualifier", {
  # Setup condition
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  Template <- GetConditionTemplate(TestName, TestId, TestUrl)
  Template$modifiers <- data.frame(
    actionID = c("TAL_", "AT", ""),
    level1   = rep(-1L, 3),
    level2	 = rep(-2L, 3),
    level3   = rep(-3L, 3),
    level4   = c(rep(-4L, 2), -99L)
  )
  cond <- ConditionBase$new(Template)
  
  # TEST - any string should work
  cond$ChangeLevel(to = 2)
  expect_identical(cond$GetModifier("AT"), -2L)
  expect_identical(cond$GetModifier("TAL_42"), -2L)
  expect_identical(cond$GetModifier("TAL_189"), -2L)
  expect_identical(cond$GetModifier("UNKNOWN ACTION"), -2L)
  expect_identical(cond$GetModifier("BULLSHIT"), -2L)

  cond$ChangeLevel(to = 4)
  expect_identical(cond$GetModifier("AT"), -4L)
  expect_identical(cond$GetModifier("TAL_42"), -4L)
  expect_identical(cond$GetModifier("TAL_189"), -4L)
  expect_identical(cond$GetModifier("UNKNOWN ACTION"), -99L)
  expect_identical(cond$GetModifier("BULLSHIT"), -99L)
})



# Carryovers -----
test_that("No carryovers", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  cond <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  
  expect_null(cond$GetCarryoverIds())
})


test_that("Carryovers at level 2 and 4: upwards", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  Template <- GetConditionTemplate(TestName, TestId, TestUrl)
  
  # Add carryover condition: with a carryover at level 2 and 4
  carry <- data.frame(
    conditionID = c("COND_9"),
    level1      = c(0L),
    level2	    = c(+1L),
    level3      = c(0L),
    level4      = c(+2L)
  )
  Template$carryovers <- carry
  
  # ... and create condition COND_5
  cond5 <- ConditionBase$new(Template)
  
  # Create carryover condition COND_9
  TestName <- "Carryover"
  TestId <- "COND_9"
  TestUrl <- "www.elsewhere.com"
  cond9 <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  
  # TEST!
  # check if condition is aware of the carryover
  expect_identical(c("COND_9"), cond5$GetCarryoverIds())
  # level 0 is just level 0
  expect_identical(0L, cond5$GetLevel())
  expect_identical(0L, cond9$GetLevel())
  # level 1, no carryover
  expect_identical(1L, cond5$ChangeLevel(by = 1L, Others = list(cond9))$GetLevel())
  expect_identical(0L, cond9$GetLevel())
  # level 2, carryover of +1
  expect_identical(2L, cond5$ChangeLevel(by = 1L, Others = list(cond9))$GetLevel())
  expect_identical(1L, cond9$GetLevel())
  # level 3, carryover is back to 0
  expect_identical(3L, cond5$ChangeLevel(by = 1L, Others = list(cond9))$GetLevel())
  expect_identical(0L, cond9$GetLevel())
  # level 4, carryover of +2
  expect_identical(4L, cond5$ChangeLevel(by = 1L, Others = list(cond9))$GetLevel())
  expect_identical(2L, cond9$GetLevel())
})


test_that("Carryovers at level 2 and 4: backwards", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  Template <- GetConditionTemplate(TestName, TestId, TestUrl)
  
  # Add carryover condition: with a carryover at level 2 and 4
  carry <- data.frame(
    conditionID = c("COND_9"),
    level1      = c(0L),
    level2	    = c(+1L),
    level3      = c(0L),
    level4      = c(+2L)
  )
  Template$carryovers <- carry
  
  # ... and create condition COND_5
  cond5 <- ConditionBase$new(Template)

  # Create carryover condition COND_9
  TestName <- "Carryover"
  TestId <- "COND_9"
  TestUrl <- "www.elsewhere.com"
  cond9 <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  
  # TEST!
  # check if condition is aware of the carryover
  expect_identical(c("COND_9"), cond5$GetCarryoverIds())
  # level 0 is just level 0
  expect_identical(0L, cond5$GetLevel())
  expect_identical(0L, cond9$GetLevel())
  # level 4, carryover of +2
  expect_identical(4L, cond5$ChangeLevel(to = +4L, Others = list(cond9))$GetLevel())
  expect_identical(2L, cond9$GetLevel())
  # level 3, carryover is back to 0
  expect_identical(3L, cond5$ChangeLevel(by = -1L, Others = list(cond9))$GetLevel())
  expect_identical(0L, cond9$GetLevel())
  # level 2, carryover of +1
  expect_identical(2L, cond5$ChangeLevel(by = -1L, Others = list(cond9))$GetLevel())
  expect_identical(1L, cond9$GetLevel())
  # level 1, no carryover
  expect_identical(1L, cond5$ChangeLevel(by = -1L, Others = list(cond9))$GetLevel())
  expect_identical(0L, cond9$GetLevel())
  # level 0 is just level 0
  expect_identical(0L, cond5$ChangeLevel(by = -1L, Others = list(cond9))$GetLevel())
  expect_identical(0L, cond9$GetLevel())
})


