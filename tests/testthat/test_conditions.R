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
  lays <- data.frame(
    conditionID = NULL,
    level1      = NULL,
    level2	    = NULL,
    level3      = NULL,
    level4      = NULL
  )
  
  Encumbrance <- list(
    name = "Belastung",
    attrID = "COND_5",
    url = Url,
    modifiers = mods,
    layovers = NULL
  )
  
  return(Encumbrance)
}


# Constructors -----
test_that("Initialize properly", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  
  expect_silent(
    cond <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  )
  
  expect_identical(cond$GetName(), TestName)
  expect_identical(cond$GetId(), TestId)
  expect_identical(cond$GetUrl(), TestUrl)
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
  
  # explicitly specifying the `to` argument
  expect_identical(cond$ChangeLevel(to = 1L)$GetLevel(), 1L)
  # Implicitely choosing `to`
  expect_identical(cond$ChangeLevel(2L)$GetLevel(), 2L)
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
test_that("Modifiers", {
  TestName <- "Belastung"
  TestId <- "COND_5"
  TestUrl <- "www.somewhere.com"
  cond <- ConditionBase$new(GetConditionTemplate(TestName, TestId, TestUrl))
  
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
  
})


