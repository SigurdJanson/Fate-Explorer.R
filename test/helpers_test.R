setwd("..")
source("./src/dicelogic.R")
setwd("./test")

# Helper Functions ----
test_that("VerifyCombatRoll", {
  # PRECONDITIONS
  expect_error(VerifyCombatRoll(0, 20))
  expect_error(VerifyCombatRoll(21, 20))
  expect_error(VerifyCombatRoll(1, -1))
  # Criticals!!! Skill makes no difference
  for(s in 0:20) { #20 skill levels
    r <- 1
    o <- VerifyCombatRoll(r, s)
    e <- "Critical"
    expect_identical(o, e, info = paste(r, s))
  }
  # Fumbles!!! Skill makes no difference
  for(s in 0:20) { #20 skill levels
    r <- 20
    o <- VerifyCombatRoll(r, s)
    e <- "Fumble"
    expect_identical(o, e, info = paste(r, s))
  }
  
  for(s in 0:20) { #20 skill levels
    for(r in 2:19) {#20 rolls
      o <- VerifyCombatRoll(r, s)
      e <- ifelse(r <= s, "Success", "Fail")
      expect_identical(o, e, info = paste(r, s))
    }
  }
})