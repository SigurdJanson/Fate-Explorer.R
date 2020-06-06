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
  
  for(s in 0:20) { #20 skill levels
    for(r in 2:19) {#rolls
      for(p in 0:(-5)) {
        o <- VerifyCombatRoll(r, s, p)
        e <- ifelse(r <= s+p, "Success", "Fail")
        expect_identical(o, e, info = paste(r, s))
      }
    }
  }
  
})



test_that("DamageRoll", {
  # 1W6 + Modifier
  for (m in c(0, 1, 2, 4, 8, 12)) {  #Modifier
    for (i in 1:25) {
      o <- DamageRoll(1, m)
      expect_gte(o, 1+m)
      expect_lte(o, 6+m)
    }
  }
  # 2W6 + Modifier
  for (m in c(0, 1, 4, 8, 12)) {  #Modifier
    for (i in 1:25) {
      o <- DamageRoll(2, m)
      expect_gte(o, 2+m)
      expect_lte(o, 12+m)
    }
  }
  
})



test_that("CombatFumbleRoll", {
  for(i in 1:100) {
    o <- CombatFumbleRoll()
    expect_gte(o, 2)
    expect_lte(o, 12)
  }
})



test_that("GetCombatFumbleEffect", {
  expect_error(GetCombatFumbleEffect(0))
  expect_error(GetCombatFumbleEffect(1))
  expect_error(GetCombatFumbleEffect(13))
  
  for(r in 2:12) {
    o <- expect_silent(GetCombatFumbleEffect(r))
    expect_gte(length(o), 0)
  }
  
  expect_identical(GetCombatFumbleEffect(2), "Weapon destroyed")
  expect_identical(GetCombatFumbleEffect(12), "Hurt yourself bad")
})




test_that("SkillRollQuality", {
  expect_identical(SkillRollQuality(-1), 0L)
  
  Result <- c(rep(1L,4), rep(2:6, each = 3L))
  for(i in 0:18) {
    expect_identical(SkillRollQuality(i), Result[i+1])
  }
  
})