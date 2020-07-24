require(jsonlite)
setwd("../src")
source("./dicelogic.R")
source("./rules.R")
setwd("../test")

# General -----
test_that("VerifyConfirmation", {
  o <- VerifyConfirmation("Success", "Anything")
  e <- "Success"
  expect_identical(o, e)

  o <- VerifyConfirmation("Fail", "Something")
  e <- "Fail"
  expect_identical(o, e)
  
  o <- VerifyConfirmation("Critical", "Critical")
  e <- "Critical"
  expect_identical(o, e)
  o <- VerifyConfirmation("Critical", "Success")
  e <- "Critical"
  expect_identical(o, e)
  o <- VerifyConfirmation("Critical", "Fail")
  e <- "Success"
  expect_identical(o, e)
  o <- VerifyConfirmation("Critical", "Fumble")
  e <- "Success"
  expect_identical(o, e)
  
  o <- VerifyConfirmation("Fumble", "Critical")
  e <- "Fail"
  expect_identical(o, e)
  o <- VerifyConfirmation("Fumble", "Success")
  e <- "Fail"
  expect_identical(o, e)
  o <- VerifyConfirmation("Fumble", "Fail")
  e <- "Fumble"
  expect_identical(o, e)
  o <- VerifyConfirmation("Fumble", "Fumble")
  e <- "Fumble"
  expect_identical(o, e)
})


test_that("GetFumbleEffect", {
  # PRECONDITIONS
  expect_error(GetFumbleEffect(), "No roll given")
  expect_error(GetFumbleEffect(1.9), "Invalid fumble roll")
  expect_error(GetFumbleEffect(12.1), "Invalid fumble roll")
  setwd("../src")
  expect_silent(GetFumbleEffect(12, "S", "Magic"))
  setwd("../test")
  expect_error(GetFumbleEffect(12, "M")) #does not exist
  
  # 
  o <- GetFumbleEffect(2L, "Skill", "Magic")
  expect_identical(o, list(id = "FMBL_31", 
                           label = "Seelentausch", 
                           descr = "Der Geist des Zauberers tauscht für 1W6 Tage den Körper mit dem nächsten Lebewesen in seiner Nähe, das größer ist als eine Ratte."))
  o <- GetFumbleEffect(12L, "Skill", "Liturgical")
  expect_identical(o, list(id = "FMBL_61", 
                           label = "Verwurzelung", 
                           descr = "Der Geweihte verwurzelt mit dem Boden und kann seine Füße für 1W6 Minuten nicht bewegen. Er erhält währenddessen den Status Fixiert."))
  o <- GetFumbleEffect(3L, "Attack", "Melee")
  expect_identical(o, list(id = "FMBL_2", 
                           label = "Waffe schwer beschädigt", 
                           descr = "Die Waffe ist nicht mehr einsetzbar, bis sie repariert wird. Bei unzerstörbaren Waffen wird das Ergebnis wie bei 5 behandelt."))
  o <- GetFumbleEffect(3L, "Attack", "Unarmed")
  expect_identical(o, list(id = "FMBL_14", 
                           label = "Stolpern", 
                           descr = "Der Held stolpert, seine nächste Handlung ist um 2 erschwert."))
  o <- GetFumbleEffect(8L, "Attack", "Ranged")
  expect_identical(o, list(id = "FMBL_7", 
                           label = "Zerrung", 
                           descr = "Der Held hat Rückenschmerzen und erleidet für die nächsten 3 Kampfrunden eine Stufe Schmerz."))
  
  o <- GetFumbleEffect(10L, "Dodge", "Melee")
  expect_identical(o, list(id = "FMBL_16", 
                           label = "Beule", 
                           descr = "Der Held hat sich im Eifer des Gefechts den Kopf gestoßen. Er erhält für eine Stunde eine Stufe Betäubung."))
  o <- GetFumbleEffect(4L, "Dodge", "Shield")
  expect_identical(o, list(id = "FMBL_15", 
                           label = "Fuß verdreht", 
                           descr = "Der Held erhält für 3 Kampfrunden eine Stufe Schmerz."))
})



# Combat ----
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
      o <- DamageRoll(D = 1L, Mod = m)
      expect_gte(o, 1+m)
      expect_lte(o, 6+m)
    }
  }
  # 2W6 + Modifier
  for (m in c(0, 1, 4, 8, 12)) {  #Modifier
    for (i in 1:25) {
      o <- DamageRoll(D = 2L, Mod = m)
      expect_gte(o, 2+m)
      expect_lte(o, 12+m)
    }
  }
  
})



test_that("FumbleRoll", {
  for(i in 1:100) {
    o <- FumbleRoll()
    expect_gte(o, 2)
    expect_lte(o, 12)
  }
})



# Skill -------------

test_that("SkillRollQuality", {
  expect_identical(SkillRollQuality(-1), 0L)
  
  Result <- c(rep(1L,4), rep(2:6, each = 3L))
  for(i in 0:18) {
    expect_identical(SkillRollQuality(i), Result[i+1])
  }
  
})

test_that("VerifySkillRoll", {
  
  # Different rolls
  for (r in 2:10) {
    o <- VerifySkillRoll(rep(r, 3), Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = 0L)
    expect_identical(o, list(Message = "Success", QL = 1L, Remainder = 0L))
  }
  for (r in 11:19) {
    o <- VerifySkillRoll(rep(r, 3), Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = 0L)
    expect_identical(o, list(Message = "Fail", QL = 0L, Remainder = -3L*(r-10L)))
  }
  o <- VerifySkillRoll(c(1, 20, 1), Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = 0L)
  expect_identical(o, list(Message = "Critical", QL = 1L, Remainder = 0L))
  o <- VerifySkillRoll(c(20, 20, 1), Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = 0L)
  expect_identical(o, list(Message = "Fumble", QL = 0L, Remainder = 0L))
  
  # Different mods
  for (m in -5L:-1L) {
    o <- VerifySkillRoll(rep(10L, 3), Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = m)
    expect_identical(o, list(Message = "Fail", QL = 0L, Remainder = 3L*m))
  }
  for (m in 0L:5L) {
    o <- VerifySkillRoll(rep(10L, 3), Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = m)
    expect_identical(o, list(Message = "Success", QL = 1L, Remainder = 0L))
  }

  # A skill < 0 is always a fail (even with fantastic abilities and divine mod)
  o <- VerifySkillRoll(rep(10L, 3), Abilities = c(19L, 17L, 18L), Skill = -1L, Modifier = 99L)
  expect_identical(o, list(Message = "Fail", QL = 0L, Remainder = -1L))
})


test_that("CanRoutineSkillCheck / RoutineCheck", {
  # Preconditions
  expect_error(CanRoutineSkillCheck(10:11, Skill, 0), "Three abilities make a skill check")
  expect_error(VerifyRoutineSkillCheck(10:11, Skill, 0), "Three abilities make a skill check")
  expect_error(CanRoutineSkillCheck(11:14, Skill, 0), "Three abilities make a skill check")
  expect_error(VerifyRoutineSkillCheck(11:14, Skill, 0), "Three abilities make a skill check")
  expect_error(CanRoutineSkillCheck(11:13, numeric(), 0), "Exactly one skill value is needed for skill check")
  expect_error(VerifyRoutineSkillCheck(11:13, numeric(), 0), "Exactly one skill value is needed for skill check")
  expect_error(CanRoutineSkillCheck(11:13, 2:1, 0), "Exactly one skill value is needed for skill check")
  expect_error(VerifyRoutineSkillCheck(11:13, 2:1, 0), "Exactly one skill value is needed for skill check")
  
  # Never possible because ability[3] < 13
  Abilities <- c(13, 13, 12)
  Mod <- 3
  for (Skill in 0L:12L) {
    expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), FALSE)
    expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Fail", QL = 0L, Remainder = "."))
  }
  Skill <- 12
  for (Mod in -3L:3L) {
    expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), FALSE)
    expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Fail", QL = 0L, Remainder = "."))
  }
  
  # Depends on combination of Skill & Mod
  Abilities <- c(13, 13, 13)
  Skill <- 0 # way too low
  Mod <- 0
  expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), FALSE)
  expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Fail", QL = 0L, Remainder = "."))
  Skill <- 9 # too low
  Mod <- 0
  expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), FALSE)
  expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Fail", QL = 0L, Remainder = "."))
  Skill <- 10 # just enough
  Mod <- 0
  expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), TRUE)
  expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Success", QL = 2L, Remainder = "."))
  
  Skill <- 1 
  Mod <- -3 # way too low
  expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), FALSE)
  expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Fail", QL = 0L, Remainder = "."))
  Skill <- 1 
  Mod <- 2 # too low
  expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), FALSE)
  expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Fail", QL = 0L, Remainder = "."))
  Skill <- 1 
  Mod <- 3 # just enough
  expect_identical(CanRoutineSkillCheck(Abilities, Skill, Mod), TRUE)
  expect_identical(VerifyRoutineSkillCheck(Abilities, Skill, Mod), list(Message = "Success", QL = 1L, Remainder = "."))
})