library(testthat)
library(jsonlite)
library(shiny)

.testdir <- getwd()
setwd("../../R")
.srcdir <- getwd()
source("./rules.R")
setwd(.testdir)



# Data Base Access Functions ----
test_that("CombatTechniques", {
  setwd(.srcdir)
  CT <- GetCombatTechniques()
  setwd(.testdir)
  expect_s3_class(CT, "data.frame")
  expect_equal(CT[[1]], paste0("CT_", 1:21))
  expect_equal(nrow(CT), 21)
  expect_equal(ncol(CT), 6)
})


test_that("Abilities", {
  setwd(.srcdir)
  AB <- GetAbilities()
  setwd(.testdir)
  expect_s3_class(AB, "data.frame")
  expect_named(AB, c("attrID", "shortname", "name"))
  expect_equal(nrow(AB), 8)
  expect_equal(ncol(AB), 3)
})

test_that("Skills", {
  setwd(.srcdir)
  SK <- GetSkills()
  setwd(.testdir)
  expect_s3_class(SK, "data.frame")
  expect_named(SK, c("attrID", "name", "class", "classID", "ab1", "ab2", "ab3"))
  expect_equal(nrow(SK), 59)
  expect_equal(ncol(SK), 7)
})




# Combat Functions ----
## TODO: GetCombatTechniques()

test_that("GetWeapons(): PRECONDITIONS", {
  # PRECONDITIONS
  expect_error(GetWeapons(Type = "Blabla"), "'arg' sollte eines  von '“Melee”, “Unarmed”, “Ranged”, “Any”' sein")
  expect_error(GetWeapons(Which = "All", Type = "Any"), "Invalid combination of arguments.")
})

test_that("Weapons from database", {
  # MELEE WEAPONS
  cn <- c("name", "combattech", "primeattr", "threshold", "damage",
          "bonus", "at", "pa", "range", "weight", "price", "sf", 
          "combattechID", "primeattrID", "improvised", "url", "clsrng", 
          "armed", "templateID")
  setwd(.srcdir)
  W <- GetWeapons() # Force the function to load the data
  setwd(.testdir)

  expect_s3_class(W, "data.frame")
  expect_equal(nrow(W), 184)
  expect_equal(ncol(W), length(cn))
  expect_named(W, cn, ignore.order = TRUE)
  
  W <- GetWeapons("Waqqif") # Use name, "Melee" is default
  expect_equal(W$name, "Waqqif")
  expect_equal(W$templateID, "ITEMTPL_7")
  setwd(.srcdir)
  W <- GetWeapons("Waqqif", "Any") # Use name
  setwd(.testdir)
  expect_equal(W$name, "Waqqif")
  expect_equal(W$templateID, "ITEMTPL_7")

  W <- GetWeapons("Waqqif", "Unarmed") # same result as "Melee"
  expect_equal(W$name, "Waqqif")
  expect_equal(W$templateID, "ITEMTPL_7")
  
  W <- GetWeapons("Waqqif", "Ranged") # Use name
  expect_equal(nrow(as.data.frame(W)), 0)
  
  
  W <- GetWeapons("ITEMTPL_549") # Use Optolith ID
  expect_equal(W$name, "Nostrisches Langschwert")
  
  # RANGED WEAPONS
  cn <- c("combattech", "loadtime", "damage", "name", 
          "bonus", "ammo", "range", "weight", "price", "sf", 
          "combattechID", "improvised", "url", "clsrng", 
          "armed", "templateID", "primeattr", "primeattrID")
  setwd(.srcdir)
  W <- GetWeapons(Type = "Ranged")
  setwd(.testdir)
  expect_s3_class(W, "data.frame")
  expect_equal(nrow(W), 52)
  expect_equal(ncol(W), length(cn))
  expect_named(W, cn, ignore.order = TRUE)
  
  W <- GetWeapons("Waqqif", "Ranged") # Use name
  expect_equal(nrow(W), NULL) # close combat weapon
  W <- GetWeapons("Fledermaus", "Ranged") 
  expect_equal(W$name, "Fledermaus")
  expect_equal(W$templateID, NA_character_)
  
  W <- GetWeapons("ITEMTPL_539", "Ranged") # Use Optolith ID
  expect_equal(W$name, "Windhager Schleuder")
  
  # Weapon NOT FOUND
  W <- GetWeapons("UNKNOWN WEAPON THAT DOES NOT EXIST") # Use wrong Optolith ID
  expect_identical(W, NULL)
  W <- GetWeapons("UNKNOWN WEAPON THAT DOES NOT EXIST", "Ranged") # Use wrong Optolith ID
  expect_identical(W, NULL)
})



test_that("PrimaryWeaponAttribute", {
  o <- GetPrimaryWeaponAttribute("Waqqif")
  e <- "ATTR_6"
  expect_identical(o, e)
  
  o <- GetPrimaryWeaponAttribute("Barbarenschwert")
  e <- c("ATTR_6", "ATTR_8")
  expect_identical(o, e)
  
  o <- GetPrimaryWeaponAttribute("Turnierlanze")
  e <- "ATTR_8"
  expect_identical(o, e)
})

test_that("GetPrimaryWeaponAttributeByCombatTechnique()", {
  # PRECONDITION
  expect_error(GetPrimaryWeaponAttributeByCombatTechnique())
  
  # Normal techniques
  o <- GetPrimaryWeaponAttributeByCombatTechnique("CT_1")
  expect_identical(o, "ATTR_5")
  
  o <- GetPrimaryWeaponAttributeByCombatTechnique("CT_10")
  expect_identical(o, "ATTR_8")
  
  o <- GetPrimaryWeaponAttributeByCombatTechnique("CT_21")
  expect_identical(o, "ATTR_8")
  
  # Two primary attributes in techique
  o <- GetPrimaryWeaponAttributeByCombatTechnique("CT_9")
  expect_identical(o, c("ATTR_6", "ATTR_8"))
  
  # Combat technique does not exist
  o <- GetPrimaryWeaponAttributeByCombatTechnique("CT_0")
  expect_identical(o, character())
  o <- GetPrimaryWeaponAttributeByCombatTechnique("CT_22")
  expect_identical(o, character())
  
})



test_that("GetHitpointBonus", {
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 11L), 
                  class = "data.frame", row.names = c(NA, -1L))
  
  o <- GetHitpointBonus("Waqqif", ab) # thrshold: 14
  e <- 2L
  expect_identical(o, e)
  
  o <- GetHitpointBonus("Barbarenschwert", ab) # thrshold: 15
  e <- 1L
  expect_identical(o, e)
  
  o <- GetHitpointBonus("Turnierlanze", ab) # no threshold
  e <- 0L
  expect_identical(o, e)

  
  # ATTR_6 and 8 are both > 15 but 8 is larger
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 17L), 
                  class = "data.frame", row.names = c(NA, -1L))
  o <- GetHitpointBonus("Barbarenschwert", ab)# thrshold: 15
  e <- 2L
  expect_identical(o, e)

  # ATTR_6 and 8 are both below threshold of 15
  ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L, 
                       ATTR_5 = 13L, ATTR_6 = 15L, ATTR_7 = 11L, ATTR_8 = 15L), 
                  class = "data.frame", row.names = c(NA, -1L))
  o <- GetHitpointBonus("Barbarenschwert", ab) # threshold: 15
  e <- 0L
  expect_identical(o, e)
})



test_that("IsRangedWeapon", {
  # PRECONDITIONS
  expect_error(IsRangedWeapon(), "No arguments to define weapon")
  
  # Melee
  expect_identical(IsRangedWeapon("Shakagra-Krummsäbel"), FALSE)
  expect_identical(IsRangedWeapon("Bratspieß"), FALSE)
  expect_identical(IsRangedWeapon("Ogerschelle"), FALSE)
  expect_identical(IsRangedWeapon("Zwergenschlägel"), FALSE)
  
  expect_identical(IsRangedWeapon("ITEMTPL_15"), FALSE)
  expect_identical(IsRangedWeapon("ITEMTPL_35"), FALSE)
  expect_identical(IsRangedWeapon("ITEMTPL_476"), FALSE)
  expect_identical(IsRangedWeapon("ITEMTPL_584"), FALSE)
  
  # Ranged
  expect_identical(IsRangedWeapon("Schwere Armbrust"), TRUE)
  expect_identical(IsRangedWeapon("Wurfspeer"), TRUE)
  expect_identical(IsRangedWeapon("Feuerspeien"), TRUE)
  expect_identical(IsRangedWeapon("Diskus"), TRUE)
  
  expect_identical(IsRangedWeapon("ITEMTPL_60"), TRUE)
  expect_identical(IsRangedWeapon("ITEMTPL_70"), TRUE)
  expect_identical(IsRangedWeapon("ITEMTPL_465"), TRUE)
  expect_identical(IsRangedWeapon("ITEMTPL_540"), TRUE)
  
  # By combat technique
  ct <- paste0("CT_", c(1:21))
  expect_identical(IsRangedWeapon(CombatTech = ct), 
                   c(TRUE, TRUE, rep(FALSE, 8), 
                     TRUE, rep(FALSE, 2), 
                     TRUE, rep(FALSE, 2), 
                     rep(TRUE, 3), rep(FALSE, 2)))
})



test_that("IsParryWeapon", {
  # PRECONDITIONS
  expect_error(IsParryWeapon(), "No arguments to define weapon")

  expect_identical(IsParryWeapon("Shakagra-Krummsäbel"), TRUE)
  expect_identical(IsParryWeapon("Bratspieß"), TRUE)
  expect_identical(IsParryWeapon("Ogerschelle"), FALSE)
  expect_identical(IsParryWeapon("Zwergenschlägel"), TRUE)
  
  expect_identical(IsParryWeapon("ITEMTPL_15"), TRUE)
  expect_identical(IsParryWeapon("ITEMTPL_24"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_449"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_584"), TRUE)
  
  # Ranged
  expect_identical(IsParryWeapon("Schwere Armbrust"), FALSE)
  expect_identical(IsParryWeapon("Wurfspeer"), FALSE)
  expect_identical(IsParryWeapon("Feuerspeien"), FALSE)
  expect_identical(IsParryWeapon("Diskus"), FALSE)
  
  expect_identical(IsParryWeapon("ITEMTPL_60"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_70"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_465"), FALSE)
  expect_identical(IsParryWeapon("ITEMTPL_540"), FALSE)
  
  # By combat technique
  ct <- paste0("CT_", c(1:21))
  o <- IsParryWeapon(CombatTech = ct)
  expect_identical(o, 
                   c(FALSE, FALSE, rep(TRUE, 3), #5
                     rep(FALSE, 3), rep(TRUE, 2), #10
                     FALSE, rep(TRUE, 2), #10
                     FALSE, rep(TRUE, 2), 
                     rep(FALSE, 3), TRUE, FALSE))
  expect_identical(IsParryWeapon(CombatTech = "CT_17"), FALSE) # spit fire
  expect_identical(IsParryWeapon(CombatTech = "CT_12"), TRUE) # sword
})



test_that(".GetEnum", {
  o <- .GetEnum("Hero.Weapon.CloseCombatRange.Medium")
  e <- expression(.CloseCombatRange)
  expect_identical(o, e)
  
  o <- eval(.GetEnum("Hero.Weapon.CloseCombatRange.Medium"))
  e <- .CloseCombatRange
  expect_identical(o, e)
  
  # This string is malformed according to specs. This is just for testing
  # purposes.
  o <- eval(.GetEnum("Opponent.MeansOfMovement", +1))
  e <- .MeansOfMovement
  expect_identical(o, e)
  
  # 
  expect_error(.GetEnum("Opponent.MeansOfMovement", +2)) # no elements behind last one
  expect_error(.GetEnum("Opponent.MeansOfMovement", -1)) # no elements before first one
})



.GetTestingCombatEnvironment <- function(Type, WithObsoletes = FALSE, ...) {
  # Create template
  CombatEnv <- list(
    Hero = list(
      Weapon = list(
        WeaponType = Type
      )
    ),
    Opponent = list(
      TargetSize   = .TargetSize["Medium"]
    ),
    Environment = list(
      Visibility   = .Visibility["Impaired"], 
      CrampedSpace = .CrampedSpace["Cramped"],
      UnderWater   = .UnderWater["Dry"]
    )
  )
  
  if (Type == .WeaponType["Melee"]) {
    CombatEnv$Hero$Weapon$CloseCombatRange <- .CloseCombatRange["Short"]
    if (WithObsoletes) { # ignored in close combat
      CombatEnv$Hero$RangedCombatRange <- sample(.RangedCombatRange, 1)
      CombatEnv$Hero$MeansOfMovement   <- sample(.MeansOfMovement, 1)
      CombatEnv$Hero$Movement          <- sample(.Movement, 1)
    }
    
    CombatEnv$Opponent$CloseCombatRange <- .CloseCombatRange["Short"]
    if (WithObsoletes) { # ignored in ranged combat
      CombatEnv$Opponent$RangedCombatRange <- sample(.RangedCombatRange, 1)
      CombatEnv$Opponent$Movement        <- sample(.Movement, 1)
      CombatEnv$Opponent$EvasiveMovement <- sample(.EvasiveMovement, 1)
      #CombatEnv$Opponent$TargetDistance  <- sample(.TargetDistance, 1)
    }
  }
  if (Type == .WeaponType["Unarmed"]) {
  }
  if (Type == .WeaponType["Shield"]) {
  }
  if (Type == .WeaponType["Ranged"]) {
    if (WithObsoletes) # ignored in ranged combat
      CombatEnv$Hero$Weapon$CloseCombatRange <- sample(.CloseCombatRange, 1)
    CombatEnv$Hero$RangedCombatRange <- .RangedCombatRange["Medium"]
    CombatEnv$Hero$MeansOfMovement   <- .MeansOfMovement["OnFoot"]
    CombatEnv$Hero$Movement          <- .Movement["Stationary"] # depends on `Means...`

    if (WithObsoletes) # ignored in ranged combat
      CombatEnv$Opponent$CloseCombatRange <- sample(.CloseCombatRange, 1)
    CombatEnv$Opponent$Movement          <- .Movement["Slow"]
    CombatEnv$Opponent$EvasiveMovement   <- .EvasiveMovement["None"]
    #CombatEnv$Opponent$TargetDistance    <- .TargetDistance["Medium"]
  }
  
  CombatEnv$Opponent$TargetSize      <- .TargetSize["Medium"]

  CombatEnv$Environment$Visibility   <- .Visibility["Clear"]
  CombatEnv$Environment$CrampedSpace <- .CrampedSpace["Free"]
  CombatEnv$Environment$UnderWater   <- .UnderWater["Dry"]
  
  # handle args
  Args <- list(...)
  if (length(Args) > 0)
    for (a in 1:length(Args)) {
      #print(paste0("CombatEnv$", names(Args[a])))
      eval(str2expression(paste0("CombatEnv$", names(Args[a]), "<- Args[[a]]")))
    }
  
  return(CombatEnv)
}



test_that("ModifyCheck: Neutral Enviroment has no effect on roll check", {
  Check        <- c(at = 10, pa = 10, do = 10)

  # Melee, no obsoletes
  BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], FALSE)
  setwd(.srcdir)
  o <- ModifyCheck(Check, BattleGround)
  setwd(.testdir)
  expect_identical(o, Check)
  
  # Melee, including obsoletes
  BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], TRUE)
  o <- ModifyCheck(Check, BattleGround)
  expect_identical(o, Check)

  
  # # Unarmed, no obsoletes
  # BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Unarmed"], FALSE)
  # o <- ModifyCheck(Check, BattleGround)
  # expect_identical(o, Check)
  # 
  # # Unarmed, including obsoletes
  # BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Unarmed"], TRUE)
  # o <- ModifyCheck(Check, BattleGround)
  # expect_identical(o, Check)

    
  # Ranged, no obsoletes
  BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Ranged"], FALSE)
  o <- ModifyCheck(Check, BattleGround)
  expect_identical(o, Check)
  
  # Ranged, including obsoletes
  BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Ranged"], TRUE)
  o <- ModifyCheck(Check, BattleGround)
  expect_identical(o, Check)
})
test_that("ModifyCheck: Variations of Melee Combat", {
  Check        <- c(at = 10, pa = 10, do = 10)
  
  # TARGET SIZE
  for (val in .TargetSize) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], WithObsoletes = FALSE,
                                                 `Opponent$TargetSize` = .TargetSize[val])
    setwd(.srcdir)
    o <- ModifyCheck(Check, BattleGround)
    setwd(.testdir)
    e <- switch(val, Check - c(4, 0, 0), Check, Check, Check * c(1, 0, 1), Check * c(1, 0, 1))
    expect_identical(o, e, label = names(.TargetSize[val]))
  }
  
  # VISIBILITY  
  for (val in .Visibility) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], FALSE,
                                                 `Environment$Visibility` = .Visibility[val])
    e <- switch(val, Check, Check-1, Check-2, Check-3, c(Check["at"] * 0.5 , pa = 1, do = 1))
    o <- ModifyCheck(Check, BattleGround)
    expect_identical(o, e, label = names(.Visibility[val]))
  }
  
  # CRAMPED SPACE  
  for (val in .CrampedSpace) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], FALSE,
                                                 `Environment$CrampedSpace` = .CrampedSpace[val])
    o <- ModifyCheck(Check, BattleGround)
    e <- switch(val, Check, Check, Check - c(4, 4, 0), Check - c(8, 8, 0))
    expect_identical(o, e, label = names(.CrampedSpace[val]))
  }
  
  # COMBAT RANGE - Weapon length
  for (val in .CloseCombatRange) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], FALSE,
                                                 `Hero$Weapon$CloseCombatRange` = .CloseCombatRange[val],
                                                 `Opponent$CloseCombatRange` = .CloseCombatRange["Short"])
    o <- ModifyCheck(Check, BattleGround)
    e <- switch(val, Check, Check, Check)
    expect_identical(o, e, label = names(.CloseCombatRange[val]))
  }
  for (val in .CloseCombatRange) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], FALSE,
                                                 `Hero$Weapon$CloseCombatRange` = .CloseCombatRange[val],
                                                 `Opponent$CloseCombatRange` = .CloseCombatRange["Medium"])
    o <- ModifyCheck(Check, BattleGround)
    e <- switch(val, Check - c(2, 0, 0), Check, Check)
    expect_identical(o, e, label = names(.CloseCombatRange[val]))
  }
  for (val in .CloseCombatRange) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], FALSE,
                                                 `Hero$Weapon$CloseCombatRange` = .CloseCombatRange[val],
                                                 `Opponent$CloseCombatRange` = .CloseCombatRange["Long"])
    o <- ModifyCheck(Check, BattleGround)
    e <- switch(val, Check - c(4, 0, 0), Check - c(2, 0, 0), Check)
    expect_identical(o, e, label = names(.CloseCombatRange[val]))
  }
})

test_that("ModifyCheck: A combination of Melee Combat", {
  Check        <- c(at = 10, pa = 10, do = 10)
  
  # TARGET SIZE
  for (val in .TargetSize) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Melee"], WithObsoletes = FALSE,
                                                 `Opponent$TargetSize` = .TargetSize[val])
    setwd(.srcdir)
    o <- ModifyCheck(Check, BattleGround)
    setwd(.testdir)
    e <- switch(val, Check - c(4, 0, 0), Check, Check, Check * c(1, 0, 1), Check * c(1, 0, 1))
    expect_identical(o, e, label = names(.TargetSize[val]))
  }
})
test_that("ModifyCheck: Shield", {
  Check        <- c(at = 10, pa = 10, do = 10)
  
  # TARGET SIZE
  for (val in .CloseCombatRange) {
    BattleGround <- .GetTestingCombatEnvironment(.WeaponType["Shield"], WithObsoletes = FALSE,
                                                 `Opponent$CloseCombatRange` = .CloseCombatRange[val])
    setwd(.srcdir)
    o <- ModifyCheck(Check, BattleGround)
    setwd(.testdir)
    e <- switch(val, Check, Check - c(2, 0, 0), Check - c(4, 0, 0))
    expect_identical(o, e, label = names(.CloseCombatRange[val]))
  }
})