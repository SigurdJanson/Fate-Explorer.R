# Weapon R6
library(R6)
require(jsonlite)
source("./src/dicelogic.R")
source("./src/rules.R")
source("./src/readoptjson.R")

# Enumerations
.WeaponType    <- c(Unarmed = 0L, Melee = 1L, Ranged = 2L)
.CombatActions <- c(Attack = 1L, Parry = 2L, Dodge = 3L)
.SuccessLevels <- c(Fumble = 1L, Fail = 2L, Success = 3L, Critical = 4L)
.CloseCombatRange  <- c(Short = 1L, Medium = 2L, Long = 3L)
.RangedCombatRange <- c(Close = 1L, Medium = 2L, Far = 3L)

##' WeaponBase class object (abstract base class for weapons)
##' @importFrom R6 R6Class
##' @export
WeaponBase <- R6Class("WeaponBase", public = list(

  Name = "",
  Type = NA, # Weaponless, Melee, Ranged
  Technique = NA,
  Range = NA, # interpretation differs based on `Type`
  Skill  = list(Attack = 0, Parry = 0, Dodge = 0), # dodge this is actually not dependent on the active weapon
  Damage = list(N = 1L, DP = 6L, Bonus = 0L), # [n]d[dp] + [bonus]
  Modifier = 0L, # default modifier because of special abilities
  
  RawWeaponData = NULL,
  
  LastRoll     = NA, # die roll
  LastAction   = NA, # Parry or Attack
  LastModifier = NA, # additional situation dependent modifier
  LastResult   = NA, # Critical, Success, Fail, Botch
  LastDamage   = NA, # Hit points
  ConfirmationMissing = NA, # T/F - does last roll wait for confirmation? 
  ConfirmRoll  = NA, 
  Confirmed    = NA,
  LastFumbleEffect = NA, # EffectOfFumble: consequence of 2d6
  
  
  #' Constructor
  #' @param Weapon name of the weapon (character)
  #' @param Abilities Character abilities (data frame)
  #' @param CombatTecSkills Named list of combat tech skills (name is the combattec ID)
  #' @return `self`
  initialize = function(Weapon, Abilities, CombatTecSkills) {
    if (missing(Weapon)) stop("Empty weapon")
    
    if (is.character(Weapon)) self$RawWeaponData <- GetWeapons(Weapon)
    
    self$Name      <- self$RawWeaponData[["name"]]
    self$Type      <- .WeaponType[1+ self$RawWeaponData[["armed"]] + !self$RawWeaponData[["clsrng"]] ]
    self$Technique <- self$RawWeaponData[["combattechID"]]
    self$Range     <- self$RawWeaponData[["range"]]
    self$CalcSkill(Abilities, CombatTecSkills)
    self$CalcDamage(Abilities)
    self$Modifier  <- 0L

    invisible(self)
  },
  
  
  #' CalcSkill
  #' Computes weapons skill for character
  #' @param CharAbs Data frame of character abilities
  #' @param CombatTecSkill A single value for the combat skill of the weapon's technique
  #' @return `self`
  CalcSkill = function(CharAbs, CombatTecSkill) {
    AtPaSkill  <- GetCombatSkill(self$Name, CharAbs, Skill = CombatTecSkill)
    self$Skill <- list(Attack = AtPaSkill$AT, 
                       Parry = AtPaSkill$PA, 
                       Dodge = ceiling(CharAbs[["ATTR_6"]] / 2))
    return(invisible(self))
  },

  CalcDamage = function(CharAbs) {
    Die <- unlist(strsplit(self$RawWeaponData[["damage"]], split = "W"))
    AddedBonus <- GetHitpointBonus(self$Name, Abilities = CharAbs)
    self$Damage <- list(N = as.integer(Die[1]), 
                        DP = as.integer(Die[2]), 
                        Bonus = as.integer(self$RawWeaponData[["bonus"]]) + AddedBonus)
    return(invisible(self))
  },

  Attack = function(Modifier = 0L) self$Roll("Attack", Modifier), # wrapper
  Parry  = function(Modifier = 0L) self$Roll("Parry", Modifier), # wrapper
  Dodge  = function(Modifier = 0L) self$Roll("Dodge", Modifier), # wrapper

  Roll = function(Action = "Attack", Modifier = 0L) {
    # PRECONDITIONS
    if (is.numeric(Action)) {
      if (Action %in% .CombatActions)
        self$LastAction <- Action
      else
        stop("Unknown combat action")
    }
    else if (is.character(Action))
      if (Action %in% names(.CombatActions))
        self$LastAction <- .CombatActions[Action]
      else
        stop("Unknown combat action")
    else stop("Unknown combat action")

    # RUN
    Skill <- self$Skill[[ names(.CombatActions)[self$LastAction] ]]
    
    self$LastRoll <- CombatRoll()
    self$LastModifier <- self$Modifier + Modifier
    Verification  <- VerifyCombatRoll(self$LastRoll, Skill, self$LastModifier) # interim variable
    self$LastResult  <- .SuccessLevels[Verification]

    self$LastDamage <- 0L
    if (self$LastAction == .CombatActions["Attack"])
      if (self$LastResult %in% .SuccessLevels[c("Success", "Critical")])
        self$LastDamage <- DamageRoll(self$Damage$N, self$Damage$Bonus)
      
    self$ConfirmationMissing <- self$LastResult %in% .SuccessLevels[c("Fumble", "Critical")]
    self$ConfirmRoll <- NA
    self$Confirmed   <- NA
    self$LastFumbleEffect <- NA

    return(self$LastRoll)
  },
  
  Confirm = function() {
    if (is.na(self$LastRoll)) return(FALSE)
    if (!self$ConfirmationMissing) return(TRUE)
      
    self$ConfirmationMissing <- FALSE
    Skill <- self$Skill[names(.CombatActions)[self$Action]]

    self$ConfirmRoll <- CombatRoll()
    Result <- .SuccessLevels[VerifyCombatRoll(self$ConfirmRoll, Skill, self$Modifier)]
    # Has previous result been confirmed?
    NewResult <- .SuccessLevels[VerifyConfirmation(names(self$LastResult), names(Result))]
    self$Confirmed <- (NewResult == self$LastResult)
    self$LastResult <- NewResult
    # Effects: criticals do double damage - Fumble do bad
    if (self$LastResult == .SuccessLevels["Critical"])
      self$LastDamage <- self$LastDamage * 2
    if (self$LastResult == .SuccessLevels["Fumble"]) {
      self$LastFumbleEffect <- GetCombatFumbleEffect(CombatFumbleRoll())
    }

    return(Result)
  },
  
  RollNeedsConfirmation = function() {
    return(!is.na(self$LastRoll) && self$ConfirmationMissing)
  },
  
  GetHitPoints = function() {
    return(self$LastDamage)
  },
  
  GetFumbleEffect = function() {
    return(self$LastFumbleEffect)
  }
))

# ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L,
#                      ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 11L),
#                 class = "data.frame", row.names = c(NA, -1L))
# ct <- list(CT_3 = 15, CT_9 = 15, CT_12 = 12, CT_14 = 13)
# setwd("./src") #for testing purposes
#  W <- WeaponBase$new("Waffenlos", ab, ct) #"Waqqif"
#  print(W$Roll("Attack"))
# setwd("../") #for testing purposes
