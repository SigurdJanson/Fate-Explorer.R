# Weapon R6
library(R6)
require(jsonlite)
source("./dicelogic.R")
source("./rules.R")
source("./readoptjson.R")

# Open tasks
# - Ranged: use range to modify values
# - Ranged: Use target size
# - Make use of permanent modifier


# BASE CLASS =====================================================

##' WeaponBase class (abstract base class for weapons)
##' This class wraps basic functions.
##' @importFrom R6 R6Class
##' @export
WeaponBase <- R6Class(
  "WeaponBase", 
  active = list(
    Name = function(value) {
      if (missing(value)) {
        return(private$.Name)
      } else {
        private$.Name <- value
        private$OnValueChange()
      }
    }
  ),
  public = list(

  Type = NA,      # .WeaponType # Weaponless, Melee, Ranged, Shield
  Technique = NA, # Combat technique
  Range = NA,     # interpretation differs based on `Type`
  Skill  = list(Attack = 0L, Parry = 0L, Dodge = 0L), # dodge this is actually not dependent on the active weapon
  Damage = list(N = 1L, DP = 6L, Bonus = 0L), # [n]d[dp] + [bonus]
  Modifier = 0L,  # permanent default modifier because of special abilities
  
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
  #' @param Weapon name of the weapon (character) or a list containing the data
  #' @param Abilities Character abilities (data frame)
  #' @param CombatTecSkills Named list of combat tech skills 
  #' (names are the `combattechID`)
  #' @return `self`
  initialize = function(Weapon = NULL, Abilities = NULL, CombatTecSkills = NULL, ...) {
    if (missing(Weapon)) {
      args <- list(...)
      self$Name <- NA
      self$Type <- NA
      self$Technique <- NA
      self$Range     <- NA
      self$Skill  <- args[["Skill"]]
      self$Damage <- args[["Damage"]]
      self$Modifier  <- 0L
    } else {
      if (is.character(Weapon)) # `Weapon` is a name or ID
        self$RawWeaponData <- GetWeapons(Weapon)
      else 
        self$RawWeaponData <- Weapon
      
      self$Name      <- self$RawWeaponData[["name"]]
      self$Type      <- .WeaponType[1+ self$RawWeaponData[["armed"]] + !self$RawWeaponData[["clsrng"]] ]
      self$Technique <- self$RawWeaponData[["combattechID"]]
      self$Range     <- self$RawWeaponData[["range"]]
      self$CalcSkill(Abilities, CombatTecSkills)
      self$CalcDamage(Abilities)
      self$Modifier  <- 0L
    }
    invisible(self)
  },
  
  # CALLBACK SECTION
  RegisterOnValueChange = function(Callback) {
    if (mode(Callback) != "function") 
      stop(sprintf("Cannot register object of type '%s' as callback"), typeof(Callback))
    
    #if (!(Callback %in% private$ValueChangeCallbacks)) # avoid duplicates
    if (!(any(sapply(private$ValueChangeCallbacks, identical, Callback))))
      private$ValueChangeCallbacks <- c(private$ValueChangeCallbacks, Callback)
  },
  UnregisterOnValueChange = function(Callback) {
    if (mode(Callback) != "function")
      stop(sprintf("Cannot register object of type %s as callback"), typeof(Callback))
    
    # Use `setdiff` because duplicates should not exist
    private$ValueChangeCallbacks <- setdiff(private$ValueChangeCallbacks, c(Callback))
  },
  
  
  #' CalcSkill
  #' Computes weapons skill for character
  #' @param CharAbs Data frame of character abilities
  #' @param CombatTecSkill A single value for the combat skill of the weapon's technique
  #' @return `self`
  CalcSkill = function(CharAbs, CombatTecSkill) {
    # if RawWeaponData has not been enriched by character data, yet, do so ...
    if (is.null(self$RawWeaponData[["AT.Skill"]]) || is.null(self$RawWeaponData[["PA.Skill"]])) {
      AtPaSkill <- GetCombatSkill(self$Name, CharAbs, Skill = CombatTecSkill)
      self$RawWeaponData[["AT.Skill"]] <- AtPaSkill$AT
      self$RawWeaponData[["PA.Skill"]] <- AtPaSkill$PA
    }
    ##TODO: DodgeSkill <- GetDodgeSkill()

    self$Skill <- list(Attack = self$RawWeaponData[["AT.Skill"]], 
                       Parry  = self$RawWeaponData[["PA.Skill"]], 
                       Dodge  = ceiling(CharAbs[["ATTR_6"]] / 2L))
    return(invisible(self))
  },

  
  #' CalcDamage
  #' Computes the hit point formula of the weapon. It takes the 
  #' character's abilities into account to get the bonus right.
  #' @details The damage is determined by three components: [N]d[DP] + [Bonus]
  #' @param CharAbs The character's abilities
  #' @return Invisible returns `self`
  CalcDamage = function(CharAbs) {
    # if RawWeaponData has not been enriched by character data, yet, do so ...
    if (is.null(self$RawWeaponData[["damageDiceNumber"]]) || 
        is.null(self$RawWeaponData[["damageDiceSides"]])  ||
        is.null(self$RawWeaponData[["damageFlat"]])) {
      DamageDice <- unlist(strsplit(self$RawWeaponData[["damage"]], split = "W"))
      self$RawWeaponData[["damageDiceNumber"]] <- as.integer(DamageDice[1])
      self$RawWeaponData[["damageDiceSides"]]  <- as.integer(DamageDice[2])
      Bonus <- as.integer(self$RawWeaponData[["bonus"]])
      if (!isTruthy(Bonus)) Bonus <- 0
      self$RawWeaponData[["damageFlat"]] <- Bonus + GetHitpointBonus(self$Name, Abilities = CharAbs)
    }
    
    self$Damage <- list(N = self$RawWeaponData[["damageDiceNumber"]], 
                        DP = self$RawWeaponData[["damageDiceSides"]], 
                        Bonus = self$RawWeaponData[["damageFlat"]])

    return(invisible(self))
  },

  
  Attack = function(Modifier = 0L) self$Roll("Attack", Modifier), # wrapper
  Parry  = function(Modifier = 0L) self$Roll("Parry", Modifier), # wrapper
  Dodge  = function(Modifier = 0L) self$Roll("Dodge", Modifier), # wrapper

  Roll = function(Action = "Attack", Modifier = 0L) {
    # PRECONDITIONS
    if (is.numeric(Action)) {
      if (Action %in% .CombatAction)
        self$LastAction <- Action
      else
        stop("Unknown combat action")
    }
    else {
      if (is.character(Action) && Action %in% names(.CombatAction))
        self$LastAction <- .CombatAction[Action]
      else
        stop("Unknown combat action")
    }
    
    # RUN
    if (length(Modifier) == length(.CombatAction)) # if actions have different modifiers ...
      Modifier <- Modifier[.CombatAction[Action]]  # ... select the right one
    self$LastModifier <- self$Modifier + Modifier
    
    Skill <- self$Skill[[ names(.CombatAction)[self$LastAction] ]]
    
    self$LastRoll <- CombatRoll()
    Verification  <- VerifyCombatRoll(self$LastRoll, Skill, self$LastModifier) # interim variable
    self$LastResult  <- .SuccessLevel[Verification]
    
    self$LastDamage <- 0L
    if (self$LastAction == .CombatAction["Attack"])
      if (self$LastResult %in% .SuccessLevel[c("Success", "Critical")])
      {
        self$LastDamage <- DamageRoll(self$Damage$N, self$Damage$DP, self$Damage$Bonus)
      }
    
    self$ConfirmationMissing <- self$LastResult %in% .SuccessLevel[c("Fumble", "Critical")]
    self$ConfirmRoll <- NA
    self$Confirmed   <- NA
    self$LastFumbleEffect <- NA
    
    return(self$LastRoll)
  },
  

  #' Confirm
  #' Confirm the last critical/fumble roll
  #' @return A value from the `.SuccessLevel` enum. `NA` if the last roll was not
  #' a critical or fumble. Also `NA` when the weapon has not been used in this 
  #' session, yet.
  Confirm = function() {
    if (is.na(self$LastRoll)) return(NA)
    if (!self$ConfirmationMissing) return(NA)
      
    self$ConfirmationMissing <- FALSE
    Skill <- self$Skill[[ names(.CombatAction)[self$LastAction] ]]

    self$ConfirmRoll <- CombatRoll()
    Result <- .SuccessLevel[VerifyCombatRoll(self$ConfirmRoll, Skill, self$LastModifier)]
    # Has previous result been confirmed?
    NewResult <- .SuccessLevel[VerifyConfirmation(names(self$LastResult), names(Result))]
    self$Confirmed <- (NewResult == self$LastResult)
    self$LastResult <- NewResult
    # Effects: Criticals do double damage - Fumble do bad
    if (self$LastResult == .SuccessLevel["Critical"])
      self$LastDamage <- self$LastDamage * 2

    return(Result)
  },
  
  
  #' FumbleRoll
  #' Rolls the consequences of a confirmed fumble roll.
  #' @note This method merely wraps 
  #' @seealso [GetFumbleEffect()] which this function wraps.
  FumbleRoll = function() {
    if (!isTruthy(self$LastFumbleEffect))
      if (self$LastResult == .SuccessLevel["Fumble"]) {
        self$LastFumbleEffect <- GetFumbleEffect(FumbleRoll(),
                                                 names(self$LastAction),
                                                 names(self$Type))
      }
    return(self$LastFumbleEffect)
  },
  
  
  #' RollNeedsConfirmation
  #' Is a confirmation roll required to complete the fighting roll?
  #' @return `TRUE` if a confirmation roll is required. `FALSE` if
  #' there is no last roll or the last roll is complete.
  RollNeedsConfirmation = function() {
    return(!is.na(self$LastRoll) && self$ConfirmationMissing)
  },
  
  
  #' GetHitPoints
  #' Damage of the last roll
  GetHitPoints = function() {
    return(self$LastDamage)
  },
  
  
  #' CanParry
  #' Does the weapon allow a parry roll?
  CanParry = function() {
    if (!is.na(self$Technique))
      Can <- IsParryWeapon(CombatTech = self$Technique) && 
             self$Skill$Parry > 0
    else
      Can <- self$Skill$Parry > 0
    return(Can)
  }
), # public

private = list(
  .Name = "",
  # Callbacks to notify other changes in the weapon
  ValueChangeCallbacks = NULL,
  
  OnValueChange = function() {
    if (length(private$ValueChangeCallbacks) > 0)
      for (f in private$ValueChangeCallbacks)
        do.call(f, alist(self))
  }
))


# MELEE =====================================================

##' MeleeWeapon class
##' @importFrom R6 R6Class
##' @export
MeleeWeapon <- R6Class("MeleeWeapon", 
  inherit = WeaponBase, 
  public = list(

    #' Constructor
    #' @param Weapon name of the weapon (character)
    #' @param Abilities Character abilities (data frame)
    #' @param CombatTecSkills Named list of combat tech skills (name is the combattec ID)
    #' @return `self`
    initialize = function(Weapon, Abilities, CombatTecSkills, ...) {
      super$initialize(Weapon, Abilities, CombatTecSkills, ...)
      
      if (!missing(Weapon)) {
        if (!(self$RawWeaponData[["clsrng"]])) # if ranged weapon
          stop("This class is for close combat only")
      }

      invisible(self)
    },
    
    
    #' CalcSkill
    #' Computes weapons skill for character
    #' @param CharAbs Data frame of character abilities
    #' @param CombatTecSkill A single value for the combat skill of the weapon's technique
    #' @return Invisibly returns `self`
    CalcSkill = function(CharAbs, CombatTecSkill) {
      super$CalcSkill(CharAbs, CombatTecSkill)
      return(invisible(self))
    },

    
    #' CalcDamage
    #' Computes the hit point formula of the weapon. It takes the 
    #' character's abilities into account to get the bonus right.
    #' @details The damage is determined by three components: [N]d[DP] + [Bonus]
    #' @param CharAbs The character's abilities
    #' @return Invisibly returns `self` 
    CalcDamage = function(CharAbs) {
      return(invisible(super$CalcDamage(CharAbs)))
    },
    

    #' Roll
    Roll = function(Action = "Attack", Modifier = 0L) {
      return(super$Roll(Action, Modifier))
    }
))



# RANGED =====================================================

##' RangedWeapon class
##' @importFrom R6 R6Class
##' @export
RangedWeapon <- R6Class("RangedWeapon", 
  inherit = WeaponBase, 
  public = list(
  
  #' Constructor
  #' @param Weapon name of the weapon (character)
  #' @param Abilities Character abilities (data frame)
  #' @param CombatTecSkills Named list of combat tech skills (name is the combattec ID)
  #' @return `self`
  initialize = function(Weapon, Abilities, CombatTecSkills, ...) {
   if (is.character(Weapon)) Weapon <- GetWeapons(Weapon, "Ranged")
   super$initialize(Weapon, Abilities, CombatTecSkills, ...)
   
    if (!missing(Weapon)) {
      if (self$RawWeaponData[["clsrng"]]) # if ranged weapon
        stop("This class is for close combat only")
    }
   
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
                      Parry = 0, 
                      Dodge = ceiling(CharAbs[["ATTR_6"]] / 2L))
   return(invisible(self))
  },
  
  CalcDamage = function(CharAbs) {
   super$CalcDamage(CharAbs)
   return(invisible(self))
  },
  
  Roll = function(Action = "Attack", Modifier = 0L) {
    # PRECONDITIONS
    if(Action == "Parry" || Action == .CombatAction["Parry"])
      stop("Ranged weapons cannot be used to parry attacks")
    
    super$Roll(Action, Modifier)
    return(self$LastRoll)
  }
))

# ab <- structure(list(ATTR_1 = 12L, ATTR_2 = 11L, ATTR_3 = 13L, ATTR_4 = 14L,
#                      ATTR_5 = 13L, ATTR_6 = 16L, ATTR_7 = 11L, ATTR_8 = 11L),
#                 class = "data.frame", row.names = c(NA, -1L))
# ct <- list(CT_3 = 15, CT_9 = 15, CT_12 = 12, CT_14 = 13)
# setwd("./R") #for testing purposes
#  W <- WeaponBase$new("Waffenlos", ab, ct) #"Waqqif"
#  print(W$Roll("Attack"))
# setwd("../") #for testing purposes
