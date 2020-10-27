# Skill set
require(R6)
require(jsonlite)
source("./dicelogic.R")
source("./rules.R")
source("./readoptjson.R")

# SkillSet -------------

SkillSet <- R6Class("SkillSet", public = list(
  # Skill properties
  Type = NA,      # enum Type `.SkillType` (Mundane, Magic, Blessed)
  Modifier = 0L,  # permanent default modifier because of special abilities
  Skills = NA,    # List of skills
  # Roll properties
  LastSkill     = NA, # numeric index of the skill of last roll
  LastRoll      = NA, # die roll, `numeric(3)` or TRUE/FALSE in case of routine check
  LastModifier  = NA, # additional situation dependent modifier
  LastResult    = NA, # Critical, Success, Fail, Botch
  ConfirmationMissing = NA,
  ConfirmRoll = NA,
  LastQL        = NA, # Quality level
  LastRemainder = NA, # remaining skill points
  LastFumbleEffect = NA, # EffectOfFumble: consequence of 2d6
  
  
  #' Constructor
  #' @param Type is skill set mundane or supernatural (i.e. magic or blessed)
  #' (integer or string of enum `.SkillType`)
  #' @param Skills Data frame of skills (1 column with values; row names are skill IDs)
  #' @param Abilities Character abilities (data frame)
  #' @return `self`
  initialize = function(Type, Skills = NULL, Abilities) {
    self$Type     <- .SkillType[Type]
    if (missing(Skills)) {
      self$Skills   <- data.frame(attrID = "ANY", name = "ANY", 
                                  class = "ALL", classID = 99,
                                  ab1 = "", ab2 = "", ab3 = "", value = 0L,
                                  abval1 = -1L, abval2 = -1L, abval3 = -1L,
                                  stringsAsFactors = FALSE)
      self$Modifier <- 0L
    } else {
      if (Type == .SkillType["Mundane"]) {
        self$Skills <- GetSkills_Opt(Skills)
      } else if (Type == .SkillType["Magic"]) {
        self$Skills <- GetSpells_Opt(Skills)
      } else  if (Type == .SkillType["Blessed"]) {
        self$Skills <- GetChants_Opt(Skills)
      } else stop("Unknown type of skill")
      self$Modifier <- 0L
      # Add ability values to each skill

      AbilityIDs  <- self$Skills[, paste0("ab", 1:3)]
      AbilityVals <- as.data.frame(sapply(AbilityIDs, function(x) Abilities[x]))
      names(AbilityVals) <- paste0("abval", 1:3)
      self$Skills <-  cbind(self$Skills, AbilityVals)
    }
    
    invisible(self)
  },

    
  #' GetAbilities
  #' Extract relevant abilities from data frame of with all character abilities
  GetAbilities = function(SkillIndex) {
    if (!isTruthy(SkillIndex) || SkillIndex < 1 || SkillIndex > nrow(self$Skills))
      stop("Invalid skill index")

    Abilities  <- self$Skills[SkillIndex, paste0("abval", 1:3)]
    return(unlist(Abilities))
  },

  #' GetAbilityID
  #' Get the IDs of the abilities that a skill roll is rolled against
  GetAbilityID = function(SkillIndex) {
    if (!isTruthy(SkillIndex) || SkillIndex < 1 || SkillIndex > nrow(self$Skills))
      stop("Invalid skill index")
    
    Abilities  <- self$Skills[SkillIndex, paste0("ab", 1:3)]
    return(unlist(Abilities))
  },
  
  #' GetAbilities
  #' Extract rlevant abilities from data frame of with all character abilities
  GetAbilityLabels = function(SkillIndex) {
    if (!isTruthy(SkillIndex) || SkillIndex < 1 || SkillIndex > nrow(self$Skills))
      stop("Invalid skill index")
    
    Mapping   <- GetAbilities() # get all abilities from db
    Abilities <- self$Skills[SkillIndex, paste0("ab", 1:3)]
    if (all(startsWith(unlist(Abilities), "ATTR_")))
      Labels    <- Mapping[match(Abilities, Mapping[["attrID"]]), "shortname"]
    else
      Labels <- NULL
    
    return(Labels)
  },
  
  #' SetAbility
  #' Set the ability values for exactly 1 skill
  SetAbility = function(SkillIndex, Abilities) {
    if (!isTruthy(SkillIndex) || SkillIndex < 1L || SkillIndex > nrow(self$Skills))
      stop("Invalid skill index")
    if(length(Abilities) != 3) stop("A skill requires exactly 3 abilities")
    
    if (length(names(Abilities)) > 0L)
      if(all(startsWith(names(Abilities), "ATTR_")))
        self$Skills[SkillIndex, paste0("ab", 1:3)] <- names(Abilities)
    self$Skills[SkillIndex, paste0("abval", 1:3)] <- as.integer(Abilities)
    return(invisible(self))
  },
  
  
  #' GetSkillIndex
  #' Get numeric index of skill in `self$Skills` from ID string, name. 
  #' If `SkillIdent` is already a number it will returned as is.
  GetSkillIndex = function(SkillIdent) {
    if (missing(SkillIdent)) stop("Skill must be identified")

    if (is.numeric(SkillIdent))
      SkillIndex <- SkillIdent
    else {
      SkillIndex <- which(self$Skills[["attrID"]] %in% SkillIdent)
      if (length(SkillIndex) < 1) # try name if ID did not yield a result
        SkillIndex <- which(self$Skills[["name"]] %in% SkillIdent)
      if (length(SkillIndex) == 0) SkillIndex <- NA_integer_
    }
    
    if (isTruthy(SkillIndex) && all(SkillIndex > 0) && all(SkillIndex <= nrow(self$Skills)))
      return(SkillIndex)
    else {
      SkillIndex[SkillIndex < 1] <- NA_integer_
      SkillIndex[SkillIndex > nrow(self$Skills)] <- NA_integer_
      return(SkillIndex)
    }
  },
  
  
  #' GetSkillValues
  #' Returns the effective numeric ability and skill value (after subtracting 
  #' modifiers)
  GetSkillValues = function(SkillIdent, Mod, NoSkill = FALSE) {
    SkillIndex <- self$GetSkillIndex(SkillIdent)
    
    if (isTruthy(SkillIndex)) { # get numeric values
      Abilities <- unlist(self$Skills[SkillIndex, paste0("abval", 1:3)])
      if (any(Abilities < 0L)) return(NULL)
      Abilities <- Abilities + Mod + self$Modifier
    } else return(NA)

    if (NoSkill)
      return(Abilities + Mod + self$Modifier)
    else
      return(c(Abilities, value = self$Skills[SkillIndex, "value"]))
  },
  
  
  #' GetSkillName
  #' Returns the names of a given vector of skills
  GetSkillName = function(SkillIdent) {
    if (length(SkillIdent) == 1 && !is.na(pmatch(SkillIdent, "All")))
      SkillIndex <- 1:nrow(self$Skills)
    else
      SkillIndex <- self$GetSkillIndex(SkillIdent)
    
    return(self$Skills[SkillIndex, "name"])
  },

  
  #' GetSkillID
  #' Returns the ID of a skill
  GetSkillID = function(SkillIdent) {
    SkillIndex <- self$GetSkillIndex(SkillIdent)
    
    return(self$Skills[SkillIndex, "attrID"])
  },
  

  #' GetClasses
  GetClasses = function() {
    unique(self$Skills[, "class"])
  },
  
  
  #' Verifies if the desired skill supports a skill check
  CanRoutineCheck = function(SkillIdent, Mod) {
    Values <- self$GetSkillValues(SkillIdent, 0, NoSkill = FALSE)
    if (!is.null(Values))
      Result <- CanRoutineSkillCheck(Values[1:3], Values[4], self$Modifier + Mod)
    else
      Result <- FALSE
    
    return(Result)
  },

  
  #' SetSkill
  #' 
  #' @param SkillIdent Identifier of the skill, either an ID string, the name, 
  #' or the numeric index in the list of skills.
  #' @param Abilities Three abilities that make that roll as vector (numeric).
  #' @param SkillValue A single value for the skill.
  #' @return returns the *effective* values of the new skill
  SetSkill = function(SkillIdent, Abilities = NULL, SkillValue = NULL) {
    SkillIndex <- self$GetSkillIndex(SkillIdent)
    
    if (isTruthy(SkillIndex)) {
      if (!missing(Abilities)) {
        Abilities <- as.integer(Abilities)
        self$Skills[SkillIndex, "abval1"] <- Abilities[1]
        self$Skills[SkillIndex, "abval2"] <- Abilities[2]
        self$Skills[SkillIndex, "abval3"] <- Abilities[3]
      } else { # if not set we still need a return value
        Abilities <- unlist(self$Skills[SkillIndex, paste0("abval", 1:3)])
      }

      if (!missing(SkillValue)) {
        SkillValue <- as.integer(SkillValue)
        self$Skills[["value"]][SkillIndex] <- SkillValue
      } else {
        SkillValue <- self$Skills[["value"]][SkillIndex]
      }
    } else {
      return(NA)
    }
    
    names(Abilities) <- paste0("abval", 1:3)
    return(c(Abilities, value = SkillValue))
  },


  #' Change the skill so that this class will do a simple die roll
  #' without checking it against any values.
  UncheckSkill = function(SkillIndent) {
    self$SetSkill(SkillIndent, rep(-1L, 3L), 0L)
    return(invisible(self))
  },
  
  
  #' Roll
  #' Roll 3d20 for a skill check
  Roll = function(SkillIdent, Mod, Routine = FALSE) {
    SkillIndex <- self$GetSkillIndex(SkillIdent)
    self$LastSkill <- SkillIndex
    self$LastModifier <- Mod + self$Modifier

    if (!Routine)
      self$LastRoll <- SkillRoll()
    else 
      self$LastRoll <- TRUE
    self$VerifyLastRoll()

    invisible(self)
  },
  
  
  #' InvalidateRoll
  #' Reset the last roll (e.g. when the user changed the skill
  #' and the roll does not make sense anymore)
  #' @return invisible self
  InvalidateRoll = function() {
    self$LastModifier <- NA
    self$LastRoll <- NA
    self$LastQL   <- NA
    self$LastResult <- NA
    self$LastRemainder <- NA
    self$ConfirmationMissing <- NA
    self$ConfirmRoll <- NA
    self$LastFumbleEffect <- NA
    
    invisible(self)
  },
  

  #' VerifyLastRoll
  #' Returns the success level (of `.SuccessLevel`) of the last roll
  VerifyLastRoll = function() {
    if (!isTruthy(self$LastRoll)) return(NA)
    
    SkillIndex    <- self$LastSkill
    LastAbilities <- as.integer(unlist(self$Skills[SkillIndex, paste0("abval", 1L:3L)]))
    LastSkillVal  <- self$Skills[["value"]][SkillIndex]

    if(is.numeric(self$LastRoll))
      if(any(LastAbilities < 0L)) {
        # Unchecked roll not against any values
        Result <- list(Message = "", QL = "-", Remainder = NA)
      } else {
        Result <- VerifySkillRoll(self$LastRoll, LastAbilities, 
                                  LastSkillVal, self$LastModifier)
      }
    else if (isTRUE(self$LastRoll))
      Result <- VerifyRoutineSkillCheck(LastAbilities, LastSkillVal, 
                                        self$LastModifier)

    self$LastQL <- Result[["QL"]]
    self$LastResult <- .SuccessLevel[ Result[["Message"]] ]
    self$LastRemainder <- Result[["Remainder"]]
    self$ConfirmationMissing <- self$LastResult %in% .SuccessLevel[ c("Fumble", "Critical") ]
    self$ConfirmRoll <- NA
    self$LastFumbleEffect <- NA
    return(Result)
  },
  
  
  #' Return the dice score of the latest roll
  GetLastScore = function() {
    if (is.logical(self$LastRoll))
      return(rep(NA, 3))
    else if (is.numeric(self$LastRoll))
      return(self$LastRoll)
    else
      return(NA)
  },
  
  #' Return the quality level (compute it first if necessary).
  GetLastQL = function() {
    if (!isTruthy(self$LastQL) || !is.numeric(self$LastQL))
      self$VerifyLastRoll()
    
    return(self$LastQL)
  },
  
  
  UpdateModifier = function(RollMod, PermanentMod = NULL) {
    if (missing(PermanentMod)) 
      PermanentMod <- self$Modifier
    else
      self$Modifier
    
    self$LastModifier <- RollMod + PermanentMod
    
    return(self)
  },
  
  
  RollNeedsConfirmation = function() {
    return(!is.na(self$LastRoll) && self$ConfirmationMissing)
  },


  Confirm = function() {
    if (anyNA(self$LastRoll)) return(NA)
    if (!self$ConfirmationMissing) return(NA)

    LastSkillVal  <- self$Skills[["value"]][self$LastSkill]
    LastAbilities <- self$GetAbilities(self$LastSkill)
    
    self$ConfirmRoll <- SkillRoll()
    Result <- VerifySkillRoll(self$ConfirmRoll, LastAbilities,
                              LastSkillVal, self$Modifier)
    NewResult <- VerifyConfirmation(names(self$LastResult), Result$Message)
    self$LastResult <- .SuccessLevel[NewResult]

    self$ConfirmationMissing <- FALSE
    return(self$ConfirmRoll)
  },

  
  NeedFumbleRoll = function() {
    if (self$Type == .SkillType["Mundane"]) 
      return(FALSE) # not applicable for mundane skills
    else {
      return(is.na(self$LastFumbleEffect) && 
               self$LastResult == .SuccessLevel["Fumble"])
    }
  },
  
  
  FumbleRoll = function() {
    if (self$NeedFumbleRoll()) {
      Roll <- FumbleRoll()
      Effect <- GetFumbleEffect(Roll, "Skill", names(self$Type))
      self$LastFumbleEffect <- Effect
    }
    return(Roll)
  }
))





#
#
# CharacterSkills class -------------
CharacterSkills <- R6Class("CharacterSkills", public = list(
  
  Sets = list(Mundane = NA, Magic = NA, Blessed = NA),

  #' Constructor
  #' @param Type is skill set mundane or supernatural (i.e. magic or blessed)
  #' (integer or string of enum `.SkillType`)
  #' @param Skills Data frame of skills (1 column with values; row names are skill IDs)
  #' @param Abilities Character abilities (data frame)
  #' @return `self`
  initialize = function(MundaneSet, MagicSet = NULL, BlessedSet = NULL) {
    self$Sets[["Mundane"]]  <- MundaneSet
    self$Sets[["Magic"]]    <- MagicSet
    self$Sets[["Blessed"]]  <- BlessedSet
    invisible(self)
  },
  
  
  #' Determines and returns the complete set of skills requested with the
  #' skill identifier (via `Ident`) or skill `Class`.
  #' @return R6 class `SkillSet`. 
  #' `Null` if the requested information is not available. 
  GetSkillSet = function(Ident = NULL, Class = NULL) {
    SkillSource <- NULL
    for (s in self$Sets) {
      if (isTruthy(s)) {
        if(!missing(Ident) && !is.na(s$GetSkillIndex(Ident)))
          SkillSource <- s
        else if(!missing(Class) && Class %in% s$GetClasses())
          SkillSource <- s
      }
      if(!is.null(SkillSource)) break
    }
    return(SkillSource)
  },
  

  #' Provide the names of all available skills
  #' @note Skill names are translated into active language
  #' @return Vector of strings. Empty vector if nothing is found.
  GetSkillNames = function() {
    AllNames <- character()
    for (s in self$Sets) {
      if (isTruthy(s)) {
        AllNames <- c(AllNames, s$GetSkillName("All"))
      }
    }
    return(AllNames)
  },
  
  
  #' Provide the names of all available skill classes
  #' (i.e. physical, social, nature, ...)
  #' @note Class names are translated into active language
  #' @return Vector of strings. Empty vector if nothing is found.
  GetSkillClasses = function() {
    AllClasses <- character()
    for (s in self$Sets) {
      if (isTruthy(s)) {
        AllClasses <- c(AllClasses, s$GetClasses())
      }
    }
    return(AllClasses)
  },
  
  #' Check if a character has magical or blessed skills
  #' @param Type list id taken from the `.SkillType` enum
  #' @return `TRUE` if the character is capable of the type of skill
  HasTalent = function(Type = .SkillType) {
    if (! (Type %in% .SkillType)) stop("Unknown type of skill")
    return( any(names(self$Sets) == names(.SkillType[Type])) )
    #TODO: check if 'any' is the right choice here or should it rather be 'all' or shall it be returned individually?
  }
  
))
  
  