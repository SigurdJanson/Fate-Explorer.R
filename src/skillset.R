# Skill set
require(R6)
require(jsonlite)
source("./dicelogic.R")
source("./rules.R")
source("./readoptjson.R")

SkillSet <- R6Class("SkillSet", public = list(
  
  Type = NA,      # enum Type `.SkillType` (Profane, Magic, Sacred)
  Modifier = 0L,  # permanent default modifier because of special abilities
  Skills = NA,    # List of skills
  
  LastSkill     = NA, # skill of last roll
  LastRoll      = NA, # die roll, `numeric(3)` or TRUE/FALSE in case of routine check
  LastAbilities = NA,
  LastSkillVal  = NA,
  LastModifier  = NA, # additional situation dependent modifier
  LastResult    = NA, # Critical, Success, Fail, Botch
  LastQL        = NA, # Quality level
  LastRemainder = NA, # remaining skill points
  LastFumbleEffect = NA, # EffectOfFumble: consequence of 2d6
  
  
  #' Constructor
  #' @param Type is skill set profane or supernatural (i.e. magic or sacred)
  #' (integer or string of enum `.SkillType`)
  #' @param Skills Data frame of skills (1 column with values; row names are skill IDs)
  #' @param Abilities Character abilities (data frame)
  #' @return `self`
  initialize = function(Type, Skills = NULL, Abilities) {
    self$Type     <- .SkillType[Type]
    if (missing(Skills)) {
      self$Skills   <- data.frame(attrID = "ANY", name = "ANY", 
                                  class = "ALL", classID = 99,
                                  ab1 = "ATTR", ab2 = "ATTR", ab3 = "ATTR", value = 5,
                                  abval1 = 10, abval1 = 10, abval1 = 10)
      self$Modifier <- 0L
    } else {
      if (Type == .SkillType["Profane"]) {
        self$Skills <- GetSkills_Opt(Skills)
      } else if (Type == .SkillType["Magic"]) {
        self$Skills <- GetSpells_Opt(Skills)
      } else  if (Type == .SkillType["Sacred"]) {
        #TODO#################
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
  #' Extract rlevant abilities from data frame of with all character abilities
  GetAbilities = function(SkillIndex) {
    if (!isTruthy(SkillIndex) || SkillIndex < 1 || SkillIndex > nrow(self$Skills))
      stop("Invalid skill index")

    Abilities  <- self$Skills[SkillIndex, paste0("abval", 1:3)]
    return(unlist(Abilities))
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
    if (!is.na(pmatch(SkillIdent, "All")))
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
  
  
  #' Roll 3d20 for a skill check
  Roll = function(SkillIdent, Mod, Routine = FALSE) {
    SkillIndex <- self$GetSkillIndex(SkillIdent)
    self$LastSkill <- SkillIndex
    Values <- self$GetSkillValues(SkillIndex, Mod + self$Modifier)
    self$LastAbilities <- Values[1:3]
    self$LastSkillVal <- Values[4]
    self$LastModifier <- Mod + self$Modifier
    self$LastQL <- NA
    self$LastResult <- NA
    self$LastRemainder <- NA
    self$LastFumbleEffect <- NA
    
    if (!Routine)
      self$LastRoll <- SkillRoll()
    else 
      self$LastRoll <- TRUE

    invisible(self)
  },
  
  

  #' Returns the success level (of `.SuccessLevel`) of the last roll
  VerifyLastRoll = function() {
    if (!isTruthy(self$LastRoll)) return(NA)
    
    if(is.numeric(self$LastRoll))
      Result <- VerifySkillRoll(self$LastRoll, self$LastAbilities, 
                                self$LastSkillVal, self$LastModifier)
    else if (isTRUE(self$LastRoll))
      Result <- VerifyRoutineSkillCheck(self$LastAbilities, 
                                        self$LastSkillVal, 
                                        self$LastModifier)
    else
      Result <- NA
    
    self$LastQL <- Result[["QL"]]
    self$LastResult <- Result[["Message"]]
    self$LastRemainder <- Result[["Remainder"]]
    return(Result)
  },
  
  
  #' Return the quality level (compute it first if necessary).
  GetLastQL = function() {
    if (!isTruthy(self$LastQL) || !is.numeric(self$LastQL))
      self$VerifyLastRoll()
    
    return(self$LastQL)
  },
  
  #' Verifies if the desired skill supports a skill check
  CanRoutineCheck = function(SkillIdent, Mod) {
#browser()
    Values <- self$GetSkillValues(SkillIdent, 0, NoSkill = FALSE)
    Result <- CanRoutineSkillCheck(Values[1:3], Values[4], self$Modifier + Mod)
    return(Result)
  },
  
  
  FumbleRollRequired = function() {
    return(FALSE) # not applicable for profane skills
  },
  
  RollFumble = function() {
    invisible(self) # not applicable for profane skills
  }
))
