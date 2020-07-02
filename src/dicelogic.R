# 

#' VerifyConfirmation
#' Ascertain whether a confirmation roll was successful based on the result strings
#' of the original roll and its confirmation (i.e. it's success assessment).
#' @param RollResult A string  (one of Critical, Success, Fail, Fumble)
#' @param ConfirmationResult  A string  (one of Critical, Success, Fail, Fumble)
#' @return A string indicating the result (Critical, Success, Fail, Fumble)
VerifyConfirmation <- function( RollResult, ConfirmationResult ) {
  if (RollResult %in% c("Fumble", "Critical")) {
    PositiveCheck   <- (RollResult == "Critical")
    PositiveConfirm <- (ConfirmationResult %in% c("Success", "Critical"))
    Result <- c("Fumble", "Fail", "Success", "Critical")
    Result <- Result[(PositiveCheck*2 + PositiveConfirm) +1]
    return(Result)
  } else { # if not a critical/fumble this function does not add information
    return(RollResult) 
  }
}

# ABILITIES ---------------------------------------

#' AbilityRoll
#' An abilityRoll roll is a roll with a 20-sided die.
#' @return A random number between 1 and 20
AbilityRoll <- function() {
  return(sample.int(20L, 1L))
}

#' VerifyAbilityRoll
#' @param Roll Result of a die roll (1d20).
#' @param Ability The characters ability value (value between 1 and 20).
#' @param Modifier Values < 0 are penalties, >0 bonuses (integer).
#' @return A string indicating the result, one of: Fumble, Fail, Success, Critical.
VerifyAbilityRoll <- function(Roll, Ability, Modifier = 0L) {
  # PRECONDITIONS
  if (missing(Roll) || is.null(Roll)) return(NA)
  if (Roll < 1L || Roll > 20L) stop("Invalid roll")
  if(Ability < 0L) stop("Invalid ability")
  
  # RUN
  if (Roll == 20L)
    Success <- "Fumble"
  else if (Roll == 1L)
    Success <- "Critical"
  else
    Success <- ifelse(Roll <= Ability+Modifier, "Success", "Fail")
  
  return(Success)
}



# COMBAT ---------------------------

#' CombatRoll
#' A combat roll is a roll with a 20-sided die.
#' @return A random number between 1 and 20
CombatRoll <- function() {
  return(sample.int(20L, 1L))
}

#' DamageRoll
#' A damage roll uses 1 or more d6 depending on the weapon.
#' @param D6 Number of d6 - depends on weapon.
#' @param Mod Additional modifier - depends on weapon.
#' @return nD + m, i.e. a random number between n+m and (n*6)+m
DamageRoll <- function(D = 1L, DP = 6L, Mod = 0L) {
  return(sum(sample.int(DP, D, replace = TRUE)) + Mod)
}

#' CombatFumbleRoll
#' Determine the effect after a fumble in combat.
#' @return A 2d6 random number (i.e. between 2 and 12)
CombatFumbleRoll <- function() {
  Result <- sum(sample.int(6L, 2L, replace = TRUE))
}


#' GetCombatFumbleEffect
#' Describe the effect of a fumble in combat.
#' @param Roll Number between 2 and 12. The result of `CombatFumbleRoll`.
#' @return A string
GetCombatFumbleEffect <- function(Roll) {
  if(missing(Roll)) stop("No roll given")
  if(Roll < 2L || Roll > 12L) stop("Invalid fumble roll")
  
  Text <- data.frame(Roll = 2:12, Effect = c("Weapon destroyed",
                                             "Weapon heavily damaged",
                                             "Weapon damaged",
                                             "Weapon lost",
                                             "Weapon stuck",
                                             "Fall",
                                             "Stumble",
                                             "Twisted foot",
                                             "Bump on the head",
                                             "Hurt yourself",
                                             "Hurt yourself bad"))
  Result <- Text$Effect[Text$Roll == Roll]
  return(as.character(Result))
}


#' VerifyCombatRoll
#' Confirms a fumble or critical with a d20.
#' @param Roll The result of a d20
#' @param Skill The combat skill against which was rolled.
#' @param Penalty A penalty on the skill, integer <= 0
#' @return A string indicating the result, one of: Fumble, Fail, Success, Critical.
VerifyCombatRoll <- function(Roll, Skill, Penalty = 0L) {
  # PRECONDITIONS
  if (Roll < 1L || Roll > 20L) stop("Invalid roll")
  if(Skill < 0L) stop("Invalid skill")

  # RUN
  if (Roll == 20L) 
    Success <- "Fumble"
  else if (Roll == 1L)
    Success <- "Critical"
  else
    Success <- ifelse(Roll <= Skill+Penalty, "Success", "Fail")
  
  return(Success)
}
#VerifyCombatRoll(2, 9)


# SKILL ------------------------------

#' SkillRoll
#' A combat roll is a 3 x d20 roll.
#' @return A numeric vector with 3 random numbers between 1 and 20
SkillRoll <- function() {
  return(sample.int(20, 3, TRUE))
}


#' SkillRollQuality
#' Determine quality level
#' @param Remainder The remainder of a skill roll.
#' @return  A quality level. If the roll wasn't successfull it returns 0.
SkillRollQuality <- function(Remainder) {
  if (Remainder < 0L) return(0L)
  return(max( ((Remainder-1L) %/% 3L)+1L, 1L ))
}


#' VerifySkillRoll
#' Checks if a skill roll was successfull
#' @param Roll Three results of a 1d20 (integer)
#' @param Abilities Values of three abilities (integer)
#' @param Skill Skill value (integer)
#' @param Modifier Penalty or advantage (integer)
#' @return A string indicating the result, one of: Fumble, Fail, Success, Critical.
VerifySkillRoll <- function(Roll, Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = 0L) {
  if (length(Roll) != 3L) stop("Skill roll shall have exactly three values")
  if (length(Abilities) != 3L) stop("Skill roll requires 3 abilities to roll against")
  
  EffectiveQualities <- Abilities + Modifier
  Check <- pmax(Roll - EffectiveQualities, rep(0L, 3L))
  Remainder <- Skill - sum(Check)
  
  Success <- ifelse(Remainder >= 0, "Success", "Fail")
  QL <- SkillRollQuality(Remainder)
  
  if (sum(Roll == 20L) >= 2L) 
    Success <- "Fumble"
  else if (sum(Roll == 1L) >= 2L)
    Success <- "Critical"
  
  return(list(Message = Success, QL = QL, Remainder = Remainder))
}



#' CanRoutineSkillCheck
#' Determines for a given skill if a routine check is valid
#' @param Skill A single skill value (integer)
#' @param Abilities A vector with exactly three ability values (integer)
#' @param Mod A check modifier
#' @details 
#' `(-Mod+4)*3-2`with `Mod <- c(-3:3)` returns the values in table 
#' of [routine check requirements](https://ulisses-regelwiki.de/index.php/GR_Routineprobe.html).
#' @source Basic rules, german edition Seite 184/185.
#' @return TRUE/ FALSE. If `Mod`is `NA` then a 
CanRoutineSkillCheck <- function(Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = NA) {
  if (length(Abilities) != 3) stop("Three abilities make a skill check")
  if (length(Skill) != 1) stop("Exactly one skill value is needed for skill check")
  
  if (is.na(Modifier)) 
    SufficientSkill <- TRUE
  else
    SufficientSkill <- Skill >= (-Modifier+4)*3-2
  
  SufficientAbility <- all(Abilities >= 13)
  #
  return( SufficientAbility & SufficientSkill )
}


#' VerifyRoutineSkillCheck
#' Verify the result of a routine skill check. 
#' @param Skill A list with all the skill data
#' @param Abilities A data frame containing the ability values
#' @param Mod A check modifier
#' @return Returns a list with success, quality level and remaining skill points (equivalent
#' to [VerifySkillRoll()]).
VerifyRoutineSkillCheck <- function(Abilities = c(10L, 10L, 10L), Skill = 0L, Modifier = 0) {
  if (CanRoutineSkillCheck(Abilities, Skill, Modifier)) {
    QL <- as.integer( SkillRollQuality(round(Skill / 2)) )
  } else {
    QL <- 0L
  }
  return(list(Message = ifelse(QL > 0, "Success", "Fail"), QL = QL, Remainder = "."))
}