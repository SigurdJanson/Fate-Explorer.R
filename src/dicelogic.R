# 

#' AbilityRoll
#' An abilityRoll roll is a roll with a 20-sided die.
#' @return A random number between 1 and 20
AbilityRoll <- function() {
  return(sample.int(20L, 1L))
}

#' Title
#'
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
DamageRoll <- function(D6 = 1L, Mod = 0L) {
  return(sum(sample.int(6L, D6)) + Mod)
}

#' CombatFumbleRoll
#' Determine the effect after a fumble in combat.
#' @return A 2d6 random number (i.e. between 2 and 12)
CombatFumbleRoll <- function() {
  Result <- sum(sample.int(6L, 2L))
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
