# 

#' CombatRoll
#' A combat roll is a roll with a 20-sided die.
#' @return A random number between 1 and 20
CombatRoll <- function() {
  return(sample.int(20, 1))
}

#' DamageRoll
#' A damage roll uses 1 or more d6 depending on the weapon.
#' @param D6 Number of d6 - depends on weapon.
#' @param Mod Additional modifier - depends on weapon.
#' @return nD + m, i.e. a random number between n+m and (n*6)+m
DamageRoll <- function(D6 = 1, Mod = 0) {
  return(sum(sample.int(6, D6)) + Mod)
}

#' CombatFumbleRoll
#' Determine the effect after a fumble in combat.
#' @return A 2d6 random number (i.e. between 2 and 12)
CombatFumbleRoll <- function() {
  Result <- sum(sample.int(6, 2))
}


#' GetCombatFumbleEffect
#' Describe the effect of a fumble in combat.
#' @param Roll Number between 2 and 12. The result of `CombatFumbleRoll`.
#' @return A string
GetCombatFumbleEffect <- function(Roll) {
  if(missing(Roll)) stop("No roll given")
  if(Roll < 2 || Roll > 12) stop("Invalid fumble roll")
  
  Text <- data.frame(Roll = 2:12, Effect = c("Weapon destroyed",
                                             "Weapon heavily damaged",
                                             "Weapon damaged",
                                             "Weapon lost",
                                             "Weapon stuck",
                                             "You fell",
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
  if (Roll < 1 || Roll > 20) stop("Invalid roll")
  if(Skill < 0) stop("Invalid skill")

  # RUN
  if (Roll == 20) 
    Success <- "Fumble"
  else if (Roll == 1)
    Success <- "Critical"
  else
    Success <- ifelse(Roll <= Skill+Penalty, "Success", "Fail")
  
  return(Success)
}
#VerifyCombatRoll(2, 9)




#' VerifySkillRoll
#' Checks if a skill roll was successfull
#' @param Roll Three results of a 1d20
#' @param Abilities Values of three abilities
#' @param Skill Skill value (integer)
#' @param Modifier Integer
#' @return A string indicating the result, one of: Fumble, Fail, Success, Critical.
VerifySkillRoll <- function(Roll, Abilities = c(10, 10, 10), Skill = 0, Modifier = 0) {
  if (length(Roll) != 3) stop("Skill roll shall have exactly three values")
  if (length(Abilities) != 3) stop("Skill roll requires 3 abilities to roll against")
  
  EffectiveQualities <- Abilities + Modifier
  Check <- pmax(Roll - EffectiveQualities, rep(0, 3))
  Success <- ifelse(sum(Check) <= Skill, "Success", "Fail")
  if (sum(Roll == 20) >= 2) 
    Success <- "Fumble"
  else if (sum(Roll == 1) >= 2)
    Success <- "Critical"
  
  return(Success)
}
