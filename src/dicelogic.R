# 

CombatRoll <- function() {
  return(sample.int(20, 1))
}

DamageRoll <- function(Mod = 0) {
  return(sample.int(6, 1) + Mod)
}


#' VerifyCombatRoll
#'
#' @param Roll 
#' @param Skill 
#'
#' @return
#' @export
#'
#' @examples
VerifyCombatRoll <- function(Roll, Skill) {
  # PRECONDITIONS
  if (Roll < 1 || Roll > 20) stop("Invalid roll")
  if(Skill < 0) stop("Invalid skill")

  # RUN
  if (Roll == 20) 
    Success <- "Fumble"
  else if (Roll == 1)
    Success <- "Critical"
  else
    Success <- ifelse(Roll <= Skill, "Success", "Fail")
  
  return(Success)
}
#VerifyCombatRoll(2, 9)




VerifySkillRoll <- function(Roll, Qualities = c(10, 10, 10), Skill = 0, Modifier = 0) {
  if (length(Roll) != 3) stop("Skill roll shall have exactly three values")
  EffectiveQualities <- Qualities + Modifier
  Check <- pmax(Roll - EffectiveQualities, rep(0, 3))
  Success <- ifelse(sum(Check) <= Skill, "Success", "Fail")
  if (sum(Roll == 20) >= 2) 
    Success <- "Fumble"
  else if (sum(Roll == 1) >= 2)
    Success <- "Critical"
  
  return(Success)
}
