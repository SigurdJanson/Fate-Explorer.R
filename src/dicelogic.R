# 

CombatRoll <- function() {
  return(sample.int(20, 1))
}

DamageRoll <- function(W6 = 1, Mod = 0) {
  return(sum(sample.int(6, W6)) + Mod)
}

CombatFumbleRoll <- function() {
  Result <- sum(sample.int(6, 2))
}

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
