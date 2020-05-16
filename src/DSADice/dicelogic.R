# 

VerifySkillRoll <- function(Roll, Qualities = c(10, 10, 10), Skill = 0, Modifier = 0) {
  if (length(Roll) != 3) stop("Skill roll shall have exactly three values")
  EffectiveQualities <- Qualities + Modifier
  #Check <- pmax(Roll, EffectiveQualities)
  Check <- pmax(Roll - EffectiveQualities, rep(0, 3))
  Success <- sum(Check) <= Skill
  Success
}
