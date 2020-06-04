# Explore Probabilities / Chances


#' dSumOfDice
#' Get the probability of rolling a sum of dice.
#' @param Expected 
#' @param DieCount Number of dice to sum up.
#' @param DieSides Sides of all dice
#' @note Works up to a `DieCount` of 20 with a d6.
#' @return The probability of rolling the sum `Expected` with `DieCount` dice with 
#' `DieSides` sides each.
#' @source Weisstein, E. W. "Dice." From MathWorld--A Wolfram Web Resource. https://mathworld.wolfram.com/Dice.html
dSumOfDice <- function(Expected, DieCount, DieSides) {
  if (Expected < DieCount || Expected > DieCount*DieSides)
    return(0)
  kmax <- floor((Expected-DieCount) / DieSides) # floor((p-n)/s)
  k <- seq(0L, kmax)
  term1 <- (-1)^k
  term2 <- choose(DieCount, k)
  #term3 <- choose((Expected - DieSides*k -1), (Expected - DieSides*k -DieCount))
  term3 <- choose((Expected - DieSides*k -1), (DieCount-1))
  sum(term1*term2*term3) / DieSides^DieCount
}



#' ChancesOfAttack
#' Returns the probabilities of achieveing botch, fail and hit points 
#' of an attack roll.
#' @param Value Attack value
#' @param Modifier Modifier, i.e. penalty or advantage on attack roll. 
#' Negative values are penalties.
#' @param DmgDieCount Number of dice used in the hit point roll
#' @param DmgDieSides Sides of dice in hit point roll
#' @param DmgMod Modifier for hit point roll
#' @return A data frame with the result in the first column and the probability
#' in the second.
ChancesOfAttack <- function(Value, Modifier = 0, 
                            DmgDieCount = 1, DmgDieSides = 6, DmgMod = 0) {
  ev <- Value + Modifier # Effective Value
  # Basic probabilities
  pC <- ev / 20 / 20      # probability p(Critical)
  pB <- (20-ev) / 20 / 20 # p(Botch)
  pS <- ev/20 - pC        # p(Success)
  pF <- (20-ev)/20 - pB   # p(Fail)
  
  # Damage probabilities: vector
  MinPts <- DmgDieCount # without modifier
  MaxPts <- DmgDieCount * DmgDieSides # without modifier 
  Points <- MinPts:MaxPts
  Chance <- sapply(Points, dSumOfDice, DmgDieCount, DmgDieSides)
  
  HitPoints <- Points + Modifier # = [2, 12] for a 2d6+0
  CriticalHitPoints <- (Points + Modifier)*2
  HitPointChance <- Chance * pS
  CriticalHitPointChance <- Chance * pC
  
  HitPoints <- cbind(HitPoints, HitPointChance)
  CriticalHitPoints <- cbind(CriticalHitPoints, CriticalHitPointChance)
  
  TotalHitPoints <- merge(HitPoints, CriticalHitPoints, by.x=1, by.y=1, all = TRUE)
  TotalHitPoints$TotalChance <- rowSums(TotalHitPoints[, 2:3], na.rm = TRUE)
  TotalHitPoints <- TotalHitPoints[, -c(2,3)]
  TotalHitPoints <- rbind(list("Fumble", pB), list("Fail", pF), TotalHitPoints)
  
  return(TotalHitPoints)
}



#' ChancesOfDefense
#' Probabilities of parry and dodge rolls.
#' @param Value Either a parry or dodge value
#' @param Modifier Check modifier
#' @return Data frame with probabities
ChancesOfDefense <- function(Value, Modifier = 0) {
  ev <- Value + Modifier # Effective Value
  # Basic probabilities
  pC <- ev / 20 / 20      # probability p(Critical)
  pB <- (20-ev) / 20 / 20 # p(Botch)
  pS <- ev/20 - pC        # p(Success)
  pF <- (20-ev)/20 - pB   # p(Fail)
  
  TotalHitPoints <- data.frame(character(), TotalChance = numeric())
  TotalHitPoints <- rbind(list("Fumble", pB), 
                          list("Fail", pF), 
                          list("Success", pS),
                          list("Critical", pC))
  return(as.data.frame(TotalHitPoints))
}



