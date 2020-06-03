# Explore Probabilities / Chances


dSumOfDice <- function(Expected, DieCount, DieSides) {
  if (Expected < DieCount || Expected > DieCount*DieSides)
    return(0)
  kmax <- floor((Expected-DieCount) / DieSides)
  k <- seq(0L, kmax)
  term1 <- sum( (-1)^k * choose(DieCount, k) *
                choose((Expected - DieSides*k -1), (Expected - DieSides*k -DieCount)) )
  term1 / DieSides^DieCount
}


#' ChancesOfAttack
#'
#' @param Value Attack value
#' @param Modifier Modifier
#' @param DmgDieCount 
#' @param DmgDieSides 
#' @param DmgMod 
#'
#' @return A list
#' @examples 
ChancesOfAttack <- function(Value, Modifier = 0, 
                            DmgDieCount = 1, DmgDieSides = 6, DmgMod = 0) {
  # Basic probabilities
  pC <- Value / 20 / 20      # probability p(Critical)
  pB <- (20-Value) / 20 / 20 # probability p(Botch)
  pS <- Value/20 - pC        # p(Success)
  pF <- (20-Value)/20 - pB
  
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

