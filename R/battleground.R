# library(R6)
require(jsonlite)
source("./rules.R")


initCombatEnvironment <- function(Type, Range, HeroMoves, HeroSpeed,
                                  EnemyRange, EnemySize, EnemySpeed, Evasive,
                                  Visibility, ElbowRoom, Underwater) {
  # PRECONDITIONS
  if (isFALSE(Type %in% .WeaponType)) 
    stop("Weapon type is the most important criterion of a weapon and cannot be omitted")
  
  CombatEnv <- list(
    Hero = list(), Opponent = list(), Environment = list()
  )

  # Hero Section: build hero sub-list
  CombatEnv[["Hero"]]$WeaponType <- .WeaponType[Type]

  if (.WeaponType[Type] == .WeaponType["Ranged"]) 
    CombatEnv[["Hero"]]$RangedCombatRange <- .RangedCombatRange[Range]
   else
    CombatEnv[["Hero"]]$CloseCombatRange  <- .CloseCombatRange[Range]
  
  if (.WeaponType[Type] == .WeaponType["Ranged"]) {
    CombatEnv[["Hero"]]$MeansOfMovement <- .MeansOfMovement[HeroMoves]
    CombatEnv[["Hero"]]$Movement        <- .Movement[HeroSpeed]
  }
  
  # Opponent Section: build sub-list
  CombatEnv[["Opponent"]]$CloseCombatRange <- .CloseCombatRange[EnemyRange]
  CombatEnv[["Opponent"]]$TargetSize       <- .TargetSize[EnemySize]

  if (.WeaponType[Type] == .WeaponType["Ranged"]) {
    CombatEnv[["Opponent"]]$Movement        <- .Movement[EnemySpeed]
    Evasive <- ifelse(is.null(Evasive), .EvasiveMovement["None"], Evasive)
    CombatEnv[["Opponent"]]$EvasiveMovement <- .EvasiveMovement[Evasive]
  }
  
  # Environment Section: build list, allow defaults
  Visibility <- ifelse(is.null(Visibility), .Visibility["Clearly"], Visibility) #default
  CombatEnv[["Environment"]]$Visibility   = .Visibility[Visibility]
  
  ElbowRoom <- ifelse(is.null(ElbowRoom), .CrampedSpace["Free"], ElbowRoom) #default
  CombatEnv[["Environment"]]$CrampedSpace = .CrampedSpace[ElbowRoom]
  
  Underwater <- ifelse(is.null(Underwater), .UnderWater["Dry"], Underwater) #default
  CombatEnv[["Environment"]]$UnderWater   = .UnderWater[Underwater]
  
  return(CombatEnv)
}




defaultCombatEnvironment <- function(WeaponType = .WeaponType["Melee"]) {
  if (isFALSE(WeaponType %in% .WeaponType)) stop("Unknown weapon type")

  if (WeaponType == .WeaponType["Ranged"]) {
    Range <- c(0, 0, 0)
    HeroSpeed <- .Movement["Slow"]
    EnemyRange <- .RangedCombatRange["Medium"]
  }
  else {
    Range <- .CloseCombatRange["Short"]
    HeroSpeed <- .Movement["Stationary"]
    EnemyRange <- Range
  }
  
  return(
    initCombatEnvironment(
      Type  = WeaponType, 
      Range = Range, 
      HeroMoves  = .MeansOfMovement["OnFoot"], 
      HeroSpeed  = HeroSpeed,
      EnemyRange = EnemyRange,
      EnemySize  = .TargetSize["Medium"], 
      EnemySpeed = .Movement["Slow"],
      Evasive    = .EvasiveMovement["None"],
      Visibility = .Visibility["Clearly"],
      ElbowRoom  = .CrampedSpace["Free"], 
      Underwater = .UnderWater["Dry"])
  )
}
