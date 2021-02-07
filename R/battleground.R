library(R6)
require(jsonlite)
source("./rules.R")
#source("./R/rules.R")


##' WeaponBase class (abstract base class for weapons)
##' This class wraps basic functions.
##' @importFrom R6 R6Class
##' @export
CombatEnvironment <- R6Class(
  "CombatEnvironment",
  active = list(
    WeaponType = function(value) {
      if (missing(value)) {
        return(private$.WeaponType)
      } else {
        if (private$.WeaponType %in% .WeaponType)
          private$.WeaponType <- .WeaponType[value]
      }
    },

    CombatRange = function(value) {
      if (missing(value)) {
        return(private$.Hero.CombatRange)
      } else {
        if (private$.WeaponType == .WeaponType["Ranged"])
          private$.Hero.CombatRange <- .RangedCombatRange[value]
        else
          private$.Hero.CombatRange <- .CloseCombatRange[value]
      }
    },

    Visibility = function(value) {
      if (missing(value)) {
        return(private$.Environment.Visibility)
      } else {
        isValid <- value %in% .Visibility | value %in% names(.Visibility)
        if (!isValid && (is.integer(value) || is.character(value)))
          stop(sprintf("Visibility of '%s' is unknown", str(value)))
        else
          value <- .Visibility["Clearly"]  # default

        private$.Environment.Visibility <- .Visibility[value]
      }
    },

    CrampedSpace = function(value) {
      if (missing(value)) {
        return(private$.Environment.CrampedSpace)
      } else {
        isValid <- value %in% .CrampedSpace | value %in% names(.CrampedSpace)
        if (!isValid && (is.integer(value) || is.character(value)))
          stop(sprintf("Cramped space of '%s' is unknown", str(value)))
        else
          value <- .CrampedSpace["Free"]  # default

        private$.Environment.CrampedSpace <- .CrampedSpace[value]
      }
    },

    UnderWater = function(value) {
      if (missing(value)) {
        return(private$.Environment.UnderWater)
      } else {
        isValid <- value %in% .UnderWater | value %in% names(.UnderWater)
        if (!isValid && (is.integer(value) || is.character(value)))
          stop(sprintf("Under water condition of '%s' is unknown", str(value)))
        else
          value <- .UnderWater["Dry"]  # default

        private$.Environment.UnderWater <- .UnderWater[value]
      }
    }


  ),
  public = list(
    #' Constructor
    #' @param WeaponType A `.WeaponType` enum value
    #' @return `self`
    initialize = function(WeaponType) {
      if (isFALSE(WeaponType %in% .WeaponType)) stop("Unknown weapon type")

      if (WeaponType == .WeaponType["Ranged"]) {
        Range <- c(0, 0, 0)
        HeroSpeed  <- .Movement["Stationary"]
        EnemyRange <- .RangedCombatRange["Medium"]
      }
      else {
        Range <- .CloseCombatRange["Short"]
        HeroSpeed  <- .Movement["Stationary"]
        EnemyRange <- Range
      }

      return(invisible(
        self$initCombatEnvironment(
          Type  = WeaponType,
          Range = Range,
          HeroMoves  = .MeansOfMovement["OnFoot"],
          HeroSpeed  = HeroSpeed,
          EnemyRange = EnemyRange,
          EnemySize  = .TargetSize["Medium"],
          EnemyDistance = .TargetDistance["Medium"],
          EnemySpeed = .Movement["Slow"],
          Evasive    = .EvasiveMovement["None"],
          Visibility = .Visibility["Clearly"],
          ElbowRoom  = .CrampedSpace["Free"],
          Underwater = .UnderWater["Dry"])
      ))
    },


    #'
    initCombatEnvironment = function(Type, Range, HeroMoves, HeroSpeed,
                                      EnemyRange, EnemySize, EnemyDistance, EnemySpeed, Evasive,
                                      Visibility, ElbowRoom, Underwater) {
      # PRECONDITIONS
      if (isFALSE(Type %in% .WeaponType))
        stop("Weapon type is the most important criterion of a weapon and cannot be omitted")
      private$.WeaponType <- .WeaponType[Type]

      # Hero Section: build hero sub-list
      self$CombatRange <- Range
      self$SetHeroMovement(HeroMoves, HeroSpeed)

      self$SetOpponent(EnemyRange, EnemySize, EnemyDistance, EnemySpeed, Evasive)

      self$Visibility   <- Visibility
      self$CrampedSpace <- ElbowRoom
      self$UnderWater   <- Underwater

      return(invisible(self))
    },


    #' @title SetOpponent
    #' @param CloseCombatRange A `.CloseCombatRange` enum
    #' @param Size A `.TargetSize` enum
    #' @param Movement A `.Movement` enum
    #' @param Evasive An `.EvasiveMovement` enum
    #' @return `invisible(self)`
    SetOpponent = function(CloseCombatRange, Size, Distance, Movement, Evasive) {
      if (!missing(CloseCombatRange)) {
        if (any(is.na(CloseCombatRange)) ||
            CloseCombatRange %in% .CloseCombatRange ||
            CloseCombatRange %in% names(.CloseCombatRange)) {
          private$.Opponent.CloseCombatRange <- .CloseCombatRange[CloseCombatRange]
        }
      }
      if (!missing(Size)) {
        if (any(is.na(Size)) ||
            Size %in% .TargetSize || Size %in% names(.TargetSize)) {
          private$.Opponent.Size <- .TargetSize[Size]
        }
      }
      if (!missing(Distance)) {
        if (any(is.na(Distance)) ||
            Size %in% .TargetDistance || Size %in% names(.TargetDistance)) {
          private$.Opponent.Distance <- .TargetDistance[Distance]
        }
      }
      if (!missing(Movement)) {
        if (any(is.na(Movement)) ||
            Movement %in% .Movement || Movement %in% names(.Movement)) {
          private$.Opponent.Movement <- .Movement[Movement]
        }
      }
      if (!missing(Evasive)) {
        if (any(is.na(Evasive)) ||
            Evasive %in% .EvasiveMovement || Evasive %in% names(.EvasiveMovement)) {
          private$.Opponent.EvasiveMovement <- .EvasiveMovement[Evasive]
        }
      }
      return(invisible(self))
    },


    SetHeroMovement = function(Means, Speed) {
      if (Means %in% .MeansOfMovement || Means %in% names(.MeansOfMovement))
        private$.Hero.MeansOfMovement <- .MeansOfMovement[Means]
      else
        stop("Unknown means of movement")

      if (.MeansOfMovement[Means] == .MeansOfMovement["OnFoot"])
        private$.Hero.Movement <- .Movement[Speed]
      else if (.MeansOfMovement[Means] == .MeansOfMovement["Mounted"])
        private$.Hero.Movement <- .MountedMovement[Speed]
      else
        stop("Unhandled means of movement")

      return(invisible(self))
    },


    GetCombatEnvironment = function(weaponType = self$WeaponType) {
      if (weaponType %in% .WeaponType)
        self$WeaponType <- .WeaponType[weaponType]

      CombatEnv <- list(
        Hero = list(
          WeaponType = private$.WeaponType
        ),
        Opponent = list(
          TargetSize   = private$.Opponent.Size
        ),
        Environment = list(
          Visibility   = private$.Environment.Visibility,
          UnderWater   = private$.Environment.UnderWater
        )
      )

      if (private$.WeaponType == .WeaponType["Melee"]) {
        CombatEnv[["Hero"]]$CloseCombatRange     <- private$.Hero.CombatRange
        CombatEnv[["Opponent"]]$CloseCombatRange <- private$.Opponent.CloseCombatRange
        CombatEnv[["Environment"]]$CrampedSpace  <- private$.Environment.CrampedSpace
      }

      if (private$.WeaponType == .WeaponType["Unarmed"]) {
        CombatEnv[["Hero"]]$CloseCombatRange     <- private$.Hero.CombatRange
        CombatEnv[["Opponent"]]$CloseCombatRange <- private$.Opponent.CloseCombatRange
      }

      if (private$.WeaponType == .WeaponType["Shield"]) {
        CombatEnv[["Hero"]]$CloseCombatRange     <- private$.Hero.CombatRange
        CombatEnv[["Opponent"]]$CloseCombatRange <- private$.Opponent.CloseCombatRange
        CombatEnv[["Environment"]]$CrampedSpace  <- private$.Environment.CrampedSpace
      }

      if (private$.WeaponType == .WeaponType["Ranged"]) {
        CombatEnv[["Hero"]]$RangedCombatRange <- private$.Hero.CombatRange
        CombatEnv[["Hero"]]$MeansOfMovement   <- private$.Hero.MeansOfMovement
        CombatEnv[["Hero"]]$Movement          <- private$.Hero.Movement

        CombatEnv[["Opponent"]]$Distance        <- private$.Opponent.Distance
        CombatEnv[["Opponent"]]$Movement        <- private$.Opponent.Movement
        CombatEnv[["Opponent"]]$EvasiveMovement <- private$.Opponent.EvasiveMovement
      }

      return(CombatEnv)
    }
  ),
  private = list(
    .WeaponType = NA,     # .WeaponType
    .Hero.CombatRange = NA,    # .CloseCombatRange / .RangedCombatRange
    .Hero.MeansOfMovement = NA, # .MeansOfMovement
    .Hero.Movement = NA,        # .Movement / .MountedMoevement
    .Opponent.CloseCombatRange = NA, # .CloseCombatRange
    .Opponent.Size = NA,             # .TargetSize
    .Opponent.Distance = NA,         # .TargetDistance
    .Opponent.Movement = NA,         # .Movement
    .Opponent.EvasiveMovement = NA,  # .EvasiveMovement
    .Environment.Visibility = NA,    # .Visibility
    .Environment.CrampedSpace = NA,  # .CrampedSpace
    .Environment.UnderWater = NA     # .UnderWater
  )
)

#  ce <- CombatEnvironment$new(.WeaponType["Ranged"])
# x <- ce$GetCombatEnvironment(.WeaponType["Ranged"])
# View(x)

# initCombatEnvironment <- function(Type, Range, HeroMoves, HeroSpeed,
#                                   EnemyRange, EnemySize, EnemySpeed, Evasive,
#                                   Visibility, ElbowRoom, Underwater) {
#   # PRECONDITIONS
#   if (isFALSE(Type %in% .WeaponType))
#     stop("Weapon type is the most important criterion of a weapon and cannot be omitted")
#
#   CombatEnv <- list(
#     Hero = list(), Opponent = list(), Environment = list()
#   )
#
#   # Hero Section: build hero sub-list
#   CombatEnv[["Hero"]]$WeaponType <- .WeaponType[Type]
#
#   if (.WeaponType[Type] == .WeaponType["Ranged"])
#     CombatEnv[["Hero"]]$RangedCombatRange <- .RangedCombatRange[Range]
#   else
#     CombatEnv[["Hero"]]$CloseCombatRange  <- .CloseCombatRange[Range]
#
#   if (.WeaponType[Type] == .WeaponType["Ranged"]) {
#     CombatEnv[["Hero"]]$MeansOfMovement <- .MeansOfMovement[HeroMoves]
#     if (.MeansOfMovement[HeroMoves] == .MeansOfMovement["OnFoot"])
#       CombatEnv[["Hero"]]$Movement <- .Movement[HeroSpeed]
#     else
#       CombatEnv[["Hero"]]$Movement <- .MountedMovement[HeroSpeed]
#   }
#
#   # Opponent Section: build sub-list
#   CombatEnv[["Opponent"]]$CloseCombatRange <- .CloseCombatRange[EnemyRange]
#   CombatEnv[["Opponent"]]$TargetSize       <- .TargetSize[EnemySize]
#
#   if (.WeaponType[Type] == .WeaponType["Ranged"]) {
#     CombatEnv[["Opponent"]]$Movement        <- .Movement[EnemySpeed]
#     Evasive <- ifelse(is.null(Evasive), .EvasiveMovement["None"], Evasive)
#     CombatEnv[["Opponent"]]$EvasiveMovement <- .EvasiveMovement[Evasive]
#   }
#
#   # Environment Section: build list, allow defaults
#   Visibility <- ifelse(is.null(Visibility), .Visibility["Clearly"], Visibility) #default
#   CombatEnv[["Environment"]]$Visibility   = .Visibility[Visibility]
#
#   ElbowRoom <- ifelse(is.null(ElbowRoom), .CrampedSpace["Free"], ElbowRoom) #default
#   CombatEnv[["Environment"]]$CrampedSpace = .CrampedSpace[ElbowRoom]
#
#   Underwater <- ifelse(is.null(Underwater), .UnderWater["Dry"], Underwater) #default
#   CombatEnv[["Environment"]]$UnderWater   = .UnderWater[Underwater]
#
#   return(CombatEnv)
# }
#
# defaultCombatEnvironment <- function(WeaponType = .WeaponType["Melee"]) {
#   if (isFALSE(WeaponType %in% .WeaponType)) stop("Unknown weapon type")
#
#   if (WeaponType == .WeaponType["Ranged"]) {
#     Range <- c(0, 0, 0)
#     HeroSpeed <- .Movement["Slow"]
#     EnemyRange <- .RangedCombatRange["Medium"]
#   }
#   else {
#     Range <- .CloseCombatRange["Short"]
#     HeroSpeed <- .Movement["Stationary"]
#     EnemyRange <- Range
#   }
#
#   return(
#     initCombatEnvironment(
#       Type  = WeaponType,
#       Range = Range,
#       HeroMoves  = .MeansOfMovement["OnFoot"],
#       HeroSpeed  = HeroSpeed,
#       EnemyRange = EnemyRange,
#       EnemySize  = .TargetSize["Medium"],
#       EnemySpeed = .Movement["Slow"],
#       Evasive    = .EvasiveMovement["None"],
#       Visibility = .Visibility["Clearly"],
#       ElbowRoom  = .CrampedSpace["Free"],
#       Underwater = .UnderWater["Dry"])
#   )
# }
