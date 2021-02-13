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
        if (is.null(value) || is.na(value)) value <- .Visibility["Clearly"]  # default

        isValid <- value %in% .Visibility | value %in% names(.Visibility)
        if (isFALSE(isValid))
          stop(sprintf("Visibility of '%s' is unknown", str(value)))

        private$.Environment.Visibility <- .Visibility[value]
      }
    },

    CrampedSpace = function(value) {
      if (missing(value)) {
        return(private$.Environment.CrampedSpace)
      } else {
        if (is.null(value) || is.na(value)) value <- .CrampedSpace["Free"]  # default

        isValid <- value %in% .CrampedSpace | value %in% names(.CrampedSpace)
        if (isFALSE(isValid))
          stop(sprintf("Cramped space of '%s' is unknown", str(value)))

        private$.Environment.CrampedSpace <- .CrampedSpace[value]
      }
    },

    UnderWater = function(value) {
      if (missing(value)) {
        return(private$.Environment.UnderWater)
      } else {
        if (is.null(value) || is.na(value)) value <- .UnderWater["Dry"]

        isValid <- value %in% .UnderWater | value %in% names(.UnderWater)
        if (isFALSE(isValid))
          stop(sprintf("Under water condition of '%s' is unknown", str(value)))

        private$.Environment.UnderWater <- .UnderWater[value]
      }
    }


  ),
  public = list(
    Defaults = list(
      WeaponType = .WeaponType, # c(Unarmed = 1L, Melee = 2L, Ranged = 3L, Shield = 4L)
      Hero.CloseCombatRange = rep(.CloseCombatRange["Short"], 4L),
      Hero.MeansOfMovement  = rep(.MeansOfMovement["OnFoot"], 4L),
      Hero.Movement         = rep(.Movement["Stationary"],    4L),
      Opponent.CloseCombatRange = rep(.CloseCombatRange["Short"], 4L),
      Opponent.TargetSize       = rep(.TargetSize["Medium"], 4L),
      Opponent.TargetDistance   = c(rep(.TargetDistance["Close"], 2L), .TargetDistance["Medium"], .TargetDistance["Close"]),
      Opponent.Movement     = c(rep(.Movement["Stationary"], 2L), .Movement["Slow"], .Movement["Stationary"]),
      Opponent.EvasiveMovement  = rep(.EvasiveMovement["None"], 4L),
      Environment.Visibility    = rep(.Visibility["Clearly"], 4L),
      Environment.CrampedSpace  = rep(.CrampedSpace["Free"], 4L),
      Environment.UnderWater    = rep(.UnderWater["Dry"], 4L)
    ),

    #' Constructor
    #' @param WeaponType A `.WeaponType` enum value
    #' @return `self`
    initialize = function(WeaponType) {
      if (isFALSE(WeaponType %in% .WeaponType)) stop("Unknown weapon type")

      self$resetToDefault(WeaponType)
      return(invisible(self))
    },


    #'
    initCombatEnvironment = function(Type, Range, HeroMoves, HeroSpeed,
                                      EnemyRange, EnemySize, EnemyDistance, EnemySpeed, Evasive,
                                      Visibility, ElbowRoom, Underwater) {
#-browser()
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


    resetToDefault = function(weaponType) {
      if (isFALSE(weaponType %in% .WeaponType)) stop("Unknown weapon type")

      invisible(
        self$initCombatEnvironment(
          Type  = weaponType,
          Range = self$Defaults[["Hero.CombatRange"]][weaponType],
          HeroMoves     = self$Defaults[["Hero.MeansOfMovement"]][weaponType],
          HeroSpeed     = self$Defaults[["Hero.Movement"]][weaponType],
          EnemyRange    = self$Defaults[["Opponent.CloseCombatRange"]][weaponType],
          EnemySize     = self$Defaults[["Opponent.TargetSize"]][weaponType],
          EnemyDistance = self$Defaults[["Opponent.TargetDistance"]][weaponType],
          EnemySpeed    = self$Defaults[["Opponent.Movement"]][weaponType],
          Evasive       = self$Defaults[["Opponent.EvasiveMovement"]][weaponType],
          Visibility    = self$Defaults[["Environment.Visibility"]][weaponType],
          ElbowRoom     = self$Defaults[["Environment.CrampedSpace"]][weaponType],
          Underwater    = self$Defaults[["Environment.UnderWater"]][weaponType]
        ))
    },


    getValue = function(groupId = "", valueId) {
      Names <- names(private)
      Found <- endsWith(Names, valueId)
      Found <- Names[Found]
      if (length(Found) > 1) {
        Found <- paste("", groupId, valueId, sep = ".")
      }
      return(private[[Found]])
    },

    getDefault = function(groupId = "", valueId) {
  #browser()
      Names <- names(self$Defaults)
      Found <- endsWith(Names, valueId)
      Found <- Names[Found]
      if (length(Found) > 1) {
        Found <- paste(groupId, valueId, sep = ".")
      }
      Row    <- .WeaponType[self$Defaults$WeaponType] == .WeaponType[private$.WeaponType]
      Column <- names(self$Defaults) == Found
      Result <- self$Defaults[[which(Column)]][Row]
      if (isFALSE(nrow(Result) > 0 && ncol(Result) > 0)) Result <- NA
      return(Result)
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
          private$.Opponent.TargetSize <- .TargetSize[Size]
        }
      }
      if (!missing(Distance)) {
        if (any(is.na(Distance)) ||
            Distance %in% .TargetDistance || Distance %in% names(.TargetDistance)) {
          private$.Opponent.TargetDistance <- .TargetDistance[Distance]
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

      if (isTRUE(.MeansOfMovement[Means] == .MeansOfMovement["OnFoot"]))
        private$.Hero.Movement <- .Movement[Speed]
      else if (isTRUE(.MeansOfMovement[Means] == .MeansOfMovement["Mounted"]))
        private$.Hero.Movement <- .MountedMovement[Speed]
      else
        stop("Unhandled means of movement")

      return(invisible(self))
    },

    GetDefaultCombatEnvironment = function(weaponType = self$WeaponType) {
      if (weaponType %in% .WeaponType)
        self$WeaponType <- .WeaponType[weaponType]

      Values <- lapply(self$Defaults, `[`, .WeaponType[private$.WeaponType])

      Names <- names(Values)
      Names <- strsplit(Names, ".", fixed = TRUE)
      Names <- sapply(Names, function(x) x[[length(x)]]) #TODO try ..., tail, n = 1L
      names(Values) <- Names

      CombatEnv <- list(
        Hero = Values[c("WeaponType", "CloseCombatRange", "MeansOfMovement", "Movement")],
        Opponent = Values[c("CloseCombatRange", "TargetSize", "TargetDistance", "Movement", "EvasiveMovement")],
        Environment = Values[c("Visibility", "CrampedSpace", "UnderWater")]
      )

      return(CombatEnv)
    },


    GetCombatEnvironment = function(weaponType = self$WeaponType) {
      if (weaponType %in% .WeaponType)
        self$WeaponType <- .WeaponType[weaponType]
      # Fill all values with defaults
      CombatEnv <- self$GetDefaultCombatEnvironment(self$WeaponType)

      CombatEnv[["Hero"]]$WeaponType     <- private$.WeaponType
      CombatEnv[["Opponent"]]$TargetSize <- private$.Opponent.TargetSize
      CombatEnv[["Environment"]]$Visibility <- private$.Environment.Visibility
      CombatEnv[["Environment"]]$UnderWater <- private$.Environment.UnderWater

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
        #CombatEnv[["Hero"]]$RangedCombatRange <- private$.Hero.CombatRange
        CombatEnv[["Hero"]]$MeansOfMovement   <- private$.Hero.MeansOfMovement
        CombatEnv[["Hero"]]$Movement          <- private$.Hero.Movement

        CombatEnv[["Opponent"]]$TargetDistance  <- private$.Opponent.TargetDistance
        CombatEnv[["Opponent"]]$Movement        <- private$.Opponent.Movement
        CombatEnv[["Opponent"]]$EvasiveMovement <- private$.Opponent.EvasiveMovement
      }

      return(CombatEnv)
    }
  ),
  private = list(
    .WeaponType = NA,     # .WeaponType
    .Hero.CombatRange = NA,     # .CloseCombatRange / .RangedCombatRange
    .Hero.MeansOfMovement = NA, # .MeansOfMovement
    .Hero.Movement = NA,        # .Movement / .MountedMoevement
    .Opponent.CloseCombatRange = NA, # .CloseCombatRange
    .Opponent.TargetSize = NA,       # .TargetSize
    .Opponent.TargetDistance = NA,   # .TargetDistance
    .Opponent.Movement = NA,         # .Movement
    .Opponent.EvasiveMovement = NA,  # .EvasiveMovement
    .Environment.Visibility = NA,    # .Visibility
    .Environment.CrampedSpace = NA,  # .CrampedSpace
    .Environment.UnderWater = NA     # .UnderWater
  )
)

# ce <- CombatEnvironment$new(.WeaponType["Melee"])
# print(ce$getValue("", "CombatRange"))
# print(ce$getValue("Hero", "CombatRange"))
# print(ce$getValue("", "Visibility"))
# print(ce$getValue("Environment", "Visibility"))
#
# print(ce$getDefault("", "CombatRange"))
# print(ce$getDefault("Opponent", "Distance"))
# print(ce$getDefault("", "Visibility"))
# print(ce$getDefault("Environment", "Visibility"))
#
# ce <- CombatEnvironment$new(.WeaponType["Melee"])
# print(ce$getDefault("Hero", "CombatRange"))


# x <- ce$GetCombatEnvironment(.WeaponType["Ranged"])
#View(x)

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
