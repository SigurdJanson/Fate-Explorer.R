library(R6)
require(jsonlite)
source("./rules.R")
#source("./R/rules.R")


#' WeaponBase class (abstract base class for weapons)
#' This class wraps basic functions.
#' @importFrom R6 R6Class
#' @export
CombatEnvironment <- R6Class(
  "CombatEnvironment",
  active = list(

    #' @field WeaponType Gets/sets the weapon type (enum of .WeaponType)
    WeaponType = function(value) {
      if (missing(value)) {
        return(private$.Hero.WeaponType)
      } else {
        if (private$.Hero.WeaponType %in% .WeaponType)
          private$.Hero.WeaponType <- .WeaponType[value]
      }
    },

    #' @field CombatRange Gets/sets the heroes close combat range
    #' (enum of .CloseCombatRange)
    CombatRange = function(value) {
      if (missing(value)) {
        return(private$.Hero.CloseCombatRange)
      } else {
          private$.Hero.CloseCombatRange <- .CloseCombatRange[value]
      }
    },

    #' @field Visibility Gets/sets the visibility (enum of .Visibility)
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

    #' @field Visibility Gets/sets if the space is cramped (enum of .CrampedSpace)
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

    #' @field UnderWater Gets/sets if there is water around (enum of .UnderWater)
    UnderWater = function(value) {
      if (missing(value)) {
        return(private$.Environment.UnderWater)
      } else {
        if (is.null(value) || is.na(value))
          value <- .UnderWater["Dry"]

        isValid <- value %in% .UnderWater | value %in% names(.UnderWater)
        if (isFALSE(isValid))
          stop(sprintf("Under water condition of '%s' is unknown", str(value)))

        private$.Environment.UnderWater <- .UnderWater[value]
      }
    }


  ),
  public = list(

    #' @field Defaults The collection of all default values for combat environments
    Defaults = list(
      Hero.WeaponType = .WeaponType, # c(Unarmed = 1L, Melee = 2L, Ranged = 3L, Shield = 4L)
      Hero.CloseCombatRange = rep(.CloseCombatRange["Short"], 4L),
      Hero.MeansOfMovement  = rep(.MeansOfMovement["OnFoot"], 4L),
      Hero.Movement         = rep(.Movement["Stationary"],    4L),
      Opponent.CloseCombatRange = rep(.CloseCombatRange["Short"], 4L),
      Opponent.TargetSize       = rep(.TargetSize["Medium"],      4L),
      Opponent.TargetDistance   = c(rep(.TargetDistance["Close"], 2L), .TargetDistance["Medium"], .TargetDistance["Close"]),
      Opponent.Movement     = c(rep(.Movement["Stationary"], 2L), .Movement["Slow"], .Movement["Stationary"]),
      Opponent.EvasiveMovement  = rep(.EvasiveMovement["None"], 4L),
      Environment.Visibility    = rep(.Visibility["Clearly"], 4L),
      Environment.CrampedSpace  = rep(.CrampedSpace["Free"], 4L),
      Environment.UnderWater    = rep(.UnderWater["Dry"], 4L)
    ),

    #' Constructor
    #' @details Create a new combat environment and initialise with default values
    #' @param WeaponType A `.WeaponType` enum value
    #' @return `invisible(self)`
    initialize = function(WeaponType) {
      if (isFALSE(WeaponType %in% .WeaponType)) stop("Unknown weapon type")

      self$resetToDefault(WeaponType)
      return(invisible(self))
    },


    #' Method to set all environment variables at once
    #' @param weaponType,closeRange,HeroMoves,HeroSpeed Characteristics of the hero
    #' @param EnemyRange,EnemySize,EnemyDistance,EnemySpeed,Evasive
    #' @param Visibility,crampedSpace,Underwater
    #' @return `invisible(self)`
    initCombatEnvironment = function(weaponType, closeRange, HeroMoves, HeroSpeed,
                                      EnemyRange, EnemySize, EnemyDistance, EnemySpeed, Evasive,
                                      Visibility, crampedSpace, Underwater) {
#-browser()
      # PRECONDITIONS
      if (isFALSE(weaponType %in% .WeaponType))
        stop("Weapon type is the most important criterion of a weapon and cannot be omitted")
      private$.Hero.WeaponType <- .WeaponType[weaponType]

      # Hero Section: build hero sub-list
      self$CombatRange <- closeRange
      self$SetHeroMovement(HeroMoves, HeroSpeed)

      self$SetOpponent(EnemyRange, EnemySize, EnemyDistance, EnemySpeed, Evasive)

      self$Visibility   <- Visibility
      self$CrampedSpace <- crampedSpace
      self$UnderWater   <- Underwater

      return(invisible(self))
    },


    #' Method to reset all environment variables to default at once
    #' @param WeaponType A `.WeaponType` enum value
    #' @return `invisible(self)`
    resetToDefault = function(weaponType) {
      if (isFALSE(weaponType %in% .WeaponType)) stop("Unknown weapon type")

      invisible(
        self$initCombatEnvironment(
          weaponType  = weaponType,
          closeRange  = self$Defaults[["Hero.CloseCombatRange"]][weaponType],
          HeroMoves     = self$Defaults[["Hero.MeansOfMovement"]][weaponType],
          HeroSpeed     = self$Defaults[["Hero.Movement"]][weaponType],
          EnemyRange    = self$Defaults[["Opponent.CloseCombatRange"]][weaponType],
          EnemySize     = self$Defaults[["Opponent.TargetSize"]][weaponType],
          EnemyDistance = self$Defaults[["Opponent.TargetDistance"]][weaponType],
          EnemySpeed    = self$Defaults[["Opponent.Movement"]][weaponType],
          Evasive       = self$Defaults[["Opponent.EvasiveMovement"]][weaponType],
          Visibility    = self$Defaults[["Environment.Visibility"]][weaponType],
          crampedSpace  = self$Defaults[["Environment.CrampedSpace"]][weaponType],
          Underwater    = self$Defaults[["Environment.UnderWater"]][weaponType]
        ))
    },

    #' Method to get a value by name and group of the property.
    #' @param groupId one of `c("Hero", "Opponent", "Environment")`
    #' (optional, only required when valueId is not unambiguous)
    #' @param valueId The name of the property
    #' @return The requested value
    #' @examples
    #' getValue(valueId = "TargetDistance")
    #' getValue(valueId = "CloseCombatRange", "Hero") # group required
    getValue = function(groupId = "", valueId) {
      Names <- names(private)
      Found <- endsWith(Names, valueId)
      Found <- Names[Found]
      if (length(Found) > 1) {
        Found <- paste("", groupId, valueId, sep = ".")
      }
      return(private[[Found]])
    },

    #' Method to get the default value of a property by name (and group).
    #' @param groupId one of `c("Hero", "Opponent", "Environment")`
    #' (optional, only required when valueId is not unambiguous)
    #' @param valueId The name of the property
    #' @return The requested value
    #' @examples
    #' getDefault(valueId = "TargetDistance")
    #' getDefault(valueId = "CloseCombatRange", "groupId = Hero") # group required
    getDefault = function(groupId = "", valueId) {
  #browser()
      Names <- names(self$Defaults)
      Found <- endsWith(Names, valueId)
      Found <- Names[Found]
      if (length(Found) > 1) {
        Found <- paste(groupId, valueId, sep = ".")
      } else if (length(Found) < 1)
        return(NA)
      Row    <- .WeaponType[self$Defaults$Hero.WeaponType] == .WeaponType[private$.Hero.WeaponType]
      Column <- which( names(self$Defaults) == Found )

      if (length(Column) != 1) return(NA)

      Result <- self$Defaults[[Column]][Row]
      if (isTRUE(is.na(Result) || !is.numeric(Result))) return(NA)

      return(Result)
    },



    #' SetOpponent: method to set all opponent properties
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


    #' SetHeroMovement: method to set the movement properties of the hero
    #' @param Means Which means does the hero have to move (a `.MeansOfMovement` enum)
    #' @param Movement Movement speed (`.Movement` or `.MountedMovement` enum)
    #' @return `invisible(self)`
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


    #' Method to return the currently valid default settings as
    #' nested combat environment list
    #' @param weaponType (`.WeaponType` enum)
    #' @return A named list with the default combat environment
    GetDefaultCombatEnvironment = function(weaponType = self$WeaponType) {
      if (weaponType %in% .WeaponType || weaponType %in% names(.WeaponType))
        self$WeaponType <- .WeaponType[weaponType]

      # Extract list
      Values <- lapply(self$Defaults, `[`, .WeaponType[private$.Hero.WeaponType])

      # Sort the values into three sub-lists (hero, opponent, environment)
      # Keeping the vector names and getting the list names makes it more complicated
      Result <- list()
      for (v in 1:length(Values)) {
        NameBlocks <- unlist(strsplit(names(Values[v]), ".", fixed = TRUE))
        NewVal <- Values[[v]]

        CategoryName <- NameBlocks[length(NameBlocks)-1L]
        ValName <- tail(NameBlocks, 1L) #names(NewVal)

        if (is.null(Result[[CategoryName]]))
          Result <- c(Result, setNames(list(setNames(list(NewVal), ValName)), CategoryName))
        else
          Result[[CategoryName]] <- c(Result[[CategoryName]], setNames(list(NewVal), ValName))
      }

      return(Result)
    },


    #' Method to return the current combat environment  as
    #' nested combat environment list.
    #' @param weaponType (`.WeaponType` enum)
    #' @return A named list with the current combat environment
    GetCombatEnvironment = function(weaponType = self$WeaponType) {
      if (weaponType %in% .WeaponType)
        self$WeaponType <- .WeaponType[weaponType]
      # Fill all values with defaults
      CombatEnv <- self$GetDefaultCombatEnvironment(self$WeaponType)

      CombatEnv[["Hero"]]$WeaponType     <- private$.Hero.WeaponType
      CombatEnv[["Opponent"]]$TargetSize <- private$.Opponent.TargetSize
      CombatEnv[["Environment"]]$Visibility <- private$.Environment.Visibility
      CombatEnv[["Environment"]]$UnderWater <- private$.Environment.UnderWater

      if (private$.Hero.WeaponType == .WeaponType["Melee"]) {
        CombatEnv[["Hero"]]$CloseCombatRange     <- private$.Hero.CloseCombatRange
        CombatEnv[["Opponent"]]$CloseCombatRange <- private$.Opponent.CloseCombatRange
        CombatEnv[["Environment"]]$CrampedSpace  <- private$.Environment.CrampedSpace
      }

      if (private$.Hero.WeaponType == .WeaponType["Unarmed"]) {
        CombatEnv[["Hero"]]$CloseCombatRange     <- private$.Hero.CloseCombatRange
        CombatEnv[["Opponent"]]$CloseCombatRange <- private$.Opponent.CloseCombatRange
      }

      if (private$.Hero.WeaponType == .WeaponType["Shield"]) {
        CombatEnv[["Hero"]]$CloseCombatRange     <- private$.Hero.CloseCombatRange
        CombatEnv[["Opponent"]]$CloseCombatRange <- private$.Opponent.CloseCombatRange
        CombatEnv[["Environment"]]$CrampedSpace  <- private$.Environment.CrampedSpace
      }

      if (private$.Hero.WeaponType == .WeaponType["Ranged"]) {
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
    # Naming convention here is really important. Do not change lightly!
    .Hero.WeaponType = NA,     # .WeaponType
    .Hero.CloseCombatRange = NA,     # .CloseCombatRange
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
# ce$getDefault(valueId = "This valueId does not exist ... I am sure of it")
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
