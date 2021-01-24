# DSA 5 Rules


# Enumerations ----
.WeaponType   <- c(Unarmed = 0L, Melee = 1L, Ranged = 2L, Shield = 3L)
.CombatAction <- c(Attack = 1L, Parry = 2L, Dodge = 3L)
.SuccessLevel <- c(Fumble = 1L, Fail = 2L, Success = 3L, Critical = 4L)
.CloseCombatRange  <- c(Short = 1L, Medium = 2L, Long = 3L)
.RangedCombatRange <- c(Close = 1L, Medium = 2L, Far = 3L)
.SkillType    <- c(Mundane = 1L, Magic = 2L, Blessed = 3L)

## relevant enums for combat modifiers
.Visibility      <- c(Clearly = 1L, Impaired = 2L, ShapesOnly = 3L, Barely = 4L, NoVision = 5L)
.MeansOfMovement <- c(OnFoot = 1L, Mounted = 2L)
.Movement        <- c(Stationary = 1L, Slow = 2L, Fast = 3L)
.MountedMovement <- c(Standing = 1L, Walk = 2L, Trot = 3L, Gallop = 4L)
.EvasiveMovement <- c(None = 1L, Zigzagging = 2L)
#.TargetDistance  <- c(Close = 1L, Medium = 2L, Far = 3L)
.TargetSize      <- c(Tiny = 1L, Small = 2L, Medium = 3L, Large = 4L, Huge = 5L)
.CrampedSpace    <- c(Free = 1L, Cramped = 2L)
.UnderWater      <- c(Dry = 1L, KneeDeep = 2L, WaistDeep = 3L, ChestDeep = 4L, NeckDeep = 5L, Submerged = 6L)


# Data objects ----
.Attribs <- NULL
.Skills  <- NULL
.Spells  <- NULL
.Chants  <- NULL
.ComTecs <- NULL
.Melee   <- NULL
.Ranged  <- NULL
.FumbleEffects <- NULL
.CombatMods <- NULL

.Language <- "de"


# Data Base Access Functions ----
#' 
#' Required for language changes
ReloadRules <- function(lang = .Language) {
  .Attribs <<- NULL
  GetAbilities(lang)
  .Skills <<- NULL
  GetSkills(lang)
  .ComTecs <<- NULL
  GetCombatTechniques(lang)
  .FumbleEffects <<- NULL
  GetFumbleEffects(lang)
  #.Melee   <<- NULL# Currently not required
  #GetWeapons(Type = "Melee") # Currently not required because not localised
  #.Ranged   <<- NULL# Currently not required
  #GetWeapons(Type = "Ranged") # Currently not required because not localised
}


#' GetAbilities
#' Read list of basic character abilities available in DSA5
#' @param lang Requested language to translate labels ("en", "de").
#' @return 
GetAbilities <- function(lang = .Language) {
  if (is.null(.Attribs)) {

    JsonFile <- file.path("data", paste0("attributes_", lang, ".json"))
    .Attribs <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Attribs)
}



#' GetSkills
#' Get the list of skills available in DSA5 (excluding magic and religious skills).
#' @param lang Requested language to translate labels ("en", "de").
#' @return a data frame of skills.
GetSkills <- function(lang = .Language) {
  if (is.null(.Skills)) {
    JsonFile <- file.path("data", paste0("skills_", lang, ".json"))
    .Skills <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Skills)
}


#' GetSpells
#' Get the list of magic skills available in DSA5.
#' @param lang Requested language to translate labels ("en", "de").
#' @return a data frame of skills.
GetSpells <- function(lang = .Language) {
  if (is.null(.Spells)) {
    JsonFile <- file.path("data", paste0("spells_", lang, ".json"))
    .Spells <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Spells)
}


#' GetChants
#' Get the list of magic skills available in DSA5.
#' @param lang Requested language to translate labels ("en", "de").
#' @return a data frame of skills.
GetChants <- function(lang = .Language) {
  if (is.null(.Chants)) {
    JsonFile <- file.path("data", paste0("chants_", lang, ".json"))
    .Chants <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Chants)
}


#' GetFumbleEffects
#' Get the tables that contain the effects of fumbles.
#' @param lang Requested language to translate labels ("en", "de").
#' @return A list with two data frames.
GetAllFumbleEffects <- function(lang = .Language) {
  if (is.null(.FumbleEffects)) {
    JsonFile <- file.path("data", paste0("fumbles_", lang, ".json"))
    .FumbleEffects <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.FumbleEffects)
}



# Combat Functions ----

#' GetCombatTechniques
#' Get the list of combat techniques available in DSA5
#' @param lang  Requested language to translate labels ("en", "de").
#' @return
GetCombatTechniques <- function(lang = .Language) {
  if (is.null(.ComTecs)) {
    JsonFile <- file.path("data", paste0("combattechs_", lang, ".json"))
    .ComTecs <<- read_json(JsonFile, simplifyVector = TRUE, flatten = TRUE)
  }
  return(.ComTecs)
}


#' GetWeapons
#' Returns a list of weapons with DSA attributes as available in DSA5
#' @param Which A string identifying a weapon or "All" which returns all 
#' weapons of the given type (string).
#' @param Type "Melee", "Unarmed", "Ranged" or "Any" (default is "Melee", 
#' "Unarmed" is treated as "Melee") (string).
#' @details The search by weapon name ignores upper/lower case and 
#' space characters.
#' @note "All" for argument "which" cannot be combined with type "Any".
#' @return A list; returns `NULL` is no item has been found
GetWeapons <- function(Which = "All", Type = c("Melee", "Unarmed", "Ranged", "Any")) {
  # PRECONDITIONS
  if (is.null(Which) || is.na(Which)) return(NULL)
  Type <- match.arg(Type)

  if (Type == "Any" && Which == "All") 
    stop("Invalid combination of arguments. 'all' weapons not allowed with type 'any'")
  if (Type == "Unarmed") Type <- "Melee"
  
  # Make sure the data is available - load if not
  if (!is.na(pmatch(Type, c("Melee", "Any")))) { 
    if (is.null(.Melee)) {
      JsonFile <- file.path("data", paste0("melee-list", ".json"))
      .Melee <<- read_json(JsonFile, simplifyVector = TRUE)
    }
  }
  if (!is.na(pmatch(Type, c("Ranged", "Any")))) {
    if (is.null(.Ranged)) {
      JsonFile <- file.path("data", paste0("ranged-list", ".json"))
      .Ranged <<- read_json(JsonFile, simplifyVector = TRUE)
    } 
  } 
  
  # Match the requested Weapon "Which"
  if (Which == "All") {
    if (Type == "Melee") return(.Melee)
    if (Type == "Ranged") return(.Ranged)
    # Not supported: if (Type == "Any") ...
  }
  
  if (Type %in% c("Melee", "Any")) {
    Which <- gsub("[[:blank:]]", "", Which) # remove any spaces
    Select <- .Melee[ which(.Melee$templateID == Which), ]
    if (length(unlist(Select)) == 0) { # if list is empty
      NamedWhich <- tolower(Which)
      Names      <- tolower(gsub("[[:blank:]]", "", .Melee$name))
      Select     <- .Melee[ which(Names == NamedWhich), ]
    }
    if (Type == "Melee")
      if (length(unlist(Select)) == 0) 
        return(NULL)
      else
        return(as.list(Select))
    else
      if (length(unlist(Select)) != 0)
        return(as.list(Select))
  }

  if (Type %in% c("Ranged", "Any")) {
    Which <- gsub("[[:blank:]]", "", Which) # remove any spaces
    Select <- .Ranged[ which(.Ranged$templateID == Which), ]
    if (length(unlist(Select)) == 0) { # if list is empty
      NamedWhich <- tolower(Which)
      Names      <- tolower(gsub("[[:blank:]]", "", .Ranged$name))
      Select     <- .Ranged[ which(Names == NamedWhich), ]
    }
    if (Type == "Ranged")
      if (length(unlist(Select)) == 0) 
        return(NULL)
    else
      return(as.list(Select))
    else
      if (length(unlist(Select)) != 0)
        return(as.list(Select))
  }
  
  # If nothing has been found: return NULL
  return(NULL)
}
# setwd("./R")
# W <- GetWeapons("Schwere Armbrust", "Any")
# setwd("../")


#' IsRangedWeapon
#' Is a weapon a ranged weapon?
#' @param Weapon A vector of IDs (which is preferred) or weapon names (character).
#' @param CombatTech A combat tech ID (character).
#' @return A logical vector.
IsRangedWeapon <- function( Weapon = NULL, CombatTech = NULL ) {
  if (missing(Weapon) && missing(CombatTech)) 
    stop("No arguments to define weapon")
  
  if (!missing(Weapon)) {
    DatabaseWeapon <- GetWeapons(Weapon, "Any")
    if (!is.null(DatabaseWeapon)) { # if not empty
      IsRanged <- !(DatabaseWeapon$clsrng)
    } else {
      IsRanged <- FALSE
    }
  } else { #if (!missing(CombatTech)) {
    ct <- GetCombatTechniques()
    IsRanged <- !is.na(match(CombatTech, paste0("CT_", c(1:2, 11, 14, 17:19))))
  }
  
  return(IsRanged)
}


#' IsParryWeapon
#' Does the carried weapon support a parry roll?
#' @param Weapon A vector of IDs (which is preferred) or weapon names (character).
#' @param CombatTech A combat tech ID (character).
#' @return A logical vector.
IsParryWeapon <- function( Weapon = NULL, CombatTech = NULL ) {
  if (missing(Weapon) && missing(CombatTech)) 
    stop("No arguments to define weapon")
  
  if (isTruthy(Weapon)) {
    DatabaseWeapon <- GetWeapons(Weapon, "Any")
    if (!is.null(DatabaseWeapon)) { # if not empty
      CombatTech <- DatabaseWeapon$combattechID
    }
  }
    
  if (!missing(CombatTech)) {
    ct <- GetCombatTechniques()
    IsParry <- ct[ct[["id"]] == CombatTech, "parry"]
  } else {
    IsParry <- FALSE
  }
  
  return(IsParry)
}


#' IsImprovisedWeapon
#' Looks the weapon up in the data base and finds out if the weapon is a "real" 
#' weapon or improvised.
#' @param Weapon A string identifying the weapon (template id or name)
#' @return TRUE/FALSE
IsImprovisedWeapon <- function( Weapon = NULL ) {
  if (missing(Weapon) && missing(CombatTech)) 
    stop("No arguments to define weapon")

  IsImprov <- FALSE
  if (!missing(Weapon)) {
    DatabaseWeapon <- GetWeapons(Weapon, "Any")
    if (!is.null(DatabaseWeapon)) { # if not empty
      IsImprov <- DatabaseWeapon$improvised
    }
  }
  
  return(IsImprov)
}


#' GetPrimaryWeaponAttribute
#' Get the primary attribute of a weapon
#' @param Weapon A string identifying the weapon (template id or name)
#' @note Function is not vectorised.
#' @return The string of the ability or `character(0)` if the weapon
#' has no primary ability.
GetPrimaryWeaponAttribute <- function( Weapon ) {
  # PRECONDITIONS
  if(missing(Weapon)) stop("A weapon is required")

  # RUN
  W <- GetWeapons(Weapon, "Any")

  if (!is.null(W)) { 
    PrimeAttr <- W[["primeattrID"]] ##UPDATE for vectorisation: sapply(W, `[[`, "primeattrID")
    # Parse and translate
    if(length(PrimeAttr) > 0 && !is.na(PrimeAttr)) { # two attributes are separated by "/"
      PrimeAttr <- unlist(strsplit(PrimeAttr, "/"))
    }
  } else PrimeAttr <- character(0)

  return(PrimeAttr)
}


GetPrimaryWeaponAttributeByCombatTechnique <- function( CombatTec ) {
  # PRECONDITIONS
  if(missing(CombatTec)) stop("A combat technique is required")
  
  # RUN
  # Get list of all combat techniques and isolate the needed one
  AllCTs <- GetCombatTechniques()
  CombatTec <- AllCTs[which(AllCTs$id == CombatTec), ]
  #print(CombatTec)

  if (!is.null(CombatTec)) { 
    PrimeAttr <- CombatTec[["primeattrID"]] ##UPDATE for vectorisation: sapply(W, `[[`, "primeattrID")
    # Parse and translate
    if(length(PrimeAttr) > 0 && !is.na(PrimeAttr)) { # two attributes are separated by "/"
      PrimeAttr <- unlist(strsplit(PrimeAttr, "/"))
    }
  } else PrimeAttr <- character(0)
  
  return(PrimeAttr)
}


#' GetHitpointBonus
#' Get possible hit point bonus depending on character's abilities.
#' @param Weapon The actual name of one weapon (see `GetPrimaryWeaponAttribute()`)
#' @param Abilities A data frame with the ability values
#' @return A numeric value indicating the extra bonus that must be added to the 
#' weapons hit points.
GetHitpointBonus <- function( Weapon, Abilities ) {
  # PRECONDITIONS
  if (missing(Weapon)) return(0L)
  if (missing(Abilities) || !is.data.frame(Abilities)) 
    stop("Argument 'Abilities' is missing")

  # RUN
  WeaponData <- GetWeapons( Weapon, "Melee" ) # only melee has a bonus
  Primaries  <- GetPrimaryWeaponAttribute( Weapon )

  if (!anyNA(Primaries)) {
    Threshold  <- WeaponData[["threshold"]]
    AbIndex <- which(names(Abilities) %in% Primaries)
    Bonus <- max(0L, unlist(Abilities[, AbIndex])-Threshold, na.rm = TRUE)
  } else Bonus <- 0L
  
  return(Bonus)
}
#GetHitpointBonus("Barbarenschwert", ab)



#' GetCombatModifiers
#' Read data structure of combat modifiers
#' @param lang Requested language to translate labels ("en", "de").
#' @return the data structure
GetCombatModifiers <- function(lang = .Language) {
  if (is.null(.CombatMods)) {
    
    JsonFile <- file.path("data", paste0("combatmods_", lang, ".json"))
    .CombatMods <<- read_json(JsonFile, simplifyVector = FALSE)
  }
  return(.CombatMods)
}



#' .GetEnum
#' Helper function to extract a Fate Explorer enum from a string
#' @param Pointer 
#' @note Used by `ModifyCheck`
#' @example eval(GetEnum("Hero.Weapon.CloseCombatRange.Medium"))
#' # Evaluates to `.CloseCombatRange`
.GetEnum <- function(Pointer, IndexCorrect = 0L) {
  # get the *name* of the required enum
  ## split pointer in it's parts and get (length-1)th element
  Index <- nchar(gsub("[^.]", "", Pointer))
  EnumName <- unlist(strsplit(Pointer, "[.]"))[Index+IndexCorrect]
  EnumName <- paste0(".", EnumName)
  ## check if it exists
  if (!exists(EnumName))
    stop(paste("Enumeration", EnumName, "from Combat Mods file cannot be found (", Pointer, ")"))
  ## 
  Enum <- str2expression(EnumName)
  return(Enum)
}


#' `%./%`
#' Specialized integer division `x1 / x2` that rounds correctly to the 
#' nearest value.
#' @param x1,x2 numerator and denominator (integer)
#' @note Used by `ModifyCheck`
#' @return integer
#' @source https://stackoverflow.com/questions/36377244/make-int-round-off-to-nearest-value/36377365
`%./%` <- function(x1, x2) {
  return( x1 %/% x2 + (x1 %% x2) %/% (x2 %/% 2L + x2 %% 2L) )
}

#' ModifyCheck
#'
#' @param CheckValues An integer vector
#' @param Environment 
#' @return The modified values of `CheckValue`
#' @examples
ModifyCheck <- function(CheckValues, Environment) {
  EnvFlat <- unlist(Environment)

  Topics <- GetCombatModifiers()
  for (Topic in Topics) {
    for (Variation in Topic) {
      # Check the conditions of the Topic
      Conditions <- Variation[["Conditions"]]
      ConditionsMet <- TRUE
      for (cnd in 1:length(Conditions)) { # {"Environment.Cramped": true, "Hero.WeaponType": "Melee"}
        # Compare Expected (from DB) with Observed (from CombatEnv)
        # Use the pointers (like Hero.WeaponType.Melee) to do it
        Expected <- paste(names(Conditions[cnd]), Conditions[[cnd]], sep=".")
        Observed <- startsWith(names(EnvFlat), names(Conditions[cnd]))
        Observed <- names(EnvFlat)[Observed]
        
        if (length(Observed) == 0 || !(Observed %in% Expected) ) {
          ConditionsMet <- FALSE
          break
        }
      }
      if (!ConditionsMet) next
      
      # Apply the modifiers
      Modifiers <- Variation[["Modifiers"]]
      ## 
      CriterionName <- Variation[["Criterion"]] # returns a "Pointer" like 'Opponent.TargetSize.Large'
      CriterionName <- startsWith(names(EnvFlat), CriterionName)
      CriterionName <- names(EnvFlat)[CriterionName]
      if (length(CriterionName) == 0) { 
        #print(paste("Skipped:", Variation[["Criterion"]])); 
        break
      }
      # get the *value* of the enum (i.e. a call like `.Movement["Slow"]` )
      Criterion <- do.call("[", alist(eval(.GetEnum(CriterionName)), EnvFlat[CriterionName]))
      
      Modifiers <- Modifiers[[names(Criterion)]]
      for (Action in names(CheckValues)) {
        if (is.null(Modifiers[[Action]])) next
        # executes an expression like `"+"(CheckValue, -2)`
        CheckValues[Action] <- do.call(Modifiers[[Action]][["op"]], 
                                       alist(CheckValues[Action], 
                                             Modifiers[[Action]][["arg"]]))
      }
      
    }
  }

  CheckValues <- round(CheckValues)
  mode(CheckValues) <- "integer" #CheckValues<- as.integer(round(CheckValues))
  CheckValues <- pmax(CheckValues, 0L)
  return(CheckValues)
}