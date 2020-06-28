# DSA 5 Rules

.Attribs <- NULL
.Skills  <- NULL
.ComTecs <- NULL
.Melee   <- NULL
.Ranged  <- NULL


#' 
#' Required for language changes
ReloadRules <- function(lang = "de") {
  .Attribs <<- NULL
  GetAbilities(lang)
  .Skills <<- NULL
  GetSkills(lang)
  .ComTecs <<- NULL
  GetCombatTechniques(lang)
  #.Melee   <<- NULL# Currently not required
  #GetWeapons(Type = "Melee") # Currently not required
  #.Ranged   <<- NULL# Currently not required
  #GetWeapons(Type = "Ranged") # Currently not required
}


GetAbilities <- function(lang = "de") {
  if (is.null(.Attribs)) {

    JsonFile <- file.path("data", paste0("attributes_", lang, ".json"))
    .Attribs <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Attribs)
}



GetSkills <- function(lang = "de") {
  if (is.null(.Skills)) {
    
    JsonFile <- file.path("data", paste0("skills_", lang, ".json"))
    .Skills <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Skills)
}


GetCombatTechniques <- function(lang = "de") {
  if (is.null(.ComTecs)) {
    JsonFile <- file.path("data", paste0("combattechs_", lang, ".json"))
    .ComTecs <<- read_json(JsonFile, simplifyVector = FALSE, flatten = TRUE)
    .ComTecs <<- as.data.frame(.ComTecs, stringsAsFactors = FALSE)
  }
  return(.ComTecs)
}


#' GetWeapons
#' Returns a list of weapons with DSA attributes
#' @param Which A string identifiyng a weapon or "All" which returns all 
#' weapons of the given type (character).
#' @param Type "Melee", "Ranged" or "Any" (default is "Melee"; character).
#' @note "All" for argument "which" cannot be combined with "Any" type.
#' @return A list
GetWeapons <- function(Which = "All", Type = c("Melee", "Ranged", "Any")) {
  # PRECONDITIONS
  Type <- match.arg(Type)
  if (Type == "Any" && Which == "All") 
    stop("Invalid combination of arguments. 'all' weapons not allowed with type 'any'")
  
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
  if (Type %in% c("Melee", "Any")) {
    if (Which == "All")
      return(.Melee)
    else {
      Which <- gsub("[[:blank:]]", "", Which)
      Select <- .Melee[ which(.Melee$templateID == Which), ]
      if (length(unlist(Select)) == 0)
        Select <- .Melee[ which(.Melee$name == Which), ]
      if (length(unlist(Select)) != 0 || Type == "Melee")
        return(as.list(Select))
    }
  }

  if (Type %in% c("Ranged", "Any")) {
    if (Which == "All")
      return(.Ranged)
    else {
      Which <- gsub("[[:blank:]]", "", Which)
      Select <- .Ranged[ which(.Ranged$templateID == Which), ]
      if (length(unlist(Select)) == 0)
        Select <- .Ranged[ which(.Ranged$name == Which), ]
      return(as.list(Select))
    }
  }
}
# setwd("./src")
# W <- GetWeapons("Waqqif", "Ranged")
# setwd("../")



#' GetPrimaryWeaponAttribute
#' Get the primary attribute of a weapon
#' @param A string with the actual name of the weapon.
GetPrimaryWeaponAttribute <- function( Weapon ) {
  # PRECONDITIONS
  if(missing(Weapon)) stop("A weapon is required.")
  # RUN
  W <- GetWeapons()
  # Try finding weapon by "ID" first, then "name" if not successful
  row <- which(W[["templateID"]] == Weapon) #TODO: using a code would be safer
  if (length(row) == 0)
    row <- which(W[["name"]] == Weapon)
  
  PrimeAttr <- W[row, "primeattrID"]
  # Parse and translate
  if(length(PrimeAttr) > 0 && !is.na(PrimeAttr)) { # two attributes are separated by "/"
    PrimeAttr <- unlist(strsplit(PrimeAttr, "/"))
  }
  return(PrimeAttr)
}



#' GetHitpointBonus
#'
#' @param Weapon The actual name of one weapon (see `GetPrimaryWeaponAttribute()`)
#' @param Abilities A data frame with the ability values
#' @return A numeric value indicating the extra bonus that must be added to the 
#' weapons hit points.
GetHitpointBonus <- function( Weapon, Abilities ) {
  # PRECONDITIONS
  if (missing(Weapon)) return(0)
  if (missing(Abilities) || !is.data.frame(Abilities)) 
    stop("Argument 'Abilities' is missing")

  # RUN
  WeaponData <- GetWeapons( Weapon, "Melee" ) #only melee has a bonus
  Primaries  <- GetPrimaryWeaponAttribute( Weapon )
  
  if (!anyNA(Primaries)) {
    Threshold  <- WeaponData[["threshold"]]
    AbIndex <- which(names(Abilities) %in% Primaries)
    Bonus <- max(0L, unlist(Abilities[, AbIndex])-Threshold)
  } else Bonus <- 0L
  
  return(Bonus)
}
#GetHitpointBonus("Barbarenschwert", ab)

