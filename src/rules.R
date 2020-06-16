# DSA 5 Rules

.Attribs <- NULL
.Skills  <- NULL
.ComTecs <- NULL
.Melee   <- NULL


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
  #GetWeapons() # Currently not required
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


GetWeapons <- function(Which = "All", Type = "Melee") {
  if (Type == "Melee") {
    if (is.null(.Melee)) {
      JsonFile <- file.path("data", paste0("weapon-list", ".json"))
      .Melee <<- read_json(JsonFile, simplifyVector = TRUE)
    }
    if (Which == "All")
      return(.Melee)
    else {
      Which <- gsub("[[:blank:]]", "", Which)
      return(as.list(.Melee[ which(.Melee$name == Which),]))
    }
  } else {
    stop("Other weapons than melee are not supported, yet")
  }
}


#' GetPrimaryWeaponAttribute
#' Get the primary attribute of a weapon
#' @param A string with the actual name of the weapon.
GetPrimaryWeaponAttribute <- function( Weapon ) {
  # PRECONDITIONS
  if(missing(Weapon)) stop("A weapon is required.")
  # RUN
  W <- GetWeapons()
  row <- which(W[["name"]] == Weapon) #TODO: using a code would be safer
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
  WeaponData <- GetWeapons( Weapon )
  Primaries  <- GetPrimaryWeaponAttribute( Weapon )
  
  if (!anyNA(Primaries)) {
    Threshold  <- WeaponData[["schwelle"]]
    AbIndex <- which(names(Abilities) %in% Primaries)
    Bonus <- max(0L, unlist(Abilities[, AbIndex])-Threshold)
  } else Bonus <- 0L
  
  return(Bonus)
}
#GetHitpointBonus("Barbarenschwert", ab)

