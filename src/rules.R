# DSA 5 Rules

.Attribs <- NULL
.Skills  <- NULL
.ComTecs <- NULL
.Melee   <- NULL


#' 
#' Required for language changes
ReloadRules <- function() {
  .Attribs <<- NULL
  GetAbilities()
  .Skills <<- NULL
  GetSkills()
  .ComTecs <<- NULL
  GetCombatTechniques()
  #.Melee   <<- NULL# Currently not required
  #GetWeapons() # Currently not required
}


GetAbilities <- function() {
  if (is.null(.Attribs)) {

    lang <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
    JsonFile <- file.path("data", paste0("attributes_", lang, ".json"))
    .Attribs <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Attribs)
}


IsRoutineCheckValid <- function(Skill, Modifier = 0) {
  stop("Not implemented, yet")
  # Get ability values for Skill
  # if (any(Ability < 13)) return(NA)
  # Get TaW for Skill
  # Get minimum skill value for given Modifier
  SRMin <- seq(19, by = -3, length.out =  7) # 
  SRMin <- SRMin[Modifier +1]
  return(SRMin)
}


GetSkills <- function() {
  if (is.null(.Skills)) {
    
    lang <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
    JsonFile <- file.path("data", paste0("skills_", lang, ".json"))
    .Skills <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Skills)
}


GetCombatTechniques <- function() {
  if (is.null(.ComTecs)) {
    lang <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
    JsonFile <- file.path("data", paste0("combattechs_", lang, ".json"))
    .ComTecs <<- read_json(JsonFile, simplifyVector = FALSE, flatten = TRUE)
    .ComTecs <<- as.data.frame(.ComTecs, stringsAsFactors = FALSE)
  }
  return(.ComTecs)
}


GetWeapons <- function(Which = "All", Type = "Melee") {
  #browser()
  if (Type == "Melee") {
    if (is.null(.Melee)) {
      JsonFile <- file.path("data", paste0("weapon-list", ".json"))
      .Melee <<- read_json(JsonFile, simplifyVector = TRUE)
    }
    if (Which == "All")
      return(.Melee)
    else
      return(as.list(.Melee[ which(.Melee$name == Which),]))
    } else {
    stop("Other weapons than melee are not supported, yet")
  }
}
#GetWeapons("Waqqif")


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

