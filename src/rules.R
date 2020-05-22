# DSA 5 Rules

.Attribs <- NULL
.ComTecs <- NULL
.Melee   <- NULL


GetAbilities <- function() {
  if (is.null(.Attribs)) {
    JsonFile <- file.path("data", paste0("attributes_", i18n$translation_language, ".json"))
    .Attribs <<- read_json(JsonFile, simplifyVector = TRUE)
  }
  return(.Attribs)
}


GetCombatTechniques <- function() {
  if (is.null(.ComTecs)) {
    JsonFile <- file.path("data", paste0("combattechs_", i18n$translation_language, ".json"))
    .ComTecs <<- read_json(JsonFile, simplifyVector = FALSE, flatten = TRUE)
    .ComTecs <<- as.data.frame(.ComTecs, stringsAsFactors = FALSE)
  }
  return(.ComTecs)
}


GetWeapons <- function(Which = "All", Type = "Melee") {
  #browser()
  if (Type == "Melee") {
    if (is.null(.Melee)) {
      JsonFile <- file.path("data", paste0("weapon-list_", i18n$translation_language, ".json"))
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


GetPrimaryWeaponAttribute <- function( Weapon ) {
  # PRECONDITIONS
  if(missing(Weapon)) stop("A weapon is required.")
  # RUN
  W <- GetWeapons()
  row <- which(W[["name"]] == Weapon)
  PrimeAttr <- W[row, "primeattrID"]
  # Parse and translate
  if(length(PrimeAttr) > 0 && !is.na(PrimeAttr)) { # two attributes are separated by "/"
    PrimeAttr <- unlist(strsplit(PrimeAttr, "/"))
  }
  return(PrimeAttr)
}

