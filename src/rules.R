# DSA 5 Rules
require(jsonlite)


.Attribs <- NULL
.ComTecs <- NULL
.Melee   <- NULL


GetAbilities <- function() {
  if (is.null(.Attribs)) {
    .Attribs <<- read_json("./data/attributes_en.json", simplifyVector = TRUE)
  }
  return(.Attribs)
}


GetCombatTechniques <- function() {
  if (is.null(.ComTecs)) {
    .ComTecs <<- read_json("./data/combattechs_en.json", simplifyVector = FALSE, flatten = TRUE)
    .ComTecs <<- as.data.frame(.ComTecs, stringsAsFactors = FALSE)
  }
  return(.ComTecs)
}


GetWeapons <- function(Which = "All", Type = "Melee") {
  #browser()
  if (Type == "Melee") {
    if (is.null(.Melee)) {
      .Melee <<- read_json("./data/weapon-list.json", simplifyVector = TRUE)
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

