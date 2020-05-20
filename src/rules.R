# DSA 5 Rules
require(jsonlite)


Rules <- local({
  Attribs <- NULL
  ComTecs <- NULL
  Melee   <- NULL
  
  list(
    Abilities = function() {
      if (is.null(Attribs)) {
        Attribs <<- read_json("./data/attributes_en.json", simplifyVector = TRUE)
      }
      return(Attribs)
    },
    CombatTechniques = function() {
      if (is.null(ComTecs)) {
        ComTecs <<- read_json("./data/combattechs_en.json", simplifyVector = FALSE, flatten = TRUE)
        ComTecs <<- as.data.frame(ComTecs, stringsAsFactors = FALSE)
      }
      return(ComTecs)
    },
    Weapons = function(Type = "Melee") {
      if (Type == "Melee") {
        if (is.null(Melee)) {
          Melee <<- read_json("./data/weapon-list.json", simplifyVector = TRUE)
          return(Melee)
        } else {}
      } else {
        stop("Other weapons than melee are not supported, yet")
      }
    },
    GetPrimaryWeaponAttribute <- function( Weapon, Technique ) {
      stop("Not yet implemented")
      # PRECONDITIONS
      if(missing(Weapon) && missing(Technique)) 
        stop("Either a weapon technique or a specific weap are required.")
      if(!missing(Weapon) && !missing(Technique))
        if (Weapon$combatTechnique != Technique)
          stop("Combat techniques do not match")
    }
    )#list
})

