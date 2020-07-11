# Import and transform data


#' GetAttributes_Opt
#' Read character attributes/abilities as data frame.
#' @param Attr An Optholit Json attribute node
#' @return Data frame with attribute names as column names and values in
#' first row.
GetAbilities_Opt <- function(Attr) {
  # Create data frame with attributes
  Result <- data.frame(lapply(Attr, `[[`, 2))
  colnames(Result) <- lapply(Attr, `[[`, 1)
  return(Result)
}


#' GetSkills_Opt
#' Takes the skills from an Optholit Json object and merges it with the information
#' from the database to return a data frame with characters skill (excluding magical and
#' religious skills).
#' @param Skills Skills as extracted from Optholit Json
#' @param Language String indicating the requested language ("en" or "de")
#' @return Data frame of character skills
GetSkills_Opt <- function(Skills, Language = "de") {
  # Get data frame with skill definitions
  SkillList <- GetSkills(Language)
  SkillList$value <- 0
  # 
  SkillValID <- match(names(Skills), SkillList$attrID)
  SkillList$value[SkillValID] <- unlist(Skills)
  
  return(SkillList)
}



GetSpells_Opt <- function(Spells, Language = "de") {
  # Get data frame with skill definitions
  SpellList <- GetSpells(Language)
  SpellList$value <- 0
  # 
  SpellValID <- match(names(Spells), SpellList[["attrID"]])
  # filter out NAs when spell is not in db
  Spells     <- Spells[!is.na(SpellValID)]
  SpellValID <- SpellValID[!is.na(SpellValID)]
  # sort and filter out the spells the hero hasn't  mastered, yet
  SpellList <- SpellList[SpellValID, ]
  SpellList$value <- unlist(Spells)

  return(SpellList)
}


GetChants_Opt <- function(Chants, Language = "de") {
  # Get data frame with skill definitions
  ChantList <- GetChants(Language)
  ChantList$value <- 0
  # 
  ChantValID <- match(names(Chants), ChantList[["attrID"]])
  # filter out NAs when spell is not in db
  Chants     <- Chants[!is.na(ChantValID)]
  ChantValID <- ChantValID[!is.na(ChantValID)]
  # sort and filter out the spells the hero hasn't  mastered, yet
  ChantList <- ChantList[ChantValID, ]
  ChantList$value <- unlist(Chants)
  
  return(ChantList)
}


#' GetCombatSkill
#' Compute combat skill based on DSA5 rules
#' @details 
#' Base values for AT and PA depend on: courage, character skill, weapon modifiers, 
#' and enhancements through the primary attribute.
#' @param WeaponName String
#' @param Attr A data frame containing the characters abilities
#' @param Skill Skill value(s) of combat techniques.
#' @note Not vectorised
GetCombatSkill <- function(WeaponName, Attr, Skill = NULL) {
  WeaponName <- gsub(" ", "", WeaponName, fixed = TRUE) # trim
  Weapon     <- GetWeapons(Which = WeaponName, Type = "Any")

  if (length(unlist(Weapon)) != 0L) { # Weapon found!
    Courage  <- Attr[["ATTR_1"]]
    PrimeAttr <- GetPrimaryWeaponAttribute(WeaponName) # 
    if (length(PrimeAttr) > 1L) { # more than 1 primary attribute
      # choose max
      PrimeAttr <- max(unlist(Attr[, PrimeAttr]))
    } else if (length(PrimeAttr) == 1L) {
      PrimeAttr <- Attr[[PrimeAttr]]
    } else PrimeAttr <- 0L # this way it has no effect later
    
    Technique <- Weapon[["combattechID"]]
    Skill <- Skill[[Technique]]
    if (is.null(Skill)) Skill <- 6L # default value
    
    if (Weapon[["clsrng"]]) { # close combat
      ATSkill    <- Skill
      PASkill    <- ceiling(Skill/2L)
      ATSkillMod <- Weapon[["at"]]
      PASkillMod <- Weapon[["pa"]]
      ATAttrMod  <- max((Courage-8L) %/% 3L, 0L)
      PAAttrMod  <- max((PrimeAttr-8L) %/% 3L, 0L)
    } else { # ranged weapon
      ATSkill    <- Skill
      PASkill    <- 0L
      ATSkillMod <- 0L
      PASkillMod <- 0L
      ATAttrMod  <- max((PrimeAttr-8L) %/% 3L, 0L)
      PAAttrMod  <- 0L
    }
    CT <- list(AT = Skill + ATAttrMod + ATSkillMod,
               PA = ceiling(Skill/2L) + PAAttrMod + PASkillMod)
  } else CT <- list()
  
  return(CT)
}





#' GetWeapons_Opt
#' Returns a data frame with the character's weapons.
#' @param Belongings The complete list of items carried by a character as loaded from an
#' Optholit character sheet.
#' @param CombatTechniques Characters combat skills per combat technique.
#' @param Abilities Data frame with character abilities
#' @param AddUnarmed Wether to include "unarmed" as pseudo weapon in the result 
#' (default: TRUE; logical).
#' @param AddImprov Wether to include improvised weapons in the result (default: TRUE; logical).
#' @return Data frame with a column per weapon
GetWeapons_Opt <- function(Belongings, CombatTechniques, Abilities, 
                           AddUnarmed = TRUE, AddImprov = FALSE) {

  if (AddUnarmed) {
    Weaponless <- list(list(name = "Waffenlos", template = "WEAPONLESS", 
                           combatTechnique = "CT_9", at = 0L, pa = 0L, 
                           damageDiceNumber = 1L, damageFlat = 0L))
    Belongings <- c(Belongings, WEAPONLESS = Weaponless)
  }

  Weapons <- NULL
  for (Item in Belongings) {
    DatabaseWeapon <- GetWeapons(Item$template, "Any")
    ItemIsWeapon <- length(unlist(DatabaseWeapon)) > 0
    if (!AddImprov) ItemIsWeapon <- ItemIsWeapon & isFALSE(DatabaseWeapon$improvised)

    if (ItemIsWeapon) {
      Skill <- GetCombatSkill(Item$template, Abilities, CombatTechniques) 
      if (is.null(Item$damageDiceNumber)) Item$damageDiceNumber <- DatabaseWeapon$damage
      if (is.null(Item$damageFlat)) Item$damageFlat <- DatabaseWeapon$bonus
      Weapons <- cbind( c(Item$name, Item$template, Skill$AT, Skill$PA, 
                          Item$damageDiceNumber, Item$damageFlat),
                        Weapons )
      colnames(Weapons)[1L] <- Item$name
    }
  }# for
  rownames(Weapons) <- c("Name", "templateID", "AT", "PA", "DamageDice", "DamageMod")
  
  return(Weapons)
}


