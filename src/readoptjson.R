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


GetSkills_Opt <- function(Skills, Language = "de") {
  # Get data frame with skill definitions
  SkillList <- GetSkills(Language)
  SkillList$value <- 0
  # 
  SkillValID <- match(names(Skills), SkillList$attrID)
  SkillList$value[SkillValID] <- unlist(Skills)
  
  return(SkillList)
}



#' GetCombatSkill
#' Compute combat skill based on DSA5 rules
#' @details 
#' Base values for AT and PA depend on: courage, character skill, weapon modifiers, 
#' and enhancements through the primary attribute.
#' @param WeaponName String
#' @param Attr 
#' @param Skill 
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
               PA = round(Skill/2L) + PAAttrMod + PASkillMod)
  } else CT <- list()
  
  return(CT)
}





GetWeapons_Opt <- function(Belongings, CombatTechniques, Traits, AddUnarmed = TRUE) {
  if (AddUnarmed) {
    Belongings <- c(Belongings, 
                    WEAPONLESS = list(list(name = "Waffenlos",
                                           template = "WEAPONLESS", 
                                           combatTechnique = "CT_9",
                                           at = 0L, pa = 0L, 
                                           damageDiceNumber = 1L, 
                                           damageFlat = 0L)))
  }
  Weapons <- NULL
  for (Item in Belongings) {
    DatabaseWeapon <- GetWeapons(Item$template, "Any")
    ItemIsWeapon <- length(unlist(DatabaseWeapon)) > 0
    if (ItemIsWeapon) {
      Skill <- GetCombatSkill(Item$template, Traits, CombatTechniques) 
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


