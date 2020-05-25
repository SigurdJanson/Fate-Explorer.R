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


GetSkills_Opt <- function(Skills) {
  # Get data frame with skill definitions
  SkillList <- GetSkills()
  SkillList$value <- 0
  # 
  SkillValID <- match(names(Skills), SkillList$attrID)
  #SkillValID <- SkillValID[-which(is.na(SkillValID))]
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
  #browser()
  
  WeaponName <- gsub(" ", "", WeaponName, fixed = TRUE)
  Weapon     <- GetWeapons(Which = WeaponName)
  
  Courage  <- Attr[["ATTR_1"]]
  PrimeAttr <- GetPrimaryWeaponAttribute(WeaponName) # Attr[["GE"]] # TODO
  if (length(PrimeAttr) > 1L) { # more than 1 primary attribute
    # choose max
    PrimeAttr <- max(unlist(Attr[, PrimeAttr]))
  } else 
    PrimeAttr <- Attr[[PrimeAttr]]
  
  Technique <- Weapon[["combattechID"]]
  ATMod <- Weapon[["at"]]
  PAMod <- Weapon[["pa"]]
  Skill <- Skill[[Technique]]
  if (is.null(Skill)) Skill <- 6L # default value
  
  CourageMod   <- ((Courage-8L) %/% 3L)
  PrimeAttrMod <- ((PrimeAttr-8L) %/% 3L)
  CT <- list(AT = Skill + CourageMod + ATMod,
             PA = round(Skill/2L) + PrimeAttrMod + PAMod)
  return(CT)
}





GetWeapons_Opt <- function(BelongingItems, CombatTechniques, Traits, AddUnarmed = TRUE) {
  # name = c("Unarmed", "User-defined"), 
  # at = rep(NA, 2), pa = rep(NA, 2)
  # Unarmed = c("Unarmed", NA, NA, "1", "0"),
  # Other = c("User-Defined", NA, NA, NA, NA)
  if (AddUnarmed) {
    BelongingItems <- c(BelongingItems, 
                        ITEM_99 = list(list(id = 99L, name = "Waffenlos", combatTechnique = "CT_9",
                                       at = 0L, pa = 0L, damageDiceNumber = 1L, damageFlat = 0L)))
  }
  
  Weapons <- NULL
  for (Item in BelongingItems) {
    #[["belongings"]][["items"]][["ITEM_22"]][["damageDiceNumber"]]
    if (!is.null(Item$combatTechnique) && length(Item$combatTechnique) > 0) {
      Skill <- GetCombatSkill(Item$name, Traits, CombatTechniques) #(CombatTechniques, Item$combatTechnique, Traits)
      
      Weapons <- cbind(
        c(Item$name, Skill$AT, Skill$PA, 
          Item$damageDiceNumber, Item$damageFlat),
        Weapons
      )
      colnames(Weapons)[1L] <- Item$name
    }
  }# for
  rownames(Weapons) <- c("Name", "AT", "PA", "DamageDice", "DamageMod")
  
  return(Weapons)
}


