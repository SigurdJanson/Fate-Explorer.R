# Import and transform data


#' ReadAttributes_Opt
#' Read character attributes/abilities as data frame.
#' @param Attr An Optholit Json attribute node
#' @return Data frame with attribute names as column names and values in
#' first row.
ReadAttributes_Opt <- function(Attr) {
  NameMapping <- GetAttributes()
  # Create data frame with attributes
  Result <- data.frame(lapply(Attr, `[[`, 2))
  colnames(Result) <- NameMapping[match(sapply(Attr, `[[`, 1), NameMapping[,1]), 2]
  return(Result)
}


#' GetCombatSkill
#' Compute combat skill based on DSA5 rules
#' @details 
#' Base values for AT and PA depend on: courage, character skill, weapon modifiers, 
#' and enhancements through the primary attribute.
#' @param CombatTechniques 
#' @param Technique 
#' @param Attr 
GetCombatSkill <- function(CombatTechniques, Technique, Attr) {
  Courage  <- Attr[["MU"]]
  PrimeAttr <- Attr[["GE"]]
  
  Skill <- CombatTechniques[[Technique]]
  if (is.null(Skill)) Skill <- 6 # default value
  
  CourageMod   <- ((Courage-8) %/% 3)
  PrimeAttrMod <- ((PrimeAttr-8) %/% 3)
  CT <- list(AT = Skill + CourageMod,
             PA = round(Skill/2) + PrimeAttrMod)
  return(CT)
}





GetWeapons_Opt <- function(BelongingItems, CombatTechniques, Traits) {
  Weapons <- data.frame(
    Brawl = c("Brawl", NA, NA, "1", "0"),
    Other = c("User-Defined", NA, NA, NA, NA)
  )
  rownames(Weapons) <- c("Name", "AT", "PA", "DamageDice", "DamageMod")
  
  for (Item in BelongingItems) {
    #[["belongings"]][["items"]][["ITEM_22"]][["damageDiceNumber"]]
    if (!is.null(Item$combatTechnique) && length(Item$combatTechnique) > 0) {
      Skill <- list(AT=666, PA=656)#
      Skill <- GetCombatSkill(CombatTechniques, Item$combatTechnique, Traits)
      
      Weapons <- cbind(
        c(Item$name, 
          Item$at + Skill$AT, 
          Item$pa + Skill$PA, 
          Item$damageDiceNumber, Item$damageFlat),
        Weapons
      )
      colnames(Weapons)[1] <- Item$name
    }
  }# for
  
  return(Weapons)
}


