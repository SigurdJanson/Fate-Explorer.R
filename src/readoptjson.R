# Read Optholit JSON Files

GetWeapons_Opt <- function(BelongingItems) {
  
  Weapons <- data.frame(
    Brawl = c("Brawl", NA, NA, "1", "0"),
    Other = c("User-Defined", NA, NA, NA, NA)
  )
  rownames(Weapons) <- c("Name", "AT", "PA", "DamageDice", "DamageMod")
  for (Item in BelongingItems) {
    #[["belongings"]][["items"]][["ITEM_22"]][["damageDiceNumber"]]
    if (!is.null(Item$combatTechnique) && length(Item$combatTechnique) > 0) {
      Weapons <- cbind(
        c(Item$name, Item$at, Item$pa, Item$damageDiceNumber, Item$damageFlat),
        Weapons
      )
      colnames(Weapons)[1] <- Item$name
    }
  }# for
  
  return(Weapons)
}


