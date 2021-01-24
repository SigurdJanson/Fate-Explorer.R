# Import and transform data


#' GetAttributes_Opt
#' Read character attributes/abilities as data frame.
#' @param Attr An Optholit Json "attr" node (attributes)
#' @return Data frame with attribute names as column names and values in
#' first row.
GetAbilities_Opt <- function(AttrNode) {
  # Create data frame with attributes
  Result <- data.frame(lapply(AttrNode, `[[`, 2))
  colnames(Result) <- lapply(AttrNode, `[[`, 1)
  return(Result)
}



#' GetSkills_Opt
#' Takes the skills from an Optholit Json object and merges it with the information
#' from the database to return a data frame with characters skill (excluding magical and
#' religious skills).
#' @param Skills Skills as extracted from Optolith Json (i.e. a "talents" node)
#' @return Data frame of character skills
GetSkills_Opt <- function(Skills) {
  # Get data frame with skill definitions
  SkillList <- GetSkills()
  SkillList$value <- 0
  # 
  SkillValID <- match(names(Skills), SkillList$attrID)
  SkillList$value[SkillValID] <- unlist(Skills)
  
  return(SkillList)
}



#' GetSpells_Opt
#' Takes the spells and rituals from an Optolith Json object and merges it 
#' with the information  from the database to return a data frame with 
#' character's spells.
#' @param Spells Spells json as extracted from Optolith Json
#' @return Data frame of character spells
GetSpells_Opt <- function(Spells) {
  # Get data frame with skill definitions
  SpellList <- GetSpells()
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



#' GetChants_Opt
#' Takes the chants and liturgies from an Optolith Json object and merges it 
#' with the information  from the database to return a data frame with 
#' character's chants.
#' @param Chants Chants json as extracted from Optolith Json
#' @return Data frame of character chants
GetChants_Opt <- function(Chants) {
  # Get data frame with skill definitions
  ChantList <- GetChants()
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
#' @param WeaponName String to identify a weapon. Preferably the ID but a 
#' name can work, too.
#' @param Attr A data frame containing the characters abilities
#' @param Skill Skill value(s) of combat techniques (data frame with 1 row).
#' @param IsUniqueWeapon Weapon is unique so that data cannot be be taken 
#' from the data base
#' @param UniqueWeapon Data structure that describes the unique weapon as 
#' taken from an Optolith Json file. Is ignored unless `IsUniqueWeapon` 
#' is `TRUE`.
#' @note Not vectorised
GetCombatSkill <- function(WeaponName, Attr, Skill = NULL, 
                           IsUniqueWeapon = FALSE, UniqueWeapon = NULL) {
  
  # Determine the values needed to compute combat skills
  if (!IsUniqueWeapon) { # default
    # Get the weapon
    WeaponName <- gsub(" ", "", WeaponName, fixed = TRUE) # remove all spaces
    Weapon     <- GetWeapons(Which = WeaponName, Type = "Any")
    # 
    if (!is.null(Weapon)) { # Weapon found!
      PrimeAttr <- GetPrimaryWeaponAttribute(WeaponName) # 
      Technique <- Weapon[["combattechID"]]
      IsRanged <- !Weapon[["clsrng"]]
      
    } else { 
      stop(paste("Weapon", WeaponName,"not found"))
    }
    
  # Unique weapon  
  } else {
    # Create a mock for the weapon with the least required values
    Weapon <- list(at = UniqueWeapon[["at"]], pa = UniqueWeapon[["pa"]])
    # 
    PrimeAttr <- GetPrimaryWeaponAttributeByCombatTechnique(UniqueWeapon$combatTechnique)
    Technique <- UniqueWeapon$combatTechnique
    IsRanged  <- IsRangedWeapon(CombatTech = UniqueWeapon$combatTechnique)
  }

  # Get value for primary ability/attribute
  if (length(PrimeAttr) > 1L) { # more than 1 primary attribute
    # choose max
    PrimeAttr <- max(unlist(Attr[, PrimeAttr]))
  } else if (length(PrimeAttr) == 1L) {
    PrimeAttr <- Attr[[PrimeAttr]]
  } else PrimeAttr <- 0L # this way it has no effect later
  # Ability "Courage"
  Courage   <- Attr[["ATTR_1"]]

  # Skill for given technique      
  Skill <- Skill[[Technique]]
  if (is.null(Skill)) Skill <- 6L # default value

  # Compute skills based on PrimeAttr, Courage, Skill
  if (!IsRanged) { # close combat
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
  
  CombatSkill <- list(AT = Skill + ATAttrMod + ATSkillMod,
                      PA = ceiling(Skill/2L) + PAAttrMod + PASkillMod)
  return(CombatSkill)
}



#' UniqueWeaponFromCharacter
#' Creates a data structure for a weapon in compliance with the 
#' weapons data base so that this weapon can be treated the 
#' same way.
#' @param Item An list with an item from the 'belongings' 
#' data structure of a Optholit character file.
#'
#' @return A weapons data structure as list (equivalent to a weapon's 
#' database entry)
UniqueWeaponFromCharacter <- function(Item) {
  if (is.null(Item)) stop("Need an item to parse")
  if (length(Item) < 12) stop("Not sufficient data")
  if (is.null(Item[["combatTechnique"]])) stop("Unique item is not a weapon")
  
  CT <- GetCombatTechniques()
  CombatTechName  <- CT[which(CT$id == Item[["combatTechnique"]]), "name"]

  Attr <- GetAbilities()
  PrimaryAttrID <- Item[["primaryThreshold"]][["primary"]]
  if (is.null(PrimaryAttrID)) # if character does not have the info, get it from character
    PrimaryAttrID <- GetPrimaryWeaponAttributeByCombatTechnique(Item[["combatTechnique"]])
  PrimaryAttr <- Attr[Attr$attrID %in% PrimaryAttrID, "shortname"]
  SF          <- NA_integer_
  if (length(PrimaryAttrID) > 1) {
    PrimaryAttrID <- paste0(c(PrimaryAttrID), collapse = "/")
    PrimaryAttr <- paste0(c(PrimaryAttr), collapse = "/")
  }
  
  IsRanged <- IsRangedWeapon(CombatTech = Item$combatTechnique)
  
  if(!IsRanged) {
    Range       <- switch(Item$reach, "kurz", "mittel", "lang") # TODO: L10N
    
    Weapon <- list(name = Item[["name"]], 
                   combattech = CombatTechName, 
                   primeattr  = PrimaryAttr, 
                   threshold  = Item[["primaryThreshold"]]$threshold,
                   damage     = paste0(Item$damageDiceNumber, "W", Item$damageDiceSides), 
                   bonus      = Item[["damageFlat"]],
                   at = Item[["at"]], pa = Item[["pa"]], 
                   range      = Range, 
                   weight     = Item[["weight"]],
                   price      = Item[["price"]],
                   sf = SF,
                   combattechID = Item[["combatTechnique"]],
                   primeattrID = PrimaryAttrID,
                   improvised = ifelse(is.null(Item[["imp"]]), FALSE, as.logical(Item[["imp"]])),
                   url = "",
                   clsrng = TRUE,
                   armed  = TRUE,
                   templateID = Item[["template"]] 
    )
  } else {
    Weapon <- list(name       = Item[["name"]], 
                   combattech = CombatTechName, 
                   primeattr  = PrimaryAttr, 
                   damage     = paste0(Item$damageDiceNumber, "W", Item$damageDiceSides), 
                   bonus      = Item[["damageFlat"]],
                   at = Item[["at"]], pa = Item[["pa"]], 
                   range      = Item[["range"]],
                   loadtime   = Item[["reloadTime"]],
                   ammo       = Item[["ammunition"]],
                   weight     = Item[["weight"]],
                   price      = Item[["price"]],
                   sf = SF,
                   combattechID = Item[["combatTechnique"]],
                   primeattrID = PrimaryAttrID,
                   improvised = ifelse(is.null(Item[["imp"]]), FALSE, as.logical(Item[["imp"]])),
                   url = "",
                   clsrng = FALSE,
                   armed  = TRUE,
                   templateID = Item[["template"]] 
    )
  }

  # Corrections
  if (is.null(Weapon[["templateID"]])) Weapon[["templateID"]] <- ""
  
  return(Weapon)
}


#' GetWeapons_Opt
#' Returns a data frame with the character's weapons.
#' @param Belongings The complete list of items carried by a character as 
#' loaded from an Optolith character sheet.
#' @param CombatTechniques Characters combat skills per combat technique.
#' @param Abilities Data frame with character abilities
#' @param AddUnarmed Whether to include "unarmed" as pseudo weapon in the result 
#' (default: TRUE; logical).
#' @param AddImprov Whether to include improvised weapons in the result (default: TRUE; logical).
#' @return Data frame with a column per weapon
#' @details Improvised weapons must be in the data base or unique items. 
GetWeapons_Opt <- function(Belongings, CombatTechniques, Abilities, 
                           AddUnarmed = TRUE, AddImprov = FALSE) {
  # Parsing belongings
  Melee <- data.frame()
  Ranged <- data.frame()

  for (Item in Belongings) {
    if (Item[["isTemplateLocked"]]) # values should come from DB is template is locked
      DatabaseWeapon <- GetWeapons(Item$template, "Any")
    
    ItemIsUnique <- !Item[["isTemplateLocked"]] || is.null(DatabaseWeapon)
    if (ItemIsUnique) {
      if (is.null(Item$combatTechnique)) next # Item isn't a weapon
      DatabaseWeapon <- UniqueWeaponFromCharacter(Item)
      if (is.null(DatabaseWeapon)) next # Item isn't a weapon
    }
        
    # However: ignore item if it is improvised and that is not requested
    if (!AddImprov && isTRUE(DatabaseWeapon$improvised)) next

    # Add Weapon
    Skill <- GetCombatSkill(Item$template, Abilities, CombatTechniques, 
                            IsUniqueWeapon = ItemIsUnique, 
                            UniqueWeapon = Item)
    if (is.null(Item$template)) Item$template <- ""
    DamageDice <- unlist(strsplit(DatabaseWeapon[["damage"]], split = "W"))
    DamageDiceNumber <- as.integer(DamageDice[1])
    DamageDiceSides  <- as.integer(DamageDice[2])

    DatabaseWeapon[["damageDiceNumber"]] <- DamageDiceNumber
    DatabaseWeapon[["damageDiceSides"]]  <- DamageDiceSides
    DatabaseWeapon[["damageFlat"]]       <- DatabaseWeapon$bonus
    DatabaseWeapon[["AT.Skill"]]         <- Skill$AT
    DatabaseWeapon[["PA.Skill"]]         <- Skill$PA
      
    if (DatabaseWeapon[["clsrng"]]) { # close combat
      Melee <- rbind( data.frame(DatabaseWeapon), Melee )
      rownames(Melee)[1L] <- Item[["name"]]
    } else {
      Ranged <- rbind( data.frame(DatabaseWeapon), Ranged )
      rownames(Ranged)[1L] <- Item[["name"]]
    }
  }# for

  # Shall unarmed combat be added?
  if (AddUnarmed) {
    DatabaseWeapon <- GetWeapons("WEAPONLESS", "Unarmed")
    Skill <- GetCombatSkill("WEAPONLESS", Abilities, CombatTechniques)
    DamageDice <- unlist(strsplit(DatabaseWeapon[["damage"]], split = "W"))
    DamageDiceNumber <- as.integer(DamageDice[1])
    DamageDiceSides  <- as.integer(DamageDice[2])
    
    DatabaseWeapon[["damageDiceNumber"]] <- DamageDiceNumber
    DatabaseWeapon[["damageDiceSides"]]  <- DamageDiceSides
    DatabaseWeapon[["damageFlat"]]       <- DatabaseWeapon$bonus
    DatabaseWeapon[["AT.Skill"]]         <- Skill$AT
    DatabaseWeapon[["PA.Skill"]]         <- Skill$PA

    Melee <- rbind( data.frame(DatabaseWeapon), Melee )
    rownames(Melee)[1L] <- DatabaseWeapon[["name"]]
  }
  
  # Add row names and return
  if(nrow(Melee) == 0) Melee <- NULL
  if(nrow(Ranged) == 0) Ranged <- NULL
  if (is.null(Melee) && is.null(Ranged))
    return(NULL)
  else
    return(list(Melee = Melee, Ranged = Ranged))
}


