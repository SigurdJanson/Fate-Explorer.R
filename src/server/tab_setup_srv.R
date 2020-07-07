# SETUP

Character <- reactiveValues(Name = "No character has been uploaded",
                            Attr = NULL, Skills = NULL, Weapons = NULL,
                            CombatSkills = NULL, MagicSkills = NULL)
RawCharacterFile <- reactiveVal(NULL) # raw data container of json content


#' SetupCharacterWeapons
#' Global function to extract the list of weapons from Json
#' @param AddImprov Shall improvised weapons be loaded as well? (`TRUE`/`FALSE`)
#' @return NULL
SetupCharacterWeapons <- function(AddImprov = FALSE) {
  Weapons <- GetWeapons_Opt(RawCharacterFile()[["belongings"]][["items"]], # belongings
                            Character$CombatSkills,  # combat techniques
                            Character$Attr, AddUnarmed = TRUE, AddImprov = AddImprov)
  Character$Weapons <- NULL
  WeaponNames <- NULL
  for (w in Weapons["templateID", ]) {
    if ( IsRangedWeapon(w) )
      ActiveWeapon <- RangedWeapon$new(w, Character$Attr, Character$CombatSkills)
    else
      ActiveWeapon <- MeleeWeapon$new(w, Character$Attr, Character$CombatSkills)

    Character$Weapons <- c(Character$Weapons, ActiveWeapon)
    WeaponNames <- c(WeaponNames, ActiveWeapon$Name)
    #TODO: Correct for encumbrance: TODO (EEC = Effective Encumbrance)
  }
  
  # Update dropdown list on Combat Tab
  updateSelectInput(session, "cmbCombatSelectWeapon", choices = WeaponNames, selected = 1)
  
  invisible(NULL)
}


#' SetupMagicSkills
#' Global function to extract the list of weapons from Json and merge it with the 
#' skill values of the character.
#' @return NULL
SetupMagicSkills <- function() {
#browser()
  Data <- RawCharacterFile()[["spells"]]
  if (isTruthy(Data) && length(Data) > 0) {
    Character$MagicSkills <- GetSpells_Opt(Data)
  }
  
  invisible(NULL)
}


# Open new character file (.json)
observeEvent(input$CharFile, {
  if (input$CharFile$type != "application/json") 
    RawCharacterFile(paste(i18n$t("Fate Explorer only understands Json files."), 
                                  input$CharFile$name, i18n$t("is not Json.")))
  

  # handle dependencies to components that display data of last character
  Language <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
  Data <- read_json(path = input$CharFile$datapath)
  
  RawCharacterFile(Data) # update raw data container

  Character$Name    <- Data$name
  Character$Attr    <- GetAbilities_Opt(Data[["attr"]][["values"]])
  Character$Skills  <- GetSkills_Opt(Data[["talents"]], Language)
  Character$CombatSkills <- Data[["ct"]]
  SetupCharacterWeapons(input$chbShowImprovWeapons)
  SetupMagicSkills()
  
  # THIS SECTION IS A BIT OUT OF PLACE HERE
  # Update dropdown list on Skills Tab
  UpdateSkillSourceRadioButton(session, IsCharacterLoaded = TRUE )
  updateSelectInput(session, "lbCharSkills", choices = Character$Skills[, "name"])
  updateSelectInput(session, "lbSkillGroups", 
                    choices = c('All Skills' = '', unique(Character$Skills[, "class"])))
  # Update Combat value with weaponless brawling
  updateSliderInput(session, "inpDodgeValue", value = Character$Weapons[[1]]$Skill$Dodge)
}, ignoreNULL = TRUE, ignoreInit = TRUE) # If new JSON file


# Show/Hide improvised weapons
observeEvent(input$chbShowImprovWeapons, {
  req(RawCharacterFile())
  SetupCharacterWeapons(input$chbShowImprovWeapons)
}, ignoreNULL = TRUE, ignoreInit = TRUE)



# CHARACTER TITLE --------------------
# Name of hero / character
output$CharacterName <- renderPrint({
  if(!is.na(Character$Name)) {
    Result <- Character$Name
  } else Result <- i18n$t("No character has been uploaded")
  cat(Result)
})



# Json Panel ------------------------
output$ShowSetupJson <- reactive({
  return( !is.null(RawCharacterFile()) )
})
outputOptions(output, 'ShowSetupJson', suspendWhenHidden = FALSE)

output$RawContents <- renderPrint({
  req(RawCharacterFile())
  print(RawCharacterFile())
})



# Attributes Panel ---------------------
output$ShowSetupAttr <- reactive({
  return( !is.null(Character$Attr) )
})
outputOptions(output, 'ShowSetupAttr', suspendWhenHidden = FALSE)

output$SetupAttr <- renderTable({
  Result <- Character$Attr
  # Show names not codes
  Language <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
  NameMapping <- GetAbilities(Language)
  colnames(Result) <- NameMapping[match(names(Result), NameMapping[["attrID"]]), "shortname"]
  
  Result
}, rownames = FALSE, na = "-", digits = 0L)



# Skills Panels ---------------------
output$ShowSetupSkills <- reactive({
  return( !is.null(Character$Skills) )
})
outputOptions(output, 'ShowSetupSkills', suspendWhenHidden = FALSE)

output$SetupSkills <- renderTable({
  Result <- Character$Skills
  Result <- Result[-which(names(Result) == "attrID")]
  Result <- Result[-which(names(Result) == "classID")]
  # Show names not codes
  Language <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
  NameMapping <- GetAbilities(Language)
  for (ablty in paste0("ab", 1:3)) {
    Result[[ablty]] <- NameMapping[match(Result[[ablty]], NameMapping[["attrID"]]), "shortname"]
  }
  # Column names
  colnames(Result) <- c(i18n$t("Skill"), i18n$t("Set"), paste(i18n$t("SC"), 1:3), i18n$t("SR"))
  
  Result
}, rownames = FALSE, na = "-", digits = 0L, hover = TRUE)



output$ShowSetupSpells <- reactive({
  return( !is.null(Character$MagicSkills) )
})
outputOptions(output, 'ShowSetupSpells', suspendWhenHidden = FALSE)

output$SetupSpells <- renderTable({
  Result <- Character$MagicSkills
  Result <- Result[-which(names(Result) == "url")]
  Result <- Result[-which(names(Result) == "spellid")]
  
  # Show names not codes
  Language <- ifelse(length(i18n$translation_language) == 0L, "en", i18n$translation_language)
  NameMapping <- GetAbilities(Language)
  for (ablty in paste0("ab", 1:3)) {
    Result[[ablty]] <- NameMapping[match(Result[[ablty]], NameMapping[["attrID"]]), "shortname"]
  }
  # Column names
  colnames(Result) <- c(i18n$t("Skill"), i18n$t("Zauber"), paste(i18n$t("SC"), 1:3), i18n$t("Modifier"), "___", i18n$t("SR"))
  
  Result
})


# Combat Panel ------------------------
output$SetupWeapons <- renderTable({
  req(Character$Weapons)
  WT <- data.frame(a = character(), b = character(), c = integer(), d = integer(), e = character(),
                   stringsAsFactors = FALSE)
  for (W in Character$Weapons) {
    WT <- rbind(WT, list(W$Name, i18n$t(names(W$Type)),
                         as.integer(W$Skill$Attack), as.integer(W$Skill$Parry),
                         sprintf(i18n$t("%dd%d+%d"), W$Damage$N, W$Damage$DP, W$Damage$Bonus)),
                stringsAsFactors = FALSE)
  }
  names(WT) <- i18n$t(c("Name", "Type", "AT", "PA", "Hit points"))
  return(WT)
}, rownames = TRUE, na = "-")

