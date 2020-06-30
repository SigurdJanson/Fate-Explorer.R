# SETUP

Character <- reactiveValues(Name = "No character has been uploaded",
                            Attr = NULL, Skills = NULL, Weapons = NULL,
                            CombatSkills = NULL)
RawCharacterFile <- reactiveVal(NULL) # raw data container of json content


# Global function to extract the list of weapons from Json
SetupCharacterWeapons <- function(AddImprov = FALSE) {
  Character$Weapons <- GetWeapons_Opt(RawCharacterFile()[["belongings"]][["items"]], # belongings
                                      Character$CombatSkills,  # combat techniques
                                      Character$Attr,
                                      AddUnarmed = TRUE, AddImprov = AddImprov)
  # Correct weapon's hit points
  for (w in 1:ncol(Character$Weapons)) {
    ID <- Character$Weapons["Name", w]
    if ( input$chbShowImprovWeapons && IsImprovisedWeapon(ID) ) {
      Bonus  <- GetHitpointBonus(ID, Character$Attr)
      Character$Weapons["DamageMod", w] <- as.numeric(Character$Weapons["DamageMod", w]) + Bonus
    }
  }
  
  #TODO: Correct for encumbrance: TODO (EEC = Effective Encumbrance)
  
  # Update dropdown list on Combat Tab
  updateSelectInput(session, "cmbCombatSelectWeapon", choices = Character$Weapons[1,], selected = 1)
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
  #Character$Weapons <- GetWeapons_Opt(Data[["belongings"]][["items"]], Data[["ct"]], Character$Attr)
  SetupCharacterWeapons(input$chbShowImprovWeapons)
  
  # THIS SECTION IS A BIT OUT OF PLACE HERE
  # Update dropdown list on Skills Tab
  UpdateSkillSourceRadioButton(session, IsCharacterLoaded = TRUE )
  updateSelectInput(session, "lbCharSkills", choices = Character$Skills[, "name"])
  updateSelectInput(session, "lbSkillGroups", choices = c('All Skills' = '', unique(Character$Skills[, "class"])))
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



# Skills Panel ---------------------
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



# Combat Panel ------------------------
output$SetupWeapons <- renderTable({
  req(Character$Weapons)
  Character$Weapons
}, rownames = TRUE, na = "-")

