# SETUP

Character <- reactiveValues(Name = "No character has been uploaded",
                            Attr = NULL, Skills = NULL, Weapons = NULL)


output$CharacterName <- renderPrint({
  if(!is.na(Character$Name)) {
    Result <- Character$Name
  } else Result <- i18n$t("No character has been uploaded")
  cat(Result)
})





# Json Panel ------------------------
output$ShowSetupJson <- reactive({
  return( !is.null(Character$Attr) )
})
outputOptions(output, 'ShowSetupJson', suspendWhenHidden = FALSE)

output$RawContents <- renderPrint({
  req(input$CharFile)

  Data <- read_json(path = input$CharFile$datapath)
  
  Character$Name    <- Data$name
  Character$Attr    <- GetAbilities_Opt(Data[["attr"]][["values"]])
  Character$Skills  <- GetSkills_Opt(Data[["talents"]])
  Character$Weapons <- GetWeapons_Opt(Data[["belongings"]][["items"]], Data[["ct"]], Character$Attr)
  # Update dropdown list on Combat Tab
  updateSelectInput(session, "CombatSelectWeapon", choices = Character$Weapons[1,])
  # Update dropdown list on Skills Tab
  updateSelectInput(session, "lbCharSkills", choices = Character$Skills[, "name"])
  updateSelectInput(session, "lbSkillGroups", choices = c('All Skills' = '', unique(Character$Skills[, "class"])))
  
  print(Data)
})



# Attributes Panel ---------------------
output$ShowSetupAttr <- reactive({
  return( !is.null(Character$Attr) )
})
outputOptions(output, 'ShowSetupAttr', suspendWhenHidden = FALSE)

output$SetupAttr <- renderTable({
  Result <- Character$Attr
  # Show names not codes
  NameMapping <- GetAbilities()
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
  NameMapping <- GetAbilities()
  for (ablty in paste0("ab", 1:3)) {
    Result[[ablty]] <- NameMapping[match(Result[[ablty]], NameMapping[["attrID"]]), "shortname"]
  }
  # Column names
  colnames(Result) <- c(i18n$t("Skill"), i18n$t("Set"), 
                        paste(i18n$t("SC"), 1:3), i18n$t("SR"))
  
  Result
}, rownames = FALSE, na = "-", digits = 0L, hover = TRUE)


# Weapons Panel ------------------------
output$ShowSetupWeapons <- reactive({
  return( !is.null(Character$Weapons) )
})
outputOptions(output, 'ShowSetupWeapons', suspendWhenHidden = FALSE)

output$SetupWeapons <- renderTable({
  Character$Weapons
}, rownames = TRUE, na = "-")



