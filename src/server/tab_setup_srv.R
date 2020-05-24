# SETUP

Character <- reactiveValues(Name = "No character has been uploaded",
                            Attr = NULL, Weapons = NULL)


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
  
  Character$Name <- Data$name
  Character$Attr <- GetAbilities_Opt(Data[["attr"]][["values"]])
  
  Character$Weapons <- GetWeapons_Opt(Data[["belongings"]][["items"]], Data[["ct"]], Character$Attr)
  updateSelectInput(session, "CombatSelectWeapon", choices = Character$Weapons[1,])
  
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
}, rownames = FALSE, na = "-", digits = 0)


# Weapons Panel ------------------------
output$ShowSetupWeapons <- reactive({
  return( !is.null(Character$Weapons) )
})
outputOptions(output, 'ShowSetupWeapons', suspendWhenHidden = FALSE)

output$SetupWeapons <- renderTable({
  Character$Weapons
}, rownames = TRUE, na = "-")



