# SETUP

# JSON Schema: optolith-client/app/Schema/Hero/Hero.experimental.schema.json
# JSON Schema: optolith-client/app/Schema/Hero/Hero.schema.json

Character <- reactiveValues(Name = "No character has been uploaded", 
                            Attr = NULL, Weapons = NULL)


output$CharacterName <- renderPrint({
  if(!is.na(Character$Name)) {
    Result <- Character$Name
  } else Result <- "No character has been uploaded"
  cat(Result)
})

# Json Panel ------------------------
output$ShowSetupJson <- reactive({
  return( !is.null(Character$Attr) )
})
outputOptions(output, 'ShowSetupJson', suspendWhenHidden = FALSE)

output$RawContents <- renderPrint({
  req(input$CharFile)
  
  Data <- fromJSON(file = input$CharFile$datapath)
  
  Character$Name <- Data$name
  Character$Attr <- GetAbilities_Opt(Data[["attr"]][["values"]])
  
  Character$Weapons <- GetWeapons_Opt(Data[["belongings"]][["items"]], Data[["ct"]], Character$Attr)
  updateVarSelectInput(session, "CombatSelectWeapon", data = Character$Weapons)
  
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



# TESTING  ----------------
observeEvent(input$doTestChallenge, {
  Character$Name <- paste(Character$Name, sample(letters, 1))
})
