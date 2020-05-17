# SETUP

# JSON Schema: optolith-client/app/Schema/Hero/Hero.experimental.schema.json
# JSON Schema: optolith-client/app/Schema/Hero/Hero.schema.json

Character <- reactiveValues(Name = "No character has been uploaded", 
                            Attr = numeric(), Weapons = NULL)


output$CharacterName <- renderPrint({
  if(!is.na(Character$Name)) {
    Result <- Character$Name
  } else Result <- "No character has been uploaded"
  cat(Result)
})


output$RawContents <- renderPrint({
  req(input$CharFile)

  Data <- fromJSON(file = input$CharFile$datapath)

  Character$Name <- Data$name
  Character$Attr <- Data$attr
  Character$Weapons <- GetWeapons_Opt(Data$belongings$items)
  
  print(Data)
})

# Damage Panel
output$ShowSetupWeapons <- reactive({
  return( !is.null(Character$Weapons) )
})
outputOptions(output, 'ShowSetupWeapons', suspendWhenHidden = FALSE)

output$SetupWeapons <- renderTable({
  Character$Weapons
}, rownames = TRUE, na = "-")


observeEvent(input$doTestChallenge, {
  Character$Name <- paste(Character$Name, sample(letters, 1))
})
