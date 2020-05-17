# SETUP

# JSON Schema: optolith-client/app/Schema/Hero/Hero.experimental.schema.json
# JSON Schema: optolith-client/app/Schema/Hero/Hero.schema.json

Character <- reactiveValues(Name = "No character has been uploaded", Attr = numeric())


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
  
  print(Data)
})


observeEvent(input$doTestChallenge, {
  Character$Name <- paste(Character$Name, sample(letters, 1))
  #session$sendCustomMessage(type = 'testmessage',
  #                          message = 'Thank you for clicking')
})
