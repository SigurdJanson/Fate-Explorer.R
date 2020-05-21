# SETUP


sidebarLayout(
  sidebarPanel(
    fileInput("CharFile", "Choose Optholit json File",
              multiple = FALSE,
              accept = c("text/json","text/plain",".json")),
    hr(),
    actionButton("doTestChallenge", "test")
  ),
  mainPanel(
    h3(textOutput("CharacterName")),
    conditionalPanel(
      condition = "output.ShowSetupAttr",
      hr(), h3("Attributes"),
      tableOutput("SetupAttr")
    ),
    conditionalPanel(
      condition = "output.ShowSetupWeapons",
      hr(), h3("Weapons"),
      tableOutput("SetupWeapons")
    ),
    #conditionalPanel(
    #  condition = "output.ShowSetupJson",
      hr(), h3("Json File"),
      textOutput("RawContents")
    #)
  )
)

