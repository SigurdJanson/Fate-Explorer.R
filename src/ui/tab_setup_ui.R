# SETUP


sidebarLayout(
  sidebarPanel(
    #"Import your character",
    #runif(1, 1, 100),
    fileInput("CharFile", "Choose Optholit json File",
              multiple = FALSE,
              accept = c("text/json","text/plain",".json")),
    hr(),
    actionButton("doTestChallenge", "test")
  ),
  mainPanel(
    h3(textOutput("CharacterName")),
    conditionalPanel(
      condition = "output.ShowSetupWeapons",
      hr(), h3("Weapons"),
      tableOutput("SetupWeapons")
    ),
    hr(), h3("Json File"),
    textOutput("RawContents")
  )
)

