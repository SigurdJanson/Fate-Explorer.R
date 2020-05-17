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
    hr(),
    textOutput("RawContents")
  )
)

