
sidebarLayout(
  sidebarPanel("Using the settings on the skill tab"
  ),
  mainPanel(
    "Skill Roll",
    plotOutput("imgProbabilities", width = "100%", height = "300px")#,
    #"Combat Roll",
    #plotOutput("imgAttackChances", width = "100%", height = "300px")
  )
)
