
sidebarLayout(
  sidebarPanel(
    sliderInput("ATValue", "AT", min = 1, max = 20, value = 11),
    sliderInput("PAValue", "PA", min = 1, max = 20, value = 4),
    numericInput("Damage", "Damage W6+", value = 1),
  ),
  mainPanel(
    actionButton("doAttackThrow", "Attack!", icon = icon("skull"), width = "49%"),
    actionButton("doParryThrow", "Parry!", icon = icon("shield-alt"), width = "49%"),
    hr(),
    h3(textOutput("CombatAction")),
    conditionalPanel(
      condition = "output.ShowCombatConfirm",
      textOutput("CombatConfirm")
    ),
    conditionalPanel(
      condition = "output.ShowCombatDamage",
      textOutput("CombatDamage")
    ),
    conditionalPanel(
      condition = "output.ShowCombatFumble",
      actionButton("doCombatFumble", "See what happens...", icon = icon("shield-alt")),
      textOutput("CombatFumble")
    )
    
  )
)
