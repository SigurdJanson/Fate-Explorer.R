
sidebarLayout(
  sidebarPanel(
    sliderInput("ATValue", "Attack", min = 1, max = 20, value = 11),
    sliderInput("PAValue", "Parry",  min = 1, max = 20, value = 4),
    numericInput("Damage", "Damage W6+", value = 1),
    hr(),
    sliderInput("DodgeValue",   "Dodge",  min = 1, max = 10, value = 5),
  ),
  mainPanel(
    actionButton("doAttackThrow", "Attack", icon = icon("skull"), width = "32%"),
    actionButton("doParryThrow", "Parry", icon = icon("shield-alt"), width = "32%"),
    actionButton("doDodge", "Dodge", icon = icon("running"), width = "32%"),
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
      hr(),
      actionLink("doCombatFumble", "See what happens...", icon = icon("shield-alt")),
      textOutput("CombatFumble")
    )
  )
)
