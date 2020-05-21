
sidebarLayout(
  sidebarPanel(
    conditionalPanel(
      condition = "output.ShowSetupWeapons",  #see "tab_setup_srv"
      checkboxInput("PredefinedWeapon", "Use Weapon from Character", TRUE)
    ),
    conditionalPanel(
      condition = "output.ShowPredefinedWeapons",
      selectizeInput("CombatSelectWeapon", "Select Weapon", choices = "",
                     options = list(
                         placeholder = 'Select your weapon',
                         onInitialize = I('function() { this.setValue(""); }'))
      )
    ),
    hr(),
    sliderInput("ATValue", "Attack", min = 1, max = 20, value = 11),
    sliderInput("PAValue", "Parry",  min = 1, max = 20, value = 4),
    splitLayout(cellWidths = c("12rem", "12rem"),
        numericInput("DamageDieCount", "Dice 4 Damage", value = 1, min = 1, width = "12rem"),
        numericInput("Damage", "Modifier", value = 2, min = 0, width = "12rem")
    ),
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
