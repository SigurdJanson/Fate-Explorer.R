
sidebarLayout(
  sidebarPanel(
    conditionalPanel(
      condition = "output.ShowSetupWeapons",  #see "tab_setup_srv"
      checkboxInput("PredefinedWeapon", i18n$t("Use Weapon from Character"), TRUE)
    ),
    conditionalPanel(
      condition = "output.ShowPredefinedWeapons",
      selectizeInput("CombatSelectWeapon", i18n$t("Select Weapon"), choices = "",
                     options = list(
                         placeholder = i18n$t("Select your weapon"),
                         onInitialize = I('function() { this.setValue(""); }'))
      )
    ),
    hr(),
    sliderInput("ATValue", i18n$t("Attack"), min = 1, max = 20, value = 11),
    sliderInput("PAValue", i18n$t("Parry"),  min = 1, max = 20, value = 4),
    splitLayout(cellWidths = c("49%", "49%"),
        numericInput("DamageDieCount", i18n$t("Dice 4 Damage"), value = 1, min = 1, width = "100%"),
        numericInput("Damage", i18n$t("Modifier"), value = 2, min = 0, width = "100%")
    ),
    hr(),
    sliderInput("DodgeValue", i18n$t("Dodge"),  min = 1, max = 10, value = 5),
  ),
  mainPanel(
    actionButton("doAttackThrow", i18n$t("Attack"), icon = icon("skull"), width = "32%"),
    actionButton("doParryThrow", i18n$t("Parry"), icon = icon("shield-alt"), width = "32%"),
    actionButton("doDodge", i18n$t("Dodge"), icon = icon("running"), width = "32%"),
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
      actionLink("doCombatFumble", i18n$t("See what happens..."), icon = icon("shield-alt")),
      textOutput("CombatFumble")
    )
  )
)
