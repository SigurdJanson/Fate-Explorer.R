
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
    sliderInput("ATValue", i18n$t("Attack"), min = 1L, max = 20L, value = 11L, step = 1L),
    sliderInput("PAValue", i18n$t("Parry"),  min = 1L, max = 20L, value = 4L, step = 1L),
    splitLayout(cellWidths = c("49%", "49%"),
        numericInput("DamageDieCount", i18n$t("Dice 4 Damage"), value = 1L, min = 1L, width = "100%"),
        numericInput("Damage", i18n$t("Modifier"), value = 2L, min = 0L, width = "100%")
    ),
    hr(),
    sliderInput("DodgeValue", i18n$t("Dodge"),  min = 1L, max = 10L, value = 5L, step = 1L),
    hr(),
    sliderTextInput("CombatPenalty", i18n$t("Penalty"),  choices = seq(from = 0L, to = -10L, by = -1L),
                    selected = 0L, grid = TRUE)
    ),
  mainPanel(
    actionButton("doAttackThrow", i18n$t("Attack"), icon = icon("skull"), width = "32%"),
    actionButton("doParryThrow", i18n$t("Parry"), icon = icon("shield-alt"), width = "32%"),
    actionButton("doDodge", i18n$t("Dodge"), icon = icon("running"), width = "32%"),
    hr(),
    h3(textOutput("CombatAction")),
    conditionalPanel(
      condition = "output.ShowCombatConfirm",
      actionLink("doCombatConfirm", "-"),
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
    ),
    conditionalPanel(
      condition = "output.ShowWeaponDetails",
      hr(),
      htmlOutput("WeaponDetails")
    )
  )
)
