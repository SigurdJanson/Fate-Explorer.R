
sidebarLayout(
  sidebarPanel(
    fluidRow(
      column(width=5,
           conditionalPanel(
               condition = "output.ShowSetupWeapons",  #see "tab_setup_srv"
               checkboxInput("chbPredefinedWeapon", i18n$t("Use Weapon Character"), FALSE)
      )),
      column(width = 7,
           conditionalPanel(
               condition = "output.ShowPredefinedWeapons",
               selectizeInput("cmbCombatSelectWeapon", NULL, choices = "",
                              options = list(
                                placeholder = i18n$t("Select your weapon"),
                                onInitialize = I('function() { this.setValue(""); }')))
      ))
    ),

    hr(),
    sliderInput("AttackValue", i18n$t("Attack"), min = 1L, max = 20L, value = 11L, step = 1L),
    sliderInput("ParryValue", i18n$t("Parry"),  min = 1L, max = 20L, value = 7L, step = 1L),
    splitLayout(cellWidths = c("49%", "49%"),
        numericInputIcon("DamageDieCount", i18n$t("Dice 4 Damage"), value = 1L, min = 1L, 
                         width = "100%", icon = list(NULL, "W6")),
        numericInputIcon("Damage", i18n$t("Modifier"), value = 2L, min = 0L, 
                         width = "100%", icon = list("+"))
    ),
    hr(),
    sliderInput("DodgeValue", i18n$t("Dodge"),  min = 1L, max = 10L, value = 5L, step = 1L),
    hr(),
    sliderTextInput("CombatPenalty", i18n$t("Penalty"),  choices = seq(from = 0L, to = -10L, by = -1L),
                    selected = 0L, grid = TRUE)
    ),
  mainPanel(
    actionButton("doAttackThrow", i18n$t("Attack"), icon = gicon("battle-axe", lib = "gameicon"),
                 width = "32%", style = "font-size: 140%"),
    actionButton("doParryThrow", i18n$t("Parry"), icon = gicon("shield", lib = "gameicon"),
                 width = "32%", style = "font-size: 140%"),
    actionButton("doDodge", i18n$t("Dodge"), icon = gicon("dodging", lib = "gameicon"),
                 width = "32%", style = "font-size: 140%"),

    hr(),
    htmlOutput("uiCombatRoll"),
    
    conditionalPanel(
      condition = "output.ShowWeaponDetails",
      hr(),
      htmlOutput("WeaponDetails")
    ),
    conditionalPanel(
      condition = "output.ShowExploreFightingChances",
      h3(i18n$t("Combat Roll")),
      plotOutput("imgAttackChances", width = "100%", height = "200px")
    )
  )
)
