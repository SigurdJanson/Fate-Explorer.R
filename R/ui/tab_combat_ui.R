
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
    sliderInput("inpAttackValue", i18n$t("Attack"), min = 1L, max = 20L, value = 11L, step = 1L),
    sliderInput("inpParryValue", i18n$t("Parry"),  min = 1L, max = 20L, value = 7L, step = 1L),
    splitLayout(cellWidths = c("49%", "49%"),
        numericInputIcon("inpDamageDieCount", i18n$t("Dice 4 Damage"), value = 1L, min = 1L, 
                         width = "100%", icon = list(NULL, "W6")),
        numericInputIcon("inpDamage", i18n$t("Modifier"), value = 2L, min = 0L, 
                         width = "100%", icon = list("+"))
    ),
    hr(),
    sliderInput("inpDodgeValue", i18n$t("Dodge"),  min = 1L, max = 10L, value = 5L, step = 1L),
    hr(),
    div(style="float:right",
        conditionalPanel(condition = "input.inpCombatMod < 0", 
                         gicon("minus-circle"), i18n$t("Impediment")),
        conditionalPanel(condition = "input.inpCombatMod > 0", 
                         gicon("plus-circle"), i18n$t("Advantage"))
    ),
    sliderInput("inpCombatMod", i18n$t("Modifier"),  min = -10L, max = 10L, step = 1L, value = 0L)
    ),
  mainPanel(
    actionButton("doAttackThrow", i18n$t("Attack"), icon = gicon("battle-axe"),
                 width = "32%", style = "font-size: 140%"),
    actionButton("doParryThrow", i18n$t("Parry"), icon = gicon("shield"),
                 width = "32%", style = "font-size: 140%"),
    actionButton("doDodge", i18n$t("Dodge"), icon = gicon("dodge"),
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
