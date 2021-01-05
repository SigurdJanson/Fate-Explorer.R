
sidebarLayout(
  sidebarPanel(
    # Select source of ability values: manual or from character sheet
    conditionalPanel(condition = "output.ShowAbilitySoureSelection",
             radioGroupButtons(inputId = "rdbAbilitySource", label = i18n$t("Where do your values come from?"),
                               choiceNames = i18n$t(c("Input", "Character")), 
                               choiceValues = c("ManualAbility", "CharAbility"), 
                               justified = TRUE)
    ),
    # Button for each ability
    conditionalPanel(condition = "output.ShowCharacterAbilities",
        radioGroupButtons(inputId = "rdbCharacterAbility",
               choiceNames = i18n$t(c("COU", "SAG", "INT", "CHA", "DEX", "AGI", "CON", "STR")),
               choiceValues = paste0("ATTR_", 1:8),
               justified = TRUE
             )
    ),
    # Sliders to set values
    sliderInput("inpAbility", i18n$t("Ability"), min = 1L, max = 20L, value = 11L, step = 1L),
    # Modifier
    div(style="float:right",
        conditionalPanel(condition = "input.inpAbilityMod < 0", 
                         gicon("minus-circle"), i18n$t("Impediment")),
        conditionalPanel(condition = "input.inpAbilityMod > 0", 
                         gicon("plus-circle"), i18n$t("Advantage"))
    ),
    sliderInput("inpAbilityMod", i18n$t("Modifier"), min = -10L, max = 10L, value = 0L, step = 1L)
  ),
  mainPanel(
    actionButton("doAbilityRoll", span(i18n$t("Ability"), id="lbldoAbilityRoll"), icon = gicon("body-swapping"), 
               width = "100%", style = "font-size: 140%"),
    hr(),
    htmlOutput("AbilityRoll")
  )
)
